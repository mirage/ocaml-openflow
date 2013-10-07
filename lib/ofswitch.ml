(*
 * Copyright (c) 2011 Richard Mortier <mort@cantab.net>
 * Copyright (c) 2011 Charalampos Rotsos <cr409@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt

open Ofswitch_config

module OP = Openflow.Ofpacket
module OSK = Openflow.Ofsocket 

exception Packet_type_unknw

let sp = Printf.sprintf
let pr = Printf.printf
let pp = Printf.printf
let ep = Printf.eprintf
let cp = OS.Console.log

(* XXX should really stndardise these *)
type uint16 = OP.uint16
type uint32 = OP.uint32
type uint64 = OP.uint64
type byte   = OP.byte

type port = uint16
type cookie = uint64

type device = string (* XXX placeholder! *)

let resolve t = Lwt.on_success t (fun _ -> ())

let get_new_buffer len = 
  let buf = OS.Io_page.to_cstruct (OS.Io_page.get 1) in 
    Cstruct.sub buf 0 len 

let get_ethif mgr id = 
    let lst = Net.Manager.get_intfs mgr in 
    let (_, ethif) = List.find (fun (dev_id,_) -> id = dev_id) lst in 
    ethif
 
module Entry = struct
  type table_counter = {
    n_active: uint32;
    n_lookups: uint64;
    n_matches: uint64;
  }

  type flow_counter = {
    mutable n_packets: uint64;
    mutable n_bytes: uint64;
    flags : OP.Flow_mod.flags; 
    priority: uint16;
    cookie: int64;
    insert_sec: int;
    insert_nsec: int;
    mutable last_sec: int;
    mutable last_nsec: int;
    idle_timeout: int;
    hard_timeout:int;
  }

  type queue_counter = {
    tx_queue_packets: uint64;
    tx_queue_bytes: uint64;
    tx_queue_overrun_errors: uint64;
  }

  let init_flow_counters t =
    let ts = int_of_float (OS.Clock.time ()) in
    {n_packets=0L; n_bytes=0L; priority=t.OP.Flow_mod.priority; 
     cookie=t.OP.Flow_mod.cookie; insert_sec=ts; insert_nsec=0; 
     last_sec=ts;last_nsec=0; idle_timeout=t.OP.Flow_mod.idle_timeout; 
    hard_timeout=t.OP.Flow_mod.hard_timeout; flags=t.OP.Flow_mod.flags; }


  type t = { 
    counters: flow_counter;
    actions: OP.Flow.action list;
    mutable cache_entries: OP.Match.t list;
  }
  let update_flow pkt_len flow = 
    flow.counters.n_packets <- Int64.add flow.counters.n_packets 1L;
    flow.counters.n_bytes <- Int64.add flow.counters.n_bytes pkt_len;
    flow.counters.last_sec <- int_of_float (OS.Clock.time ())


  let flow_counters_to_flow_stats of_match table_id flow =
    let priority = flow.counters.priority in
    let idle_timeout=flow.counters.idle_timeout in
    let hard_timeout=flow.counters.hard_timeout in
    let cookie=flow.counters.cookie in
    let packet_count=flow.counters.n_packets in
    let byte_count=flow.counters.n_bytes in
    let action=flow.actions in
    OP.Flow.({table_id; of_match; 
    duration_sec = Int32.of_int (flow.counters.last_sec -
    flow.counters.insert_sec);
    duration_nsec = Int32.of_int (flow.counters.last_nsec -
    flow.counters.insert_nsec);
    priority; idle_timeout; hard_timeout; cookie;
    packet_count; byte_count; action; })

end

module Table = struct
  type t = {
    tid: cookie;
    (* This entry stores wildcard and exact match entries as
     * transmitted by the controller *)
    mutable entries: (OP.Match.t, Entry.t) Hashtbl.t;
    (* Intermediate table to store exact match flos deriving from wildcard
     * entries *)
    mutable cache : (OP.Match.t, Entry.t ref) Hashtbl.t;
    stats : OP.Stats.table;
  }

  let init_table () = 
    { tid = 0_L; entries = (Hashtbl.create 10000); cache = (Hashtbl.create 10000);
    stats = OP.Stats.(
      {table_id=(OP.Stats.table_id_of_int 1); name="main_tbl"; 
      wildcards=(OP.Wildcards.exact_match ()); max_entries=1024l; active_count=0l; 
      lookup_count=0L; matched_count=0L});}

  (* TODO fix flow_mod flag support. overlap is not considered *)
  let add_flow st table t verbose =
    (* TODO check if the details are correct e.g. IP type etc. *)
    let open OP.Flow_mod in 
    let open OP.Match in 
    let _ =
      (* max priority for exact match rules *)
      if  (t.of_match.wildcards=(OP.Wildcards.exact_match ())) then
        t.priority <- 0x1001
    in
    let entry = Entry.({actions=t.OP.Flow_mod.actions; counters=(init_flow_counters t); 
             cache_entries=[];}) in  
    let _ = Hashtbl.replace table.entries t.of_match entry in
    (* In the fast path table, I need to delete any conflicting entries *)
    let _ = 
      Hashtbl.iter (
        fun a e -> 
          if ((flow_match_compare a t.of_match t.of_match.wildcards) && 
              Entry.(entry.counters.priority >= (!e).counters.priority)) then ( 
                let _ = (!e).Entry.cache_entries <- 
                  List.filter (fun c -> a <> c) (!e).Entry.cache_entries in 
                let _ = Hashtbl.replace table.cache a (ref entry) in 
                  entry.Entry.cache_entries <- a :: entry.Entry.cache_entries
              )
      ) table.cache in
    let _ = if (verbose) then 
        cp (sp "[switch] Adding flow %s" (OP.Match.match_to_string t.of_match))
    in
    return ()

  (* check if a list of actions has an output action forwarding packets to
   * out_port.
   * Used when removing a port from the switch control in order to clean related
   * flows *)
  let rec is_output_port out_port = function
    | [] -> false
    | OP.Flow.Output(port, _)::_ when (port = out_port) -> true
    | head::tail -> is_output_port out_port tail 

  let del_flow table ?(xid=(Random.int32 Int32.max_int)) 
        ?(reason=OP.Flow_removed.DELETE) tuple out_port t verbose =
    (* Delete all matching entries from the flow table*)
    let remove_flow = 
      Hashtbl.fold (
        fun of_match flow ret -> 
          if ((OP.Match.flow_match_compare of_match tuple
                  tuple.OP.Match.wildcards) && 
              ((out_port = OP.Port.No_port) || 
               (is_output_port out_port flow.Entry.actions))) then ( 
            let _ = Hashtbl.remove table.entries of_match in 
            
            (* log removal of flow *)
(*            let _ = 
              match Lwt.get OS.Topology.node_name with
              | None -> ()
              | Some(node_name) -> 
                  let flow_str = OP.Match.match_to_string of_match in
                  let action_str = OP.Flow.string_of_actions flow.Entry.actions in
                  let msg = Rpc.Dict [ 
                    ("name", (Rpc.String node_name));
                    ("type", (Rpc.String "del"));
                    ("flow", (Rpc.String flow_str)); 
                    ("action", (Rpc.String action_str));] in
                    OS.Console.broadcast "flow" (Jsonrpc.to_string msg)
    in *)
               (of_match, flow)::ret
          ) else ret
          ) table.entries [] in

    (* Delete all entries from cache *)
    let _ = 
      List.iter (
        fun (_, flow) -> 
          List.iter (Hashtbl.remove table.cache) flow.Entry.cache_entries
      ) remove_flow in 

    (* Check for notification flag in flow and send 
    * flow modification warnings *)
      Lwt_list.iter_s (
      fun (of_match, flow) ->
        let _ = 
          if verbose then
            cp (sp "[switch] Removing flow %s" (OP.Match.match_to_string of_match)) 
        in 
        match(t, flow.Entry.counters.Entry.flags.OP.Flow_mod.send_flow_rem) with
        | (Some t, true) -> 
          let duration_sec = (int_of_float (OS.Clock.time ()))  -
            flow.Entry.counters.Entry.insert_sec in
          let fl_rm = OP.Flow_removed.(
            {of_match; cookie=flow.Entry.counters.Entry.cookie; 
            priority=flow.Entry.counters.Entry.priority;
            reason; duration_sec=(Int32.of_int duration_sec); duration_nsec=0l;
            idle_timeout=flow.Entry.counters.Entry.idle_timeout;
            packet_count=flow.Entry.counters.Entry.n_packets;
            byte_count=flow.Entry.counters.Entry.n_bytes;}) in
          let h = OP.Header.(create ~xid FLOW_REMOVED (OP.Flow_removed.get_len)) in
              OSK.send_packet t (OP.Flow_removed (h,fl_rm)) 
        | _ -> return ()
    ) remove_flow 

  (* table stat update methods *)
  let update_table_found table =
    let open OP.Stats in 
    table.stats.lookup_count <- Int64.add table.stats.lookup_count 1L;
    table.stats.matched_count <- Int64.add table.stats.matched_count 1L

  let update_table_missed table =
    let open OP.Stats in 
    table.stats.lookup_count <- Int64.add table.stats.lookup_count 1L

  (* monitor thread to timeout flows *)
  let monitor_flow_timeout table t verbose = 
    let open Entry in 
    let check_flow_timeout table t verbose = 
      let ts = int_of_float (OS.Clock.time ()) in 
      let flows = Hashtbl.fold (
        fun of_match entry ret -> 
          let hard = ts - entry.counters.insert_sec in
          let idle = ts - entry.counters.last_sec in
          match (hard, idle) with 
            | (l, _) when ((entry.counters.hard_timeout > 0) && 
                           (l >= entry.counters.hard_timeout)) ->
                (of_match, entry, OP.Flow_removed.HARD_TIMEOUT )::ret
            | (_, l) when ((entry.counters.idle_timeout > 0) &&
                           (l >= entry.counters.idle_timeout)) ->
                ret @ [(of_match, entry, OP.Flow_removed.IDLE_TIMEOUT )]
            | _ -> ret 
      ) table.entries [] in 
        Lwt_list.iter_s (
          fun (of_match, entry, reason) -> 
            del_flow table ~reason of_match OP.Port.No_port t verbose
        ) flows
    in
    while_lwt true do 
      lwt _ = OS.Time.sleep 1.0 in 
        check_flow_timeout table t verbose 
    done 
end

module Switch = struct
  type port = {
    mgr: Net.Manager.t;
    port_id: int;
    ethif: Net.Manager.id; 
    netif: OS.Netif.t;
    port_name: string;
    counter: OP.Port.stats;
    phy: OP.Port.phy;
    in_queue: Cstruct.t Lwt_stream.t;
    in_push : (Cstruct.t option -> unit);
    out_queue: Cstruct.t Lwt_stream.t;
    out_push : (Cstruct.t option -> unit);
    mutable pkt_count : int;
  }

  let init_port mgr port_no id = 
    let ethif = Net.Manager.get_ethif ( get_ethif mgr id ) in 
    let netif = Net.Ethif.get_netif ethif in 
    let name = OS.Netif.string_of_id (OS.Netif.id (Net.Ethif.get_netif ethif )) in 
    let hw_addr = Net.Ethif.mac ethif in
    let (in_queue, in_push) = Lwt_stream.create () in
    let (out_queue, out_push) = Lwt_stream.create () in
    let counter = OP.Port.(
        { port_id=port_no; rx_packets=0L; tx_packets=0L; rx_bytes=0L; 
          tx_bytes=0L; rx_dropped=0L; tx_dropped=0L; rx_errors=0L; 
          tx_errors=0L; rx_frame_err=0L; rx_over_err=0L; rx_crc_err=0L; 
          collisions=0L;}) in

    let features = OP.Port.(
        {pause_asym=true; pause=true; autoneg=true; fiber=true;
         copper=true; f_10GB_FD=true; f_1GB_FD=true; f_1GB_HD=true; 
         f_100MB_FD=true; f_100MB_HD=true; f_10MB_FD=true; 
         f_10MB_HD=true;}) in
    let port_config = OP.Port.(
        { port_down=false; no_stp=false; no_recv=false; 
          no_recv_stp=false; no_flood=false; no_fwd=false; 
          no_packet_in=false;}) in 
    let port_state = OP.Port.(
        {link_down =false; stp_listen =false; stp_learn =false;
         stp_forward =false; stp_block =false;}) in
    let phy = OP.Port.(
        {port_no; hw_addr;name; config= port_config;
         state= port_state; curr=features; advertised=features; 
         supported=features; peer=features;}) in
    
    {port_id=port_no; mgr; port_name=name; counter;
     ethif=id;netif;phy;in_queue;in_push;pkt_count=0;
        out_queue;out_push;}

  type stats = {
    mutable n_frags: uint64;
    mutable n_hits: uint64;
    mutable n_missed: uint64;
    mutable n_lost: uint64;
  }

  type lookup_ret = 
         Found of Entry.t ref
       | NOT_FOUND

  type t = {
    (* Mapping Netif objects to ports *)
    mutable dev_to_port: (Net.Manager.id, port ref) Hashtbl.t;

    (* Mapping port ids to port numbers *)
    mutable int_to_port: (int, port ref) Hashtbl.t;
    mutable ports : port list;
    mutable controller: OSK.conn_state option;
    mutable last_echo_req : float;
    mutable echo_resp_received : bool;
    table: Table.t;
    stats: stats;
    mutable errornum : uint32; 
    mutable portnum : int;
    features : OP.Switch.features;
    mutable packet_buffer: OP.Packet_in.t list;
    mutable packet_buffer_id: int32;
    ready : unit Lwt_condition.t ;
    verbose : bool;
  }
 let supported_actions () = 
   OP.Switch.({ output=true; set_vlan_id=true; set_vlan_pcp=true; strip_vlan=true;
   set_dl_src=true; set_dl_dst=true; set_nw_src=true; set_nw_dst=true;
   set_nw_tos=true; set_tp_src=true; set_tp_dst=true; enqueue=false;vendor=true; })
 let supported_capabilities () = 
   OP.Switch.({flow_stats=true;table_stats=true;port_stats=true;stp=true;
   ip_reasm=false;queue_stats=false;arp_match_ip=true;})
 let switch_features datapath_id = 
   OP.Switch.({datapath_id; n_buffers=0l; n_tables=(char_of_int 1); 
   capabilities=(supported_capabilities ()); actions=(supported_actions ()); 
   ports=[];})


  let update_port_tx_stats pkt_len port = 
    OP.Port.(port.counter.tx_packets <- Int64.add port.counter.tx_packets 1L);
    OP.Port.(port.counter.tx_bytes <- Int64.add port.counter.tx_bytes pkt_len)

  let update_port_rx_stats pkt_len port = 
    OP.Port.(port.counter.rx_packets <- Int64.add port.counter.rx_packets 1L);
    OP.Port.(port.counter.rx_bytes <- Int64.add port.counter.rx_bytes pkt_len)

  cstruct dl_header {
    uint8_t   dl_dst[6];
    uint8_t   dl_src[6]; 
    uint16_t  dl_type 
  } as big_endian

  cstruct arphdr {
    uint16_t ar_hrd;         
    uint16_t ar_pro;         
    uint8_t ar_hln;              
    uint8_t ar_pln;              
    uint16_t ar_op;          
    uint8_t ar_sha[6];  
    uint32_t nw_src;
    uint8_t ar_tha[6];  
    uint32_t nw_dst 
  } as big_endian

  cstruct nw_header {
    uint8_t        hlen_version;
    uint8_t        nw_tos;
    uint16_t       total_len;
    uint8_t        pad[5];
    uint8_t        nw_proto; 
    uint16_t       csum;
    uint32_t       nw_src; 
    uint32_t       nw_dst
  } as big_endian 

  cstruct tp_header {
    uint16_t tp_src;
    uint16_t tp_dst
  } as big_endian 

  cstruct icmphdr {
    uint8_t typ;
    uint8_t code;
    uint16_t checksum
  } as big_endian

  cstruct tcpv4 {
    uint16_t src_port;
    uint16_t dst_port;
    uint32_t sequence;
    uint32_t ack_number;
    uint32_t  dataoff_flags_window;
    uint16_t checksum
  } as big_endian

  cstruct pseudo_header {
    uint32_t src;
    uint32_t dst;
    uint8_t res;
    uint8_t proto;
    uint16_t len
  } as big_endian 

  let tcp_checksum ~src ~dst =
    let pbuf = Cstruct.sub (Cstruct.of_bigarray (OS.Io_page.get 1)) 0 sizeof_pseudo_header in
    fun data ->
      set_pseudo_header_src pbuf (Ipaddr.V4.to_int32  src);
      set_pseudo_header_dst pbuf (Ipaddr.V4.to_int32 dst);
      set_pseudo_header_res pbuf 0;
      set_pseudo_header_proto pbuf 6;
      set_pseudo_header_len pbuf (Cstruct.lenv data);
      Net.Checksum.ones_complement_list (pbuf::data)

  let send_packet port bits =
    update_port_tx_stats (Int64.of_int (Cstruct.len bits)) port;
    return (port.out_push (Some bits))
(*    OS.Netif.write port.netif bits *)
(*    Net.Manager.inject_packet port.mgr port.ethif bits *)

  
  let forward_frame st in_port bits pkt_size checksum port = 
    let _ = 
      if ((checksum) && ((get_dl_header_dl_type bits) = 0x800)) then
        let ip_data = Cstruct.shift bits sizeof_dl_header in
        let len = (get_nw_header_hlen_version ip_data) land 0xf in 
        let _ = set_nw_header_csum ip_data 0 in
        let csm = Net.Checksum.ones_complement (Cstruct.sub ip_data 0 (len*4)) in
        let _ = set_nw_header_csum ip_data csm in
        let _ = 
          match (get_nw_header_nw_proto ip_data) with
          | 6 (* TCP *) -> 
              let src = Ipaddr.V4.of_int32 (get_nw_header_nw_src
              ip_data) in 
              let dst = Ipaddr.V4.of_int32 (get_nw_header_nw_dst
              ip_data) in 
              let tp_data = Cstruct.shift ip_data (len*4) in  
              let _ = set_tcpv4_checksum tp_data 0 in
              let csm = tcp_checksum ~src ~dst [tp_data] in 
                set_tcpv4_checksum tp_data csm  
          | 17 (* UDP *) -> ()
          | _ -> ()
        in
          ()
    in 
    match port with 
    | OP.Port.Port(port) -> 
      if Hashtbl.mem st.int_to_port port then
        let out_p = (!( Hashtbl.find st.int_to_port port))  in
        send_packet out_p bits 
(*          Net.Manager.inject_packet out_p.mgr out_p.ethif bits *)
      else
        return (cp (sp "[switch] forward_frame: Port %d not registered\n%!" port))
    | OP.Port.No_port -> return ()
    | OP.Port.Flood 
    |OP.Port.All ->
      Lwt_list.iter_p  
        (fun port -> 
           if(port.port_id != (OP.Port.int_of_port in_port)) then 
             send_packet port bits
           else 
             return ()
        ) st.ports
    | OP.Port.In_port ->
      let port = (OP.Port.int_of_port in_port) in 
      if Hashtbl.mem st.int_to_port port then
        send_packet (!(Hashtbl.find st.int_to_port port))  bits
      else
        return (cp (sp "[switch] forward_frame: Port %d unregistered\n%!" port))
    | OP.Port.Local ->
      let local = OP.Port.int_of_port OP.Port.Local in 
      if Hashtbl.mem st.int_to_port local then
        send_packet !(Hashtbl.find st.int_to_port local) bits
      else
        return (cp (sp "[switch] forward_frame: Port %d unregistered \n%!" local))
    | OP.Port.Controller -> begin 
      let size = 
         if (Cstruct.len bits > pkt_size) then
           pkt_size
         else 
           Cstruct.len bits
       in
       let (h, pkt_in) = 
         OP.Packet_in.(create_pkt_in ~buffer_id:(-1l) ~in_port 
                      ~reason:ACTION ~data:(Cstruct.sub bits 0 size)) in
       match st.controller with
       | None -> return ()
       | Some conn -> OSK.send_packet conn (OP.Packet_in (h, pkt_in))
    end 
        (*           | Table
         *           | Normal  *)
        | _ -> 
            return (cp (sp "[switch] forward_frame: unsupported output port\n"))

  (* Assumwe that action are valid. I will not get a flow that sets an ip
   * address unless it defines that the ethType is ip. Need to enforce
   * these rule in the parsing process of the flow_mod packets *)
  let apply_of_actions st in_port bits actions =
    let apply_of_actions_inner st in_port bits checksum action =
      try_lwt 
        match action with
        | OP.Flow.Output (port, pkt_size) ->
          (* Make a packet copy in case the buffer is modified and multiple
           * outputs are defined? *)
          lwt _ = forward_frame st in_port bits pkt_size checksum port in 
          return false
        | OP.Flow.Set_dl_src(eaddr) ->
          let _ = set_dl_header_dl_src (Macaddr.to_bytes eaddr) 0 bits in 
          return checksum
        | OP.Flow.Set_dl_dst(eaddr) ->
          let _ = set_dl_header_dl_dst (Macaddr.to_bytes eaddr) 0 bits in 
          return checksum
  (* TODO: Add for this actions to check when inserted if 
    * the flow is an ip flow *)
        | OP.Flow.Set_nw_tos(tos) -> 
          let ip_data = Cstruct.shift bits sizeof_dl_header in
          let _ = set_nw_header_nw_tos ip_data (int_of_char tos) in
          return true
  (* TODO: wHAT ABOUT ARP?
   * *)
        | OP.Flow.Set_nw_src(ip) -> 
          let ip_data = Cstruct.shift bits sizeof_dl_header in
          let _ = set_nw_header_nw_src ip_data (Ipaddr.V4.to_int32 ip) in 
          return true
        | OP.Flow.Set_nw_dst(ip) -> 
          let ip_data = Cstruct.shift bits sizeof_dl_header in
          let _ = set_nw_header_nw_dst ip_data (Ipaddr.V4.to_int32 ip) in 
          return true
        | OP.Flow.Set_tp_src(port) ->
          let ip_data = Cstruct.shift bits sizeof_dl_header in
          let len = (get_nw_header_hlen_version ip_data) land 0xf in 
          let tp_data = Cstruct.shift ip_data (len*4) in
          let _ = set_tp_header_tp_src tp_data port in 
          return true
        | OP.Flow.Set_tp_dst(port) ->
          let ip_data = Cstruct.shift bits sizeof_dl_header in
          let len = (get_nw_header_hlen_version ip_data) land 0xf in 
          let tp_data = Cstruct.shift ip_data (len*4) in 
          let _ = set_tp_header_tp_dst tp_data port in 
          return true
  (*      | OP.Flow.Enqueue(_, _)
          | OP.Flow.Set_vlan_pcp _
          | OP.Flow.Set_vlan_vid _
          | OP.Flow.VENDOR_ACT 
          | OP.Flow.STRIP_VLAN *)
        | act ->
          let _ = cp (sp "[switch] apply_of_actions: Unsupported action %s" 
                        (OP.Flow.string_of_action act)) in 
          return checksum
      with exn -> 
        let _ = cp(sp  "[switch] apply_of_actions: (packet size %d) %s %s\n%!" 
                     (Cstruct.len bits) (OP.Flow.string_of_action action) 
                     (Printexc.to_string exn )) in
        return checksum
    in
    let rec apply_of_actions_rec st in_port bits checksum = function
      | [] -> return false
      | head :: actions -> 
        lwt checksum = apply_of_actions_inner st in_port bits checksum head in
        apply_of_actions_rec st in_port bits checksum actions 
    in 
    lwt _ = apply_of_actions_rec st in_port bits false actions in
    return ()
      
   let lookup_flow st of_match =
     (* Check first the match table cache
      * NOTE an exact match flow will be found on this step and thus 
      * return a result immediately, without needing to get to the cache table
      * and consider flow priorities *)
   let open Table in
   let open OP.Match in
   if (Hashtbl.mem st.table.cache of_match ) then 
     let entry = (Hashtbl.find st.table.cache of_match) in
     Found(entry) 
   else begin
     (* Check the wilcard card table *)
     let lookup_flow flow entry r =
       match (r, (flow_match_compare of_match flow flow.wildcards)) with
       | (_, false) -> r
       | (None, true) -> Some(flow, entry)
       | (Some(f,e), true) when (Entry.(e.counters.priority > entry.counters.priority)) -> r
       | (Some(f,e), true) when (Entry.(e.counters.priority <= entry.counters.priority)) -> 
         Some(flow, entry)
       | (_, _) -> r
     in
     let flow_match = Hashtbl.fold lookup_flow st.table.entries None in
     match (flow_match) with
     | None ->  NOT_FOUND
     | Some(f,e) ->
       Hashtbl.add st.table.cache of_match (ref e);
       Entry.(e.cache_entries <- of_match :: e.cache_entries); 
       Found(ref e)
   end
end

type t = Switch.t

(*********************************************
 * Switch OpenFlow data plane 
 *********************************************)
let process_frame_inner st p frame =
  let open Switch in
  let open OP.Packet_in in
  try_lwt
    let in_port = (OP.Port.port_of_int p.Switch.port_id) in 
    let tupple = (OP.Match.raw_packet_to_match in_port frame ) in
    (* Update port rx statistics *)
    let _ = Switch.update_port_rx_stats (Int64.of_int (Cstruct.len frame)) p in

    (* Lookup packet flow to existing flows in table *)
    match  (Switch.lookup_flow st tupple) with 
    | Switch.NOT_FOUND -> begin
      Table.update_table_missed st.table;
      let buffer_id = st.packet_buffer_id in
         (*TODO Move this code in the Switch module *)
       st.packet_buffer_id <- Int32.add st.packet_buffer_id 1l;
       let (h, pkt_in) = create_pkt_in ~buffer_id ~in_port ~reason:NO_MATCH ~data:frame in 
       st.packet_buffer <- pkt_in::st.packet_buffer; 

       (* Disable for now packet trimming for buffered packets *)
       let size =  
         if (Cstruct.len frame > 92) then 92
         else Cstruct.len frame in
       let (h, pkt_in) = create_pkt_in ~buffer_id ~in_port ~reason:NO_MATCH 
           ~data:(Cstruct.sub frame 0 size) in
       return (
         match st.Switch.controller with
          | None -> ()
          | Some conn -> ignore_result (OSK.send_packet conn (OP.Packet_in(h,pkt_in)))
       )
      end
       (* generate a packet in event *)
    | Switch.Found(entry) ->
      let _ = Table.update_table_found st.table in
      let _ = Entry.update_flow (Int64.of_int (Cstruct.len frame)) !entry in
      apply_of_actions st tupple.OP.Match.in_port frame (!entry).Entry.actions
  with exn ->
    return (cp (sp "[switch] process_frame_inner: control channel error: %s\n" 
        (Printexc.to_string exn)))

let check_packets st = 
  match_lwt (Lwt_list.exists_p (fun p -> return (p.Switch.pkt_count > 0))
  st.Switch.ports) with
  | false -> OS.Time.sleep 0.5 
  | true -> return ()

let forward_thread st =
(*  while_lwt true do
    lwt _ = check_packets st <?> Lwt_condition.wait st.Switch.ready in 
    Lwt_list.iter_s (fun p ->
(*      lwt empty = Lwt_stream.is_empty p.Switch.queue in *)
      if (p.Switch.pkt_count = 0) then
(*        let _ = cp (sp "port %d no packets\n" p.Switch.port_id) in *)
        return ()
      else
        lwt frames = Lwt_stream.nget p.Switch.pkt_count p.Switch.queue in
        lwt _ = 
          Lwt_list.iter_p 
          (fun f -> 
            p.Switch.pkt_count <- p.Switch.pkt_count - 1; process_frame_inner st p f) 
          frames in 
(*        let _ = cp (sp "port %d got packet\n" p.Switch.port_id) in *)
(*        let _ = cp (sp "port %d processed packet\n" p.Switch.port_id) in *)
        return ()
    ) st.Switch.ports 
  done *)
  Lwt_list.iter_p (fun p ->
    while_lwt true do
(*      if (p.Switch.pkt_count > 0) then 
        lwt frames = Lwt_stream.nget 10 p.Switch.queue in 
        Lwt_list.iter_p (
          fun f -> p.Switch.pkt_count <- p.Switch.pkt_count - 1; 
          process_frame_inner st p f) frames
      else *)
        lwt _ = Lwt_stream.next p.Switch.in_queue >>= process_frame_inner st p in
(*       let _ = 
        if (p.Switch.pkt_count mod 20 = 1) then
       cp (sp "port %d got packet %d" p.Switch.port_id p.Switch.pkt_count) in
      *        *)
        return (p.Switch.pkt_count <- p.Switch.pkt_count - 1)
    done <&> (
    while_lwt true do
        lwt frame = Lwt_stream.next p.Switch.out_queue in
(*        lwt _ = OS.Time.sleep 0.0 in *)
(*        let frames = Lwt_stream.get_available p.Switch.out_queue in*)
(*        let _ = Printf.printf "got %d packets\n%!" (1+(List.length frames)) in
   *        *)
        OS.Netif.writev p.Switch.netif [frame] (*frame::frames*)
    done
    )
    ) st.Switch.ports 

let process_frame st p _ frame =
  let _ = 
    try 
      match frame with
      | Net.Ethif.Output _ -> ()
      | Net.Ethif.Input frame ->
(*        let _ = Lwt_condition.broadcast st.Switch.ready () in *)
(*          if (p.Switch.pkt_count < 1000) then *)
            let _ = p.Switch.pkt_count <- p.Switch.pkt_count + 1 in
(*            let _ = Printf.printf "pushing packet to port %d %d\n%!"
                p.Switch.port_id p.Switch.pkt_count in *)
            p.Switch.in_push (Some frame)
(*            Printf.printf "pushed packet to port %d\n%!" p.Switch.port_id*)
(*          else
            cp "[process_frame] blocked queue" *)
    with 
    | Not_found -> cp (sp "[switch] process_frame: Invalid port\n%!")
    | Packet_type_unknw -> cp (sp "[switch] process_frame: malformed packet\n%!")
    | exn -> cp (sp "[switch] process_frame: switch error: %s\n%!" (Printexc.to_string exn))
  in
  return ()

(*************************************************
 * Switch OpenFlow control channel 
 *************************************************)
let get_flow_stats st of_match =
  let open OP.Match in 
  let match_flows of_match key value ret =
    if (flow_match_compare key of_match of_match.wildcards) then ( 
      (Entry.flow_counters_to_flow_stats key (char_of_int 1) value)::ret
    ) else 
      ret 
  in
    Hashtbl.fold (fun key value r -> match_flows of_match key value r) 
    st.Switch.table.Table.entries []  

let process_buffer_id st t msg xid buffer_id actions =
  let open OP.Header in
  let pkt_in = ref None in 
  let _ = 
    st.Switch.packet_buffer <- 
      List.filter ( fun a -> 
        if (a.OP.Packet_in.buffer_id = buffer_id) then 
          (pkt_in := Some(a); false )
        else true ) st.Switch.packet_buffer in 
  match (!pkt_in) with 
  | None ->
      cp (sp "[switch] invalid buffer id %ld\n%!" buffer_id);
    let bits = OP.marshal msg in 
    let h = create ~xid ERROR (get_len + 4 + (Cstruct.len bits)) in 
    OSK.send_packet t (OP.Error(h, OP.REQUEST_BUFFER_UNKNOWN, bits)) 
  | Some(pkt_in) ->
    OP.Packet_in.(Switch.apply_of_actions st pkt_in.in_port pkt_in.data actions)

let process_openflow st t msg =
  let open OP.Header in 
  let _ = if st.Switch.verbose then cp (sp "[switch] %s\n%!" (OP.to_string msg)) in 
  match msg with
  | OP.Hello (h) -> return ()
  | OP.Echo_resp h -> return (st.Switch.echo_resp_received <- true)
  | OP.Echo_req h -> (* Reply to ECHO requests *)
    OSK.send_packet t (OP.Echo_req (create ~xid:h.xid ECHO_RESP sizeof_ofp_header))
 | OP.Features_req (h)  -> 
    let h = create ~xid:(h.xid) FEATURES_RESP (OP.Switch.get_len st.Switch.features) in 
    OSK.send_packet t (OP.Features_resp (h, st.Switch.features))
 | OP.Stats_req(h, req) -> begin
   let xid = h.xid in 
   match req with
   | OP.Stats.Desc_req(req) ->
     let p = OP.Stats.(Desc_resp ({st_ty=DESC; more=false;},
                                  { imfr_desc="Mirage"; hw_desc="Mirage";
                                    sw_desc="Mirage"; serial_num="0.1";dp_desc="Mirage";})) in 
      let h = create ~xid STATS_RESP (OP.Stats.resp_get_len p) in 
        OSK.send_packet t (OP.Stats_resp (h, p))
   | OP.Stats.Flow_req(req_h, of_match, table_id, out_port) ->
      (*TODO Need to consider the  table_id and the out_port and 
       * split reply over multiple openflow packets if they don't
       * fit a single packet. *)
      let flows = get_flow_stats st of_match in
      let stats = OP.Stats.({st_ty=FLOW; more=true;}) in 
      lwt (_, flows) = 
        Lwt_list. fold_right_s (
          fun fl (sz, flows) ->
            let fl_sz = OP.Flow.flow_stats_len fl in 
              if (sz + fl_sz > 0xffff) then 
                let r = OP.Stats.Flow_resp(stats, flows) in
                let h = create ~xid STATS_RESP (OP.Stats.resp_get_len r) in 
                lwt _ = OSK.send_packet t (OP.Stats_resp (h, r)) in
                return ((OP.Header.get_len + OP.Stats.get_resp_hdr_size + fl_sz), [fl])
              else
                return ((sz + fl_sz), (fl::flows)) )
          flows ((OP.Header.get_len + OP.Stats.get_resp_hdr_size), []) in 
      let stats = OP.Stats.({st_ty=FLOW; more=false;}) in 
      let r = OP.Stats.Flow_resp(stats, flows) in
      let h = create ~xid STATS_RESP (OP.Stats.resp_get_len r) in
      OSK.send_packet t (OP.Stats_resp (h, r)) 
   | OP.Stats.Aggregate_req (req_h, of_match, table, port) -> 
      let match_flows_aggr of_match key value (fl_b, fl_p, fl)=
        let open OP.Match in
        let open Entry in 
        if (flow_match_compare key of_match of_match.wildcards) then 
          ((Int64.add fl_b value.counters.n_bytes), (Int64.add fl_p
          value.counters.n_packets), (Int32.succ fl))
        else (fl_b, fl_p, fl) in 
      let (byte_count, packet_count,flow_count) = 
        Hashtbl.fold (match_flows_aggr of_match)
                    st.Switch.table.Table.entries (0L, 0L, 0l) in
      let stats = OP.Stats.({st_ty=AGGREGATE; more=false;}) in  
      let r = OP.Stats.Aggregate_resp(stats, 
                    OP.Stats.({byte_count;packet_count;flow_count;})) in 
      let h = create ~xid STATS_RESP (OP.Stats.resp_get_len r) in 
        OSK.send_packet t (OP.Stats_resp (h, r)) 
   | OP.Stats.Table_req(req) ->
      let stats = OP.Stats.({st_ty=TABLE; more=false;}) in  
      let r = OP.Stats.Table_resp(stats, [st.Switch.table.Table.stats]) in 
      let h = create ~xid STATS_RESP (OP.Stats.resp_get_len r) in 
        OSK.send_packet t (OP.Stats_resp (h, r)) 
   | OP.Stats.Port_req(req_h, port) -> begin
       match port with
       | OP.Port.No_port -> 
         let port_stats = List.map (fun p -> p.Switch.counter) st.Switch.ports in
         let stats = OP.Stats.({st_ty=PORT; more=false;}) in 
         let r = OP.Stats.Port_resp(stats, port_stats) in 
         let h = OP.Header.create ~xid OP.Header.STATS_RESP (OP.Stats.resp_get_len r) in 
         OSK.send_packet t (OP.Stats_resp (h, r)) 
       | OP.Port.Port(port_id) -> begin
           try_lwt 
            let port = Hashtbl.find st.Switch.int_to_port port_id in
            let stats = OP.Stats.({st_ty=PORT; more=false;}) in 
            let r = OP.Stats.Port_resp(stats, [(!port).Switch.counter]) in 
            let h = create ~xid STATS_RESP (OP.Stats.resp_get_len r) in 
            OSK.send_packet t (OP.Stats_resp (h, r))
           with Not_found ->
          (* TODO reply with right error code *)
              cp (sp "[switch] unregistered port %s\n%!"(OP.Port.string_of_port port));
              let h = create ~xid ERROR (OP.Header.get_len + 4) in 
              OSK.send_packet t (OP.Error (h, OP.ACTION_BAD_OUT_PORT, (get_new_buffer 0)))
       end
       | _ -> 
          cp "[switch] unsupported stats request\n%!";
          let h = create ~xid ERROR (get_len + 4) in 
          OSK.send_packet t OP.(Error (h, ACTION_BAD_OUT_PORT, (marshal msg)))
    end
   | _ -> begin 
          let h = create ~xid ERROR (get_len + 4) in 
          OSK.send_packet t (OP.Error (h, OP.REQUEST_BAD_SUBTYPE, (get_new_buffer 0)))
        end
    end
  | OP.Get_config_req(h) ->
    let resp = OP.Switch.init_switch_config in
    let h = create ~xid:h.xid GET_CONFIG_RESP OP.Switch.config_get_len in 
    OSK.send_packet t (OP.Get_config_resp(h, resp)) 
 | OP.Barrier_req(h) ->
   OSK.send_packet t (OP.Barrier_resp(create ~xid:h.xid BARRIER_RESP sizeof_ofp_header))

 | OP.Packet_out(h, pkt) ->
   let open OP.Packet_out in 
   if (pkt.buffer_id = -1l) then
     Switch.apply_of_actions st pkt.in_port pkt.data pkt.actions
   else begin
     process_buffer_id st t msg h.xid pkt.buffer_id pkt.actions 
   end 
  | OP.Flow_mod(h,fm)  ->
      let open OP.Flow_mod in
      lwt _ = 
        match (fm.command) with
          | ADD 
          | MODIFY 
          | MODIFY_STRICT -> 
            Table.add_flow st st.Switch.table fm st.Switch.verbose
          | DELETE 
          | DELETE_STRICT ->
            (* Need to implemente strict deletion in order to enable signpost
             * switching *)
            Table.del_flow st.Switch.table fm.of_match fm.out_port (Some t) st.Switch.verbose 
      in
      if (fm.buffer_id = -1l) then return () 
      else process_buffer_id st t msg h.xid fm.buffer_id fm.actions
  | OP.Queue_get_config_resp (h, _, _)
  | OP.Queue_get_config_req (h, _)
  | OP.Barrier_resp h
  | OP.Stats_resp (h, _)
  | OP.Port_mod (h, _)
  | OP.Port_status (h, _)
  | OP.Flow_removed (h, _)
  | OP.Packet_in (h, _)
  | OP.Set_config (h, _)
  | OP.Get_config_resp (h, _)
  | OP.Features_resp (h, _)
  | OP.Vendor (h, _, _)
  | OP.Error (h, _, _) ->
      let bits = OP.marshal msg in 
      let h = OP.Header.create ~xid:h.OP.Header.xid OP.Header.ERROR 
                    (OP.Header.get_len + 4 + (Cstruct.len bits)) in 
      OSK.send_packet t (OP.Error(h, OP.REQUEST_BAD_TYPE, bits)) 

let monitor_control_channel st conn =
  let is_active = ref true in 
  while_lwt !is_active do
    let _ = st.Switch.echo_resp_received <- false in 
    let _ = st.Switch.last_echo_req <- (OS.Clock.time ()) in 
    lwt _ = OSK.send_packet conn 
      OP.(Echo_req Header.(create ECHO_REQ sizeof_ofp_header)) in
    lwt _ = OS.Time.sleep 10.0 in 
    return (is_active := st.Switch.echo_resp_received) 
  done 

let control_channel_run st conn = 
  (* Trigger the dance between the 2 nodes *)
  let h = OP.Header.(create ~xid:1l HELLO sizeof_ofp_header) in  
  lwt _ = OSK.send_packet conn (OP.Hello(h)) in
  let rec echo () =
    try_lwt
      OSK.read_packet conn >>= process_openflow st conn >>= echo 
    with
    | Net.Nettypes.Closed -> 
      return (cp "[switch] ERROR:Controller channel closed....\n%!")
    | OP.Unparsed (m, bs) -> 
      cp (sp "[switch] ERROR:unparsed! m=%s\n %!" m); echo ()
    | exn -> 
      cp (sp "[switch] ERROR:%s\n%!" (Printexc.to_string exn)); echo () 
  in
  lwt _ = 
    echo () <?> 
    Switch.(Table.monitor_flow_timeout st.table (Some conn) st.verbose) <?>
    (monitor_control_channel st conn) 
  in
  let _ = OSK.close conn in 
  let _ = st.Switch.controller <- None in 
    return (cp "[switch] control channel thread returned")

let control_channel st (addr, port) t =
  cp (sp "[switch] controller %s:%d" (Ipaddr.V4.to_string addr) port); 
  let conn = OSK.init_socket_conn_state t in
  let _ = st.Switch.controller <- (Some conn) in 
  control_channel_run st conn 

(*
 * Interaface with external applications
 * *)
let get_port_name mgr a = 
  let ethif = Net.Manager.get_ethif (get_ethif mgr a) in 
  OS.Netif.string_of_id (OS.Netif.id (Net.Ethif.get_netif ethif))

let add_port mgr ?(use_mac=false) sw id = 
  sw.Switch.portnum <- sw.Switch.portnum + 1;
  let ethif = Net.Manager.get_ethif (get_ethif mgr id) in 
  let hw_addr =  Macaddr.to_string (Net.Ethif.mac ethif) in
  let dev_name = get_port_name mgr id in
  let _ = OS.Console.log (sp "[switch] Adding port %d (%s) '%s' \n %!" 
                            sw.Switch.portnum dev_name  hw_addr) in 
  let port = Switch.init_port mgr sw.Switch.portnum id in
  sw.Switch.ports <- sw.Switch.ports @ [port];
  Hashtbl.add sw.Switch.int_to_port sw.Switch.portnum (ref port); 
  Hashtbl.add sw.Switch.dev_to_port id (ref port);
  sw.Switch.features.OP.Switch.ports  <- 
    sw.Switch.features.OP.Switch.ports @ [port.Switch.phy];
  let _ = Net.Manager.set_promiscuous mgr id (process_frame sw port) in
  let h,p = OP.Port.create_port_status OP.Port.ADD port.Switch.phy in 
  match sw.Switch.controller with
  | None -> return ()
  | Some t -> OSK.send_packet t (OP.Port_status (h,p)) 

let del_port mgr sw name =
  try_lwt
    let open Switch in 
    let port = 
      List.find (fun a -> (get_port_name mgr a.ethif) = name) sw.ports in 
    let _ = 
      sw.ports <- List.filter (fun a -> (get_port_name mgr a.ethif) <> name ) sw.ports in 
    let _ = Hashtbl.remove sw.int_to_port port.port_id in  
    let _ = Hashtbl.remove sw.dev_to_port port.ethif in 
    let h,p = OP.Port.create_port_status OP.Port.DEL port.phy in 
    let of_match = OP.Match.create_flow_match (OP.Wildcards.full_wildcard ()) () in 
    lwt _ = Table.del_flow sw.table of_match 
      (OP.Port.port_of_int port.port_id) sw.controller sw.verbose in 
    let of_match = OP.Match.create_flow_match (OP.Wildcards.in_port_match ())
        ~in_port:(port.Switch.port_id) () in 
    lwt _ = Table.del_flow sw.table of_match 
      OP.Port.No_port sw.controller sw.verbose in 
    let _ = cp (sp "[switch] Removing port %s (port_id=%d)" name port.port_id) in 
    match sw.controller with
    | None -> return ()
    | Some t -> OSK.send_packet t (OP.Port_status (h,p)) 
  with exn -> 
    return (cp (sp "[switch] del_port: port %s not found\n%!" name))

let add_port_local mgr sw ethif = 
 (*TODO Find first if a port is already registered as port 0 
  * as port 0 and disable it *)
let open Switch in 
  let local_port_id = OP.Port.int_of_port OP.Port.Local in 
  let port = init_port mgr local_port_id ethif in 
  sw.ports <- port :: (List.filter (fun a -> (a.port_id <> 0)) sw.ports);
  Hashtbl.replace sw.int_to_port local_port_id (ref port); 
  Hashtbl.iter 
    (fun a b -> if (!b.port_id = local_port_id) then 
         Hashtbl.remove sw.dev_to_port a
    ) sw.dev_to_port;
  Hashtbl.add sw.dev_to_port ethif (ref port);
  (*TODO Need to filter out any 0 port *)
  sw.features.OP.Switch.ports <- 
    port.phy ::
      (List.filter (fun a -> (a.OP.Port.port_no <> local_port_id)) 
         sw.Switch.features.OP.Switch.ports );
  let _ = cp (sp "[switch] Adding port %s (port_id=%d)" 
                (OS.Netif.string_of_id ethif) local_port_id) in 
  return (Net.Manager.set_promiscuous mgr ethif (process_frame sw port))

let add_flow st fm = Switch.(Table.add_flow st st.table fm st.verbose)
let del_flow st m = 
  Switch.(Table.del_flow st.table m OP.Port.No_port st.controller st.verbose)
 
let create_switch ?(verbose=false) dpid = 
  Switch.(
    { ports = []; int_to_port = (Hashtbl.create 64); dev_to_port=(Hashtbl.create 64); 
      controller=None; errornum = 0l; portnum=0; 
      stats={n_frags=0L; n_hits=0L;n_missed=0L;n_lost=0L;};
      table = (Table.init_table ()); features=(Switch.switch_features dpid); 
      packet_buffer=[]; last_echo_req=0.; echo_resp_received=true;
      packet_buffer_id=0l;ready=(Lwt_condition.create ());
      verbose;})

let listen st mgr loc =
  Net.Channel.listen mgr (`TCPv4 (loc, (control_channel st ))) <&>
  (forward_thread st) <&>
    (Ofswitch_config.listen_t mgr (add_port mgr st) 
    (del_port mgr st) (get_flow_stats st) (add_flow st) (del_flow st) 6634) 

let connect st mgr loc  =
  Net.Channel.connect mgr (`TCPv4 (None, loc, (control_channel st loc))) <&> 
  (forward_thread st) <&>
    (Ofswitch_config.listen_t mgr (add_port mgr st) (del_port mgr st) (get_flow_stats st) 
     (add_flow st) (del_flow st) 6634) 

let local_connect st mgr conn =
  let _ = st.Switch.controller <- (Some conn) in 
     (control_channel_run st conn) <&> 
  (forward_thread st)  <&>
    (Ofswitch_config.listen_t mgr (add_port mgr st) (del_port mgr st) (get_flow_stats st) 
     (add_flow st) (del_flow st) 6634) 

let standalone_connect st mgr loc  =
  let of_ctrl = Ofswitch_standalone.init_controller () in 
  let _ = Lwt.ignore_result (Ofswitch_config.listen_t mgr (add_port mgr st) (del_port mgr st) 
            (get_flow_stats st)  (add_flow st) (del_flow st) 6634) in 
  let _ = ignore_result (forward_thread st) in 
  let _ = cp "[switch] Listening socket...\n%!" in 
    while_lwt true do
      let t,u = Lwt.task () in 
      (
        let _ = cp "[switch] Standalone controller taking over..." in 
        lwt conn = 
          Ofswitch_standalone.run_controller mgr of_ctrl in
(*        let conn = OSK.init_local_conn_state switch_in switch_out in  *)
        let _ = st.Switch.controller <- (Some conn) in 
        lwt _ = Lwt.pick [(control_channel_run st conn); t] in 
        let _ = OSK.close conn in 
        return (cp "[switch] Standalone controller stopped..." )
      ) <&> (
      let rec connect_socket () =
        let sock = ref None in 
        try_lwt
          lwt _ = Lwt.pick
            [(Net.Channel.connect mgr (`TCPv4(None,loc,(fun t -> return (sock:=Some(t))))));
             (OS.Time.sleep 10.0)]
          in
          match !sock with
          | None -> connect_socket ()
          | Some t -> return t 
        with exn -> connect_socket () 
      in
      lwt conn = connect_socket () >|= OSK.init_socket_conn_state in 
      let _ = wakeup u (st.Switch.controller <- (Some conn)) in
      lwt _ = control_channel_run st conn in 
      let _ = OSK.close conn in 
      return (cp "[switch ]Remote controller connected...")
    )
  done
