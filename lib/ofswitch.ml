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
open Net
open Nettypes

module OP = Packet

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
type eaddr  = OP.eaddr

type port = uint16
type cookie = uint64

type device = string (* XXX placeholder! *)

let resolve t = Lwt.on_success t (fun _ -> ())

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
    insert_sec: uint32;
    insert_nsec: uint32;
    mutable last_sec: uint32;
    mutable last_nsec: uint32;
    idle_timeout: int;
    hard_timeout:int;
  }

  type queue_counter = {
    tx_queue_packets: uint64;
    tx_queue_bytes: uint64;
    tx_queue_overrun_errors: uint64;
  }

  let init_flow_counters t =
    let ts = Int32.of_float (OS.Clock.time ()) in
    {n_packets=0L; n_bytes=0L; priority=t.OP.Flow_mod.priority; 
     cookie=t.OP.Flow_mod.cookie; insert_sec=ts; insert_nsec=0l; 
     last_sec=ts;last_nsec=0l; idle_timeout=t.OP.Flow_mod.idle_timeout; 
    hard_timeout=t.OP.Flow_mod.hard_timeout; flags=t.OP.Flow_mod.flags; }


  type t = { 
    counters: flow_counter;
    actions: OP.Flow.action list;
    mutable cache_entries: OP.Match.t list;
  }
  let update_flow pkt_len flow = 
    flow.counters.n_packets <- Int64.add flow.counters.n_packets 1L;
    flow.counters.n_bytes <- Int64.add flow.counters.n_bytes pkt_len;
    let ts = OS.Clock.time () in 
      flow.counters.last_sec <- (Int32.of_float ts)


  let flow_counters_to_flow_stats of_match table_id flow =
    let priority = flow.counters.priority in
    let idle_timeout=flow.counters.idle_timeout in
    let hard_timeout=flow.counters.hard_timeout in
    let cookie=flow.counters.cookie in
    let packet_count=flow.counters.n_packets in
    let byte_count=flow.counters.n_bytes in
    let action=flow.actions in
    OP.Flow.({table_id; of_match; 
    duration_sec=(Int32.sub flow.counters.last_sec flow.counters.insert_sec);
    duration_nsec=(Int32.sub flow.counters.last_nsec flow.counters.insert_nsec);
    priority; idle_timeout; hard_timeout; cookie;
    packet_count; byte_count; action; })

end

module Table = struct
  type t = {
    tid: cookie;
    (* This stores entries as they arrive *)
    mutable entries: (OP.Match.t, Entry.t) Hashtbl.t;
    (* This stores only exact match entries.*)
    (* TODO delete an entry from both tables *)
    mutable cache : (OP.Match.t, Entry.t ref) Hashtbl.t;
    stats : OP.Stats.table;
  }

  let init_table () = 
    { tid = 0_L; entries = (Hashtbl.create 10000); cache = (Hashtbl.create 10000);
    stats = OP.Stats.(
      {table_id=(OP.Stats.table_id_of_int 1); name="main_tbl"; 
      wildcards=OP.Wildcards.exact_match; max_entries=1024l; active_count=0l; 
      lookup_count=0L; matched_count=0L});}

  (* TODO fix flow_mod flag support. overlap is not considered *)
  let add_flow table t =
    (* TODO check if the details are correct e.g. IP type etc. *)
    Hashtbl.replace table.entries t.OP.Flow_mod.of_match 
    (Entry.({actions=t.OP.Flow_mod.actions; counters=(init_flow_counters t); 
             cache_entries=[];}))

  (* check if a list of actions has an output action forwarding packets to
   * out_port *)
  let rec is_output_port out_port = function
    | [] -> false
    | OP.Flow.Output(port, _)::_ when (port = out_port) -> true
    | head::tail -> is_output_port out_port tail 

  let del_flow table ?(xid=(Random.int32 Int32.max_int)) ?(reason=OP.Flow_removed.DELETE) 
        t tuple out_port =
    (* Delete all matching entries from the flow table*)
    let remove_flow = 
      Hashtbl.fold (
        fun of_match flow ret -> 
          if ((OP.Match.flow_match_compare of_match tuple
                  tuple.OP.Match.wildcards) && 
              ((out_port = OP.Port.No_port) || 
               (is_output_port out_port flow.Entry.actions))) then ( 
            Hashtbl.remove table.entries of_match; 
            ret @ [(of_match, flow)]
          ) else 
            ret
          ) table.entries [] in

    (* Delete all entries from cache *)
    let _ = 
      List.iter (
        fun (_, flow) -> 
          List.iter (Hashtbl.remove table.cache) flow.Entry.cache_entries
      ) remove_flow in 

    (* Check for notification flag in flow and send 
    * flow modification warnings *)
    let _ = 
      List.iter (
      fun (of_match, flow) -> 
        if (flow.Entry.counters.Entry.flags.OP.Flow_mod.send_flow_rem) then
          let duration_sec = Int32.sub (Int32.of_float (OS.Clock.time ()))  
            flow.Entry.counters.Entry.insert_sec in
          let fl_rm = OP.Flow_removed.(
            {of_match; cookie=flow.Entry.counters.Entry.cookie; 
            priority=flow.Entry.counters.Entry.priority;
            reason; duration_sec; duration_nsec=0l;
            idle_timeout=flow.Entry.counters.Entry.idle_timeout;
            packet_count=flow.Entry.counters.Entry.n_packets;
            byte_count=flow.Entry.counters.Entry.n_bytes;}) in 
          let bits = OP.marshal_and_sub (OP.Flow_removed.marshal_flow_removed fl_rm) 
            (OS.Io_page.get ()) in 
            Channel.write_buffer t bits  
    ) remove_flow  in 
      Channel.flush t

  (* table stat update methods *)
  let update_table_found table = 
    table.stats.OP.Stats.lookup_count <- Int64.add
    table.stats.OP.Stats.lookup_count 1L;
    table.stats.OP.Stats.matched_count <- 
      Int64.add table.stats.OP.Stats.matched_count 1L

  let update_table_missed table =
    table.stats.OP.Stats.lookup_count <- Int64.add
    table.stats.OP.Stats.lookup_count 1L

  (* monitor thread to timeout flows *)
  let check_flow_timeout table t = 
    let ts = (Int32.of_float (OS.Clock.time ())) in 
    let flows = Hashtbl.fold (
      fun of_match entry ret -> 
        let hard = Int32.to_int (Int32.sub ts entry.Entry.counters.Entry.insert_sec) in
        let idle = Int32.to_int (Int32.sub ts entry.Entry.counters.Entry.last_sec) in
        match (hard, idle) with 
          | (l, _) when ((entry.Entry.counters.Entry.hard_timeout > 0) && 
                         (l >= entry.Entry.counters.Entry.hard_timeout)) ->
              ret @ [(of_match, entry, OP.Flow_removed.HARD_TIMEOUT )]
          | (_, l) when ((entry.Entry.counters.Entry.idle_timeout > 0) &&
                         (l >= entry.Entry.counters.Entry.idle_timeout)) ->
              ret @ [(of_match, entry, OP.Flow_removed.IDLE_TIMEOUT )]
          | _ -> ret 
    ) table.entries [] in 
      Lwt_list.iter_s (
        fun (of_match, entry, reason) -> 
          del_flow table ~reason t of_match OP.Port.No_port 
      ) flows


  let monitor_flow_timeout table t = 
    while_lwt true do 
      lwt _ = OS.Time.sleep 1.0 in 
        check_flow_timeout table t
    done 
end

module Switch = struct
  type port = {
    mgr: Manager.t;
    port_id: int;
    ethif: Net.Manager.id; 
    port_name: string;
    counter: OP.Port.stats;
    phy: OP.Port.phy;
  }
  let init_port mgr port_no ethif = 
    let name = Manager.get_intf_name mgr ethif (* get_ethif.Net.Ethif.ethif.OS.Netif.id *) in 
    let hw_addr = Manager.get_intf_mac mgr ethif in
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
    
      {port_id=port_no; mgr; port_name=name; counter; ethif;phy;}

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
    mutable controllers: (Net.Channel.t) list;
    table: Table.t;
    stats: stats;
    p_sflow: uint32; (** probability for sFlow sampling *)
    mutable errornum : uint32; 
    mutable portnum : int;
    packet_queue : (Cstruct.buf * Net.Manager.id) Lwt_stream.t;
    push_packet : ((Cstruct.buf * Net.Manager.id) option -> unit);
    (* TODO: add this in the port definition and make also 
     * packet output assyncronous *) 
    mutable queue_len : int;
    features : OP.Switch.features;
    mutable packet_buffer: OP.Packet_in.t list;
    mutable packet_buffer_id: int32; 
  }
 let supported_actions () = 
   OP.Switch.({ output=true; set_vlan_id=true; set_vlan_pcp=true; strip_vlan=true;
   set_dl_src=true; set_dl_dst=true; set_nw_src=true; set_nw_dst=true;
   set_nw_tos=true; set_tp_src=true; set_tp_dst=true; enqueue=false;vendor=true; })
 let supported_capabilities () = 
   OP.Switch.({flow_stats=true;table_stats=true;port_stats=true;stp=true;
   ip_reasm=false;queue_stats=false;arp_match_ip=true;})
 let switch_features () = 
   OP.Switch.({datapath_id=1L; n_buffers=0l; n_tables=(char_of_int 1); 
   capabilities=(supported_capabilities ()); actions=(supported_actions ()); 
   ports=[];})


  let update_port_tx_stats pkt_len port = 
    port.counter.OP.Port.tx_packets <- (Int64.add 
      port.counter.OP.Port.tx_packets 1L);
    port.counter.OP.Port.tx_bytes <- (Int64.add 
      port.counter.OP.Port.tx_bytes pkt_len)

  let update_port_rx_stats pkt_len port = 
    port.counter.OP.Port.rx_packets <- (Int64.add 
      port.counter.OP.Port.rx_packets 1L);
    port.counter.OP.Port.rx_bytes <- (Int64.add 
      port.counter.OP.Port.rx_bytes pkt_len)

  let forward_frame st in_port frame pkt_size = function
    | OP.Port.Port(port) -> 
      if Hashtbl.mem st.int_to_port port then(
        let out_p = (!( Hashtbl.find st.int_to_port port))  in
          Net.Manager.inject_packet out_p.mgr out_p.ethif frame )
      else
        return (Printf.printf "Port %d not registered \n" port)
    | OP.Port.No_port -> return ()
    | OP.Port.Flood 
    |OP.Port.All ->
      Lwt_list.iter_p  
      (fun port -> 
        if(port.port_id != (OP.Port.int_of_port in_port)) then (
          update_port_tx_stats (Int64.of_int (Cstruct.len frame)) port;
          Net.Manager.inject_packet port.mgr port.ethif frame
        ) else
          return ()
      ) st.ports
    | OP.Port.In_port ->
      let port = (OP.Port.int_of_port in_port) in 
      if Hashtbl.mem st.int_to_port port then
        let out_p = !(Hashtbl.find st.int_to_port port) in
          update_port_tx_stats (Int64.of_int (Cstruct.len frame)) out_p;
          Net.Manager.inject_packet out_p.mgr out_p.ethif frame
      else
        return (Printf.printf "Port %d not registered \n" port)
        (*           | Table
         *           | Normal
         *           | Controller -> generate a packet out. 
         *           | Local -> can I inject this frame to the network
         *           stack?  *)
        | _ -> return (Printf.printf "Not implemented output port\n")

  cstruct dl_header {
    uint8_t   dl_dst[6];
    uint8_t   dl_src[6]; 
    uint16_t  dl_type 
  } as big_endian

  cstruct arphdr {
    uint16_t ar_hrd;         
    uint16_t ar_pro;         
    uint16_t ar_hln;              
    uint16_t ar_pln;              
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

  (* TODO: Minor util function which I put it here as I have the
   * header laready defined. Maybe need to make a new module
   * and include this utils. *)
  let size_of_raw_packet bits =
    let dl_type = get_dl_header_dl_type bits in
    let bits = Cstruct.shift bits sizeof_dl_header in 
    match (dl_type) with
    | 0x0800 ->
      Some( sizeof_dl_header + (get_nw_header_total_len bits))
    | 0x0806 ->
      Some(sizeof_dl_header + sizeof_arphdr)
    | _ ->
      let _ = ep "Cannot determine size of ethtype %x\n%!" dl_type in
      let _ = Cstruct.hexdump bits in 
        None

  (* Assumwe that action are valid. I will not get a flow that sets an ip
   * address unless it defines that the ethType is ip. Need to enforce
   * these rule in the parsing process of the flow_mod packets *)
  let apply_of_actions st in_port bits actions =
    let apply_of_actions_inner st in_port bits = function
      | OP.Flow.Output (port, pkt_size) ->
        (* Make a packet copy in case the buffer is modified and multiple
         * outputs are defined? *)
        forward_frame st in_port bits pkt_size port
      | OP.Flow.Set_dl_src(eaddr) ->
        return (set_dl_header_dl_src eaddr 6 bits)
      | OP.Flow.Set_dl_dst(eaddr) ->
        return (set_dl_header_dl_dst eaddr 6 bits)
        (* TODO: Add for this actions to check when inserted if 
          * the flow is an ip flow *)
      | OP.Flow.Set_nw_tos(tos) -> 
        let ip_data = Cstruct.shift bits sizeof_dl_header in
          return (set_nw_header_nw_tos ip_data (int_of_char tos))
      (* TODO: wHAT ABOUT ARP?
       * *)
      | OP.Flow.Set_nw_src(ip) -> 
        let ip_data = Cstruct.shift bits sizeof_dl_header in
          return (set_nw_header_nw_src ip_data ip)
      | OP.Flow.Set_nw_dst(ip) -> 
        let ip_data = Cstruct.shift bits sizeof_dl_header in
          return (set_nw_header_nw_dst ip_data ip)
      | OP.Flow.Set_tp_src(port) ->
        let ip_data = Cstruct.shift bits sizeof_dl_header in
        let len = (get_nw_header_hlen_version bits) land 0xf in 
        let tp_data = Cstruct.shift ip_data (len*4) in 
          return (set_tp_header_tp_src tp_data port)
      | OP.Flow.Set_tp_dst(port) ->
        let ip_data = Cstruct.shift bits sizeof_dl_header in
        let len = (get_nw_header_hlen_version bits) land 0xf in 
        let tp_data = Cstruct.shift ip_data (len*4) in 
          return (set_tp_header_tp_dst tp_data port )
      | OP.Flow.Enqueue(_, _)
      | OP.Flow.Set_vlan_pcp _
      | OP.Flow.Set_vlan_vid _
      | OP.Flow.VENDOR_ACT 
      | OP.Flow.STRIP_VLAN ->
        (* VLAN manupulation actions *)
        return (pr "Unsupported action STRIP_VLAN\n")
    in
    let rec apply_of_actions_rec st in_port actions = function
      | [] -> return ()
      | head :: actions -> 
        let _ = apply_of_actions_inner st in_port bits head in
          apply_of_actions_rec st in_port bits actions 
    in 
      apply_of_actions_rec st in_port bits actions

   let lookup_flow st of_match =
     (* Check first the match table cache
      * NOTE an exact match flow will be found on this step and thus 
      * return a result immediately, without needing to get to the cache table
      * and consider flow priorities *)
     if (Hashtbl.mem st.table.Table.cache of_match ) then (
       let entry = (Hashtbl.find st.table.Table.cache of_match) in
       Found(entry) 
     ) else (
       (* Check the wilcard card table *)
       let lookup_flow flow entry r =
         match (r, (OP.Match.flow_match_compare of_match flow
                  flow.OP.Match.wildcards)) with
           | (_, false) -> r
           | (None, true) -> Some(flow, entry)
           | (Some(f,e), true) when (Entry.(e.counters.priority > entry.counters.priority)) -> r
           | (Some(f,e), true) when (Entry.(e.counters.priority <= entry.counters.priority)) -> 
                                     Some(flow, entry)
           | (_, _) -> r
       in
       let flow_match = Hashtbl.fold lookup_flow st.table.Table.entries None in
         match (flow_match) with
           | None ->  NOT_FOUND
           | Some(f,e) ->
               Hashtbl.add st.table.Table.cache of_match (ref e);
               e.Entry.cache_entries <- e.Entry.cache_entries @ [of_match]; 
               Found(ref e)
     )
end

type t = Switch.t

(*********************************************
 * Switch OpenFlow data plane 
 *********************************************)


(* 
 * let process_frame_depr intf_name frame =  *)
let process_frame_inner st intf frame =
  try_lwt 
      let p = (!(Hashtbl.find st.Switch.dev_to_port intf)) in  
     let in_port = (OP.Port.port_of_int p.Switch.port_id) in 
     let tupple = (OP.Match.raw_packet_to_match in_port frame ) in
     (* Update port rx statistics *)
     let _ = Switch.update_port_rx_stats (Int64.of_int (Cstruct.len frame)) p in

    (* What is the size of the frame? Need to get sub_buffer in order  to
     * process it *)
    let frame = 
      match (Switch.size_of_raw_packet frame) with
      | Some(len) -> Cstruct.sub_buffer frame 0 len
      | None -> raise Packet_type_unknw
    in 
     (* Lookup packet flow to existing flows in table *)
     let entry = (Switch.lookup_flow st tupple) in 
     match entry with 
     | Switch.NOT_FOUND ->
       let _ = Table.update_table_missed st.Switch.table in
       let buffer_id = st.Switch.packet_buffer_id in
         (*TODO Move this code in the Switch module *)
       st.Switch.packet_buffer_id <- Int32.add st.Switch.packet_buffer_id 1l;
       let pkt_in = OP.Packet_in.create_pkt_in ~buffer_id ~in_port 
                      ~reason:OP.Packet_in.NO_MATCH ~data:frame in 
         st.Switch.packet_buffer <- st.Switch.packet_buffer @ [pkt_in]; 
       let bits = OP.marshal_and_sub (OP.Packet_in.marshal_pkt_in ~data_len:64 pkt_in) 
                    (OS.Io_page.get ()) in 
       Lwt_list.iter_p 
       (fun t ->
          match (Cstruct.len bits) with
            | l when l <= 1400 -> 
                let _ = Channel.write_buffer t bits in 
                  Channel.flush t
            | _ -> 
                let buf = Cstruct.sub_buffer bits 0 1400 in 
                let _ = Channel.write_buffer t buf in 
                let buf = Cstruct.sub_buffer bits 1400 ((Cstruct.len bits) - 1400) in 
                let _ = Channel.write_buffer t buf in
                lwt _ = Channel.flush t in 
                  return ()
       )
       st.Switch.controllers
       (* generate a packet in event *)
     | Switch.Found(entry) ->
       let _ = Table.update_table_found st.Switch.table in
       let _ = Entry.update_flow (Int64.of_int (Cstruct.len frame)) !entry in
        Switch.apply_of_actions st tupple.OP.Match.in_port 
          frame (!entry).Entry.actions
    with Not_found ->
      pr "bt: %s\n%!" (Printexc.get_backtrace ());
      return ()
     | Packet_type_unknw -> return ()

let process_frame st intf_name frame =
  try_lwt 
    let p = Hashtbl.find st.Switch.dev_to_port intf_name in
    process_frame_inner st intf_name frame
(*    if (st.Switch.queue_len < 256) then (
      st.Switch.queue_len <- st.Switch.queue_len + 1;

      return(st.Switch.push_packet (Some(frame, ((!p).Switch.port_id) )))
    ) else (
      pr "dropping packet at the switch\n%!";
      return ()
    ) *)
    with 
    | Not_found -> 
      return (pr "%03.6f: Invalid port\n%!" (OS.Clock.time ()))
    | Packet_type_unknw ->
      return (pr "%03.6f: received a malformed packet\n%!" (OS.Clock.time ()))
    | exn ->
      return (pr "%03.6f: switch error: %s\n%!" (OS.Clock.time ()) (Printexc.to_string exn))

let data_plane st () = 
  try_lwt 
  while_lwt true do 
    lwt a = Lwt_stream.get st.Switch.packet_queue in
    match a with
    | Some (pkt, p) ->
      st.Switch.queue_len <- st.Switch.queue_len - 1;
      process_frame_inner st p pkt
    | None -> return ()
  done


(*************************************************
 * Switch OpenFlow control channel 
 *************************************************)

type endhost = {
  ip: Nettypes.ipv4_addr;
  port: int;
}

let process_openflow st t bits =  function
  | OP.Hello (h) -> 
    (* Reply to HELLO with a HELLO and a feature request *)
    cp "HELLO";
    return ()
  | OP.Echo_req (h, bs) -> (* Reply to ECHO requests *)
    cp "ECHO_REQ";
    let h = OP.Header.(create ECHO_RESP sizeof_ofp_header h.xid) in
    let bits = OP.marshal_and_sub (OP.Header.marshal_header h)
      (OS.Io_page.get ()) in 
    let _ = Channel.write_buffer t bits in 
      Channel.flush t
  | OP.Features_req (h)  -> 
    cp "FEAT_REQ";
    let bits = OP.marshal_and_sub
      (OP.Switch.marshal_reply_features h.OP.Header.xid st.Switch.features )
      (OS.Io_page.get ()) in 
    let _ = Channel.write_buffer t bits in
      Channel.flush t
  | OP.Stats_req(h, req) -> begin
    let xid = h.OP.Header.xid in 
    cp "STATS_REQ\n%!";
    match req with
    | OP.Stats.Desc_req(req) ->
      let desc = OP.Stats.({ imfr_desc="Mirage"; hw_desc="Mirage";
                  sw_desc="Mirage"; serial_num="0.1";dp_desc="Mirage";}) 
      in  
      let resp_h = OP.Stats.({st_ty=DESC; more_to_follow=false;}) in 
      let bits =  OP.marshal_and_sub 
                  (OP.Stats.marshal_stats_resp xid (OP.Stats.Desc_resp(resp_h,
                  desc))) 
                  (OS.Io_page.get ()) in   
      let _ = Channel.write_buffer t bits in
        Channel.flush t 
    | OP.Stats.Flow_req(req_h, of_match, table_id, out_port) ->
      (*TODO Need to consider the  table_id and the out_port and 
       * split reply over multiple openflow packets if they don't
       * fit a single packet. *)
      let match_flows of_match key value ret =
        if (OP.Match.flow_match_compare key of_match 
              of_match.OP.Match.wildcards) then ( 
          ret @ [
            (Entry.flow_counters_to_flow_stats 
              of_match (char_of_int 1) value)] 
        ) else 
          ret 
      in
      let flows = 
        Hashtbl.fold (fun key value r -> match_flows of_match key value r) 
          st.Switch.table.Table.entries [] in 
      let stats = OP.Stats.({st_ty=FLOW; more_to_follow=false;}) in 
      let reply = OP.Stats.Flow_resp(stats, flows) in 
      let bits = OP.marshal_and_sub (OP.Stats.marshal_stats_resp xid reply) 
                  (OS.Io_page.get ()) in
      let _ = Channel.write_buffer t bits in
        Channel.flush t
    | OP.Stats.Aggregate_req (req_h, of_match, table, port) -> 
      let aggr_flow_bytes = ref 0L in
      let aggr_flow_pkts = ref 0L in
      let aggr_flows = ref 0l in            
      let match_flows_aggr of_match key value =
        if (OP.Match.flow_match_compare key of_match 
              of_match.OP.Match.wildcards) then (
          aggr_flows := Int32.add (!aggr_flows) 1l;
          aggr_flow_bytes := Int64.add (!aggr_flow_bytes) 
                              value.Entry.counters.Entry.n_bytes; 
          aggr_flow_pkts := Int64.add (!aggr_flow_pkts)
                         value.Entry.counters.Entry.n_packets
          ) in 
      Hashtbl.iter (fun key value -> match_flows_aggr of_match key value)
                    st.Switch.table.Table.entries;
      let stats = OP.Stats.({st_ty=AGGREGATE; more_to_follow=false;}) in  
      let reply = OP.Stats.Aggregate_resp(stats, 
                    OP.Stats.({byte_count=(!aggr_flow_bytes);
                    packet_count=(!aggr_flow_pkts);
                    flow_count=(!aggr_flows);})) 
      in 
      let bits = OP.marshal_and_sub (OP.Stats.marshal_stats_resp xid reply)
                  (OS.Io_page.get ()) in
      let _ =  Channel.write_buffer t bits in 
        Channel.flush t
    | OP.Stats.Table_req(req) ->
      let stats = OP.Stats.({st_ty=TABLE; more_to_follow=false;}) in  
      let reply = OP.Stats.Table_resp(stats, [st.Switch.table.Table.stats]) in 
      let bits = OP.marshal_and_sub (OP.Stats.marshal_stats_resp xid reply) 
                  (OS.Io_page.get ()) in
      let _ = Channel.write_buffer t bits in 
        Channel.flush t
    | OP.Stats.Port_req(req_h, port) -> begin
      match port with
      | OP.Port.No_port -> 
        let port_stats = List.map (fun p -> p.Switch.counter) st.Switch.ports in
        let stats = OP.Stats.({st_ty=PORT; more_to_follow=false;}) in 
        let reply = OP.Stats.Port_resp(stats, port_stats) in 
        let bits = OP.marshal_and_sub (OP.Stats.marshal_stats_resp xid reply) 
                    (OS.Io_page.get ()) in
        let _ = Channel.write_buffer t bits in 
          Channel.flush t
      | OP.Port.Port(port_id) -> 
        try_lwt 
          let port = Hashtbl.find st.Switch.int_to_port port_id in
          let stats = OP.Stats.({st_ty=PORT; more_to_follow=false;}) in 
          let reply = OP.Stats.Port_resp(stats, [(!port).Switch.counter]) in 
          let bits = OP.marshal_and_sub (OP.Stats.marshal_stats_resp xid reply) 
                      (OS.Io_page.get ()) in
          let _ = Channel.write_buffer t bits in 
            Channel.flush t
        with Not_found ->
          (* TODO reply with right error code *)
          pr "Invalid port_id in stats\n%!";
          return ()
        end
      | _ -> 
        let bits = OP.marshal_and_sub (OP.marshal_error  
                    OP.REQUEST_BAD_SUBTYPE bits xid) (OS.Io_page.get ()) in
        let _ = Channel.write_buffer t bits in 
          Channel.flush t
  end
  | OP.Get_config_req(h) -> 
    let resp = OP.Switch.init_switch_config in
    let bits = OP.marshal_and_sub (OP.Switch.marshal_switch_config 
                h.OP.Header.xid resp) (OS.Io_page.get ()) in
    let _ = Channel.write_buffer t bits in 
      Channel.flush t
  | OP.Barrier_req(h) ->
    cp (sp "BARRIER_REQ: %s\n%!" (OP.Header.header_to_string h));
    let resp_h = (OP.Header.create OP.Header.BARRIER_RESP
                  (OP.Header.sizeof_ofp_header) h.OP.Header.xid) in
    let bits = OP.marshal_and_sub (OP.Header.marshal_header resp_h)        
                (OS.Io_page.get ()) in
    let _ = Channel.write_buffer t bits in 
      Channel.flush t
  | OP.Packet_out(h, pkt) ->
    cp (sp "PACKET_OUT: %s\n%!" (OP.Packet_out.packet_out_to_string pkt));
    if (pkt.OP.Packet_out.buffer_id = -1l) then
      Switch.apply_of_actions st pkt.OP.Packet_out.in_port
                pkt.OP.Packet_out.data pkt.OP.Packet_out.actions
    else begin
      let pkt_in = ref None in 
      let _ = 
        st.Switch.packet_buffer <- 
        List.filter (
          fun a -> 
            if (a.OP.Packet_in.buffer_id = pkt.OP.Packet_out.buffer_id) then 
              (pkt_in := Some(a); false )
              else true 
        ) st.Switch.packet_buffer
      in 
        match (!pkt_in) with 
        | None -> 
            let bs = OP.marshal_and_sub 
                       (OP.marshal_error OP.REQUEST_BUFFER_UNKNOWN bits h.OP.Header.xid)
                       (OS.Io_page.get ()) in 
            let _ = Channel.write_buffer t bs in 
              Channel.flush t 
        | Some(pkt_in) -> 
            Switch.apply_of_actions st pkt_in.OP.Packet_in.in_port
              pkt_in.OP.Packet_in.data pkt.OP.Packet_out.actions
    end 
  | OP.Flow_mod(h,fm)  ->
    cp (sp "FLOW_MOD: %s\n%!" (OP.Flow_mod.flow_mod_to_string fm));
    let of_match = fm.OP.Flow_mod.of_match in 
    let of_actions = fm.OP.Flow_mod.actions in
    lwt _ = 
      match (fm.OP.Flow_mod.command) with
      | OP.Flow_mod.ADD 
      | OP.Flow_mod.MODIFY 
      | OP.Flow_mod.MODIFY_STRICT -> 
        return (Table.add_flow st.Switch.table fm)
      | OP.Flow_mod.DELETE 
      | OP.Flow_mod.DELETE_STRICT ->
        Table.del_flow st.Switch.table t of_match fm.OP.Flow_mod.out_port
    in
      if (fm.OP.Flow_mod.buffer_id = -1l) then
        return () 
      else begin
        let pkt_in = ref None in 
        let _ = 
          st.Switch.packet_buffer <- 
          List.filter (
            fun a -> 
              if (a.OP.Packet_in.buffer_id = fm.OP.Flow_mod.buffer_id) then 
                (pkt_in := Some(a); false )
              else true 
          ) st.Switch.packet_buffer
        in 
          match (!pkt_in) with 
            | None -> 
                let bs = 
                  OP.marshal_and_sub 
                    (OP.marshal_error OP.REQUEST_BUFFER_UNKNOWN bits h.OP.Header.xid)
                    (OS.Io_page.get ()) in 
                let _ = Channel.write_buffer t bs in 
                  Channel.flush t 
            | Some(pkt_in) ->
                (* TODO check if the match is accurate? *)
                Switch.apply_of_actions st pkt_in.OP.Packet_in.in_port
                  pkt_in.OP.Packet_in.data of_actions 
      end 
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
  | OP.Echo_resp (h, _)
  | OP.Error (h, _) ->
    let bits = OP.marshal_and_sub (OP.marshal_error         
                OP.REQUEST_BAD_TYPE bits h.OP.Header.xid) 
                (OS.Io_page.get ()) in
    let _ = Channel.write_buffer t bits in 
      Channel.flush t

let control_channel st (remote_addr, remote_port) t =
  let rs = Nettypes.ipv4_addr_to_string remote_addr in
  Printf.eprintf "OpenFlow Switch: controller %s:%d" rs remote_port; 
  st.Switch.controllers <- (st.Switch.controllers @ [t]);

  (* Trigger the dance between the 2 nodes *)
  let h = OP.Header.(create HELLO sizeof_ofp_header 1l) in  
  let bits = OP.marshal_and_sub (OP.Header.marshal_header h)
               (OS.Io_page.get ()) in 
  let _ = Channel.write_buffer t bits in 
  lwt _ = Channel.flush t in 
  let cached_socket = Ofsocket.create_socket t in 

  let rec echo () =
    try_lwt
      lwt hbuf = Ofsocket.read_data cached_socket OP.Header.sizeof_ofp_header in
        let ofh  = OP.Header.parse_header  hbuf in
        let dlen = ofh.OP.Header.len - OP.Header.sizeof_ofp_header in 
        lwt dbuf = Ofsocket.read_data cached_socket dlen in
        let ofp  = OP.parse ofh dbuf in
          process_openflow st t dbuf ofp (* Bitstring.concat [hbuf; dbuf] *) 
          >> echo ()
    with
    | Nettypes.Closed -> 
            (* TODO Need to remove the t from st.Switch.controllers *)
        pr "Controller channel closed....\n%!"; 
        return ()
    | OP.Unparsed (m, bs) -> (pr "# unparsed! m=%s\n %!" m); echo ()
    | exn -> 
        pr "[OpenFlow-Switch-Control] ERROR:%s\n" (Printexc.to_string exn);
        (echo () ) 

  in 
    echo () <&> (Table.monitor_flow_timeout st.Switch.table t)

(*
 * Interaface with external applications
 * *)
let add_port mgr sw ethif = 
  sw.Switch.portnum <- sw.Switch.portnum + 1;
  let _ = pr "Adding port %d (%s)\n %!" sw.Switch.portnum 
            (Net.Manager.get_intf_name mgr ethif) in 
  let port = Switch.init_port mgr sw.Switch.portnum ethif in 
  sw.Switch.ports <- sw.Switch.ports @ [port];
  Hashtbl.add sw.Switch.int_to_port sw.Switch.portnum (ref port); 
  Hashtbl.add sw.Switch.dev_to_port ethif (ref port);
  sw.Switch.features.OP.Switch.ports  <- 
    sw.Switch.features.OP.Switch.ports @ [port.Switch.phy];
  let _ = Net.Manager.set_promiscuous mgr ethif (process_frame_inner sw) in
    ()

let create_switch () = 
  let (packet_queue, push_packet) = Lwt_stream.create () in
  Switch.(
    { ports = []; int_to_port = (Hashtbl.create 64); dev_to_port=(Hashtbl.create 64); 
      p_sflow = 0_l; controllers=[]; errornum = 0l; portnum=0; packet_queue; push_packet; 
      queue_len = 0; stats={n_frags=0L; n_hits=0L;n_missed=0L;n_lost=0L;};
    table = (Table.init_table ()); features=(Switch.switch_features ()); 
    packet_buffer=[]; packet_buffer_id=0l};)

let listen st mgr loc =
  Channel.listen mgr (`TCPv4 (loc, (control_channel st))) <&> (data_plane st ())

let connect st mgr loc init =
  init mgr st; 
  Channel.listen mgr (`TCPv4 (loc, (control_channel st))) <&> (data_plane st ())
