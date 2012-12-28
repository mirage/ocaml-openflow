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
open Lwt_log

open Net
open Nettypes
open Ofswitch_config
open Printf

module OP = Ofpacket

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
      wildcards=(OP.Wildcards.exact_match ()); max_entries=1024l; active_count=0l; 
      lookup_count=0L; matched_count=0L});}

  (* TODO fix flow_mod flag support. overlap is not considered *)
  let add_flow st table t =
    (* TODO check if the details are correct e.g. IP type etc. *)
    lwt _ = 
      log ~level:Notice 
      (sprintf "Adding flow %s" (OP.Match.match_to_string t.OP.Flow_mod.of_match)) in 
    let _ = Hashtbl.replace table.entries t.OP.Flow_mod.of_match 
              Entry.({actions=t.OP.Flow_mod.actions; counters=(init_flow_counters t); 
             cache_entries=[];}) in 
      return ()

  (* check if a list of actions has an output action forwarding packets to
   * out_port *)
  let rec is_output_port out_port = function
    | [] -> false
    | OP.Flow.Output(port, _)::_ when (port = out_port) -> true
    | head::tail -> is_output_port out_port tail 

  let del_flow table ?(xid=(Random.int32 Int32.max_int)) 
        ?(reason=OP.Flow_removed.DELETE) tuple out_port t =
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
      Lwt_list.iter_s (
      fun (of_match, flow) ->
        lwt _ = 
          log ~level:Notice 
          (sprintf "Adding flow %s" (OP.Match.match_to_string of_match)) in 
 
        if (t <> None) &&
          (flow.Entry.counters.Entry.flags.OP.Flow_mod.send_flow_rem) then
          let Some(t) = t in
          let duration_sec = Int32.sub (Int32.of_float (OS.Clock.time ()))  
            flow.Entry.counters.Entry.insert_sec in
          let fl_rm = OP.Flow_removed.(
            {of_match; cookie=flow.Entry.counters.Entry.cookie; 
            priority=flow.Entry.counters.Entry.priority;
            reason; duration_sec; duration_nsec=0l;
            idle_timeout=flow.Entry.counters.Entry.idle_timeout;
            packet_count=flow.Entry.counters.Entry.n_packets;
            byte_count=flow.Entry.counters.Entry.n_bytes;}) in
          let h = OP.Header.(create ~xid FLOW_REMOVED (OP.Flow_removed.get_len)) in
              Ofsocket.send_packet t (OP.Flow_removed (h,fl_rm)) 
       else 
          return ()
    ) remove_flow 

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
          del_flow table ~reason of_match OP.Port.No_port t
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
    let name = Manager.get_intf_name mgr ethif in 
    let hw_addr = Nettypes.ethernet_mac_to_bytes 
                    (Manager.get_intf_mac mgr ethif) in
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
    mutable controller: Ofsocket.conn_state option;
    table: Table.t;
    stats: stats;
    p_sflow: uint32; (** probability for sFlow sampling *)
    mutable errornum : uint32; 
    mutable portnum : int;
    packet_queue : (Cstruct.t * Net.Manager.id) Lwt_stream.t;
    push_packet : ((Cstruct.t * Net.Manager.id) option -> unit);
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


  let forward_frame st in_port bits pkt_size checksum port = 
    let _ = 
      if ((checksum) && ((get_dl_header_dl_type bits) = 0x800)) then
        let ip_data = Cstruct.shift bits sizeof_dl_header in
        let _ = set_nw_header_csum ip_data 0 in
        let csm = Net.Checksum.ones_complement (Cstruct.sub ip_data 0
        sizeof_nw_header) in
        let _ = set_nw_header_csum ip_data csm in
        let _ = 
          match (get_nw_header_nw_proto ip_data) with
          | 6 (* TCP *) -> ()
          | 17 (* UDP *) -> ()
          | _ -> ()
        in
          ()
    in 
    let frame = Net.Frame.of_buffer bits (Cstruct.len bits) in 
    match port with 
    | OP.Port.Port(port) -> 
      if Hashtbl.mem st.int_to_port port then(
(*        let _ = printf "sending to port %d\n%!" port in *)
        let out_p = (!( Hashtbl.find st.int_to_port port))  in
          Net.Manager.inject_packet out_p.mgr out_p.ethif frame
        ) 
      else
        return (Printf.printf "Port %d not registered \n" port)
    | OP.Port.No_port -> return ()
    | OP.Port.Flood 
    |OP.Port.All ->
      Lwt_list.iter_p  
      (fun port -> 
        if(port.port_id != (OP.Port.int_of_port in_port)) then (
         update_port_tx_stats (Int64.of_int (Cstruct.len bits)) port;
          Net.Manager.inject_packet port.mgr port.ethif frame
        ) else
          return ()
      ) st.ports
    | OP.Port.In_port ->
      let port = (OP.Port.int_of_port in_port) in 
      if Hashtbl.mem st.int_to_port port then
(*        let _ = printf "sending to port %d\n%!" port in *)
        let out_p = !(Hashtbl.find st.int_to_port port) in
          update_port_tx_stats (Int64.of_int (Cstruct.len bits)) out_p;
          Net.Manager.inject_packet out_p.mgr out_p.ethif frame
      else
        return (Printf.printf "Port %d not registered \n%!" port)
    | OP.Port.Local ->
        let local_port_id = OP.Port.int_of_port OP.Port.Local in 
      if Hashtbl.mem st.int_to_port local_port_id then
        let out_p = !(Hashtbl.find st.int_to_port local_port_id) in
(*        let _ = printf "sending to port %d\n%!" local_port_id in *)
        let _ = update_port_tx_stats (Int64.of_int (Cstruct.len bits)) out_p in 
          Net.Manager.inject_packet out_p.mgr out_p.ethif frame
      else
        return (Printf.printf "Port %d not registered \n%!" local_port_id)
 
        (*           | Table
         *           | Normal
         *           | Controller -> generate a packet out. 
         *           | Local -> can I inject this frame to the network
         *           stack?  *)
        | _ -> return (Printf.printf "Not implemented output port\n")

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
          let _ = set_dl_header_dl_src eaddr 0 bits in 
            return checksum
      | OP.Flow.Set_dl_dst(eaddr) ->
          let _ = set_dl_header_dl_dst eaddr 0 bits in 
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
        let _ = set_nw_header_nw_src ip_data ip in 
          return true
      | OP.Flow.Set_nw_dst(ip) -> 
        let ip_data = Cstruct.shift bits sizeof_dl_header in
        let _ = set_nw_header_nw_dst ip_data ip in 
          return true
      | OP.Flow.Set_tp_src(port) ->
        let ip_data = Cstruct.shift bits sizeof_dl_header in
        let len = (get_nw_header_hlen_version bits) land 0xf in 
        let tp_data = Cstruct.shift ip_data (len*4) in 
        let _ = set_tp_header_tp_src tp_data port in 
          return true
      | OP.Flow.Set_tp_dst(port) ->
        let ip_data = Cstruct.shift bits sizeof_dl_header in
        let len = (get_nw_header_hlen_version bits) land 0xf in 
        let tp_data = Cstruct.shift ip_data (len*4) in 
        let _ = set_tp_header_tp_dst tp_data port in 
          return true
      | OP.Flow.Enqueue(_, _)
      | OP.Flow.Set_vlan_pcp _
      | OP.Flow.Set_vlan_vid _
      | OP.Flow.VENDOR_ACT 
      | OP.Flow.STRIP_VLAN ->
        (* VLAN manupulation actions *)
          let _ = pr "Unsupported action STRIP_VLAN\n" in 
            return checksum
    with exn -> 
      let _ = eprintf "apply of action failed (packet size %d) %s %s\n%!" 
      (Cstruct.len bits)
       (OP.Flow.string_of_action action) (Printexc.to_string exn ) in
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
let process_frame_inner st p intf frame =
  match frame with
  | Net.Ethif.Output _ -> return ()
  | Net.Ethif.Input frame -> begin
    try_lwt
     let in_port = (OP.Port.port_of_int p.Switch.port_id) in 
     let tupple = (OP.Match.raw_packet_to_match in_port frame ) in

     (* Update port rx statistics *)
     let _ = Switch.update_port_rx_stats (Int64.of_int (Cstruct.len frame)) p in

   (* Lookup packet flow to existing flows in table *)
     let entry = (Switch.lookup_flow st tupple) in 
     match entry with 
     | Switch.NOT_FOUND -> begin
       let _ = Table.update_table_missed st.Switch.table in
       let buffer_id = st.Switch.packet_buffer_id in
         (*TODO Move this code in the Switch module *)
       st.Switch.packet_buffer_id <- Int32.add st.Switch.packet_buffer_id 1l;
       let (_, pkt_in) = OP.Packet_in.create_pkt_in ~buffer_id ~in_port 
                      ~reason:OP.Packet_in.NO_MATCH ~data:frame in 
       let _ = st.Switch.packet_buffer <- st.Switch.packet_buffer @ [pkt_in] in
       let size = 
         if (Cstruct.len frame > 92) then
           92
         else 
           Cstruct.len frame 
       in
       let (h, pkt_in) = OP.Packet_in.create_pkt_in ~buffer_id ~in_port 
                      ~reason:OP.Packet_in.NO_MATCH 
                      ~data:(Cstruct.sub frame 0 size) in
       match st.Switch.controller with
       | None -> return ()
       | Some conn -> Ofsocket.send_packet conn (OP.Packet_in (h, pkt_in))
    end
       (* generate a packet in event *)
     | Switch.Found(entry) ->
       let _ = Table.update_table_found st.Switch.table in
       let _ = Entry.update_flow (Int64.of_int (Cstruct.len frame)) !entry in
        Switch.apply_of_actions st tupple.OP.Match.in_port 
          frame (!entry).Entry.actions
    with exn ->
      pp "control channel error: %s\nbt: %s\n%!" 
        (Printexc.to_string exn) (Printexc.get_backtrace ());
      return ()
  end

let process_frame p st intf_name frame =
  try_lwt 
    (*let p = Hashtbl.find st.Switch.dev_to_port intf_name in *)
    process_frame_inner st p intf_name frame
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

(*let data_plane st () = 
  try_lwt 
  while_lwt true do 
    lwt a = Lwt_stream.get st.Switch.packet_queue in
    match a with
    | Some (pkt, p) ->
      st.Switch.queue_len <- st.Switch.queue_len - 1;
      process_frame_inner st p pkt
    | None -> return ()
  done *)


(*************************************************
 * Switch OpenFlow control channel 
 *************************************************)

type endhost = {
  ip: Nettypes.ipv4_addr;
  port: int;
}

let get_flow_stats st of_match = 
  let match_flows of_match key value ret =
    if (OP.Match.flow_match_compare key of_match 
              of_match.OP.Match.wildcards) then ( 
                ret @ [
                  (Entry.flow_counters_to_flow_stats 
              key (char_of_int 1) value)] 
    ) else 
      ret 
  in
    Hashtbl.fold (fun key value r -> match_flows of_match key value r) 
    st.Switch.table.Table.entries []  
 

let process_openflow st t msg = 
  match msg with
  | OP.Hello (h) -> 
    (* Reply to HELLO with a HELLO and a feature request *)
    cp "HELLO";
    return ()
  | OP.Echo_req h -> (* Reply to ECHO requests *)
    cp "ECHO_REQ";
    Ofsocket.send_packet t 
      (OP.Echo_req 
        OP.Header.(create ~xid:h.xid ECHO_RESP sizeof_ofp_header))
 | OP.Features_req (h)  -> 
    cp "FEAT_REQ";
    let h = OP.Header.create ~xid:(h.OP.Header.xid) OP.Header.FEATURES_RESP 
              (OP.Switch.get_len st.Switch.features) in 
      Ofsocket.send_packet t (OP.Features_resp (h, st.Switch.features))
 | OP.Stats_req(h, req) -> begin
    let xid = h.OP.Header.xid in 
    cp "STATS_REQ";
    match req with
    | OP.Stats.Desc_req(req) ->
      let p = 
        OP.Stats.(Desc_resp (
          {st_ty=DESC; more_to_follow=false;},
          { imfr_desc="Mirage"; hw_desc="Mirage";
            sw_desc="Mirage"; serial_num="0.1";dp_desc="Mirage";}
      )) in 
      let h = OP.Header.create ~xid OP.Header.STATS_RESP (OP.Stats.resp_get_len p) in 
        Ofsocket.send_packet t (OP.Stats_resp (h, p))
   | OP.Stats.Flow_req(req_h, of_match, table_id, out_port) ->
      (*TODO Need to consider the  table_id and the out_port and 
       * split reply over multiple openflow packets if they don't
       * fit a single packet. *)
      let flows = get_flow_stats st of_match in 
      let stats = OP.Stats.({st_ty=FLOW; more_to_follow=false;}) in 
      let r = OP.Stats.Flow_resp(stats, flows) in
      let h = OP.Header.create ~xid OP.Header.STATS_RESP (OP.Stats.resp_get_len r) in 
        Ofsocket.send_packet t (OP.Stats_resp (h, r)) 
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
      let r = OP.Stats.Aggregate_resp(stats, 
                    OP.Stats.({byte_count=(!aggr_flow_bytes);
                    packet_count=(!aggr_flow_pkts);
                    flow_count=(!aggr_flows);})) 
      in 
      let h = OP.Header.create ~xid OP.Header.STATS_RESP (OP.Stats.resp_get_len r) in 
        Ofsocket.send_packet t (OP.Stats_resp (h, r)) 
   | OP.Stats.Table_req(req) ->
      let stats = OP.Stats.({st_ty=TABLE; more_to_follow=false;}) in  
      let r = OP.Stats.Table_resp(stats, [st.Switch.table.Table.stats]) in 
      let h = OP.Header.create ~xid OP.Header.STATS_RESP (OP.Stats.resp_get_len r) in 
        Ofsocket.send_packet t (OP.Stats_resp (h, r)) 
   | OP.Stats.Port_req(req_h, port) -> begin
      match port with
      | OP.Port.No_port -> 
        let port_stats = List.map (fun p -> p.Switch.counter) st.Switch.ports in
        let stats = OP.Stats.({st_ty=PORT; more_to_follow=false;}) in 
        let r = OP.Stats.Port_resp(stats, port_stats) in 
        let h = OP.Header.create ~xid OP.Header.STATS_RESP (OP.Stats.resp_get_len r) in 
          Ofsocket.send_packet t (OP.Stats_resp (h, r)) 
      | OP.Port.Port(port_id) -> 
        try_lwt 
          let port = Hashtbl.find st.Switch.int_to_port port_id in
          let stats = OP.Stats.({st_ty=PORT; more_to_follow=false;}) in 
          let r = OP.Stats.Port_resp(stats, [(!port).Switch.counter]) in 
          let h = OP.Header.create ~xid OP.Header.STATS_RESP (OP.Stats.resp_get_len r) in 
            Ofsocket.send_packet t (OP.Stats_resp (h, r)) 
       with Not_found ->
          (* TODO reply with right error code *)
          pr "Invalid port_id in stats\n%!";
          return ()
        end
      | _ ->
          let bits = OP.marshal msg in 
          let h = OP.Header.create ~xid OP.Header.ERROR 
                    (OP.Header.get_len + 4 + (Cstruct.len bits)) in 
          Ofsocket.send_packet t (OP.Error (h, OP.REQUEST_BAD_SUBTYPE, bits))
  end
  | OP.Get_config_req(h) ->
      let resp = OP.Switch.init_switch_config in
      let h = OP.Header.create ~xid:h.OP.Header.xid OP.Header.GET_CONFIG_RESP 
                OP.Switch.config_get_len in 
        Ofsocket.send_packet t (OP.Get_config_resp(h, resp)) 
 | OP.Barrier_req(h) ->
     cp (sp "BARRIER_REQ: %s" (OP.Header.header_to_string h));
     let resp_h = (OP.Header.create ~xid:h.OP.Header.xid OP.Header.BARRIER_RESP
                  (OP.Header.sizeof_ofp_header)) in
        Ofsocket.send_packet t (OP.Barrier_resp(resp_h)) 

 | OP.Packet_out(h, pkt) ->
(*    cp (sp "PACKET_OUT: %s" (OP.Packet_out.packet_out_to_string pkt)); *)
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
            let bits = OP.marshal msg in 
            let h = OP.Header.create ~xid:h.OP.Header.xid OP.Header.ERROR 
                    (OP.Header.get_len + 4 + (Cstruct.len bits)) in 
              Ofsocket.send_packet t (OP.Error(h, OP.REQUEST_BUFFER_UNKNOWN, bits)) 
       | Some(pkt_in) ->
(*            let _ = pr "Sending as packet_out data %d\n%!" 
                      (Cstruct.len pkt_in.OP.Packet_in.data) in *)
            Switch.apply_of_actions st pkt_in.OP.Packet_in.in_port
              pkt_in.OP.Packet_in.data pkt.OP.Packet_out.actions
    end 
  | OP.Flow_mod(h,fm)  ->
    cp (sp "FLOW_MOD: %s" (OP.Flow_mod.flow_mod_to_string fm)); 
    let of_match = fm.OP.Flow_mod.of_match in 
    let of_actions = fm.OP.Flow_mod.actions in
    lwt _ = 
      match (fm.OP.Flow_mod.command) with
      | OP.Flow_mod.ADD 
      | OP.Flow_mod.MODIFY 
      | OP.Flow_mod.MODIFY_STRICT -> 
        Table.add_flow st st.Switch.table fm
      | OP.Flow_mod.DELETE 
      | OP.Flow_mod.DELETE_STRICT ->
          (* Need to implemente strict deletion in order to enable signpost
           * switching *)
        Table.del_flow st.Switch.table of_match fm.OP.Flow_mod.out_port 
          (Some t)
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
                let bits = OP.marshal msg in 
                let h = OP.Header.create ~xid:h.OP.Header.xid OP.Header.ERROR 
                    (OP.Header.get_len + 4 + (Cstruct.len bits)) in 
                Ofsocket.send_packet t (OP.Error(h, OP.REQUEST_BUFFER_UNKNOWN, bits)) 
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
  | OP.Echo_resp h
  | OP.Error (h, _, _) ->
      let bits = OP.marshal msg in 
      let h = OP.Header.create ~xid:h.OP.Header.xid OP.Header.ERROR 
                    (OP.Header.get_len + 4 + (Cstruct.len bits)) in 
      Ofsocket.send_packet t (OP.Error(h, OP.REQUEST_BAD_TYPE, bits)) 

let control_channel_run st conn t = 
  (* Trigger the dance between the 2 nodes *)
  let h = OP.Header.(create ~xid:1l HELLO sizeof_ofp_header) in  
  lwt _ = Ofsocket.send_packet conn (OP.Hello(h)) in
  let rec echo () =
    try_lwt
      lwt ofp = Ofsocket.read_packet conn in 
      lwt _ = process_openflow st conn ofp in
        echo ()
    with
    | Nettypes.Closed -> 
        pr "Controller channel closed....\n%!";
         return ()
    | OP.Unparsed (m, bs) -> (pr "# unparsed! m=%s\n %!" m); echo ()
    | exn -> 
        pr "[OpenFlow-Switch-Control] ERROR:%s\n%!" (Printexc.to_string exn);
        (echo () ) 

  in
  lwt _ = 
    ( lwt _ = t  in return (printf "thread terminated\n%!"))<?>
    echo () <?> 
    (lwt _ = Table.monitor_flow_timeout st.Switch.table (Some conn) in return (pp
    "[switch] terminated flow_timeout thread") )
  in
  let _ = Ofsocket.close conn in 
  let _ = st.Switch.controller <- None in 
    return (printf "control channel thread returned\n%!")

let control_channel st (remote_addr, remote_port) t =
  let rs = Nettypes.ipv4_addr_to_string remote_addr in
  Printf.eprintf "OpenFlow Switch: controller %s:%d" rs remote_port; 
  let conn = Ofsocket.init_socket_conn_state t in
  let _ = st.Switch.controller <- (Some conn) in 
  let t, _ = Lwt.task () in 
    control_channel_run st conn t 

(*
 * Interaface with external applications
 * *)
let add_port mgr ?(use_mac=false) sw ethif = 
  sw.Switch.portnum <- sw.Switch.portnum + 1;
  let hw_addr =  
    (Net.Nettypes.ethernet_mac_to_string
    (Manager.get_intf_mac mgr ethif)) in
  let _ = pr "Adding port %d (%s) '%s' \n %!" sw.Switch.portnum 
            (Net.Manager.get_intf_name mgr ethif) hw_addr in 
  lwt _ = log ~level:Notice (sprintf "Adding port %s (port_id=%d)" ethif
  sw.Switch.portnum) in 
  lwt _ = 
    if (use_mac) then
      lwt _ = Lwt_unix.system (sprintf "ifconfig tap0 lladdr %s" hw_addr) in
        return ()
    else
      return ()
  in
  let port = Switch.init_port mgr sw.Switch.portnum ethif in
  sw.Switch.ports <- sw.Switch.ports @ [port];
  Hashtbl.add sw.Switch.int_to_port sw.Switch.portnum (ref port); 
  Hashtbl.add sw.Switch.dev_to_port ethif (ref port);
  sw.Switch.features.OP.Switch.ports  <- 
    sw.Switch.features.OP.Switch.ports @ [port.Switch.phy];
  let _ = Net.Manager.set_promiscuous mgr ethif (process_frame_inner sw port) in
  let h,p = OP.Port.create_port_status OP.Port.ADD port.Switch.phy in 
  lwt _ = 
    match sw.Switch.controller with
    | None -> return ()
    | Some t -> Ofsocket.send_packet t (OP.Port_status (h,p)) 
  in 
   return ()

let del_port mgr sw name =
  try_lwt 
  let port = 
    List.find (
      fun a -> 
        name = (Net.Manager.get_intf_name mgr a.Switch.ethif) 
    ) sw.Switch.ports in 
  let _ = sw.Switch.ports <- List.filter (
    fun a ->
      if (name = (Net.Manager.get_intf_name mgr a.Switch.ethif)) then
        let _ = printf "removing port %s\n%!" (Net.Manager.get_intf_name mgr
        a.Switch.ethif) in 
        false
      else 
        true
(*      not (name = (Net.Manager.get_intf_name mgr a.Switch.ethif)  ) *)
  ) sw.Switch.ports in 
  let _ = Hashtbl.remove sw.Switch.int_to_port port.Switch.port_id in  
  let _ = Hashtbl.remove sw.Switch.dev_to_port port.Switch.ethif in 
  let h,p = OP.Port.create_port_status OP.Port.DEL port.Switch.phy in 
  let of_match = OP.Match.create_flow_match (OP.Wildcards.full_wildcard ()) () in 
  lwt _ = Table.del_flow sw.Switch.table of_match 
            (OP.Port.port_of_int port.Switch.port_id) 
            sw.Switch.controller in 
  let of_match = OP.Match.create_flow_match (OP.Wildcards.in_port_match ())
                  ~in_port:(port.Switch.port_id) () in 
  lwt _ = Table.del_flow sw.Switch.table of_match 
            OP.Port.No_port sw.Switch.controller in 
  lwt _ = log ~level:Notice (sprintf "Removing port %s (port_id=%d)" name
            port.Switch.port_id) in 
  lwt _ = 
    match sw.Switch.controller with
    | None -> return ()
    | Some t -> Ofsocket.send_packet t (OP.Port_status (h,p)) 
  in 
    return ()
  with exn -> 
    let _ = printf "[switch] device not found %s\n%!" name in 
      return ()

let add_port_local mgr sw ethif = 
 (*TODO Find first if a port is already registered as port 0 
  * as port 0 and disable it *)
  let local_port_id = OP.Port.int_of_port OP.Port.Local in 
  let port = Switch.init_port mgr local_port_id ethif in 
  sw.Switch.ports <- 
  (List.filter (fun a -> (a.Switch.port_id <> 0)) sw.Switch.ports) 
  @ [port];
  Hashtbl.replace sw.Switch.int_to_port local_port_id (ref port); 
  Hashtbl.iter 
    (fun a b -> 
       if (!b.Switch.port_id = local_port_id) then 
         Hashtbl.remove sw.Switch.dev_to_port a
    ) sw.Switch.dev_to_port;
  Hashtbl.add sw.Switch.dev_to_port ethif (ref port);
  (*TODO Need to filter out any 0 port *)
  sw.Switch.features.OP.Switch.ports <- 
  (List.filter (fun a -> (a.OP.Port.port_no <> local_port_id)) 
                           sw.Switch.features.OP.Switch.ports )
   @ [port.Switch.phy];
  lwt _ = log ~level:Notice (sprintf "Adding port %s (port_id=%d)" ethif
            local_port_id) in 
   let _ = Net.Manager.set_promiscuous mgr ethif (process_frame_inner sw port) in
    return ()

let add_flow st fm = Table.add_flow st st.Switch.table fm
let del_flow st m = 
  Table.del_flow st.Switch.table m OP.Port.No_port st.Switch.controller
 
let create_switch () = 
  let (packet_queue, push_packet) = Lwt_stream.create () in
  Switch.(
    { ports = []; int_to_port = (Hashtbl.create 64); dev_to_port=(Hashtbl.create 64); 
      p_sflow = 0_l; controller=None; errornum = 0l; portnum=0; packet_queue; push_packet; 
      queue_len = 0; stats={n_frags=0L; n_hits=0L;n_missed=0L;n_lost=0L;};
    table = (Table.init_table ()); features=(Switch.switch_features ()); 
    packet_buffer=[]; packet_buffer_id=0l;})

let listen st mgr loc =
  Channel.listen mgr (`TCPv4 (loc, (control_channel st ))) <&>
    (Ofswitch_config.listen_t mgr 
    (del_port mgr st) (get_flow_stats st) (add_flow st) (del_flow st) 6634)

let connect st mgr loc  =
  Channel.connect mgr (`TCPv4 (None, loc, (control_channel st loc))) <&> 
    (Ofswitch_config.listen_t mgr (del_port mgr st) (get_flow_stats st) 
     (add_flow st) (del_flow st) 6634)

let local_connect st mgr input output   =
  let conn = Ofsocket.init_local_conn_state input output in 
  let _ = st.Switch.controller <- (Some conn) in 
  let t, _ = Lwt.task () in 
     (control_channel_run st conn t) <&> 
    (Ofswitch_config.listen_t mgr (del_port mgr st) (get_flow_stats st) 
     (add_flow st) (del_flow st) 6634)

let lwt_connect st ?(standalone=true) mgr loc  =
  let of_ctrl = Ofswitch_standalone.init_controller () in 

  let _ = Lwt.ignore_result (Ofswitch_config.listen_t mgr (del_port mgr st) 
            (get_flow_stats st)  (add_flow st) (del_flow st) 6634) in 
(*  let _ = Lwt_unix.setsockopt fd Unix.SO_REUSEADDR true in 
  let _ = Lwt_unix.bind fd (Unix.ADDR_INET(Unix.inet_addr_any, 6633)) in 
  let _ = Lwt_unix.listen fd 1 in *)
  let _ = printf "[switch] Listening socket...\n%!" in 
    while_lwt true do
      let t,u = Lwt.task () in 
      ((
        lwt _ = log ~level:Notice "Standalone controller taking over..." in 
         lwt (switch_in, switch_out) = 
          Ofswitch_standalone.run_controller mgr of_ctrl in
        let conn = Ofsocket.init_local_conn_state switch_in switch_out in 
        let _ = st.Switch.controller <- (Some conn) in 
        lwt _ = control_channel_run st conn t in 
        lwt _ = log ~level:Notice "Standalone controller stopped..." in 
          return ()
      ) <&>
      (
        let rec connect_socket () =
          try_lwt
            let sock = Lwt_unix.socket 
                        Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
            let dst = Unix.ADDR_INET(
              (Unix.inet_addr_of_string "127.0.0.1"), 6633) in 
            lwt addr = Lwt_unix.connect sock dst in 
              return sock 
          with exn -> 
            lwt _ = Lwt_unix.sleep 5.0 in 
              connect_socket ()
        in
        (* lwt (sock, loc) = Lwt_unix.accept fd in *)
        lwt sock = connect_socket () in 
        let conn = Ofsocket.init_unix_conn_state sock in 
        let _ = wakeup u () in 
        let t,_ = Lwt.task () in 
        let _ = st.Switch.controller <- (Some conn) in 
        lwt _ = log ~level:Notice "Remote controller started..." in 
          control_channel_run st conn t
      ))
    done
