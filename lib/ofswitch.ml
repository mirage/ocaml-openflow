(*
 * Copyright (c) 2011 Richard Mortier <mort@cantab.net>
 * Copyright (c) 2014 Charalampos Rotsos <cr409@cl.cam.ac.uk>
 *                    Masoud Koleini <masoud.koleini@nottingham.ac.uk>
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
open V1_LWT
open Lwt

open Ofsocket

module OP = Ofpacket

exception Packet_type_unknw

let sp = Printf.sprintf
let pp = Printf.printf
let ep = Printf.eprintf
(* let cp = Console.log_s *)

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
  let buf = Io_page.to_cstruct (Io_page.get 1) in 
    Cstruct.sub buf 0 len 

(*
let get_ethif mgr id = 
    let lst = Net.Manager.get_intfs mgr in 
    let (_, ethif) = List.find (fun (dev_id,_) -> id = dev_id) lst in 
    ethif
*)

let or_error name fn t =
  fn t
  >>= function
	| `Error e -> fail (Failure ("Error starting " ^ name))
    | `Ok t -> let _ = (pp "%s works...\n" name) in
			   return t

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
    let ts = int_of_float (Clock.time ()) in
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
    flow.counters.last_sec <- int_of_float (Clock.time ())


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


module Make(T:TCPV4 (* controller *))(N:NETWORK) = struct

  module E = Ethif.Make(N)
  module Channel = Channel.Make(T)
  module OSK = Ofsocket.Make(T)

  type eth_t = E.t 
(*  type eth_netif = E.netif

  type ip_if = I.t
*)
  type tcp_t = T.t
  type tcp_flow = T.flow
  type tcp_error = T.error

  type port = {
    (* mgr: Net.Manager.t; *)
    port_id: int;
    (* ethif: Net.Manager.id; *)
    ethif: E.t; (* changed from netif *)
    port_name: string;
    counter: OP.Port.stats;
    phy: OP.Port.phy;
    in_queue: Cstruct.t Lwt_stream.t;
    in_push : (Cstruct.t option -> unit);
    out_queue: Cstruct.t Lwt_stream.t;
    out_push : (Cstruct.t option -> unit);
    mutable pkt_count : int;
  }


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
		    pp "[switch] Adding flow %s" (OP.Match.match_to_string t.of_match)
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
            pp "[switch] Removing flow %s" (OP.Match.match_to_string of_match)
        in 
        match(t, flow.Entry.counters.Entry.flags.OP.Flow_mod.send_flow_rem) with
        | (Some t, true) -> 
          let duration_sec = (int_of_float (Clock.time ()))  -
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
		  let ts = int_of_float (Clock.time ()) in 
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

  let init_port port_no ethif =

(*    let ethif = Net.Manager.get_ethif ( get_ethif mgr id ) in 
    let netif = Net.Ethif.get_netif ethif in *)
    let name = "" (* XXX todo *) (* OS.Netif.string_of_id (OS.Netif.id (Net.Ethif.get_netif ethif ))*) in 
    let hw_addr = E.mac ethif in
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
    
    {port_id=port_no; (* mgr;*) port_name=name; counter;
     ethif=ethif; (*netif;*) phy;in_queue;in_push;pkt_count=0;
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
    (* Mapping Netif objects to ports *) (* XXX each port has Netif record, do we need it here anymore? *)
    (* mutable dev_to_port: (Net.Manager.id, port ref) Hashtbl.t; *)
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
    mutable pkt_len : int;
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

  (* we have exactly the same function in pcb.mli *)
  let tcp_checksum ~src ~dst =
    let pbuf = Cstruct.sub (Cstruct.of_bigarray (Io_page.get 1)) 0 sizeof_pseudo_header in
    fun data ->
      set_pseudo_header_src pbuf (Ipaddr.V4.to_int32 src);
      set_pseudo_header_dst pbuf (Ipaddr.V4.to_int32 dst);
      set_pseudo_header_res pbuf 0;
      set_pseudo_header_proto pbuf 6;
      set_pseudo_header_len pbuf (Cstruct.lenv data);
      Tcpip_checksum.ones_complement_list (pbuf::data)

  let send_packet port bits =
    update_port_tx_stats (Int64.of_int (Cstruct.len bits)) port;
    return (port.out_push (Some bits))
(*    OS.Netif.write port.netif bits *)
(*    Net.Manager.inject_packet port.mgr port.ethif bits *)

  
  let forward_frame st (* it has controller *) in_port bits pkt_size checksum port = 
    let _ = 
      if ((checksum) && ((get_dl_header_dl_type bits) = 0x800)) then
        let ip_data = Cstruct.shift bits sizeof_dl_header in
        let len = (get_nw_header_hlen_version ip_data) land 0xf in 
        let _ = set_nw_header_csum ip_data 0 in
        let csm = Tcpip_checksum.ones_complement (Cstruct.sub ip_data 0 (len*4)) in
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
        return (pp "[switch] forward_frame: Port %d not registered\n%!" port)
    | OP.Port.No_port -> return ()
    | OP.Port.Flood 
    | OP.Port.All ->
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
        return (pp "[switch] forward_frame: Port %d unregistered\n%!" port)
    | OP.Port.Local ->
      let local = OP.Port.int_of_port OP.Port.Local in 
      if Hashtbl.mem st.int_to_port local then
        send_packet !(Hashtbl.find st.int_to_port local) bits
      else
        return (pp "[switch] forward_frame: Port %d unregistered \n%!" local)
(* XXX Controller port is removed ... *)

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
            return (pp "[switch] forward_frame: unsupported output port\n")

  (* Assume that action are valid. I will not get a flow that sets an ip
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
          let _ = (pp "[switch] apply_of_actions: Unsupported action %s" 
                        (OP.Flow.string_of_action act)) in 
          return checksum
      with exn -> 
        let _ = (pp  "[switch] apply_of_actions: (packet size %d) %s %s\n%!" 
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
  
  (* added for switch initialization *)
(*
  let ifconnect net = or_error "[Swicth] connecting to ethernet interface" E.connect net

  let ipconfig eif (switchaddr, netmask, gateway) =
	or_error "[Swicth] connecting to ipv4 stack" I.connect eif
    >>= fun i -> I.set_ipv4 i Ipaddr.V4.any (* (Ipaddr.V4.of_string_exn switchaddr) *)
    >>= fun () -> I.set_ipv4_netmask i (Ipaddr.V4.of_string_exn netmask)
    >>= fun () -> I.set_ipv4_gateways i [(Ipaddr.V4.of_string_exn gateway)]
	>> return i
*)
  (* initialize switch given network interface, and ip addresses *)

  let bind_tcp_stack ipif =
	or_error "[Swicth] connecting to tcp stack" T.connect ipif

  let create_tcp_connection tcp (contaddr, contport) =
	(* return (pp "[Switch] tcp port: %s \n" (Ipaddr.V4.to_string 
												((fun (x, y) -> x) (T.get_dest tcp))
										  )
		   ) >> *)
	T.create_connection tcp (Ipaddr.V4.of_string_exn contaddr, contport)
	>>= function 
		  | `Error e -> fail (Failure "[Swicth] failed connecting to the controller")
		  | `Ok fl -> (return fl)  (* returns flow *)

(*
  let init net sw_ip_info cont =
	ifconnect net 
	>>= fun eif -> ipconfig eif sw_ip_info
    >>= fun ipif-> bind_tcp_stack ipif *)
(* 	>>= fun tcp -> create_tcp_connection tcp cont *)


(* this has to be re-written completely *)
  let process_frame st p frame =
	(* let _ = print_endline "### processing frame ...###" in 
	let _ = pp "frame no. %d" (p.pkt_count + 1) in
	let _ = print_endline "" in *)
(*	let _ = 
	try 
	  match frame with
      | Net.Ethif.Output _ -> ()
      | Net.Ethif.Input frame -> *)
(*        let _ = Lwt_condition.broadcast st.ready () in *)
(*          if (p.Switch.pkt_count < 1000) then *)
            let _ = p.pkt_count <- p.pkt_count + 1 in
(*            let _ = Printf.printf "pushing packet to port %d %d\n%!"
                p.Switch.port_id p.Switch.pkt_count in *)
            p.in_push (Some frame);
(*            Printf.printf "pushed packet to port %d\n%!" p.Switch.port_id*)
(*          else
            cp "[process_frame] blocked queue" *)
(*	with 
    | Not_found -> pp "[switch] process_frame: Invalid port\n%!"
    | Packet_type_unknw -> pp "[switch] process_frame: malformed packet\n%!"
    | exn -> pp "[switch] process_frame: switch error: %s\n%!" (Printexc.to_string exn)
  in *)
  return ()

  let init_switch_info ?(verbose=true) dpid = 
    { (* dev_to_port=(Hashtbl.create 64); *)
	  int_to_port = (Hashtbl.create 64); ports = [];
      controller=None;
	  last_echo_req=0.; echo_resp_received=true;
      stats={n_frags=0L; n_hits=0L;n_missed=0L;n_lost=0L;};
	  errornum = 0l; portnum=0;
	  table = (Table.init_table ());
      features=(switch_features dpid); 
      packet_buffer=[]; packet_buffer_id=0l; ready=(Lwt_condition.create ());
      verbose;pkt_len=1500;}


(* add port to the switch *) 
  let add_port ?(use_mac=false) sw id = 

	sw.portnum <- sw.portnum + 1; (* (* XXX check *)
	let ethif = Net.Manager.get_ethif (get_ethif mgr id) in 
	let hw_addr =  Macaddr.to_string (E.mac id) in
	let dev_name = N.id (E.id id) in
	let _ = pp "[switch] Adding port %d (%s) '%s' \n %!" 
                            sw.portnum dev_name hw_addr in *)
	let port = init_port sw.portnum id in
	sw.ports <- sw.ports @ [port];
	Hashtbl.add sw.int_to_port sw.portnum (ref port); 
	(* Hashtbl.add sw.dev_to_port id (ref port); *)(* XXX check. Ignored for the tests *)
	sw.features.ports  <- sw.features.ports @ [port.phy];
	(* let _ = Net.Manager.set_promiscuous mgr id (process_frame sw port) in *)
	let _ = N.listen (E.id id) (process_frame sw port) in
(*	let h,p = OP.Port.create_port_status OP.Port.ADD port.phy in 
	match sw.controller with
	  | None -> return ()
	  | Some t -> OSK.send_packet t (OP.Port_status (h,p))
*) (* XXX sending this Port_status makes Frenetic controller to crash!
		disabled for the tests with Frenetic *)
	return ()

let get_flow_stats st of_match =
  let open OP.Match in 
  let match_flows of_match key value ret =
    if (flow_match_compare key of_match of_match.wildcards) then ( 
      (Entry.flow_counters_to_flow_stats key (char_of_int 1) value)::ret
    ) else 
      ret 
  in
    Hashtbl.fold (fun key value r -> match_flows of_match key value r) 
    st.table.Table.entries []  

(* for test *)
let rec print_list = function 
[] -> ()
| e::l -> pp "buff: %ld " e.OP.Packet_in.buffer_id; print_list l

let process_buffer_id st t msg xid buffer_id actions =
  let open OP.Header in
  let pkt_in = ref None in 
  let _ = (*
	let _ = print_list st.packet_buffer in
	let _ = print_endline "" in
	let _ = pp "buff id received: %ld " buffer_id in
	let _ = print_endline "" in *)
	st.packet_buffer <- 
      List.filter ( fun a -> 
    	if (a.OP.Packet_in.buffer_id = buffer_id) then 
    	  (pkt_in := Some(a); false )
      	else true ) st.packet_buffer in 
		  match (!pkt_in) with 
			| None ->
				pp "[switch**] invalid buffer id %ld\n%!" buffer_id; 
				let bits = OP.marshal msg in 
				let h = create ~xid ERROR (get_len + 4 + (Cstruct.len bits)) in 
				  OSK.send_packet t (OP.Error(h, OP.REQUEST_BUFFER_UNKNOWN, bits))
				(* XXX in testing with Beacon, if Beacon runs on the same machine,
					it sends OF Packet_Out twice in one TCP packet (as PDU.)
					Beacon works fine when it runs on a different machine. (Check the reason.)
				*) 
			| Some(pkt_in) ->
				OP.Packet_in.(apply_of_actions st pkt_in.in_port pkt_in.data actions)

let process_openflow st t msg =
  let open OP.Header in 

(*  let _ = print_endline "********************************************" in *)

  let _ = if st.verbose then pp "[switch*] %s\n%!" (OP.to_string msg) in
  match msg with
	| OP.Hello (h) -> return ()
	| OP.Echo_resp h -> return (st.echo_resp_received <- true)
	| OP.Echo_req h -> (* Reply to ECHO requests *)
    	OSK.send_packet t (OP.Echo_req (create ~xid:h.xid ECHO_RESP sizeof_ofp_header))
	| OP.Features_req (h)  -> 
      let h = create ~xid:(h.xid) FEATURES_RESP (OP.Switch.get_len st.features) in 
    	OSK.send_packet t (OP.Features_resp (h, st.features)) 

	| OP.Stats_req(h, req) -> begin
		let xid = h.xid in 
		match req with
		  | OP.Stats.Desc_req(req) ->
			let p = OP.Stats.(Desc_resp ({st_ty=DESC; more=false;},
                                  (create_desc_stat_resp "Mirage" "Mirage" "Mirage"
                                  "0.1" "Mirage"))) in 
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
				            st.table.Table.entries (0L, 0L, 0l) in
			  let stats = OP.Stats.({st_ty=AGGREGATE; more=false;}) in  
			  let r = OP.Stats.Aggregate_resp(stats, 
				            OP.Stats.({byte_count;packet_count;flow_count;})) in 
			  let h = create ~xid STATS_RESP (OP.Stats.resp_get_len r) in 
				OSK.send_packet t (OP.Stats_resp (h, r)) 
		  | OP.Stats.Table_req(req) ->
			  let stats = OP.Stats.({st_ty=TABLE; more=false;}) in  
			  let r = OP.Stats.Table_resp(stats, [st.table.Table.stats]) in 
			  let h = create ~xid STATS_RESP (OP.Stats.resp_get_len r) in 
				OSK.send_packet t (OP.Stats_resp (h, r)) 
		  | OP.Stats.Port_req(req_h, port) -> begin
			   match port with
			   | OP.Port.No_port -> 
				 let port_stats = List.map (fun p -> p.counter) st.ports in
				 let stats = OP.Stats.({st_ty=PORT; more=false;}) in 
				 let r = OP.Stats.Port_resp(stats, port_stats) in 
				 let h = OP.Header.create ~xid OP.Header.STATS_RESP (OP.Stats.resp_get_len r) in 
				 OSK.send_packet t (OP.Stats_resp (h, r)) 
			   | OP.Port.Port(port_id) -> begin
				   try_lwt 
				    let port = Hashtbl.find st.int_to_port port_id in
				    let stats = OP.Stats.({st_ty=PORT; more=false;}) in 
				    let r = OP.Stats.Port_resp(stats, [(!port).counter]) in 
				    let h = create ~xid STATS_RESP (OP.Stats.resp_get_len r) in 
				    OSK.send_packet t (OP.Stats_resp (h, r))
				   with Not_found ->
				  (* TODO reply with right error code *)
				      pp "[switch] unregistered port %s\n%!"(OP.Port.string_of_port port);
				      let h = create ~xid ERROR (OP.Header.get_len + 4) in 
				      OSK.send_packet t (OP.Error (h, OP.ACTION_BAD_OUT_PORT, (get_new_buffer 0)))
				  end 
			  | _ -> 
          		pp "[switch] unsupported stats request\n%!";
				let h = create ~xid ERROR (get_len + 4) in 
				OSK.send_packet t OP.(Error (h, ACTION_BAD_OUT_PORT, (marshal msg))) 
		  end 
		  | _ -> begin 
          	let h = create ~xid ERROR (get_len + 4) in 
          	OSK.send_packet t (OP.Error (h, OP.REQUEST_BAD_SUBTYPE, (get_new_buffer 0)))
        	end
	end

  | OP.Get_config_req(h) ->
	  let resp = OP.Switch.init_switch_config st.pkt_len in
		let h = create ~xid:h.xid GET_CONFIG_RESP OP.Switch.config_get_len in 
		 OSK.send_packet t (OP.Get_config_resp(h, resp)) 

  | OP.Barrier_req(h) ->
	  OSK.send_packet t (OP.Barrier_resp(create ~xid:h.xid BARRIER_RESP sizeof_ofp_header))

  | OP.Packet_out(h, pkt) ->
	  let open OP.Packet_out in 
		if (pkt.buffer_id = -1l) then
		  apply_of_actions st pkt.in_port pkt.data pkt.actions
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
            Table.add_flow st st.table fm st.verbose
          | DELETE 
          | DELETE_STRICT ->
            (* Need to implemente strict deletion in order to enable signpost
             * switching *)
            Table.del_flow st.table fm.of_match fm.out_port (Some t) st.verbose 
      in
      if ((fm.buffer_id = -1l) || (fm.buffer_id = 0l)) then return () 
      else 
		process_buffer_id st t msg h.xid fm.buffer_id fm.actions 

  | OP.Set_config (h, c) -> 
          (* use miss_send_len when sending a pkt_in message*)
          let _ = st.pkt_len <- c.OP.Switch.miss_send_len in
          return ()
  | OP.Queue_get_config_resp (h, _, _)
  | OP.Queue_get_config_req (h, _)
  | OP.Barrier_resp h
  | OP.Stats_resp (h, _)
  | OP.Port_mod (h, _)
  | OP.Port_status (h, _)
  | OP.Flow_removed (h, _)
  | OP.Packet_in (h, _)
  | OP.Get_config_resp (h, _)
  | OP.Features_resp (h, _)
  | OP.Vendor (h, _) 
  | OP.Error (h, _, _) ->
      let bits = OP.marshal msg in 
      let h = OP.Header.create ~xid:h.OP.Header.xid OP.Header.ERROR 
                    (OP.Header.get_len + 4 + (Cstruct.len bits)) in 
      OSK.send_packet t (OP.Error(h, OP.REQUEST_BAD_TYPE, bits)) 
(* end of process_openflow *)

  let monitor_control_channel sw conn =
	let is_active = ref true in 
  	while_lwt !is_active do
      let _ = sw.echo_resp_received <- false in 
      let _ = sw.last_echo_req <- (Clock.time ()) in 
      lwt _ = OSK.send_packet conn 
				OP.(Echo_req Header.(create ECHO_REQ sizeof_ofp_header)) in
		lwt _ = OS.Time.sleep 10.0 in 
		return (is_active := sw.echo_resp_received) 
	done 

  let control_channel_run st conn = 
	(* Trigger the dance between the 2 nodes *)
	let h = OP.Header.(create ~xid:1l HELLO sizeof_ofp_header) in  
	let _ = OSK.send_packet conn (OP.Hello(h)) in

	let rec echo () =
	  try_lwt
		OSK.read_packet conn >>= fun msg ->
		  (* let _ = print_endline "new message recieved" in (* for test *) *)
		  process_openflow st conn msg >> echo ()
	  with (*
		| Closed -> 
		  return (pp "[switch] ERROR: Controller channel closed....\n%!")
			(* XXX check how to catch Channel Closed exception *)
		*)
		| OP.Unparsed (m, bs) -> 
		  pp "[switch] ERROR:unparsed! m=%s\n %!" m; echo ()
		| exn -> 
		  return (pp "[switch] ERROR:%s\n%!" (Printexc.to_string exn)) (* ; echo () *)
	  in 
	  lwt _ = 
		echo () <?> 
		(Table.monitor_flow_timeout st.table (Some conn) st.verbose) <?>
		(monitor_control_channel st conn)
	  in 
	  let _ = OSK.close conn in 
		return (pp "[switch] control channel thread returned")

  (*********************************************
   * Switch OpenFlow data plane 
   *********************************************)

  let process_frame_inner st p frame =
(*	let _ = pp "[switch] process inner frame ..." in
	let _ = print_endline "" in *)
  	let open OP.Packet_in in
  	try_lwt
      let in_port = (OP.Port.port_of_int p.port_id) in 
      let tupple = (OP.Match.raw_packet_to_match in_port frame ) in
	  (* Update port rx statistics *)
	  let _ = update_port_rx_stats (Int64.of_int (Cstruct.len frame)) p in

	  (* Lookup packet flow to existing flows in table *)
		match  (lookup_flow st tupple) with 
		  | NOT_FOUND -> begin
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
					  match st.controller with
						| None -> let _ = pp "[switch] Controller not set!! ..." in
			  					  let _ = print_endline "" in
								  ()
						| Some conn ->
							(*	  let _ = pp "[switch] NOT_FOUND ..." in
								  let _ = print_endline "" in *)
								  ignore_result (OSK.send_packet conn (OP.Packet_in(h,pkt_in)))
					)
      		end (* switch not found*)
	       (* generate a packet in event *)
		| Found(entry) ->
			let _ = Table.update_table_found st.table in
			let _ = Entry.update_flow (Int64.of_int (Cstruct.len frame)) !entry in
			apply_of_actions st tupple.OP.Match.in_port frame (!entry).Entry.actions
	  with exn ->
		return (pp "[switch] process_frame_inner: control channel error: %s\n" 
        	(Printexc.to_string exn))


  let check_packets st = 
	match_lwt
	 (Lwt_list.exists_p (fun p -> return (p.pkt_count > 0)) st.ports) with
	| false -> OS.Time.sleep 0.5 
	| true -> return ()

  (* Swicth operation *)
  let forward_thread st =
(*  while_lwt true do
    lwt _ = check_packets st <?> Lwt_condition.wait st.Switch.ready in 
    Lwt_list.iter_s (fun p ->
(*      lwt empty = Lwt_stream.is_empty p.Switch.queue in *)
      if (p.Switch.pkt_count = 0) then
(*        let _ = pp "port %d no packets\n" p.Switch.port_id in *)
        return ()
      else
        lwt frames = Lwt_stream.nget p.Switch.pkt_count p.Switch.queue in
        lwt _ = 
          Lwt_list.iter_p 
          (fun f -> 
            p.Switch.pkt_count <- p.Switch.pkt_count - 1; process_frame_inner st p f) 
          frames in 
(*        let _ = pp "port %d golet get_flow_stats st of_match =
  let open OP.Match in 
  let match_flows of_match key value ret =
    if (flow_match_compare key of_match of_match.wildcards) then ( 
      (Entry.flow_counters_to_flow_stats key (char_of_int 1) value)::ret
    ) else 
      ret 
  in
    Hashtbl.fold (fun key value r -> match_flows of_match key value r) 
    st.Switch.table.Table.entries []  t packet\n" p.Switch.port_id in *)
(*        let _ = pp "port %d processed packet\n" p.Switch.port_id in *)
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
        lwt _ = Lwt_stream.next p.in_queue >>= process_frame_inner st p in
(*       let _ = 
        if (p.Switch.pkt_count mod 20 = 1) then
       pp "port %d got packet %d" p.Switch.port_id p.Switch.pkt_count in
      *        *)
        return (p.pkt_count <- p.pkt_count - 1)  (* XXX why packet_count is subtracted by 1? *)
	done <&> (
    while_lwt true do
	  lwt frame = Lwt_stream.next p.out_queue in
(*    	lwt _ = OS.Time.sleep 0.0 in *)
(*      let frames = Lwt_stream.get_available p.Switch.out_queue in*)
(*      let _ = Printf.printf "got %d packets\n%!" (1+(List.length frames)) in

        OS.Netif.writev p.netif [frame] (*frame::frames*)
*)
		E.write p.ethif frame
    done 
    )
    ) st.ports 

  let rec add_switch_ports sw ethlist =
	match ethlist with
	  | [] -> return ()
	  | eth::t -> add_port sw eth >> add_switch_ports sw t

  let create_switch tcp (* sw_ip_info *) cont ethlist =
	(* init net sw_ip_info cont
	>>= fun tcp -> *)
		let rec connect_socket () =
		  let sock = ref None in 
			try_lwt
			  let _ = pp "trying to connect to the remote controller\n%!" in 
			  	lwt _ = Lwt.pick
				    [create_tcp_connection tcp cont >>= (fun t -> return (sock:= Some t));
				     (OS.Time.sleep 10.0)]
				  in
				  match !sock with
				  | None -> connect_socket ()
				  | Some t -> return t 
			with exn -> connect_socket ()
		in
		  connect_socket ()
			>>= fun fl -> 
				let conn = OSK.init_socket_conn_state (OSK.create fl)
				  in (* up to here, connection in stablished correctly *) 
					let sw = init_switch_info 0x100L (* model *) in (* move verbose and dpid to the unikernel *)
					let _ = sw.controller <- (Some conn) in
					lwt _ = add_switch_ports sw ethlist in
					lwt _ = ((control_channel_run sw conn) <&> (forward_thread sw)) in 
		  			let _ = OSK.close conn in 
      				  return (pp "[switch] Remote controller connected...\n")


(*************************************************
 * Switch OpenFlow control channel 
 *************************************************)

end (* end of Switch module *)

