(* 
 * Copyright (c) 2005-2011 Charalampos Rotsos <cr409@cl.cam.ac.uk>
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
open Ofsocket

let sp = Printf.sprintf
let cp = OS.Console.log

module OP = Ofpacket

module Event = struct
  type t = 
    | DATAPATH_JOIN | DATAPATH_LEAVE | PACKET_IN | FLOW_REMOVED 
    | FLOW_STATS_REPLY | AGGR_FLOW_STATS_REPLY | DESC_STATS_REPLY 
    | PORT_STATS_REPLY | TABLE_STATS_REPLY | PORT_STATUS_CHANGE 

  type e = 
    | Datapath_join of OP.datapath_id * OP.Port.phy list 
    | Datapath_leave of OP.datapath_id
    | Packet_in of OP.Port.t * OP.Packet_in.reason * 
                     int32 * Cstruct.t * OP.datapath_id
    | Flow_removed of
        OP.Match.t * OP.Flow_removed.reason * int32 * int32 * int64 * int64
      * OP.datapath_id 
    | Flow_stats_reply of int32 * bool * OP.Flow.stats list * OP.datapath_id
    | Aggr_flow_stats_reply of int32 * int64 * int64 * int32 * OP.datapath_id
    | Port_stats_reply of int32 * bool * OP.Port.stats list *  OP.datapath_id
    | Table_stats_reply of int32 * bool * OP.Stats.table list * OP.datapath_id
    | Desc_stats_reply of
        string * string * string * string * string
      * OP.datapath_id
    | Port_status of OP.Port.reason * OP.Port.phy * OP.datapath_id

  let string_of_event = function
    | Datapath_join (dpid, _) -> sp "Datapath_join: dpid:0x%012Lx" dpid
    | Datapath_leave dpid -> sp "Datapath_leave: dpid:0x%012Lx" dpid
    | Packet_in (port, r, buffer_id, bs, dpid) -> 
      (sp "Packet_in: port:%s reason:%s dpid:0x%012Lx buffer_id:%ld"
            (OP.Port.string_of_port port) 
            (OP.Packet_in.string_of_reason r)
            dpid buffer_id ) 
    | Flow_removed (flow, reason, duration_sec, duration_usec, 
                    packet_count, byte_count, dpid) 
      -> (sp "Flow_removed: flow: %s reason:%s duration:%ld.%ld packets:%s \
              bytes:%s dpid:0x%012Lx"
            (OP.Match.match_to_string flow) 
            (OP.Flow_removed.string_of_reason reason) 
            duration_sec duration_usec
            (Int64.to_string packet_count) (Int64.to_string byte_count) dpid)
    | Flow_stats_reply(xid, more, flows, dpid) 
      -> (sp "Flow stats reply: dpid:%012Lx more:%s flows:%d xid:%ld"
            dpid (string_of_bool more) (List.length flows) xid)
    | Aggr_flow_stats_reply(xid, packet_count, byte_count, flow_count, dpid)
      -> (sp "aggr flow stats reply: dpid:%012Lx packets:%Ld bytes:%Ld \
              flows:%ld xid:%ld" 
            dpid packet_count byte_count flow_count xid)
    | Port_stats_reply (xid, _, ports, dpid) 
      -> (sp "port stats reply: dpid:%012Lx ports:%d xid%ld" 
            dpid (List.length ports) xid)
    | Table_stats_reply (xid, _, tables, dpid) 
      -> (sp "table stats reply: dpid:%012Lx tables:%d xid%ld" 
            dpid (List.length tables) xid)
    | Desc_stats_reply (mfr_desc, hw_desc, sw_desc, serial_num, dp_desc, dpid)
      -> (sp "table stats reply: dpid:%012Lx mfr_desc:%s hw_desc:%s \
              sw_desc:%s serial_num:%s dp_desc:%s" 
            dpid mfr_desc hw_desc sw_desc serial_num dp_desc)
    | Port_status (r, ph, dpid) 
      -> (sp "post stats: port:%s status:%s dpid:%012Lx" ph.OP.Port.name
            (OP.Port.reason_to_string r) dpid)
end

type t = {
  mutable dp_db: (OP.datapath_id, conn_state) Hashtbl.t;
  mutable datapath_join_cb: 
    (t -> OP.datapath_id -> Event.e -> unit Lwt.t) list;
  mutable datapath_leave_cb:
    (t -> OP.datapath_id -> Event.e -> unit Lwt.t) list;
  mutable packet_in_cb:
    (t -> OP.datapath_id -> Event.e -> unit Lwt.t) list;
  mutable flow_removed_cb:
    (t -> OP.datapath_id -> Event.e -> unit Lwt.t) list;
  mutable flow_stats_reply_cb:
    (t -> OP.datapath_id -> Event.e -> unit Lwt.t) list;
  mutable aggr_flow_stats_reply_cb:
    (t -> OP.datapath_id -> Event.e -> unit Lwt.t) list;
  mutable desc_stats_reply_cb:
    (t -> OP.datapath_id -> Event.e -> unit Lwt.t) list;
  mutable port_stats_reply_cb:
    (t -> OP.datapath_id -> Event.e -> unit Lwt.t) list;
  mutable table_stats_reply_cb:
    (t -> OP.datapath_id -> Event.e -> unit Lwt.t) list;
  mutable port_status_cb:
    (t -> OP.datapath_id -> Event.e -> unit Lwt.t) list;
  verbose : bool; 
}

let register_cb controller e cb =
  Event.(
    match e with 
      | DATAPATH_JOIN
        -> controller.datapath_join_cb <- controller.datapath_join_cb @ [cb]
      | DATAPATH_LEAVE 
        -> controller.datapath_leave_cb <- controller.datapath_leave_cb @ [cb]
      | PACKET_IN
        -> controller.packet_in_cb <- controller.packet_in_cb @ [cb]
      | FLOW_REMOVED
        -> controller.flow_removed_cb <- controller.flow_removed_cb @ [cb]
      | FLOW_STATS_REPLY 
        -> (controller.flow_stats_reply_cb
            <- controller.flow_stats_reply_cb @ [cb]
        )
      | AGGR_FLOW_STATS_REPLY 
        -> (controller.aggr_flow_stats_reply_cb 
            <- controller.aggr_flow_stats_reply_cb @ [cb]
        )
      | DESC_STATS_REPLY
        -> (controller.desc_stats_reply_cb
            <- controller.desc_stats_reply_cb @ [cb]
        )
      | PORT_STATS_REPLY 
        -> (controller.port_stats_reply_cb
            <- controller.port_stats_reply_cb @ [cb] 
        )
      | TABLE_STATS_REPLY
        -> (controller.table_stats_reply_cb
            <- controller.table_stats_reply_cb @ [cb])
      | PORT_STATUS_CHANGE
        -> controller.port_status_cb <- controller.port_status_cb @ [cb] 
  )

let process_of_packet state conn ofp = 
  let _ = if state.verbose then cp (sp "[controller] rcv: %s\n%!" (OP.to_string ofp)) in
  OP.(
    match ofp with
      | Hello (h) -> (* Reply to HELLO with a HELLO and a feature request *)
        lwt _ = send_packet conn (OP.Hello (h)) in  
        let h = OP.Header.create OP.Header.FEATURES_REQ OP.Header.get_len  in 
        send_packet conn (OP.Features_req (h) )  
      | Echo_req h -> (* Reply to ECHO requests *)
        send_packet conn (OP.Echo_resp OP.Header.(create ~xid:h.xid ECHO_RESP get_len))
      | Echo_resp h  -> return () (* At the moment ignore echo responses  *)
      | Features_resp (h, sfs) -> begin (* Generate a datapath join event *)
        let open OP.Switch in 
        let _ = conn.dpid <- sfs.datapath_id  in
        let evt = Event.Datapath_join (sfs.datapath_id, sfs.ports) in
        let _ = 
          if (Hashtbl.mem state.dp_db sfs.datapath_id) then 
            cp (sp "[controller] Deleting old state for %Lx\n%!" conn.dpid)
        in 
        let _ = Hashtbl.replace state.dp_db sfs.datapath_id conn in 
          Lwt_list.iter_p (fun cb -> cb state sfs.datapath_id  evt) 
            state.datapath_join_cb
      end
      | OP.Packet_in (h, p) -> begin (* Generate a packet_in event *)
        let open OP.Packet_in in
        let evt = 
          Event.Packet_in (p.in_port, p.reason, p.buffer_id, p.data, conn.dpid) in 
          Lwt_list.iter_p (fun cb -> cb state conn.dpid evt) state.packet_in_cb
     end
      | OP.Flow_removed (h, p) ->
        let open OP.Flow_removed in
        let evt = Event.Flow_removed (
            p.of_match, p.reason, p.duration_sec, p.duration_nsec, 
            p.packet_count, p.byte_count, conn.dpid) in
        Lwt_list.iter_p (fun cb -> cb state conn.dpid evt) state.flow_removed_cb
      | Stats_resp(h, resp) -> begin  
        match resp with 
              | OP.Stats.Flow_resp(resp_h, flows) -> begin
                let evt = Event.Flow_stats_reply(
                   h.Header.xid, resp_h.OP.Stats.more, flows, conn.dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state conn.dpid evt) 
                   state.flow_stats_reply_cb
              end
              | OP.Stats.Aggregate_resp(resp_h, aggr) -> begin
                 let evt = Event.Aggr_flow_stats_reply(
                   h.Header.xid, aggr.OP.Stats.packet_count, 
                   aggr.OP.Stats.byte_count, aggr.OP.Stats.flow_count, conn.dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state conn.dpid evt) 
                   state.aggr_flow_stats_reply_cb
              end
              | OP.Stats.Desc_resp (resp_h, aggr) -> begin
                 let evt = Event.Desc_stats_reply(
                   aggr.OP.Stats.imfr_desc, aggr.OP.Stats.hw_desc, 
                   aggr.OP.Stats.sw_desc, aggr.OP.Stats.serial_num, 
                   aggr.OP.Stats.dp_desc, conn.dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state conn.dpid evt) 
                   state.desc_stats_reply_cb
              end
                  
              | OP.Stats.Port_resp (resp_h, ports) -> begin
                 let evt = 
                   Event.Port_stats_reply(h.Header.xid, resp_h.OP.Stats.more, 
                                          ports, conn.dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state conn.dpid evt)
                   state.port_stats_reply_cb
              end
                  
              | OP.Stats.Table_resp (resp_h, tables) -> begin
                let evt = 
                  Event.Table_stats_reply(h.Header.xid, resp_h.OP.Stats.more, 
                                          tables, conn.dpid) in
                 Lwt_list.iter_p (fun cb -> cb state conn.dpid evt)
                   state.table_stats_reply_cb
              end
              | _ -> return (cp "[controller] unsupported stats response ")
      end

      | Port_status(h, st) -> begin 
          let evt = Event.Port_status (st.OP.Port.reason, st.OP.Port.desc, conn.dpid) in
          Lwt_list.iter_p (fun cb -> cb state conn.dpid evt) state.port_status_cb
      end
      | ofp -> return (cp (sp "[controller] Unsupported %s" (OP.to_string ofp)))
  )

let send_of_data controller dpid bits =
    Ofsocket.send_data_raw (Hashtbl.find controller.dp_db dpid ) bits

let send_data controller dpid ofp =
    Ofsocket.send_packet (Hashtbl.find controller.dp_db dpid ) ofp

let controller_run st conn =
  lwt _ = 
    try_lwt   
      while_lwt true do 
        read_packet conn >>= process_of_packet st conn 
      done
    with
    | Nettypes.Closed -> return (cp "[controller] switch disconnected\n%!")
    | OP.Unparsed(m, bs) 
    | OP.Unparsable(m, bs) -> 
      let _ = cp (sp "# unparsed! m=%s" m) in 
      return (Cstruct.hexdump bs)
    | exn -> return (cp (sp "[controller] ERROR:%s\n%!" (Printexc.to_string exn)))
  in
  if (conn.dpid > 0L) then
    let evt = Event.Datapath_leave (conn.dpid) in
    lwt _ = Lwt_list.iter_p (fun cb -> cb st conn.dpid evt)
              st.datapath_leave_cb in
    let _ = Hashtbl.remove st.dp_db conn.dpid in 
    return ()
  else
    return ()
  
let socket_controller st (remote_addr, remote_port) t =
  let rs = Ipaddr.V4.to_string  remote_addr in
  let _ = cp (sp "[controller]+ Controller %s:%d\n%!" rs remote_port) in
  let conn = init_socket_conn_state t in 
    controller_run st conn 

let init_controller ?(verbose=false) init = 
  let t = { verbose;
    dp_db                    = Hashtbl.create 0; 
    datapath_join_cb         = []; 
    datapath_leave_cb        = []; 
    packet_in_cb             = [];
    flow_removed_cb          = []; 
    flow_stats_reply_cb      = [];
    aggr_flow_stats_reply_cb = [];
    desc_stats_reply_cb      = []; 
    port_stats_reply_cb      = [];
    table_stats_reply_cb     = [];
    port_status_cb           = []; } in  
  let _ = init t in 
  t

let listen mgr ?(verbose=false) loc init =
  let st = init_controller ~verbose init in
    (Channel.listen mgr (`TCPv4 (loc, (socket_controller st) ))) 

let connect mgr ?(verbose=false) loc init = 
  let st = init_controller ~verbose init in
    Net.Channel.connect mgr (`TCPv4 (None, loc, 
      (socket_controller st loc) ))

let local_connect st conn = controller_run st conn 
