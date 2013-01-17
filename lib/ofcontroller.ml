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
open Lwt_list
open Net
open Printexc
open Printf
open Ofsocket

let sp = Printf.sprintf
let pp = Printf.printf
let ep = Printf.eprintf
let cp = OS.Console.log

module OP = Ofpacket


exception ReadError

module Event = struct
  type t = 
    | DATAPATH_JOIN | DATAPATH_LEAVE | PACKET_IN | FLOW_REMOVED 
    | FLOW_STATS_REPLY | AGGR_FLOW_STATS_REPLY | DESC_STATS_REPLY 
    | PORT_STATS_REPLY | TABLE_STATS_REPLY | PORT_STATUS_CHANGE 

  type e = 
    | Datapath_join of OP.datapath_id * OP.Port.phy list 
    | Datapath_leave of OP.datapath_id
    | Packet_in of OP.Port.t * int32 * Cstruct.t * OP.datapath_id
    | Flow_removed of
        OP.Match.t * OP.Flow_removed.reason * int32 * int32 * int64 * int64
      * OP.datapath_id 
    | Flow_stats_reply of int32 * bool * OP.Flow.stats list * OP.datapath_id
    | Aggr_flow_stats_reply of int32 * int64 * int64 * int32 * OP.datapath_id
    | Port_stats_reply of int32 * OP.Port.stats list *  OP.datapath_id
    | Table_stats_reply of int32 * OP.Stats.table list * OP.datapath_id 
    | Desc_stats_reply of
        string * string * string * string * string
      * OP.datapath_id
    | Port_status of OP.Port.reason * OP.Port.phy * OP.datapath_id

  let string_of_event = function
    | Datapath_join (dpid, _) -> sp "Datapath_join: dpid:0x%012Lx" dpid
    | Datapath_leave dpid -> sp "Datapath_leave: dpid:0x%012Lx" dpid
    | Packet_in (port, buffer_id, bs, dpid) 
      -> (sp "Packet_in: port:%s ... dpid:0x%012Lx buffer_id:%ld" 
            (OP.Port.string_of_port port) dpid buffer_id ) 
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
    | Port_stats_reply (xid, ports, dpid) 
      -> (sp "port stats reply: dpid:%012Lx ports:%d xid%ld" 
            dpid (List.length ports) xid)
    | Table_stats_reply (xid, tables, dpid) 
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
  OP.(
    match ofp with
      | Hello (h) -> begin (* Reply to HELLO with a HELLO and a feature request *)
        let _ = cp "HELLO" in
          lwt _ = send_packet conn (OP.Hello (h)) in  
        let h = OP.Header.create OP.Header.FEATURES_REQ OP.Header.get_len  in 
            send_packet conn (OP.Features_req (h) )  
     end
      | Echo_req h  -> begin (* Reply to ECHO requests *)
          let _ = cp "ECHO_REQ" in
          let h = OP.Header.(create ~xid:h.xid ECHO_RESP get_len) in
            send_packet conn (OP.Echo_resp h)
     end
      | Features_resp (h, sfs) -> begin (* Generate a datapath join event *)
        cp "FEATURES_RESP";
        let dpid = sfs.Switch.datapath_id in 
        let _ = conn.dpid <- dpid in

        let evt = Event.Datapath_join (dpid, sfs.Switch.ports) in
        let _ = 
          if (Hashtbl.mem state.dp_db dpid) then 
            Printf.printf "Deleting old state \n%!"
        in 
        let _ = Hashtbl.replace state.dp_db dpid conn in 
          Lwt_list.iter_p (fun cb -> cb state dpid evt) state.datapath_join_cb
 
(*           return () *)
      end
      | Packet_in (h, p) -> begin (* Generate a packet_in event *) 
(*          cp (sp "+ %s|%s" 
                  (OP.Header.header_to_string h)
                  (OP.Packet_in.packet_in_to_string p)); *)
(*
           let evt = Event.Packet_in (
              p.Packet_in.in_port, p.Packet_in.buffer_id,
              p.Packet_in.data, conn.dpid) 
            in 
             iter_p (fun cb -> cb state conn.dpid evt)
                     state.packet_in_cb
 *)
              return ()  
      end
      | Flow_removed (h, p)
        -> (cp (sp "+ %s|%s" 
                  (OP.Header.header_to_string h)
                  (OP.Flow_removed.string_of_flow_removed p)); 
            let evt = Event.Flow_removed (
              p.Flow_removed.of_match, p.Flow_removed.reason, 
              p.Flow_removed.duration_sec, p.Flow_removed.duration_nsec, 
              p.Flow_removed.packet_count, p.Flow_removed.byte_count, conn.dpid)
            in
            Lwt_list.iter_p (fun cb -> cb state conn.dpid evt) state.flow_removed_cb
        )

      | Stats_resp(h, resp) -> begin  
         cp (sp "+ %s|%s" (OP.Header.header_to_string h)
                  (OP.Stats.string_of_stats resp)); 
            match resp with 
              | OP.Stats.Flow_resp(resp_h, flows) -> begin
                let evt = Event.Flow_stats_reply(
                   h.Header.xid, resp_h.Stats.more_to_follow, flows, conn.dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state conn.dpid evt) 
                   state.flow_stats_reply_cb
              end
              | OP.Stats.Aggregate_resp(resp_h, aggr) -> begin
                 let evt = Event.Aggr_flow_stats_reply(
                   h.Header.xid, aggr.Stats.packet_count, 
                   aggr.Stats.byte_count, aggr.Stats.flow_count, conn.dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state conn.dpid evt) 
                   state.aggr_flow_stats_reply_cb
              end
              | OP.Stats.Desc_resp (resp_h, aggr) -> begin
                 let evt = Event.Desc_stats_reply(
                   aggr.Stats.imfr_desc, aggr.Stats.hw_desc, 
                   aggr.Stats.sw_desc, aggr.Stats.serial_num, 
                   aggr.Stats.dp_desc, conn.dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state conn.dpid evt) 
                   state.desc_stats_reply_cb
              end
                  
              | OP.Stats.Port_resp (resp_h, ports) -> begin
                 let evt = Event.Port_stats_reply(h.Header.xid, ports, conn.dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state conn.dpid evt)
                   state.port_stats_reply_cb
              end
                  
              | OP.Stats.Table_resp (resp_h, tables) -> begin
                let evt = Event.Table_stats_reply(h.Header.xid, tables, conn.dpid) in
                 Lwt_list.iter_p (fun cb -> cb state conn.dpid evt)
                   state.table_stats_reply_cb
              end
              | _ -> OS.Console.log "New stats response received"; return ();
      end

      | Port_status(h, st) -> begin 
         cp (sp "+ %s|%s" (OP.Header.header_to_string h)
                  (OP.Port.string_of_status st)); 
            let evt = Event.Port_status (st.Port.reason, st.Port.desc, conn.dpid) 
            in
            Lwt_list.iter_p (fun cb -> cb state conn.dpid evt) state.port_status_cb
      end
      | ofp -> 
          let _ = OS.Console.log (sp "Packet type not supported %s"
          (OP.to_string ofp)) in 
            return () 
  )

let send_of_data controller dpid bits =
  let conn = Hashtbl.find controller.dp_db dpid in
    Ofsocket.send_data_raw conn bits

let send_data controller dpid ofp =
  let conn = Hashtbl.find controller.dp_db dpid in
    Ofsocket.send_packet conn ofp


let mem_dbg name =
   Gc.compact ();  
  let s = Gc.stat () in
  Printf.printf "blocks %s: l=%d f=%d \n %!" name s.Gc.live_blocks s.Gc.free_blocks

let terminate st = 
  let _ = Hashtbl.iter 
    (fun _ c -> Ofsocket.close c ) st.dp_db in 
    Printf.printf "Terminating controller...\n%!"
 

let main_loop st conn = 
  while_lwt true do 
    lwt ofp = read_packet conn in 
    lwt _ =   
      process_of_packet st conn ofp 
    in
      return ()
  done

let controller_run st conn =
    lwt ofp = read_packet conn in 
    lwt _ =   
      process_of_packet st conn ofp 
    in
     lwt ofp = read_packet conn in 
    lwt _ =   
      process_of_packet st conn ofp 
    in
  lwt _ = 
(*     try_lwt   *)
      main_loop st conn

(*
    with
      | Nettypes.Closed -> begin
          let _ = printf "XXXXX switch disconnected\n%!" in 
            return ()
      end
      | OP.Unparsed(m, bs) 
      | OP.Unparsable(m, bs) -> 
          let _ = cp (sp "# unparsed! m=%s" m) in 
          let _ = Cstruct.hexdump bs in 
            return ()
(*            main_loop st conn *)
       | exn -> 
          pp "{OpenFlow-controller} ERROR:%s\n%!" (Printexc.to_string exn); 
          return ()
 *)
(*           main_loop st conn *)
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
  let rs = Nettypes.ipv4_addr_to_string remote_addr in
  let _ = pp "OpenFlow Controller+ %s:%d\n%!" rs remote_port in
  let conn = init_socket_conn_state t in 
    controller_run st conn 

let init_controller () = 
  { dp_db                    = Hashtbl.create 0; 
    datapath_join_cb         = []; 
    datapath_leave_cb        = []; 
    packet_in_cb             = [];
    flow_removed_cb          = []; 
    flow_stats_reply_cb      = [];
    aggr_flow_stats_reply_cb = [];
    desc_stats_reply_cb      = []; 
    port_stats_reply_cb      = [];
    table_stats_reply_cb     = [];
    port_status_cb           = []; } 

let listen mgr loc init =
  let st = init_controller () in
  let _ = init st in 
    (Channel.listen mgr (`TCPv4 (loc, (socket_controller st) ))) 

let connect mgr loc init = 
  let st = init_controller () in
  let _ = init st in 
    Net.Channel.connect mgr (`TCPv4 (None, loc, 
      (socket_controller st loc) ))

let init_controller init =
  let st = init_controller () in
  let _ = init st in
    st

let local_connect mgr st (input, output) init =
 let conn = init_local_conn_state input output in 
    controller_run st conn 
