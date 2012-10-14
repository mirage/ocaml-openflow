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

let sp = Printf.sprintf
let pp = Printf.printf
let ep = Printf.eprintf
let cp = OS.Console.log

module OP = Ofpacket

let resolve t = Lwt.on_success t (fun _ -> ())

exception ReadError

module Event = struct
  type t = 
    | DATAPATH_JOIN | DATAPATH_LEAVE | PACKET_IN | FLOW_REMOVED 
    | FLOW_STATS_REPLY | AGGR_FLOW_STATS_REPLY | DESC_STATS_REPLY 
    | PORT_STATS_REPLY | TABLE_STATS_REPLY | PORT_STATUS_CHANGE 

  type e = 
    | Datapath_join of OP.datapath_id * OP.Port.phy list 
    | Datapath_leave of OP.datapath_id
    | Packet_in of OP.Port.t * int32 * Cstruct.buf * OP.datapath_id
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
  mutable dp_db: (OP.datapath_id, Channel.t) Hashtbl.t;
  mutable channel_dp: ((Nettypes.ipv4_addr * int) , OP.datapath_id) Hashtbl.t;
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

let process_of_packet state (remote_addr, remote_port) ofp t = 
  OP.(
    let ep = (remote_addr, remote_port) in
    match ofp with
      | Hello (h) (* Reply to HELLO with a HELLO and a feature request *)
        -> ( cp "HELLO"; 
          let bits = OP.marshal_and_sub (Header.marshal_header h)
          (OS.Io_page.get ()) in 
          let _ = Channel.write_buffer t bits in
          let bits = OP.marshal_and_sub (OP.build_features_req 1l)
          (OS.Io_page.get ()) in 
          let _ = Channel.write_buffer t bits in 
            Channel.flush t
        )

      | Echo_req (h, bs)  (* Reply to ECHO requests *)
        -> ((* cp "ECHO_REQ"; *)
          let bits = OP.marshal_and_sub 
                       (OP.build_echo_resp h bs) 
                       (OS.Io_page.get ()) in 
          let _ = Channel.write_buffer t bits in
            Channel.flush t
        )

      | Features_resp (h, sfs) (* Generate a datapath join event *)
        -> ((* cp "FEATURES_RESP";*)
            let dpid = sfs.Switch.datapath_id in
            let evt = Event.Datapath_join (dpid, sfs.Switch.ports) in
            if (Hashtbl.mem state.dp_db dpid) then (
              Printf.printf "Deleting old state \n%!";
              Hashtbl.remove state.dp_db dpid;
              Hashtbl.remove state.channel_dp ep
            );
            Hashtbl.add state.dp_db dpid t;
            Hashtbl.add state.channel_dp ep dpid;
            Lwt_list.iter_p (fun cb -> cb state dpid evt) state.datapath_join_cb
        )

      | Packet_in (h, p) (* Generate a packet_in event *) 
        -> ( 
          cp (sp "+ %s|%s" 
                  (OP.Header.header_to_string h)
                  (OP.Packet_in.packet_in_to_string p)); 
            let dpid = Hashtbl.find state.channel_dp ep in
            let evt = Event.Packet_in (
              p.Packet_in.in_port, p.Packet_in.buffer_id,
              p.Packet_in.data, dpid) 
            in
             iter_p (fun cb -> cb state dpid evt)
                     state.packet_in_cb
        )
        
      | Flow_removed (h, p)
        -> ((* cp (sp "+ %s|%s" 
                  (OP.Header.string_of_h h)
                  (OP.Flow_removed.string_of_flow_removed p)); *)
            let dpid = Hashtbl.find state.channel_dp ep in
            let evt = Event.Flow_removed (
              p.Flow_removed.of_match, p.Flow_removed.reason, 
              p.Flow_removed.duration_sec, p.Flow_removed.duration_nsec, 
              p.Flow_removed.packet_count, p.Flow_removed.byte_count, dpid)
            in
            Lwt_list.iter_p (fun cb -> cb state dpid evt) state.flow_removed_cb
        )

      | Stats_resp(h, resp) 
        -> ((* cp (sp "+ %s|%s" (OP.Header.string_of_h h)
                  (OP.Stats.string_of_stats resp)); *)
            match resp with 
              | OP.Stats.Flow_resp(resp_h, flows) ->
                (let dpid = Hashtbl.find state.channel_dp ep in
                 let evt = Event.Flow_stats_reply(
                   h.Header.xid, resp_h.Stats.more_to_follow, flows, dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state dpid evt) 
                   state.flow_stats_reply_cb
                )
              | OP.Stats.Aggregate_resp(resp_h, aggr) -> 
                (let dpid = Hashtbl.find state.channel_dp ep in
                 let evt = Event.Aggr_flow_stats_reply(
                   h.Header.xid, aggr.Stats.packet_count, 
                   aggr.Stats.byte_count, aggr.Stats.flow_count, dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state dpid evt) 
                   state.aggr_flow_stats_reply_cb
                )
              | OP.Stats.Desc_resp (resp_h, aggr) ->
                (let dpid = Hashtbl.find state.channel_dp ep in
                 let evt = Event.Desc_stats_reply(
                   aggr.Stats.imfr_desc, aggr.Stats.hw_desc, 
                   aggr.Stats.sw_desc, aggr.Stats.serial_num, 
                   aggr.Stats.dp_desc, dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state dpid evt) 
                   state.desc_stats_reply_cb
                )
                  
              | OP.Stats.Port_resp (resp_h, ports) ->
                (let dpid = Hashtbl.find state.channel_dp ep in
                 let evt = Event.Port_stats_reply(h.Header.xid, ports, dpid) 
                 in
                 Lwt_list.iter_p (fun cb -> cb state dpid evt)
                   state.port_stats_reply_cb
                )
                  
              | OP.Stats.Table_resp (resp_h, tables) ->
                (let dpid = Hashtbl.find state.channel_dp ep in
                 let evt = Event.Table_stats_reply(h.Header.xid, tables, dpid)
                 in
                 Lwt_list.iter_p (fun cb -> cb state dpid evt)
                   state.table_stats_reply_cb
                )
              | _ -> OS.Console.log "New stats response received"; return ();
        ) 

      | Port_status(h, st) 
        -> ( (* cp (sp "+ %s|%s" (OP.Header.string_of_h h)
                  (OP.Port.string_of_status st)); *)
            let dpid = Hashtbl.find state.channel_dp ep in
            let evt = Event.Port_status (st.Port.reason, st.Port.desc, dpid) 
            in
            Lwt_list.iter_p (fun cb -> cb state dpid evt) state.port_status_cb
        )

      | _ -> OS.Console.log "New packet received"; return () 
  )

let send_of_data controller dpid bits = 
  let t = Hashtbl.find controller.dp_db dpid in
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
(*  let _ = Channel.write_buffer t.ch data in
Channel.flush t.ch *)

let mem_dbg name =
(*   Gc.compact ();  *)
  let s = Gc.stat () in
  Printf.printf "blocks %s: l=%d f=%d \n %!" name s.Gc.live_blocks s.Gc.free_blocks

let terminate st = 
  Hashtbl.iter (fun _ ch -> resolve (Channel.close ch) ) st.dp_db;
  Printf.printf "Terminating controller...\n"
   
let controller st (remote_addr, remote_port) t =
  let rs = Nettypes.ipv4_addr_to_string remote_addr in
  let cached_socket = Ofsocket.create_socket t in 
  let _ = pp "OpenFlow Controller+ %s:%d\n%!" rs remote_port in
  let echo () =
    try_lwt 
      lwt hbuf = Ofsocket.read_data cached_socket OP.Header.sizeof_ofp_header in
      let ofh  = OP.Header.parse_header hbuf in
      let dlen = ofh.OP.Header.len - OP.Header.sizeof_ofp_header in 
      lwt dbuf = Ofsocket.read_data cached_socket dlen in 
      let ofp  = OP.parse ofh dbuf in
      lwt () = process_of_packet st (remote_addr, remote_port) ofp t in
        return true
    with
      | Nettypes.Closed -> begin
        let dpid = Hashtbl.find st.channel_dp (remote_addr, remote_port) in
        let evt = Event.Datapath_leave (dpid) in
        lwt _ = Lwt_list.iter_p (fun cb -> cb st dpid evt)
                  st.datapath_leave_cb in
        let _ = Hashtbl.remove st.channel_dp (remote_addr, remote_port) in 
        let _ = Hashtbl.remove st.dp_db dpid in 
          return false
      end
      | OP.Unparsed(m, bs) 
      | OP.Unparsable(m, bs) -> 
        cp (sp "# unparsed! m=%s" m);
        Cstruct.hexdump bs; 
        return true
      | Not_found ->  
        Printf.printf "Error: Not found %s\n%!" (Printexc.get_backtrace ());  
        return true
      | exn -> 
          pp "{OpenFlow-controller} ERROR:%s\n%s\n%!" (Printexc.to_string exn)
            (Printexc.get_backtrace ());
          return false

    in
    let continue = ref true in
    let count = (ref 0) in 
    while_lwt !continue do
      incr count;
      lwt x = echo () in
      continue := x;
      return ()
    done

let listen mgr loc init =
  let st = { dp_db                    = Hashtbl.create 0; 
             channel_dp               = Hashtbl.create 0; 
             datapath_join_cb         = []; 
             datapath_leave_cb        = []; 
             packet_in_cb             = [];
             flow_removed_cb          = []; 
             flow_stats_reply_cb      = [];
             aggr_flow_stats_reply_cb = [];
             desc_stats_reply_cb      = []; 
             port_stats_reply_cb      = [];
             table_stats_reply_cb     = [];
             port_status_cb           = [];
           } 
  in
  let _ = init st in 
    (Channel.listen mgr (`TCPv4 (loc, (controller st) ))) 

let connect mgr loc init = 
  let st = { dp_db                    = Hashtbl.create 0; 
             channel_dp               = Hashtbl.create 0; 
             datapath_join_cb         = []; 
             datapath_leave_cb        = []; 
             packet_in_cb             = [];
             flow_removed_cb          = []; 
             flow_stats_reply_cb      = [];
             aggr_flow_stats_reply_cb = [];
             desc_stats_reply_cb      = []; 
             port_stats_reply_cb      = [];
             table_stats_reply_cb     = [];
             port_status_cb           = [];
           } 
  in
  let _ = init st in 
    Net.Channel.connect mgr (`TCPv4 (None, loc, 
      (controller st loc) ))
