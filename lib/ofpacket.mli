(*
 * Copyright (c) 2011 Richard Mortier <mort@cantab.net>
 *                    Charalampos Rotsos <cr409@cl.cam.ac.uk> 
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

exception Unparsable of string * Cstruct.buf
exception Unparsed of string * Cstruct.buf

type int16 = int
type uint8 = char
type uint16 = int
type uint32 = int32
type uint64 = int64
type ipv4 = uint32
type byte = uint8
type bytes = string
type eaddr = bytes
type vendor = uint32
type queue_id = uint32
type datapath_id = uint64

module Header :
  sig
    type msg_code =
        HELLO
      | ERROR
      | ECHO_REQ
      | ECHO_RESP
      | VENDOR
      | FEATURES_REQ
      | FEATURES_RESP
      | GET_CONFIG_REQ
      | GET_CONFIG_RESP
      | SET_CONFIG
      | PACKET_IN
      | FLOW_REMOVED
      | PORT_STATUS
      | PACKET_OUT
      | FLOW_MOD
      | PORT_MOD
      | STATS_REQ
      | STATS_RESP
      | BARRIER_REQ
      | BARRIER_RESP
      | QUEUE_GET_CONFIG_REQ
      | QUEUE_GET_CONFIG_RESP
    type h = { ver : uint8; ty : msg_code; len : uint16; xid : uint32; }
    val get_len : int
    val parse_header : Cstruct.buf -> h
    val header_to_string : h -> string
    val create : msg_code -> uint16 -> uint32 -> h
    val marshal_header : h -> Cstruct.buf -> int
    val sizeof_ofp_header : int 
  end
module Queue :
  sig
    type h = { queue_id : queue_id; }
    type t = NONE | MIN_RATE of uint16
  end
module Port :
  sig
    type t =
        Max
      | In_port
      | Table
      | Normal
      | Flood
      | All
      | Controller
      | Local
      | No_port
      | Port of int16
    val port_of_int : int16 -> t
    val int_of_port : t -> int16
    val string_of_port : t -> string
    type config = {
      port_down : bool;
      no_stp : bool;
      no_recv : bool;
      no_recv_stp : bool;
      no_flood : bool;
      no_fwd : bool;
      no_packet_in : bool;
    }
    type features = {
      pause_asym : bool;
      pause : bool;
      autoneg : bool;
      fiber : bool;
      copper : bool;
      f_10GB_FD : bool;
      f_1GB_FD : bool;
      f_1GB_HD : bool;
      f_100MB_FD : bool;
      f_100MB_HD : bool;
      f_10MB_FD : bool;
      f_10MB_HD : bool;
    }
    type state = {
      link_down : bool;
      stp_listen : bool;
      stp_learn : bool;
      stp_forward : bool;
      stp_block : bool;
    }
    type phy = {
      port_no : uint16;
      hw_addr : eaddr;
      name : string;
      config : config;
      state : state;
      curr : features;
      advertised : features;
      supported : features;
      peer : features;
    }
    val init_port_phy: ?port_no:int -> ?hw_addr:eaddr -> 
      ?name:string -> unit -> phy 
    val string_of_phy : phy -> string
    type stats = {
      mutable port_id : uint16;
      mutable rx_packets : uint64;
      mutable tx_packets : uint64;
      mutable rx_bytes : uint64;
      mutable tx_bytes : uint64;
      mutable rx_dropped : uint64;
      mutable tx_dropped : uint64;
      mutable rx_errors : uint64;
      mutable tx_errors : uint64;
      mutable rx_frame_err : uint64;
      mutable rx_over_err : uint64;
      mutable rx_crc_err : uint64;
      mutable collisions : uint64;
    }
    val string_of_port_stats_reply : stats list -> string
    type reason = ADD | DEL | MOD
    val reason_to_string: reason -> string
    type status = { reason : reason; desc : phy; }
    val create_port_status : reason -> phy -> status
    val string_of_status : status -> string
    val marshal_port_status : ?xid:int32 -> status -> Cstruct.buf -> int
  end
module Switch :
  sig
    type capabilities = {
      flow_stats : bool;
      table_stats : bool;
      port_stats : bool;
      stp : bool;
      ip_reasm : bool;
      queue_stats : bool;
      arp_match_ip : bool;
    }
    type actions = {
      output : bool;
      set_vlan_id : bool;
      set_vlan_pcp : bool;
      strip_vlan : bool;
      set_dl_src : bool;
      set_dl_dst : bool;
      set_nw_src : bool;
      set_nw_dst : bool;
      set_nw_tos : bool;
      set_tp_src : bool;
      set_tp_dst : bool;
      enqueue : bool;
      vendor : bool;
    }
    type features = {
      datapath_id : datapath_id;
      n_buffers : uint32;
      n_tables : byte;
      capabilities : capabilities;
      actions : actions;
      mutable ports : Port.phy list;
    }
    val marshal_reply_features : int32 -> features -> Cstruct.buf -> int
    type config = { drop : bool; reasm : bool; miss_send_len : uint16; }
    val init_switch_config : config
    val marshal_switch_config : int32 -> config -> Cstruct.buf -> int 
  end
module Wildcards :
  sig
    type t = {
      in_port : bool;
      dl_vlan : bool;
      dl_src : bool;
      dl_dst : bool;
      dl_type : bool;
      nw_proto : bool;
      tp_src : bool;
      tp_dst : bool;
      nw_src : byte;
      nw_dst : byte;
      dl_vlan_pcp : bool;
      nw_tos : bool;
    }
    val full_wildcard : t
    val exact_match : t
    val l2_match : t
    val l3_match : t
    val arp_match : t
    val wildcard_to_string : t -> string
  end
module Match :
  sig
    type t = {
      wildcards : Wildcards.t;
      in_port : Port.t;
      dl_src : eaddr;
      dl_dst : eaddr;
      dl_vlan : uint16;
      dl_vlan_pcp : byte;
      dl_type : uint16;
      nw_src : uint32;
      nw_dst : uint32;
      nw_tos : byte;
      nw_proto : byte;
      tp_src : uint16;
      tp_dst : uint16;
    }
    val flow_match_compare : t -> t -> Wildcards.t -> bool
    val create_flow_match :
      Wildcards.t ->
      ?in_port:int16 ->
      ?dl_src:eaddr ->
      ?dl_dst:eaddr ->
      ?dl_vlan:uint16 ->
      ?dl_vlan_pcp:byte ->
      ?dl_type:uint16 ->
      ?nw_tos:byte ->
      ?nw_proto:byte ->
      ?nw_src:uint32 ->
      ?nw_dst:uint32 -> ?tp_src:uint16 -> ?tp_dst:uint16 -> unit -> t
    val translate_port : t -> Port.t -> t
    val raw_packet_to_match : Port.t -> Cstruct.buf -> t
    val match_to_string : t -> string
  end
module Flow :
  sig
    type action =
        Output of (Port.t * int)
    | Set_vlan_vid of int 
    | Set_vlan_pcp of int 
    | STRIP_VLAN 
    | Set_dl_src of eaddr
    | Set_dl_dst of eaddr
    | Set_nw_src of ipv4 
    | Set_nw_dst of ipv4
    | Set_nw_tos of byte 
    | Set_tp_src of int16 
    | Set_tp_dst of int16
    | Enqueue of Port.t * uint32
    | VENDOR_ACT 
    
    val int_of_action : action -> int
    val string_of_action : action -> string
    val string_of_actions : action list -> string
    val marshal_action : action -> Cstruct.buf -> int
    type reason = IDLE_TIMEOUT | HARD_TIMEOUT | DELETE
    val reason_of_int : int -> reason
    val int_of_reason : reason -> int
    val string_of_reason : reason -> int
    type stats = {
      mutable table_id : byte;
      mutable of_match : Match.t;
      mutable duration_sec : uint32;
      mutable duration_nsec : uint32;
      mutable priority : uint16;
      mutable idle_timeout : uint16;
      mutable hard_timeout : uint16;
      mutable cookie : uint64;
      mutable packet_count : uint64;
      mutable byte_count : uint64;
      mutable action : action list;
    }
    val marshal_flow_stats : stats list -> Cstruct.buf -> int 
    val string_of_flow_stat : stats -> string
  end
module Packet_in :
  sig
    type reason = NO_MATCH | ACTION
    val reason_of_int : int -> reason
    val int_of_reason : reason -> int
    val string_of_reason : reason -> string
    type t = {
      buffer_id : uint32;
      in_port : Port.t;
      reason : reason;
      data : Cstruct.buf;
    }
    val packet_in_to_string : t -> string
    val create_pkt_in : ?buffer_id:uint32 -> in_port:Port.t -> 
      reason:reason -> data:Cstruct.buf -> t 
    val marshal_pkt_in : ?xid:int32 -> ?data_len:int -> t -> 
      Cstruct.buf -> int
  end
module Packet_out :
  sig
    type t = {
      buffer_id : uint32;
      in_port : Port.t;
      actions : Flow.action list;
      data : Cstruct.buf;
    }
    val create :
      ?xid:uint32 ->
      ?buffer_id:uint32 ->
      ?actions:Flow.action list ->
      data:Cstruct.buf -> in_port:Port.t -> 
        unit -> t
    val marshal_packet_out : t -> Cstruct.buf -> int
    val packet_out_to_string: t -> string
  end
module Flow_mod :
  sig
    type command = ADD | MODIFY | MODIFY_STRICT | DELETE | DELETE_STRICT
    val command_of_int : int -> command
    val int_of_command : command -> int
    val string_of_command : command -> string
    type flags = { send_flow_rem : bool; emerg : bool; overlap : bool; }
    type t = {
      of_match : Match.t;
      cookie : uint64;
      command : command;
      idle_timeout : uint16;
      hard_timeout : uint16;
      priority : uint16;
      buffer_id : int32;
      out_port : Port.t;
      flags : flags;
      actions : Flow.action list; (* array; *)
    }
    val flow_mod_to_string: t -> string
    val create :
      Match.t -> uint64 -> command ->
      ?priority:uint16 ->
      ?idle_timeout:uint16 ->
      ?hard_timeout:uint16 ->
      ?buffer_id:int ->
      ?out_port:Port.t -> ?flags:flags -> Flow.action list -> unit -> t
    val marshal_flow_mod : ?xid:int32 -> t -> Cstruct.buf -> int
  end
module Flow_removed :
  sig
    type reason = IDLE_TIMEOUT | HARD_TIMEOUT | DELETE
    val reason_of_int : int -> reason
    val int_of_reason : reason -> int
    val string_of_reason : reason -> string
    type t = {
      of_match : Match.t;
      cookie : uint64;
      priority : uint16;
      reason : reason;
      duration_sec : uint32;
      duration_nsec : uint32;
      idle_timeout : uint16;
      packet_count : uint64;
      byte_count : uint64;
    }
    val parse_flow_removed: Cstruct.buf  -> t
    val marshal_flow_removed: ?xid:int32 -> t -> Cstruct.buf -> int
    val flow_to_flow_removed: ?reason:reason -> duration_sec:int32 -> 
      duration_nsec:int32 -> packet_count:int64 -> byte_count:int64 ->
      Flow_mod.t -> t
    val string_of_flow_removed : t -> string
  end
module Port_mod :
  sig
    type t = {
      port_no : Port.t;
      hw_addr : eaddr;
      config : Port.config;
      mask : Port.config;
      advertise : Port.features;
    }
  end
module Stats :
  sig
    type table_id = All | Emergency | Table of uint8
    val table_id_of_int : int -> table_id
    val int_of_table_id : table_id -> int
    val string_of_table_id : table_id -> string
    
    type aggregate = {
      packet_count : uint64;
      byte_count : uint64;
      flow_count : uint32;
    }
    type table = {
      mutable table_id : table_id;
      mutable name : string;
      mutable wildcards : Wildcards.t;
      mutable max_entries : uint32;
      mutable active_count : uint32;
      mutable lookup_count : uint64;
      mutable matched_count : uint64;
    }
    type queue = {
      port_no : uint16;
      queue_id : uint32;
      tx_bytes : uint64;
      tx_packets : uint64;
      tx_errors : uint64;
    }
    type desc = {
      imfr_desc : bytes;
      hw_desc : bytes;
      sw_desc : bytes;
      serial_num : bytes;
      dp_desc : bytes;
    }
    type req_hdr = { ty : uint16; flags : uint16; }
    type stats_type = DESC | FLOW | AGGREGATE | TABLE | PORT | QUEUE | VENDOR
    val int_of_req_type : stats_type -> int
    val create_flow_stat_req :
      Match.t ->
      ?table_id:int ->
      ?out_port:Port.t -> ?xid:Int32.t -> Cstruct.buf -> int 
    val create_aggr_flow_stat_req :
      Match.t ->
      ?table_id:int ->
      ?out_port:Port.t -> ?xid:Int32.t -> Cstruct.buf -> int
(*     val create_vendor_stat_req : ?xid:Int32.t -> Cstruct.buf -> unit *)
    val create_table_stat_req : ?xid:Int32.t -> Cstruct.buf -> int
    val create_queue_stat_req :
      ?xid:Int32.t ->
      ?queue_id:int32 -> ?port:Port.t -> Cstruct.buf -> int
    val create_port_stat_req :
      ?xid:Int32.t -> ?port:Port.t -> Cstruct.buf -> int
    type req =
        Desc_req of req_hdr
      | Flow_req of req_hdr * Match.t * table_id * Port.t
      | Aggregate_req of req_hdr * Match.t * table_id * Port.t
      | Table_req of req_hdr
      | Port_req of req_hdr * Port.t
      | Queue_req of req_hdr * Port.t * queue_id
      | Vendor_req of req_hdr
    type resp_hdr = { st_ty : stats_type; more_to_follow : bool; }
    val int_of_stats_type : stats_type -> int
    val stats_type_of_int : int -> stats_type
    type resp =
        Desc_resp of resp_hdr * desc
      | Flow_resp of resp_hdr * Flow.stats list
      | Aggregate_resp of resp_hdr * aggregate
      | Table_resp of resp_hdr * table list
      | Port_resp of resp_hdr * Port.stats list
      | Queue_resp of resp_hdr * queue list
      | Vendor_resp of resp_hdr
    val marshal_stats_resp : int32 -> resp -> Cstruct.buf -> int
    val string_of_stats : resp -> string
  end
type error_code =
    HELLO_INCOMPATIBLE
  | HELLO_EPERM
  | REQUEST_BAD_VERSION
  | REQUEST_BAD_TYPE
  | REQUEST_BAD_STAT
  | REQUEST_BAD_VENDOR
  | REQUEST_BAD_SUBTYPE
  | REQUEST_REQUEST_EPERM
  | REQUEST_BAD_LEN
  | REQUEST_BUFFER_EMPTY
  | REQUEST_BUFFER_UNKNOWN
  | ACTION_BAD_TYPE
  | ACTION_BAD_LEN
  | ACTION_BAD_VENDOR
  | ACTION_BAD_VENDOR_TYPE
  | ACTION_BAD_OUT_PORT
  | ACTION_BAD_ARGUMENT
  | ACTION_EPERM
  | ACTION_TOO_MANY
  | ACTION_BAD_QUEUE
  | FLOW_MOD_ALL_TABLES_FULL
  | FLOW_MOD_OVERLAP
  | FLOW_MOD_EPERM
  | FLOW_MOD_EMERG_TIMEOUT
  | FLOW_MOD_BAD_COMMAND
  | FLOW_MOD_UNSUPPORTED
  | PORT_MOD_BAD_PORT
  | PORT_MOD_BAD_HW_ADDR
  | QUEUE_OP_BAD_PORT
  | QUEUE_OP_BAD_QUEUE
  | QUEUE_OP_EPERM
val marshal_and_sub : (Cstruct.buf -> int) -> Cstruct.buf -> Cstruct.buf
val marshal_and_shift : (Cstruct.buf -> int) -> Cstruct.buf -> (int * Cstruct.buf)
(* val contain_exc : string -> `a -> `a option *)
val error_code_of_int : int -> error_code
val int_of_error_code : error_code -> uint32
val string_of_error_code : error_code -> string
val marshal_error : error_code -> Cstruct.buf -> int32 -> Cstruct.buf -> int
val build_features_req : uint32 -> Cstruct.buf -> int
val build_echo_resp : Header.h -> Cstruct.buf-> Cstruct.buf -> int
type t =
  Hello of Header.h 
  | Error of Header.h * error_code
  | Echo_req of Header.h * Cstruct.buf 
  | Echo_resp of Header.h * Cstruct.buf
  | Vendor of Header.h * vendor * Cstruct.buf 
  | Features_req of Header.h
  | Features_resp of Header.h * Switch.features
  | Get_config_req of Header.h
  | Get_config_resp of Header.h * Switch.config
  | Set_config of Header.h * Switch.config
  | Packet_in of Header.h * Packet_in.t
  | Flow_removed of Header.h * Flow_removed.t
  | Port_status of Header.h * Port.status
  | Packet_out of Header.h * Packet_out.t (* Bitstring.t *)
  | Flow_mod of Header.h * Flow_mod.t
  | Port_mod of Header.h * Port_mod.t
  | Stats_req of Header.h * Stats.req
  | Stats_resp of Header.h * Stats.resp
  | Barrier_req of Header.h
  | Barrier_resp of Header.h
  | Queue_get_config_req of Header.h * Port.t
  | Queue_get_config_resp of Header.h * Port.t * Queue.t array
val parse : Header.h -> Cstruct.buf -> t
