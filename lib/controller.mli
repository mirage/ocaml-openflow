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
module Event :
  sig
    type t =
        DATAPATH_JOIN
      | DATAPATH_LEAVE
      | PACKET_IN
      | FLOW_REMOVED
      | FLOW_STATS_REPLY
      | AGGR_FLOW_STATS_REPLY
      | DESC_STATS_REPLY
      | PORT_STATS_REPLY
      | TABLE_STATS_REPLY
      | PORT_STATUS_CHANGE
    type e =
        Datapath_join of Ofpacket.datapath_id
      | Datapath_leave of Ofpacket.datapath_id
      | Packet_in of Ofpacket.Port.t * int32 * Cstruct.buf * 
          Ofpacket.datapath_id
      | Flow_removed of Ofpacket.Match.t * Ofpacket.Flow_removed.reason * 
          int32 * int32 * int64 * int64 * Ofpacket.datapath_id
      | Flow_stats_reply of int32 * bool * Ofpacket.Flow.stats list *
          Ofpacket.datapath_id
      | Aggr_flow_stats_reply of int32 * int64 * int64 * int32 *
          Ofpacket.datapath_id
      | Port_stats_reply of int32 * Ofpacket.Port.stats list * 
          Ofpacket.datapath_id
      | Table_stats_reply of int32 * Ofpacket.Stats.table list * 
          Ofpacket.datapath_id
      | Desc_stats_reply of string * string * string * string * string *
          Ofpacket.datapath_id
      | Port_status of Ofpacket.Port.reason * Ofpacket.Port.phy * 
          Ofpacket.datapath_id
    val string_of_event : e -> string
  end

type t 
val register_cb : t -> Event.t -> 
  (t -> Ofpacket.datapath_id -> Event.e -> unit  Lwt.t) -> unit
val send_of_data : t -> Ofpacket.datapath_id -> Cstruct.buf  -> unit Lwt.t
val terminate : t -> unit
val mem_dbg : string -> unit
val listen : Net.Manager.t -> Net.Nettypes.ipv4_src -> 
  (t -> 'a) -> unit Lwt.t
val connect : Net.Manager.t -> Net.Nettypes.ipv4_dst -> 
  (t -> 'a) -> unit Lwt.t
