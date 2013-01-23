(*
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

open Net.Nettypes

type t 

val init_topology: unit -> t
val add_port: t -> int64 -> int -> ethernet_mac -> unit Lwt.t
val add_channel: t -> int64 -> Ofcontroller.t -> unit
val discover: t-> unit Lwt.t
val process_lldp_packet: t -> int64 -> int -> Cstruct.t -> unit
val find_dpid_path: t -> int64 -> Ofpacket.Port.t -> int64 -> 
  Ofpacket.Port.t -> (int64 * Ofpacket.Port.t * Ofpacket.Port.t) list
val remove_dpid: t -> int64 -> unit
val is_transit_port : t -> int64 -> int -> bool

