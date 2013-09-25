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

(** FlowVisor topology discovery *)


(** Initialize a topology struct *)
val init_topology: unit -> t

(** Add to the structure a new switch and the relevant controller channel *)
val add_channel: t -> int64 -> Openflow.Ofcontroller.t -> unit
(** Add a new port on a switch *)
val add_port: t -> int64 -> int -> Macaddr.t -> unit Lwt.t
(** run a daemon which broadcasts lldp packet every 120 seconds in order to
 * discover physical connectivity between switches *)
val discover: t-> unit Lwt.t

(** parse and process an lldp packet *)
val process_lldp_packet: t -> int64 -> int -> Cstruct.t -> bool
(** discover a path between two ports of connected switches *)
val find_dpid_path: t -> int64 -> Openflow.Ofpacket.Port.t -> int64 -> 
  Openflow.Ofpacket.Port.t -> (int64 * Openflow.Ofpacket.Port.t * Openflow.Ofpacket.Port.t) list
(** remove all ports of a specific switch  *)
val remove_dpid: t -> int64 -> unit
(** reports if a link function a a transit link between two adjacent switches *)
val is_transit_port : t -> int64 -> int -> bool

