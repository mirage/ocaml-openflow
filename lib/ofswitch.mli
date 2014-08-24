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
open V1_LWT
open Lwt


module Make(T: TCPV4)(N: NETWORK) (*(N1: NETWORK)(N2: NETWORK)*) : sig

  type t
(*
  type eth_netif = Ethif.Make(N).netif *)
  type eth_t = Ethif.Make(N).t
(*  type ip_if
  type tcp_flow
  type tcp_error 
  type tcp_t

  val ifconnect : eth_netif -> eth_t Lwt.t
*)
(* val ipconfig : eth_t -> string -> string -> string -> unit Lwt.t *)
(*
  val init : eth_netif -> string -> string -> string -> ip_if Lwt.t
  val create_connection : ip_if -> string -> int -> tcp_flow Lwt.t
  val init : eth_netif -> (string * string * string) -> (string * int) -> tcp_t Lwt.t 
*)
  val create_switch : T.t -> (* (string * string * string) -> *) (string * int) -> (eth_t list)-> unit Lwt.t

end

(** [create dpid] initializes the state for a switch with a datapth id dpid *)
(* val create_switch :  ?verbose:bool -> int64 -> t *)

(*
(* Port Management *)

(** [add_port mgr st intf] add port intf under the control of the switch st *)
val add_port : Manager.t -> ?use_mac:bool -> t -> Manager.id -> unit Lwt.t
(** [del_port mgr st intf] remove port intf from the control of the switch st *)
val del_port : Manager.t -> t -> string -> unit Lwt.t
(** [add_port_local mgr st intf] add port intf as the local loopback interface
 * of th switch st *)
val add_port_local : Manager.t -> t -> Manager.id -> unit Lwt.t

(** Switch state management *)

(** [add_flow st fl] add flow definition fl to the switch st *)
val add_flow : t -> Openflow.Ofpacket.Flow_mod.t -> unit Lwt.t

(** [del_flow st fl] remove all flows matching flow definition fl 
 * from the switch st *)
val del_flow : t -> Openflow.Ofpacket.Match.t -> unit Lwt.t

(** [get_flow_stats st fl] fetch statistics for flows matching flow definition
 * fl from the switch st *)
val get_flow_stats : t -> Openflow.Ofpacket.Match.t -> Openflow.Ofpacket.Flow.stats list 

(** Daemon run *)

(** [listen st mgr addr] start a listening switch control channel on addr *)
val listen : t -> Manager.t -> Nettypes.ipv4_src -> unit Lwt.t 
(** [connect st mgr addr] connect a switch control channel  to a controller 
 * on addr *)
val connect : t -> Manager.t -> Nettypes.ipv4_dst -> unit Lwt.t
(** [local_connect st mgr conn] setup a switch control channel on the local
 * Open`flow socket conn *)
val local_connect : t -> Manager.t -> Openflow.Ofsocket.conn_state -> unit Lwt.t
(** [standalone_connect st mgr addr] same as connect method, but a local
 * learning switch is responsible to control the switch, when the remote
 * control channel is unresponsive *)
val standalone_connect : t -> Manager.t -> Nettypes.ipv4_dst -> unit Lwt.t
*)
