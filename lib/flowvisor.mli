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

open Lwt
open Net
open Net.Nettypes

type t 


val listen: t -> Manager.t -> ipv4_src -> unit Lwt.t
val local_listen: t -> Openflow.Ofsocket.conn_state -> unit Lwt.t
val create_flowvisor: ?verbose:bool -> unit -> t
val remove_slice : t -> Openflow.Ofpacket.Match.t ->  unit Lwt.t
val add_local_slice : Net.Manager.t -> t -> Openflow.Ofpacket.Match.t -> 
  Openflow.Ofsocket.conn_state -> int64 -> unit Lwt.t 
val add_slice : Net.Manager.t -> t -> Openflow.Ofpacket.Match.t -> 
  ipv4_dst -> int64 -> unit Lwt.t 
val add_local_slice : t -> Openflow.Ofpacket.Match.t -> 
  Openflow.Ofsocket.conn_state -> int64 -> unit Lwt.t 

