(*
 * Copyright (c) 2012 Haris Rotsos <cr409@cl.cam.ac.uk>
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

exception ReadError

open Net 

type conn_type 
type conn_state = {
  mutable dpid : Ofpacket.datapath_id;
  t : conn_type; 
}

val init_socket_conn_state : Channel.t -> conn_state
val init_local_conn_state: Ofpacket.t Lwt_stream.t -> 
  (Ofpacket.t option -> unit) -> conn_state
val read_packet : conn_state -> Ofpacket.t Lwt.t
val send_packet : conn_state -> Ofpacket.t -> unit Lwt.t
val send_data_raw : conn_state -> Cstruct.t -> unit Lwt.t
val close : conn_state -> unit 
