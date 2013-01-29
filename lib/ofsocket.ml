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
open Net
open Lwt
open Printf
module OP = Ofpacket

exception ReadError

let sp = Printf.sprintf
let pp = Printf.printf
let ep = Printf.eprintf
let cp = pp "%s\n%!"

let resolve t = Lwt.on_success t (fun _ -> ())

module Socket = struct  
  type t = {
    sock: Channel.t;
    mutable data_cache: Cstruct.t list; 
  }

  let create_socket sock = 
    { sock; data_cache=[];}

  let write_buffer t bits =
    let _ = Channel.write_buffer t.sock bits in 
      Channel.flush t.sock

  let close t = Channel.close t.sock

  let cache_size t =
    List.fold_right (
      fun a r -> 
        r + (Cstruct.len a) ) t.data_cache 0
end

type conn_type = 
  | Socket of Socket.t
  | Local of OP.t Lwt_stream.t * (OP.t option -> unit) 

type conn_state = {
  mutable dpid : OP.datapath_id;
  t : conn_type; 
}

let init_socket_conn_state t = 
  {dpid=0L;t=(Socket (Socket.create_socket t));}
let init_local_conn_state input output = 
  {dpid=0L;t=(Local (input, output));}

let read_packet conn =
  match conn.t with
  | Socket t -> 
      lwt hbuf = Net.Channel.read_exactly t.Socket.sock OP.Header.sizeof_ofp_header in 
      let ofh  = OP.Header.parse_header hbuf in
      let dlen = ofh.OP.Header.len - OP.Header.sizeof_ofp_header in 
      lwt dbuf = 
        if (dlen = 0) then 
          return (Cstruct.create 0)
        else
          Net.Channel.read_exactly t.Socket.sock dlen 
      in
      let ofp  = OP.parse ofh dbuf in
        return ofp
  | Local (input, _) ->
    match_lwt (Lwt_stream.get input) with
    | None -> raise Nettypes.Closed 
    | Some ofp -> return ofp

let send_packet conn ofp =
  match conn.t with
  | Socket t ->  Socket.write_buffer t (OP.marshal ofp)
  | Local (_, output) -> return (output (Some ofp ))

let send_data_raw t bits = 
  match t.t with
  | Local _ -> failwith "send_of_data is not supported in Local mode"
  | Socket t -> 
      (* Socket.write_buffer t bits *)
      let _ = Channel.write_buffer t.Socket.sock bits in 
        Channel.flush t.Socket.sock

let close conn = 
  match conn.t with
  | Socket t -> 
      resolve (
        try_lwt
          Socket.close t
        with exn -> 
          return (printf "[socket] close error: %s\n%!" (Printexc.to_string exn))
          ) 
  | Local (_, output) -> output None 
 
