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
module OP = Ofpacket

let resolve t = Lwt.on_success t (fun _ -> ())

let get_new_buffer len = 
  let buf = OS.Io_page.to_cstruct (OS.Io_page.get 1) in 
    Cstruct.sub buf 0 len 

module Socket = struct  

type t = {
  sock: Channel.t;
  data_cache: Cstruct.t ref; 
}

let create_socket sock = 
  { sock; data_cache=ref (get_new_buffer 0);}

let write_buffer t bits =
  let _ = Channel.write_buffer t.sock bits in  
    Channel.flush t.sock
 
let close t = Channel.close t.sock

let read_data t len = 
  match (len, (Cstruct.len !(t.data_cache) ) ) with
    | (0, _) -> return (get_new_buffer 0)
    | (_, 0) -> 
      lwt data = Channel.read_some t.sock in
      let ret = Cstruct.sub data 0 len in 
      let _ = t.data_cache := (Cstruct.shift data len) in 
        return ret
    | (_, l) when (l >= len) -> 
      let ret = Cstruct.sub !(t.data_cache) 0 len in
      let _ = t.data_cache := (Cstruct.shift !(t.data_cache) len) in  
        return ret 
    | (_, l) when (l < len) -> 
      let len_rest = len - l in 
      let ret = Cstruct.set_len !(t.data_cache) len in 
      lwt data = Channel.read_some t.sock in
      let _ = Cstruct.blit data 0 ret l len_rest in 
      let _ = t.data_cache := (Cstruct.shift data len_rest) in 
        return (ret)
    | _ -> failwith "invalid read data operation"
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

let init_local_conn_state () = 
  let (controller_input, switch_output) = Lwt_stream.create () in 
  let (switch_input, controller_output) = Lwt_stream.create () in 
  let ch1 = {dpid=0L;t=(Local (controller_input, controller_output));} in 
  let ch2 = {dpid=0L;t=(Local (switch_input, switch_output));} in
    (ch1, ch2)

let read_packet conn =
  match conn.t with
  | Socket t -> 
      lwt hbuf = Channel.read_exactly t.Socket.sock OP.Header.sizeof_ofp_header in  
      let ofh  = OP.Header.parse_header hbuf in
      let dlen = ofh.OP.Header.len - OP.Header.sizeof_ofp_header in 
      lwt dbuf = 
        if (dlen = 0) then 
          return (Cstruct.create 0)
        else
          Channel.read_exactly t.Socket.sock dlen 
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
          return (OS.Console.log (Printf.sprintf "[socket] close error: %s\n%!"
          (Printexc.to_string exn)))
          ) 
  | Local (_, output) -> output None 
 
