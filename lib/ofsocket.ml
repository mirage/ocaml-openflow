(*
 * Copyright (c) 2011 Haris Rotsos <cr409@cl.cam.ac.uk>
 * Copyright (c) 2014 Masoud Koleini <masoud.koleini@nottingham.ac.uk>
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

module OP = Ofpacket

let resolve t = Lwt.on_success t (fun _ -> ())

let get_new_buffer len = 
  let buf = Io_page.to_cstruct (Io_page.get 1) in 
    Cstruct.sub buf 0 len 

(* module Socket *) (* changed from a module to a functor that accepts TCPv4 *)
module Make(T:V1_LWT.TCPV4) = struct  

  module Channel = Channel.Make(T)

  type c = Channel.t

  type fl = Channel.flow

  type t = {
	sock: Channel.t;
	data_cache: Cstruct.t ref; 
  }

  type conn_type = 
	| Socket of t
	| Local of OP.t Lwt_stream.t * (OP.t option -> unit)
	  (* XXX decide: do we need local connection anymore? *)

	type conn_state = {
	  mutable dpid : OP.datapath_id;
	  t : conn_type; 
	}

  let create_socket sock = 
	{ sock; data_cache=ref (get_new_buffer 0);}

  let init_socket_conn_state t = 
	{dpid=0L;t=(Socket (create_socket t));}

  let init_local_conn_state () =  (* we may not need it, ref to the comment in conn_type *)
	let (controller_input, switch_output) = Lwt_stream.create () in 
	let (switch_input, controller_output) = Lwt_stream.create () in 
	let ch1 = {dpid=0L;t=(Local (controller_input, controller_output));} in 
	let ch2 = {dpid=0L;t=(Local (switch_input, switch_output));} in
	  (ch1, ch2)

  let read_packet conn =
	match conn.t with
	| Socket t -> 
		lwt hbuf = Channel.read_some t.sock ~len:(OP.Header.sizeof_ofp_header) in  
		let ofh  = OP.Header.parse_header hbuf in
		let dlen = ofh.OP.Header.len - OP.Header.sizeof_ofp_header in 
		lwt dbuf = 
		  if (dlen = 0) then 
		    return (Cstruct.create 0)
		  else
		    Channel.read_some t.sock ~len:dlen 
		  in 
			let ofp  = OP.parse ofh dbuf in
			  return ofp
	| Local (input, _) ->
		match_lwt (Lwt_stream.get input) with
		| None -> raise Lwt_stream.Closed
			(* XXX lwt_stream has its own Closed exception.
				what about writing it in the form if try_lwt with? *)
		| Some ofp -> return ofp

	let write_buffer t bits =
	  let _ = Channel.write_buffer t.sock bits in  
		Channel.flush t.sock

  (* send packet *)
  let send_packet conn ofp = 
	match conn.t with
	| Socket t ->  write_buffer t (OP.marshal ofp)
	| Local (_, output) -> return (output (Some ofp ))

  (* send raw data *)
  let send_data_raw t bits = 
	let _ = Channel.write_buffer t.sock bits in 
	  Channel.flush t.sock

  (* close channel *)
  let close conn = 
  	match conn.t with
  	| Socket t -> 
      resolve (
        try_lwt
          Channel.close t.sock
        with exn -> 
           return (Printf.printf "[socket] close error: %s\n%!" (Printexc.to_string exn))
          ) 
  | Local (_, output) -> output None 

  let create flow = Channel.create flow

end
