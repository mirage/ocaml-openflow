(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
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

(* TCP channel that uses the UNIX runtime to retrieve fds *)

open Nettypes
open Lwt
open Printf

type ipv4_src = ipv4_addr option * int
type ipv4_dst = ipv4_addr * int

exception Listen_error of string
exception Accept_error of string
exception Connect_error of string
exception Read_error of string
exception Write_error of string

module TCPv4 = struct
  type t =  {
    fd: Lwt_unix.file_descr;
    abort_t: unit Lwt.t;  (* abort thread *)
    abort_u: unit Lwt.u;  (* wakener for socket close *)
  }
  type mgr = Manager.t
  type src = ipv4_addr option * int
  type dst = ipv4_addr * int

  (* TODO put an istring pool in the manager? *)
  let t_of_fd fd =
    let abort_t,abort_u = Lwt.task () in
      { fd; abort_u; abort_t }

  let write t bs =
    try_lwt
      let buf = Lwt_bytes.to_string bs in 
      let len = String.length buf in 
      let snd = ref 0 in 
        while_lwt (len > !snd) do
          lwt ret = Lwt_unix.write t.fd buf !snd (len - !snd) in 
          snd := !snd + len;
          return ()
        done
    with exn -> 
      fail (Write_error (sprintf "Flow.write error: %s\n%!" (Printexc.to_string exn)))

  let read t =
    try_lwt
      let buf = String.create 4096 in
      lwt len = Lwt_unix.read t.fd buf 0 4096 in 
        match (len) with
          | 0 -> return None
          | len -> return (Some(Lwt_bytes.of_string (String.sub buf 0 len)))
    with exn -> 
      fail (Write_error 
              (sprintf "Flow.read error: %s\n%!" (Printexc.to_string exn)))

  let writev t bs =
    Lwt_list.iter_s (fun b -> write t b) bs

  let close t =
  lwt _ = Lwt_unix.close t.fd in
  (try Lwt.wakeup t.abort_u () with _ -> ());
  return ()

  let close_on_exit t fn =
    try_lwt 
      lwt x = fn t in
      close t >> return x
    with exn -> 
      close t >> fail exn

  let listen mgr src fn =
    let addr, port = match src with
      |None, port -> "0.0.0.0", port
      |Some addr, port -> 
          (Nettypes.ipv4_addr_to_string addr), port 
    in
      try_lwt 
        let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
        lwt hostinfo = Lwt_unix.gethostbyname addr in
        let _ = Printf.printf "Starting switch...\n%!" in 
        let server_address = hostinfo.Lwt_unix.h_addr_list.(0) in
        let _ = Lwt_unix.bind sock (Lwt_unix.ADDR_INET (server_address, 6633)) in
        let _ = Lwt_unix.listen sock 10 in 
        let _ = Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true in
          while_lwt true do 
            lwt (fd, sockaddr) = Lwt_unix.accept sock in
              match sockaddr with
                | Unix.ADDR_INET (dst, port) ->
                  let _ = printf "Received a connection %s:%d"
                             (Unix.string_of_inet_addr dst) port in
                  let ip = 
                    match (Nettypes.ipv4_addr_of_string (Unix.string_of_inet_addr dst)) with
                      | None -> invalid_arg "dest ip is Invalid"
                      | Some(ip) -> ip
                  in
                  let _ = Lwt_unix.set_blocking fd true in 
                  let t' = t_of_fd fd in
                    return (Lwt.ignore_result (
                      close_on_exit t' 
                        (fun t ->
                           try_lwt
                             fn (ip, port) t 
                           with exn ->
                             return (Printf.printf "EXN: %s\n%!" (Printexc.to_string exn))
                        )))
                | Unix.ADDR_UNIX(_) -> invalid_arg "invalid unix addr"
          done
      with
      | e ->
          fail (Listen_error (Printexc.to_string e))

  let connect mgr ?src ((addr,port):ipv4_dst) (fn: t -> 'a Lwt.t) =
    try_lwt 
      let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
      let addr = Nettypes.ipv4_addr_to_string addr in
      lwt hostinfo = (Lwt_unix.gethostbyname addr) in 
      let ip = hostinfo.Unix.h_addr_list.(0) in
      let dst = Unix.ADDR_INET(ip, port) in 
      lwt _ = Lwt_unix.connect sock dst in 
      let t = t_of_fd sock in
      let cancel_t = t.abort_t >> fail (Connect_error "cancelled") in
        (close_on_exit t fn) <?> cancel_t  
    with 
      | Not_found -> 
          fail (Connect_error (sprintf "Host %s not found" (Nettypes.ipv4_addr_to_string addr)))
end

type t =
  | TCPv4 of TCPv4.t 

type mgr = Manager.t

let read = function
  | TCPv4 t -> TCPv4.read t

let write = function
  | TCPv4 t -> TCPv4.write t

let writev = function
  | TCPv4 t -> TCPv4.writev t

let close = function
  | TCPv4 t -> TCPv4.close t

let connect mgr = function
  |`TCPv4 (src, dst, fn) ->
     TCPv4.connect mgr ?src dst (fun t -> fn (TCPv4 t))
  |_ -> fail (Failure "unknown protocol")

let listen mgr = function
  |`TCPv4 (src, fn) ->
     TCPv4.listen mgr src (fun dst t -> fn dst (TCPv4 t))
  |_ -> fail (Failure "unknown protocol")

