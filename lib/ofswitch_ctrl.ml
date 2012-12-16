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

open Lwt
open Printf
open Openflow.Ofswitch_config
open Lwt_unix

let check_cmd_args cmd count = 
  if ((Array.length Sys.argv) < (2 + count)) then
    failwith (sprintf "Insufficient args for command %s (required %d)"
                cmd count)

let send_cmd (input, output) =
  try_lwt
     let _ = 
       if ((Array.length Sys.argv) < 2) then 
         failwith "No command defined"
     in 
       match (Sys.argv.(1)) with
         | "add-port" -> 
             let _ = check_cmd_args Sys.argv.(1) 1 in 
             let cmd = Rpc.({name=Sys.argv.(1); params=[(Rpc.String
             Sys.argv.(2))];}) in 
             lwt _ = Lwt_io.write_line output (Jsonrpc.string_of_call cmd) in 
             lwt _ = Lwt_io.read_line input in 
               return ()
         | "del-port" ->
             let _ = check_cmd_args Sys.argv.(1) 1 in 
             let cmd = Rpc.({name=Sys.argv.(1); params=[(Rpc.String
             Sys.argv.(2))];}) in 
             lwt _ = Lwt_io.write_line output (Jsonrpc.string_of_call cmd) in 
             lwt _ = Lwt_io.read_line input in 
                return ()
          | _ -> 
             let _ = printf "Fail: unknown cmd: %s\n%!" Sys.argv.(1) in
               return ()
   with  ex -> 
     return (printf "Fail: %s" (Printexc.to_string ex))

lwt _ = 
  try_lwt 
    (* lwt cl = connect_client () in  *)
(*    let sock = socket PF_INET SOCK_STREAM 0 in  *)
    let dst = ADDR_INET( (Unix.inet_addr_of_string "127.0.0.1"), 
                         6634) in
    lwt _ = Lwt_io.with_connection dst (send_cmd) in  
(*    lwt _ = Lwt_unix.connect sock dst in 
    let output = Lwt_io.of_fd ~mode:Lwt_io.output sock in 
    let  = Lwt_io.of_fd ~mode:Lwt_io.output sock in 
    (* lwt _ = connect sock dst in *)

    lwt _ = send_cmd output in *)
      return ()
  with e ->
    Printf.eprintf "Error: %s" (Printexc.to_string e); 
    return ()
