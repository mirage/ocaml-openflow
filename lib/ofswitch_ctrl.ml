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

let send_cmd cl =
   try_lwt
     let _ = 
       if ((Array.length Sys.argv) < 2) then 
         failwith "No command defined"
     in 
       match (Sys.argv.(1)) with
         | "add-port" -> 
             let _ = check_cmd_args "add_port" 1 in 
             lwt _ = add_port cl Sys.argv.(2) in 
               return ()
         | _ -> 
             let _ = printf "Fail: unknown cmd: %s\n%!" Sys.argv.(1) in
               return ()
   with  ex -> 
     return (printf "Fail: %s" (Printexc.to_string ex))

lwt _ = 
  try_lwt 
   lwt cl = connect_client () in  
   lwt _ = send_cmd cl in 
      return ()
  with e ->
    Printf.eprintf "Error: %s" (Printexc.to_string e); 
    return ()
