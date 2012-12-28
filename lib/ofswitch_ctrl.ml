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

let flow_element = 
  ["in_port"; "dl_src"; "dl_dst"; "dl_vlan"; "dl_pcp"; "dl_type";
  "nw_src";"nw_dst";
   "nw_tos"; "nw_proto"; "tp_src"; "tp_dst"; "actions";"priority";
   "idle_timeout"; "hard_timeout"; ]

let process_flow_description flow = 
  let fields = Re_str.split (Re_str.regexp ",") flow in 
  let rec process_flow_inner = function
    | [] -> [] 
    | hd::tl -> 
        let name::value::_ = Re_str.split (Re_str.regexp "=") hd in 
        let _ = 
          if (not (List.mem name flow_element) ) then 
            failwith (sprintf "Invalid flow field %s" name)
        in
        [(name, (Rpc.String value))] @ (process_flow_inner tl)
  in
    process_flow_inner fields



let send_cmd (input, output) =
  try_lwt
     let _ = 
       if ((Array.length Sys.argv) < 2) then 
         failwith "No command defined"
     in 
     lwt resp = 
       match (Sys.argv.(1)) with
         | "add-port" -> 
             let _ = check_cmd_args Sys.argv.(1) 2 in 
             let cmd = Rpc.({name=Sys.argv.(1); params=[(Rpc.String
             Sys.argv.(3))];}) in 
             lwt _ = Lwt_io.write_line output (Jsonrpc.string_of_call cmd) in 
             lwt resp = Lwt_io.read_line input in
             let resp = Jsonrpc.response_of_string resp in 
               return (string_of_bool resp.Rpc.success)
         | "del-port" ->
             let _ = check_cmd_args Sys.argv.(1) 2 in 
             let cmd = Rpc.({name=Sys.argv.(1); params=[(Rpc.String
             Sys.argv.(3))];}) in 
             lwt _ = Lwt_io.write_line output (Jsonrpc.string_of_call cmd) in 
             lwt resp = Lwt_io.read_line input in 
             let resp = Jsonrpc.response_of_string resp in 
                return (string_of_bool resp.Rpc.success)
         | "dump-flows" -> begin
             let _ = check_cmd_args Sys.argv.(1) 2 in 
             let fields = process_flow_description Sys.argv.(3) in  
             let cmd = Rpc.({name=Sys.argv.(1); params=[(Rpc.Dict fields)];}) in
             lwt _ = Lwt_io.write_line output (Jsonrpc.string_of_call cmd) in 
             lwt resp = Lwt_io.read_line input in 
             let resp = Jsonrpc.response_of_string resp in
              match resp.Rpc.contents with
               | Rpc.Enum flows ->
                   return 
                    (List.fold_right 
                      (fun a r -> sprintf "%s%s\n%!" r (Rpc.string_of_rpc a)) flows "")
               | _ -> return ""
         end
         | "add-flow" -> begin
             let _ = check_cmd_args Sys.argv.(1) 2 in 
             let fields = process_flow_description Sys.argv.(3) in  
             let cmd = Rpc.({name=Sys.argv.(1); params=[(Rpc.Dict fields)];}) in
             lwt _ = Lwt_io.write_line output (Jsonrpc.string_of_call cmd) in 
             lwt resp = Lwt_io.read_line input in 
             let resp = Jsonrpc.response_of_string resp in 
                return (string_of_bool resp.Rpc.success)
         end
         | _ -> 
             return (sprintf "Fail: unknown cmd: %s\n%!" Sys.argv.(1))
     in
     let _ = printf "result:\n%s\n%!" resp in
      return ()
   with  ex -> 
     return (printf "Fail: %s" (Printexc.to_string ex))

lwt _ = 
  try_lwt 
    let dst = ADDR_INET( (Unix.inet_addr_of_string "127.0.0.1"), 
                         6634) in
    lwt _ = Lwt_io.with_connection dst (send_cmd) in  
     return ()
  with e ->
    Printf.eprintf "Error: %s" (Printexc.to_string e); 
    return ()
