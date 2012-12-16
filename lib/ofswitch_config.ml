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
open Lwt_unix
open Printf

type t = {
(*
  dev_m: (string * string) Lwt_condition.t; 
  res_m: bool Lwt_condition.t;
 *)
  fd: Lwt_unix.file_descr;
}

let add_port t dev = 
  let buf = String.create 4096 in 
  let req = Jsonrpc.string_of_call
              (Rpc.call "add_port" [Rpc.String dev]) in
  lwt _ = send t.fd req 0 (String.length req) [] in
  lwt len = recv t.fd buf 0 4096 [] in
  let resp = Jsonrpc.response_of_string buf in 
    return (resp.Rpc.success)
 (*
  let _ = Lwt_condition.signal t.dev_m ("add_port", dev) in
    Lwt_condition.wait t.res_m

 *)
let del_port t dev = 
  return false
  (*
  let _ = Lwt_condition.signal t.dev_m ("add_port", dev) in
    Lwt_condition.wait t.res_m
 *)

  (*
let client_t mgr client t = 
  let _ = Lwt_condition.signal client.res_m true in 
    while_lwt true do
      lwt (op, dev) = Lwt_condition.wait client.dev_m in
      let req = Jsonrpc.string_of_call
        (Rpc.call op [Rpc.String dev]) in
      let _ = Net.Channel.write_string t req 0 (String.length req) in
      lwt _ = Net.Channel.flush t in
      lwt resp_str = Net.Channel.read_some t in
      let resp =
        Jsonrpc.response_of_string
        (Cstruct.to_string resp_str) in 
        return (Lwt_condition.signal client.res_m resp.Rpc.success)
    done

let connect_client mgr dst =
  try_lwt
    let dev_m = Lwt_condition.create () in 
    let res_m = Lwt_condition.create () in
    let cl = {dev_m; res_m;} in 
    let _ = 
      Lwt.ignore_result 
        (Net.Channel.connect mgr 
           (`TCPv4 (None, dst, (client_t mgr cl)))) in
    lwt res = Lwt_condition.wait cl.res_m in
    match res with
      | true -> return cl
      | false -> failwith "ofswitch_config client failed"
  with ex -> 
    failwith (sprintf "ofswitch_config client failed: %s"
    (Printexc.to_string ex))
   *)

let connect_client  () =
  try_lwt
    let sock = socket PF_INET SOCK_STREAM 0 in  
    let dst = ADDR_INET( (Unix.inet_addr_of_string "127.0.0.1"), 
                         6634) in 
    lwt _ = connect sock dst in 
    let cl = {fd=sock;} in
      return cl
  with ex -> 
    failwith (sprintf "ofswitch_config client failed: %s"
    (Printexc.to_string ex))
  
let listen_t mgr del_port port =
  let listen_inner mgr st (input, output) =
  lwt req = Lwt_io.read_line input in
  let req = Jsonrpc.call_of_string req in 
  lwt success = 
   match (req.Rpc.name, req.Rpc.params) with
     | ("add-port", (Rpc.String (dev))::_) -> 
         let _ = printf "attaching port %s\n%!" dev in 
         Net.Manager.attach mgr dev
     | ("del-port", (Rpc.String (dev))::_) -> 
         let _ = printf "attaching port %s\n%!" dev in 
          lwt _ = del_port dev in 
          lwt _ = Net.Manager.detach mgr dev in 
            return true
     | (_, _) -> 
         let _ = printf "[ofswitch-config] invalid action %s\n%!" 
                   (req.Rpc.name) in 
         return false
  in 
  let resp = 
    Jsonrpc.string_of_response (Rpc.success (Rpc.Null)) in 
  lwt _ = Lwt_io.write_line output resp in 
  lwt _ = Lwt_io.close output in 
  lwt _ = Lwt_io.close input in 
    return ()
   in 
 let addr = Unix.ADDR_INET(Unix.inet_addr_any, 6634) in 
  let _ = Lwt_io.establish_server addr 
            (fun a -> Lwt.ignore_result (listen_inner mgr del_port a) ) in 
    return ()

