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

module OP = Ofpacket

type t = {
 fd: Lwt_unix.file_descr;
}

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
 
let hashtbl_to_flow_match map =
  let of_match =  OP.Match.wildcard () in
  let _ = 
    Hashtbl.iter (
      fun name value -> 
        match name with 
        | "in_port" -> 
            let _ = of_match.OP.Match.wildcards.OP.Wildcards.in_port <- false in 
            let _ = of_match.OP.Match.in_port <- OP.Port.port_of_int 
                      (int_of_string value) in 
              ()
        | "dl_vlan" -> 
            let _ = of_match.OP.Match.wildcards.OP.Wildcards.dl_vlan <- false in 
            let _ = of_match.OP.Match.dl_vlan <- int_of_string value in 
              ()
        | "dl_vlan_pcp" -> 
            let _ = of_match.OP.Match.wildcards.OP.Wildcards.dl_vlan_pcp <- false in 
            let _ = of_match.OP.Match.dl_vlan_pcp <- char_of_int (int_of_string value) in
            ()
         | "dl_src" -> 
            let _ = of_match.OP.Match.wildcards.OP.Wildcards.dl_src <- false in
            let _ = 
              match (Net.Nettypes.ethernet_mac_of_string value) with
              | None -> printf "Invalid mac addr %s\n%!" value
              | Some t -> of_match.OP.Match.dl_src <- Net.Nettypes.ethernet_mac_to_bytes t 
            in 
            ()
         | "dl_dst" -> 
            let _ = of_match.OP.Match.wildcards.OP.Wildcards.dl_dst <- false in 
            let _ = 
              match (Net.Nettypes.ethernet_mac_of_string value) with
              | None -> printf "Invalid mac addr %s\n%!" value
              | Some t -> of_match.OP.Match.dl_dst <- Net.Nettypes.ethernet_mac_to_bytes t 
            in 
           ()
         | "dl_type" -> 
            let _ = of_match.OP.Match.wildcards.OP.Wildcards.dl_type <- false in 
            let _ = of_match.OP.Match.dl_type <- int_of_string value in 
            ()
         | "nw_src" -> 
             let fields = 
               match (Re_str.split (Re_str.regexp "\/") value) with
               | ip::mask::_ -> 
                   match (Net.Nettypes.ipv4_addr_of_string ip) with
                   | None -> printf "Invalid ip definition"
                   | Some ip -> 
                       let _ = of_match.OP.Match.wildcards.OP.Wildcards.nw_src <-
                         char_of_int (int_of_string mask) in 
                       let _ = of_match.OP.Match.nw_src <-
                         Net.Nettypes.ipv4_addr_to_uint32 ip in 
                       ()
               | _ -> printf "Invalid ip definition"
             in
               ()
         | "nw_dst" -> 
             let fields = 
               match (Re_str.split (Re_str.regexp "\/") value) with
               | ip::mask::_ -> 
                   match (Net.Nettypes.ipv4_addr_of_string ip) with
                   | None -> printf "Invalid ip definition"
                   | Some ip -> 
                       let _ = of_match.OP.Match.wildcards.OP.Wildcards.nw_dst <-
                         char_of_int (int_of_string mask) in 
                       let _ = of_match.OP.Match.nw_dst <-
                         Net.Nettypes.ipv4_addr_to_uint32 ip in 
                       ()
               | _ -> printf "Invalid ip definition"
             in
               ()
         | "nw_tos" -> 
            let _ = of_match.OP.Match.wildcards.OP.Wildcards.dl_vlan <- false in 
            let _ = of_match.OP.Match.nw_tos <- char_of_int (int_of_string
            value) in 
            ()
         | "nw_proto" -> 
            let _ = of_match.OP.Match.wildcards.OP.Wildcards.nw_proto <- false in 
            let _ = of_match.OP.Match.nw_proto <- char_of_int (int_of_string
            value) in 
            ()
         | "tp_src" -> 
            let _ = of_match.OP.Match.wildcards.OP.Wildcards.tp_src <- false in 
            let _ = of_match.OP.Match.tp_src <- int_of_string value in 
            ()
         | "tp_dst" -> 
            let _ = of_match.OP.Match.wildcards.OP.Wildcards.tp_dst <- false in 
            let _ = of_match.OP.Match.tp_dst <- int_of_string value in 
            ()
          | _ -> 
            let _ = eprintf "Invalid field name %s" name in 
            ()
    ) map in 
      of_match


let listen_t mgr del_port get_stats port =
  let listen_inner mgr st (input, output) =
    try_lwt 
      lwt req = Lwt_io.read_line input in
      let req = Jsonrpc.call_of_string req in 
      lwt success = 
        match (req.Rpc.name, req.Rpc.params) with
         | ("add-port", (Rpc.String (dev))::_) -> 
             let _ = Net.Manager.attach mgr dev in 
               return (Rpc.Enum [(Rpc.String "true")])
         | ("del-port", (Rpc.String (dev))::_) -> 
             lwt _ = del_port dev in 
             lwt _ = Net.Manager.detach mgr dev in 
               return (Rpc.Enum [(Rpc.String "true")])
         | ("dump-flows", (Rpc.Dict t)::_) -> 
             let _ = printf "dumpflows for %s\n%!" (Rpc.string_of_call req) in
             let map = 
               List.fold_right (
                 fun (name, value) r -> 
                   let _ = Hashtbl.add r name (Rpc.string_of_rpc value) in 
                   let _ = printf "Adding %s = %s\n%!" name (Rpc.string_of_rpc
                   value) in 
                     r 
               ) t (Hashtbl.create 10) in
             let of_match = hashtbl_to_flow_match map in
             let _ = printf "Find rules matching %s\n%!"
             (OP.Match.match_to_string of_match) in 
             let flows = get_stats of_match in 
             let res = 
               List.fold_right (
                 fun a r -> 
                   r @ [(Rpc.String (OP.Flow.string_of_flow_stat a))]

               ) flows [] in 
             return (Rpc.Enum res)
         | (_, _) -> 
             let _ = printf "[ofswitch-config] invalid action %s\n%!" 
                     (req.Rpc.name) in 
             return (Rpc.Enum [(Rpc.String "false")])
      in 
      let resp = 
        Jsonrpc.string_of_response (Rpc.success success) in 
      lwt _ = Lwt_io.write_line output resp in 
      lwt _ = Lwt_io.close output in 
      lwt _ = Lwt_io.close input in 
        return ()
    with exn ->
      Lwt_log.log  ~exn ~level:Lwt_log.Notice "[ofswitch_config] server error" 
 
  in 
  let addr = Unix.ADDR_INET(Unix.inet_addr_any, 6634) in 
  let _ = Lwt_io.establish_server addr 
            (fun a -> Lwt.ignore_result (listen_inner mgr del_port a) ) in 
    return ()

