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
open Printf

module OP = Openflow.Ofpacket

let sp = Printf.sprintf
let cp = OS.Console.log

let parse_actions actions =
  let actions = Re_str.split (Re_str.regexp "/") actions in 
  let split_action = Re_str.regexp ":" in 
  List.fold_right (
    fun action actions ->
      try
        match (Re_str.split split_action action) with 
        | "output"::port::_ -> begin
            match (OP.Port.port_of_string port) with
            | Some port -> 
              actions @ [(OP.Flow.Output(port, 2000))]
            | None -> 
              let _ = printf "[ofswitch-config] Invalid port %s\n%!" port in 
              actions
          end
        | "set_vlan_vid"::vif::_  ->
          actions @ [(OP.Flow.Set_vlan_vid(int_of_string vif))]
        | "set_vlan_pcp"::pcp::_ -> 
          actions @ [(OP.Flow.Set_vlan_pcp(int_of_string pcp))]
        | "set_dl_src"::addr::_ -> begin 
            match (Macaddr.of_string  addr) with
            | None -> 
              let _ = cp (sp "[ofswitch-config] Invalid mac %s\n%!" action) in 
              actions
            | Some addr -> actions @[(OP.Flow.Set_dl_src(addr))]
          end
        | "set_dl_dst"::addr::_ -> begin 
            match (Macaddr.of_string addr) with
            | None -> 
              let _ = cp (sp "[ofswitch-config] Invalid mac %s\n%!" action) in 
              actions
            | Some addr -> actions @[(OP.Flow.Set_dl_dst(addr))]
          end
        | "set_nw_src"::addr::_ -> begin 
            match (Ipaddr.V4.of_string addr) with
            | None -> 
              let _ = cp (sp "[ofswitch-config] invalid ip %s\n%!" addr) in 
              actions
            | Some ip -> actions @ [(OP.Flow.Set_nw_src(ip))]
          end
        | "set_nw_dst"::addr::_ ->  begin
            match (Ipaddr.V4.of_string addr) with
            | None -> 
              let _ = cp (sp "[ofswitch-config] invalid ip %s\n%!" addr) in 
              actions
            | Some ip -> actions @  [(OP.Flow.Set_nw_dst(ip))]
          end
        | "set_nw_tos"::tos::_ -> 
          actions @ [(OP.Flow.Set_nw_tos(char_of_int (int_of_string tos)))]
        | "set_tp_src"::port::_ -> 
          actions @ [(OP.Flow.Set_tp_src(int_of_string port))]
        | "set_tp_dst"::port::_ -> 
          actions @ [(OP.Flow.Set_tp_dst(int_of_string port))]
        | _ -> 
          let _ = cp (sp "[ofswitch-config] invalid action %s" action) in 
          actions
      with exn -> 
        let _ = cp (sp "[ofswitch-config] error parsing action %s\n%!" action) in 
        actions
  ) actions []


let hashtbl_to_flow_match t =
  let of_match =  OP.Match.wildcard () in
  let map = 
    List.fold_right (
      fun (name, value) r -> 
        let _ = Hashtbl.add r name (Rpc.string_of_rpc value) in 
(*        let _ = printf "Adding %s = %s\n%!" name 
            (Rpc.string_of_rpc value) in *)
        r 
    ) t (Hashtbl.create 10) in
  let _ = 
    Hashtbl.iter (
      fun name value -> 
        match name with 
        | "in_port" -> begin 
            match (OP.Port.port_of_string value) with
            | Some port -> 
              let _ = of_match.OP.Match.wildcards.OP.Wildcards.in_port <- 
                        false in 
              let _ = of_match.OP.Match.in_port <- port in 
              ()
            | None -> 
              let _ = printf "[ofswitch-config] Invalid port %s\n%!" value in 
              ()
          end
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
            match (Macaddr.of_string  value) with
            | None -> printf "Invalid mac addr %s\n%!" value
            | Some t -> of_match.OP.Match.dl_src <- t 
          in 
          ()
        | "dl_dst" -> 
          let _ = of_match.OP.Match.wildcards.OP.Wildcards.dl_dst <- false in 
          let _ = 
            match (Macaddr.of_string value) with
            | None -> printf "Invalid mac addr %s\n%!" value
            | Some t -> of_match.OP.Match.dl_dst <- t 
          in 
          ()
        | "dl_type" -> 
          let _ = of_match.OP.Match.wildcards.OP.Wildcards.dl_type <- false in 
          let _ = of_match.OP.Match.dl_type <- int_of_string value in 
          ()
        | "nw_src" -> begin
            match (Re_str.split (Re_str.regexp "/") value) with
            | ip::mask::_ -> begin 
                match (Ipaddr.V4.of_string  ip) with
                | None -> printf "Invalid ip definition"
                | Some ip -> 
                  let _ = of_match.OP.Match.wildcards.OP.Wildcards.nw_src <-
                            char_of_int (int_of_string mask) in 
                  let _ = of_match.OP.Match.nw_src <- ip in 
                  ()
              end
            | _ -> printf "Invalid ip definition"
          end
        | "nw_dst" -> begin 
            match (Re_str.split (Re_str.regexp "/") value) with
            | ip::mask::_ -> begin 
                match (Ipaddr.V4.of_string  ip) with
                | None -> printf "Invalid ip definition"
                | Some ip -> 
                  let _ = of_match.OP.Match.wildcards.OP.Wildcards.nw_dst <-
                            char_of_int (int_of_string mask) in 
                  let _ = of_match.OP.Match.nw_dst <- ip
                  in
                  ()
              end
            | _ -> printf "Invalid ip definition"
          end
        | "nw_tos" -> 
          let _ = of_match.OP.Match.wildcards.OP.Wildcards.nw_tos <- false in 
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

let get_ethif mgr id = 
    let lst = Net.Manager.get_intfs mgr in 
    let (_, ethif) = List.find (fun (dev_id,_) -> id = dev_id) lst in 
    ethif

let listen_t mgr add_port del_port get_stats add_flow del_flow port =
  let manage (dip,dpt) t =
    try_lwt 
      lwt req = Net.Channel.read_line t in
      let req = 
        List.fold_right (
          fun a r -> 
            r ^ (Cstruct.to_string a)
        ) req "" in 
      let req = Jsonrpc.call_of_string req in 
      lwt success = 
        match (req.Rpc.name, req.Rpc.params) with
        | ("add-port", (Rpc.String (devname))::_) -> begin 
          try_lwt 
(*            let (fd, name) = Tuntap.opentap ~persist:true ~devname () in
            let id = OS.Netif.id_of_string name in 
            OS.Netif.add_vif id OS.Netif.ETH fd;
            lwt _ = Net.Manager.create (fun _ _ _ -> add_port id) in *)
            return (Rpc.Enum [(Rpc.String "true")])
          with exn ->
            cp (sp "[ofswitch-confid] add-port: %s\n%!"  (Printexc.to_string exn));
            return (Rpc.Enum [(Rpc.String "false")])

        end
        | ("del-port", (Rpc.String (dev))::_) -> 

          let ethif = Net.Ethif.get_netif 
              (Net.Manager.get_ethif (get_ethif mgr (OS.Netif.id_of_string dev))) in
(*          lwt _ = OS.Netif.destroy ethif in *)
          lwt _ = del_port dev in 
          return (Rpc.Enum [(Rpc.String "true")])
        | ("dump-flows", (Rpc.Dict t)::_) -> 
          let of_match = hashtbl_to_flow_match t in
          let _ = cp (sp "Find rules matching %s\n%!"
                        (OP.Match.match_to_string of_match)) in 
          let flows = get_stats of_match in 
          let res = 
            List.fold_right (
              fun a r -> (Rpc.String (OP.Flow.string_of_flow_stat a))::r) flows [] in 
          return (Rpc.Enum res)
        | ("add-flow", (Rpc.Dict t)::_) -> 
          let _ = cp (sp "adding flow %s\n%!" (Rpc.string_of_call req)) in
          let fm = OP.Flow_mod.create (OP.Match.wildcard () ) 0L OP.Flow_mod.ADD [] () in 
          let map = 
            List.fold_right (
              fun (name, value) r -> 
                match name with
                | "actions" -> 
                  fm.OP.Flow_mod.actions <- parse_actions (Rpc.string_of_rpc value);
                  r
                | "idle_timeout" -> 
                  fm.OP.Flow_mod.idle_timeout <- (Rpc.int_of_rpc value); 
                  r 
                | "hard_timeout" -> 
                  fm.OP.Flow_mod.hard_timeout <- (Rpc.int_of_rpc value); 
                  r 
                | "priority" -> 
                  fm.OP.Flow_mod.priority <- (Rpc.int_of_rpc value); 
                  r 
                | _ ->  r @ [(name, value)]
            ) t [] in
          let _ = fm.OP.Flow_mod.of_match <- hashtbl_to_flow_match map in 
          let _ = cp (sp "Add flow %s\n%!" (OP.Flow_mod.flow_mod_to_string fm)) in
          lwt _ = add_flow fm in 
          return (Rpc.Enum [(Rpc.String "true")] )
        | ("del-flow", (Rpc.Dict t)::_) -> 
          let of_match = hashtbl_to_flow_match t in
          lwt _ = del_flow of_match in 
          return (Rpc.Enum [(Rpc.String "true")] )
        | (_, _) -> 
          let _ = printf "[ofswitch-config] invalid action %s\n%!" 
              (req.Rpc.name) in 
          return (Rpc.Enum [(Rpc.String "false")])
      in 
      let resp = 
        Jsonrpc.string_of_response (Rpc.success success) in 
      let _ = Net.Channel.write_line t resp in
      lwt _ = Net.Channel.flush t in 
      lwt _ = Net.Channel.close t in 
      return ()
    with 
    | End_of_file -> return ()
    | exn ->
      let _ = cp "[ofswitch_config] server error" in 
      return ()
  in 
  Net.Channel.listen mgr (`TCPv4 ((None, 6634), manage )) 
