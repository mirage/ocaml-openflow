(* 
 * Copyright (c) 2012 Charalampos Rotsos <cr409@cl.cam.ac.uk>
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
open Net
open Net.Nettypes

let resolve t = Lwt.on_success t (fun _ -> ())

module OP = Openflow.Ofpacket
module OC = Openflow.Ofcontroller
module OE = Openflow.Ofcontroller.Event
open Switch.Ofswitch

let pp = Printf.printf
let sp = Printf.sprintf

(****************************************************************
 * OpenFlow Switch configuration 
 *****************************************************************)

let print_time () =
  while_lwt true do
    OS.Time.sleep 10.0 >>
    return (printf "%03.6f: process running..\n%!" (OS.Clock.time ()))
  done

let switch_run () = 
  let sw = create_switch 0x100L in
  let use_mac = ref true in 
  let (fd, dev) = Tuntap.opentap ~persist:true ~devname:"tap0" () in 
(*  let _ = Tuntap.set_ipv4 ~devname:("tap0") ~ipv4:"10.20.0.1"
      ~netmask:"255.255.255.0" () in *)
  let _ = OS.Netif.add_vif (OS.Netif.id_of_string dev) OS.Netif.ETH fd in 

  let (fd, dev) = Tuntap.opentap ~persist:true ~devname:"tap1" () in 
  let _ = OS.Netif.add_vif (OS.Netif.id_of_string dev) OS.Netif.ETH fd in 
  
  try_lwt 
    Manager.create 
    (fun mgr interface id ->
       match (OS.Netif.string_of_id (OS.Netif.id (Ethif.get_netif
       (Manager.get_ethif interface)))) with 
         | "tap0" 
         | "0" ->
             lwt _ = OS.Time.sleep 5.0 in
             let _ = printf "connecting switch...\n%!" in 
             let ip = 
               ( Ipaddr.V4.make 10l 20l 0l 2l, 
                 Ipaddr.V4.make 255l 255l 255l 0l, 
                 []) in  
             lwt _ = Manager.configure interface (`IPv4 ip) in
              let dst_ip = Ipaddr.V4.make 10l 20l 0l 4l in 
             lwt _ = standalone_connect sw mgr (dst_ip, 6633) in 
             let _ = printf "connect returned...\n%!" in 
              return ()
      | str_id -> 
          let find dev = 
            try 
              let _ = Re_str.search_forward (Re_str.regexp "tap") dev 0 in true
            with Not_found -> false
          in
          lwt _ =
            if (not (find str_id) ) then 
              lwt _ = add_port mgr ~use_mac:(!use_mac) sw id in 
                return (use_mac := false)
            else
              add_port mgr ~use_mac:false sw id
          in 
            return ()
    )
  with e ->
    Printf.eprintf "Error: %s" (Printexc.to_string e); 
    return ()

let _ = OS.Main.run(switch_run ())
