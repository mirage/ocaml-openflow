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

(* Simple openflow controller that listens on port 6633 and replies
   with echo request on every packet_in event *)

open Lwt
open Printf
open Net
open Net.Nettypes

let resolve t = Lwt.on_success t (fun _ -> ())

module OP = Ofpacket
module OC = Ofcontroller
module OE = Ofcontroller.Event

let pp = Printf.printf
let sp = Printf.sprintf

(* TODO this the mapping is incorrect. the datapath must be moved to the key
 * of the hashtbl *)
type mac_switch = {
  addr: OP.eaddr; 
  switch: OP.datapath_id;
}

type switch_state = {
(*   mutable mac_cache: (mac_switch, OP.Port.t) Hashtbl.t; *)
  mutable mac_cache: (OP.eaddr, OP.Port.t) Hashtbl.t; 
  mutable dpid: OP.datapath_id list;
  mutable of_ctrl: OC.t list; 
  req_count: int ref; 
}

let switch_data = 
  { mac_cache = Hashtbl.create 0;dpid = []; 
    of_ctrl = []; req_count=(ref 0);} 


let datapath_join_cb controller dpid evt =
  let dp = 
    match evt with
      | OE.Datapath_join (c) -> c
      | _ -> invalid_arg "bogus datapath_join event match!" 
  in
  switch_data.dpid <- switch_data.dpid @ [dp];
  return (pp "+ datapath:0x%012Lx\n" dp)

let req_count = (ref 0)

let add_entry_in_hashtbl mac_cache ix in_port = 
  if not (Hashtbl.mem mac_cache ix ) then
      Hashtbl.add mac_cache ix in_port
  else  
      Hashtbl.replace mac_cache ix in_port 

let packet_in_cb controller dpid evt =
  incr switch_data.req_count;
  let (in_port, buffer_id, data, dp) = 
    match evt with
      | OE.Packet_in (inp, buf, dat, dp) -> (inp, buf, dat, dp)
      | _ -> invalid_arg "bogus datapath_join event match!"
  in
  (* Parse Ethernet header *)
  let m = OP.Match.raw_packet_to_match in_port data in 

  (* Store src mac address and incoming port *)
  let ix = m.OP.Match.dl_src in
  let _ = Hashtbl.replace switch_data.mac_cache ix in_port in
 
  (* check if I know the output port in order to define what type of message
   * we need to send *)
  let broadcast = String.make 6 '\255' in
  let ix = m.OP.Match.dl_dst in
  if ( (ix = broadcast)
       || (not (Hashtbl.mem switch_data.mac_cache ix)) ) 
  then (
    let bs = 
      OP.marshal_and_sub 
      ( OP.Packet_out.marshal_packet_out  
          (OP.Packet_out.create
             ~buffer_id:buffer_id 
             ~actions:[ OP.(Flow.Output(Port.All , 2000))] 
           ~data:data ~in_port:in_port () )) (OS.Io_page.get ()) in   
        OC.send_of_data controller dpid bs
  ) else (
    let out_port = (Hashtbl.find switch_data.mac_cache ix) in
    let flags = OP.Flow_mod.({send_flow_rem=true; emerg=false; overlap=false;}) in 
    lwt _ = 
      if (buffer_id = -1l) then
        (* Need to send also the packet in cache the packet is not cached *)
        let bs = 
          OP.marshal_and_sub 
            ( OP.Packet_out.marshal_packet_out  
                (OP.Packet_out.create
                   ~buffer_id:buffer_id    
                   ~actions:[ OP.(Flow.Output(out_port, 2000))] 
                   ~data:data ~in_port:in_port () )) (OS.Io_page.get ()) in   
          OC.send_of_data controller dpid bs      
      else
        return ()
    in
    let pkt = 
      OP.marshal_and_sub 
        ( OP.Flow_mod.marshal_flow_mod 
            (OP.Flow_mod.create m 0_L OP.Flow_mod.ADD ~hard_timeout:0 
                 ~idle_timeout:0 ~buffer_id:(Int32.to_int buffer_id)  ~flags
                 [OP.Flow.Output(out_port, 2000)] ()))
        (OS.Io_page.get ()) in
      OC.send_of_data controller dpid pkt
 )

let init controller = 
  if (not (List.mem controller switch_data.of_ctrl)) then
    switch_data.of_ctrl <- (([controller] @ switch_data.of_ctrl));
  pp "test controller register datapath cb\n";
  OC.register_cb controller OE.DATAPATH_JOIN datapath_join_cb;
  pp "test controller register packet_in cb\n";
  OC.register_cb controller OE.PACKET_IN packet_in_cb

let port = 6633 

let run () =
  Net.Manager.create (fun mgr interface id ->
    try_lwt
      let ip = 
          (ipv4_addr_of_tuple (10l,0l,0l,3l),
           ipv4_addr_of_tuple (255l,255l,255l,0l), []) in  
      lwt _ = Manager.configure interface (`IPv4 ip) in
        OC.listen mgr (None, port) init
    with | e ->
      return (Printf.eprintf "Unexpected exception : %s" (Printexc.to_string e))
  )

let _ = OS.Main.run (run ())
