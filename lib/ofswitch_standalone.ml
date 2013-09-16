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
  addr: Macaddr.t; 
  switch: OP.datapath_id;
}

type switch_state = {
  mutable mac_cache: (Macaddr.t, OP.Port.t) Hashtbl.t; 
  req_count: int ref; 
}

let switch_data = 
  { mac_cache = Hashtbl.create 0; req_count=(ref 0);} 


let datapath_join_cb controller dpid evt =
  let dp = 
    match evt with
      | OE.Datapath_join (c, _) -> c
      | _ -> invalid_arg "bogus datapath_join event match!" 
  in
  return (pp "+ datapath:0x%012Lx\n" dp)

let datapath_leave_cb controller dpid evt =
  let dp = 
    match evt with
      | OE.Datapath_leave (c) -> c
      | _ -> invalid_arg "bogus datapath_leave event match!" 
  in
  let _ = Hashtbl.clear switch_data.mac_cache in 
  let _ = switch_data.req_count := 0 in 
  return (pp "- datapath:0x%012Lx\n" dp)


let req_count = (ref 0)
let port_status_cb controller dpid = function
  | OE.Port_status (OP.Port.DEL, port, _) -> 
    let macs = Hashtbl.fold (
      fun mac p r -> 
        if(p = (OP.Port.port_of_int port.OP.Port.port_no) ) then 
          r @ [mac]
        else
          r ) switch_data.mac_cache [] 
    in
      return (
        List.iter (Hashtbl.remove switch_data.mac_cache) macs)
  | _ -> return ()
let add_entry_in_hashtbl mac_cache ix in_port = 
  if not (Hashtbl.mem mac_cache ix ) then
      Hashtbl.add mac_cache ix in_port
  else  
      Hashtbl.replace mac_cache ix in_port 

let packet_in_cb controller dpid evt =
  incr switch_data.req_count;
  let (in_port, buffer_id, data, dp) = 
    match evt with
      | OE.Packet_in (inp, _, buf, dat, dp) -> (inp, buf, dat, dp)
      | _ -> invalid_arg "bogus datapath_join event match!"
  in
  (* Parse Ethernet header *)
  let m = OP.Match.raw_packet_to_match in_port data in 

  (* Store src mac address and incoming port *)
  let ix = m.OP.Match.dl_src in
  let _ = Hashtbl.replace switch_data.mac_cache ix in_port in
 
  (* check if I know the output port in order to define what type of message
   * we need to send *)
  let ix = m.OP.Match.dl_dst in
  if ( (ix = Macaddr.broadcast )
       || (not (Hashtbl.mem switch_data.mac_cache ix)) ) 
  then ( 
    let bs = 
          (OP.Packet_out.create ~buffer_id:buffer_id 
             ~actions:[ OP.(Flow.Output(Port.All , 2000))] 
           ~data:data ~in_port:in_port () ) in   
    let h = OP.Header.create OP.Header.PACKET_OUT 0 in 
        OC.send_data controller dpid (OP.Packet_out (h, bs))
  ) else (
    let out_port = (Hashtbl.find switch_data.mac_cache ix) in
    let flags = OP.Flow_mod.({send_flow_rem=true; emerg=false; overlap=false;}) in 
    lwt _ = 
      if (buffer_id = -1l) then
        (* Need to send also the packet in cache the packet is not cached *)
        let bs = 
                OP.Packet_out.create
                   ~buffer_id:buffer_id    
                   ~actions:[ OP.(Flow.Output(out_port, 2000))] 
                   ~data:data ~in_port:in_port ()  in   
        let h = OP.Header.create OP.Header.PACKET_OUT 0 in 
          OC.send_data controller dpid (OP.Packet_out (h, bs))
      else
        return ()
    in
    let pkt = 
            (OP.Flow_mod.create m 0_L OP.Flow_mod.ADD ~hard_timeout:0 
                 ~idle_timeout:0 ~buffer_id:(Int32.to_int buffer_id)  ~flags
                 [OP.Flow.Output(out_port, 2000)] ()) in
        let h = OP.Header.create OP.Header.FLOW_MOD 0 in 
          OC.send_data controller dpid (OP.Flow_mod (h, pkt))
 )

let init controller = 
  pp "test controller register datapath cb\n%!";
  OC.register_cb controller OE.DATAPATH_JOIN datapath_join_cb;
  pp "test controller register leave cb\n%!";
  OC.register_cb controller OE.DATAPATH_LEAVE datapath_leave_cb;
   pp "test controller register packet_in cb\n%!";
  OC.register_cb controller OE.PACKET_IN packet_in_cb;
   pp "test controller register packet_in cb\n%!";
  OC.register_cb controller OE.PORT_STATUS_CHANGE port_status_cb


let init_controller () = OC.init_controller ()

let run_controller mgr st = 
  let (controller, switch) = Ofsocket.init_local_conn_state () in 
  let _ = Lwt.ignore_result (
    try_lwt 
      OC.local_connect st controller init
    with exn ->
      return (printf "[switch] standalone controller dailed %s\n%!" (Printexc.to_string
      exn))
      ) in
    return switch
