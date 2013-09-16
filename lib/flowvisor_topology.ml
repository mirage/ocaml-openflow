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
open Lldp
open Graph 

module OP = Ofpacket
open OP

let sp = Printf.sprintf
let pr = Printf.printf
let pp = Printf.printf
let ep = Printf.eprintf
let cp = OS.Console.log

module V = struct
  type t = int64
  let compare = Int64.compare
  let hash = Hashtbl.hash
  let equal = (=)
end
module E = struct
  type t = (int64 * int * int64 * int * int)
  let compare v1 v2 = 
      let (src1_dpid, src1_port, dst1_dpid, dst1_port, _) = v1 in 
      let (src2_dpid, src2_port, dst2_dpid, dst2_port, _) = v2 in
        if ((src1_dpid = src2_dpid) && (src1_port = src2_port) &&
            (dst1_dpid = dst2_dpid) && (dst1_port = dst2_port)) || 
           ((src1_dpid = dst2_dpid) && (src1_port = dst2_port) &&
            (dst1_dpid = src2_dpid) && (dst1_port = src2_port)) then
          0 
        else
          Pervasives.compare v1 v2 
        
  let default = (0L, 0, 0L, 0, 1)
end

(*module PortSet = Set.Make (
  struct 
    type t = (int64 * int)
    let compare = Pervasives.compare
end) *)

module Graph = Imperative.Graph.ConcreteLabeled(V)(E)

module W = struct
  type t = float
  type label = (int64 * int * int64 * int * int)
  let weight (_, _, _, _, rate) = 1.0 /. (float_of_int rate)
  let compare = Pervasives.compare
  let add = (+.)
  let zero = 0.0
end

module Dijkstra = Path.Dijkstra(Graph)(W)

type t = {
  ports : (int64 * int, Macaddr.t * bool) Hashtbl.t; 
  channels : (int64, Ofcontroller.t) Hashtbl.t;
  topo : Graph.t;
}

let init_topology () =
  let topo = Graph.create () in 
    {ports=(Hashtbl.create 64); channels=(Hashtbl.create 64);
    topo;}

let add_channel t dpid ch = 
  Hashtbl.replace t.channels dpid ch 

let generate_lldp_discovery dpid src_mac port =
  let bits = OS.Io_page.to_cstruct (OS.Io_page.get 1) in 
  let _ = Cstruct.BE.set_uint64 bits 0 dpid in 
  let dpid = Cstruct.to_string (Cstruct.sub bits 0 8) in 
  let bits = OS.Io_page.to_cstruct (OS.Io_page.get 1) in
  let _ = Cstruct.BE.set_uint16 bits 0 port in 
  let port = Cstruct.(to_string (sub bits 0 2)) in 
    marshal_and_sub (marsal_lldp_tlvs src_mac 
                       [Tlv_chassis_id_mac(src_mac);
                        Tlv_port_id_port_comp(port);
                        Tlv_ttl(120);
                        Tlv(LLDP_TYPE_SYSTEM_DESCR, dpid);
                        Tlv_end;]) 
     (OS.Io_page.to_cstruct (OS.Io_page.get 1))

let send_port_lldp t dpid port mac = 
  let data = generate_lldp_discovery dpid mac port in
  let h = OP.Header.(create PACKET_OUT 0) in 
  let m = OP.Packet_out.create ~actions:[(OP.Flow.Output(OP.Port.Port(port), 2000))] 
            ~data ~in_port:(OP.Port.No_port) () in 
  let ch = Hashtbl.find t.channels dpid in 
    Ofcontroller.send_data ch dpid (OP.Packet_out(h, m))

let add_port t dpid port mac =
  let _ = printf "[flowvisor-topo] adding port %Ld:%d\n%!" dpid port in 
  let _ = Hashtbl.replace t.ports (dpid, port) (mac, false) in 
    send_port_lldp t dpid port mac 

let mark_port_down t dpid port down = 
  let fmac = Macaddr.of_bytes_exn "\xff\xff\xff\xff\xff\xff" in 
  try
    let (mac, _) = Hashtbl.find t.ports (dpid, port) in 
      Hashtbl.replace t.ports (dpid, port) (mac, down)
  with Not_found -> Hashtbl.add t.ports (dpid, port) (fmac, down)

let discover t = 
  while_lwt true do 
    let ports = 
      Hashtbl.fold 
        (fun (dpid, port) (mac, _) r -> (dpid, port, mac)::r) 
        t.ports [] in

    lwt _ = Lwt_list.iter_p (
        fun (dpid, port, mac) -> send_port_lldp t dpid port mac) ports  in
    lwt _ = OS.Time.sleep 120.0 in 
      return ()
  done 

let print_graph t = 
   Graph.iter_edges_e (
      fun (_, (sdpid, sport, ddpid, dport, len), _) -> 
         printf "%06Lx:%d - %06Lx:%d = %d\n%!" sdpid sport ddpid dport len 
   ) t.topo 

let process_lldp_packet t src_dpid src_port pkt = 
  let tlvs = parse_lldp_tlvs pkt in 
  let (dst_dpid, dst_port, mac) = 
    List.fold_right (
      fun tlv (dpid, port, mac) -> 
        match tlv with
          | Tlv_chassis_id_mac (mac) ->
              (dpid, port, mac)
          | Tlv_port_id_port_comp(bits) -> 
              let port_id = ref 0 in 
              let _ = String.iter (
                fun c -> 
                  port_id := (!port_id lsl 8) + (int_of_char c)
              ) bits in
                (dpid, !port_id, mac)
          | Tlv(LLDP_TYPE_SYSTEM_DESCR, bits) -> 
              let dpid = ref 0L in 
              let _ = String.iter (
                fun c -> 
                  dpid := Int64.add (Int64.shift_left !dpid 8)
                    (Int64.of_int (int_of_char c))
              ) bits in
                (!dpid, port, mac)
          | _ -> (dpid, port, mac)
    ) tlvs (0L, 0, Macaddr.broadcast ) in
    match (Hashtbl.mem t.channels dst_dpid) with
    | false -> false
    | true -> 
        let v = (src_dpid, (src_dpid, src_port, dst_dpid, dst_port, 1), dst_dpid) in
        let _ = printf "[flowvisor-topo] adding link  %Ld:%d-%Ld:%d\n%!" 
                  src_dpid src_port dst_dpid dst_port in 
        let _ = Graph.add_edge_e t.topo v in
        let _ = mark_port_down t src_dpid src_port true in 
        let _ = mark_port_down t dst_dpid dst_port true in 
          true
 
let remove_dpid t dpid =
  let _ = Graph.remove_vertex t.topo dpid in 
  let _ = 
    Hashtbl.iter (
      fun (dp, p) _ ->
        if (dpid = dp) then 
          Hashtbl.remove t.ports (dp, p)) t.ports in
  let _ = Hashtbl.remove t.channels dpid in 
    ()

let is_transit_port t dpid port = 
  try 
    let (_, down) = Hashtbl.find t.ports (dpid, port) in down
   with Not_found -> false


let find_dpid_path t src_dpid src_port dst_dpid dst_port =
(*  let _ = printf "[flowvisor-topo] looking for path %Ld:%s - %Ld:%s\n%!" 
          src_dpid (OP.Port.string_of_port src_port) 
          dst_dpid (OP.Port.string_of_port dst_port) in *)
   let (path, w) =  Dijkstra.shortest_path t.topo src_dpid dst_dpid in
   let (path, dpid, port) = List.fold_right (
      fun (sdp, (dp_1, port_1, dp_2, port_2, _), ddp) (p, curr_dp, curr_p) ->
(*        let _ = printf "[flowvisor-topo] found link %Ld:%d-%Ld:%d\n%!"
                dp_1 port_1 dp_2 port_2 in *)
         let (hop, curr_dp, curr_p) = 
            match (curr_dp) with 
               | dp when dp = dp_1 -> 
                     let hop = (curr_dp, OP.Port.Port(port_1), 
                                curr_p) in 
                        (hop, dp_2, port_2)
               | dp when dp = dp_2 -> 
                     let hop = (curr_dp,
                                 OP.Port.Port(port_2), curr_p) in
                        (hop, dp_1, port_1)
               | _ -> 
                     failwith (sp "Unknwk dpid %Ld" curr_dp) 
         in 
         ((hop :: p), curr_dp, OP.Port.Port(curr_p))
   ) path ([], dst_dpid, dst_port) in 
     (src_dpid, src_port, port) :: path
