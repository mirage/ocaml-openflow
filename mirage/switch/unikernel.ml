open V1_LWT
open Lwt
open Printf

open Openflow
open Ofswitch

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

let resolve t = Lwt.on_success t (fun _ -> ())

open Ofsocket

let contaddr= "127.0.0.1"
let contport = 6633

module Main (C: CONSOLE)(S: STACKV4)(N1: NETWORK)(N2: NETWORK) = struct


  let or_error c name fn t =
    fn t
    >>= function
    | `Error e -> fail (Failure ("error starting " ^ name))
    | `Ok t -> C.log_s c (green "%s connected..." name) >>
               return t

  module T = S.TCPV4
  module E = Ethif.Make(N1)
  module Sw = Ofswitch.Make(T)(N1)

  let start console s n1 n2 =

  let netl = [n1; n2] in
    (* mirage main module <= 1.2.0 doesn't accept list as the parameter.
       write input network devices as a list until mirage supprts list parameters *)
  let rec connect_ifs nl acc =
    match nl with
    | [] -> return acc
    | n::t -> or_error console "ethernet interface" E.connect n >>= fun x -> (connect_ifs t (x::acc))
        (* to do: ethernet interface -> name of the interface: tap1, tap2, ... *)
  in
    C.log_s console (sprintf "IP address: %s\n" (Ipaddr.V4.to_string (S.IPV4.get_ipv4 (S.ipv4 s))))
    >>
    connect_ifs netl []
    >>= fun e ->
        Sw.create_switch (S.tcpv4 s) (* (switchaddr, netmask, gateway) *) (contaddr, contport) e
    >>=
     fun fl ->
        return ();
end
