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

open Cstruct 
open Printf
open Net
open Net.Nettypes 

exception Unparsable of Cstruct.t 

(*cenum lldp_tlv_types {
  LLDP_TYPE_END           = 0; 
  LLDP_TYPE_CHASSIS_ID    = 1;
  LLDP_TYPE_PORT_ID       = 2;
  LLDP_TYPE_TTL           = 3;
  LLDP_TYPE_PORT_DESCR    = 4;
  LLDP_TYPE_SYSTEM_NAME   = 5;
  LLDP_TYPE_SYSTEM_DESCR  = 6;
  LLDP_TYPE_SYSTEM_CAP    = 7;
  LLDP_TYPE_MGMT_ADDR     = 8
} as uint8_t

cenum lldp_chassis_id_subtype {
  LLDP_CHASSIS_CHASSIS_COMP_SUBTYPE = 1; 
  LLDP_CHASSIS_INTF_ALIAS_SUBTYPE   = 2;
  LLDP_CHASSIS_PORT_COMP_SUBTYPE    = 3;
  LLDP_CHASSIS_MAC_ADDR_SUBTYPE     = 4;
  LLDP_CHASSIS_NETWORK_ADDR_SUBTYPE = 5;
  LLDP_CHASSIS_INTF_NAME_SUBTYPE    = 6;
  LLDP_CHASSIS_LOCAL_SUBTYPE        = 8
}  as uint8_t

cenum lldp_port_id_subtype {
  LLDP_PORT_INTF_ALIAS_SUBTYPE    = 1;   
  LLDP_PORT_PORT_COMP_SUBTYPE     = 2; 
  LLDP_PORT_MAC_ADDR_SUBTYPE      = 3; 
  LLDP_PORT_NETWORK_ADDR_SUBTYPE  = 4; 
  LLDP_PORT_INTF_NAME_SUBTYPE     = 5; 
  LLDP_PORT_AGENT_CIRC_ID_SUBTYPE = 6; 
  LLDP_PORT_LOCAL_SUBTYPE         = 7
} as uint8_t*)

type lldp_tlv_types =
  LLDP_TYPE_END
  | LLDP_TYPE_CHASSIS_ID
  | LLDP_TYPE_PORT_ID
  | LLDP_TYPE_TTL
  | LLDP_TYPE_PORT_DESCR
  | LLDP_TYPE_SYSTEM_NAME
  | LLDP_TYPE_SYSTEM_DESCR
  | LLDP_TYPE_SYSTEM_CAP
  | LLDP_TYPE_MGMT_ADDR

let lldp_tlv_types_of_int =
  function
  | 0 -> Some LLDP_TYPE_END
  | 1 -> Some LLDP_TYPE_CHASSIS_ID
  | 2 -> Some LLDP_TYPE_PORT_ID
  | 3 -> Some LLDP_TYPE_TTL
  | 4 -> Some LLDP_TYPE_PORT_DESCR
  | 5 -> Some LLDP_TYPE_SYSTEM_NAME
  | 6 -> Some LLDP_TYPE_SYSTEM_DESCR
  | 7 -> Some LLDP_TYPE_SYSTEM_CAP
  | 8 -> Some LLDP_TYPE_MGMT_ADDR
  | _ -> None
  
let lldp_tlv_types_to_int =
  function
  | LLDP_TYPE_END -> 0
  | LLDP_TYPE_CHASSIS_ID -> 1
  | LLDP_TYPE_PORT_ID -> 2
  | LLDP_TYPE_TTL -> 3
  | LLDP_TYPE_PORT_DESCR -> 4
  | LLDP_TYPE_SYSTEM_NAME -> 5
  | LLDP_TYPE_SYSTEM_DESCR -> 6
  | LLDP_TYPE_SYSTEM_CAP -> 7
  | LLDP_TYPE_MGMT_ADDR -> 8
  
let lldp_tlv_types_to_string =
  function
  | LLDP_TYPE_END -> "LLDP_TYPE_END"
  | LLDP_TYPE_CHASSIS_ID -> "LLDP_TYPE_CHASSIS_ID"
  | LLDP_TYPE_PORT_ID -> "LLDP_TYPE_PORT_ID"
  | LLDP_TYPE_TTL -> "LLDP_TYPE_TTL"
  | LLDP_TYPE_PORT_DESCR -> "LLDP_TYPE_PORT_DESCR"
  | LLDP_TYPE_SYSTEM_NAME -> "LLDP_TYPE_SYSTEM_NAME"
  | LLDP_TYPE_SYSTEM_DESCR -> "LLDP_TYPE_SYSTEM_DESCR"
  | LLDP_TYPE_SYSTEM_CAP -> "LLDP_TYPE_SYSTEM_CAP"
  | LLDP_TYPE_MGMT_ADDR -> "LLDP_TYPE_MGMT_ADDR"
  
type lldp_chassis_id_subtype =
  LLDP_CHASSIS_CHASSIS_COMP_SUBTYPE
  | LLDP_CHASSIS_INTF_ALIAS_SUBTYPE
  | LLDP_CHASSIS_PORT_COMP_SUBTYPE
  | LLDP_CHASSIS_MAC_ADDR_SUBTYPE
  | LLDP_CHASSIS_NETWORK_ADDR_SUBTYPE
  | LLDP_CHASSIS_INTF_NAME_SUBTYPE
  | LLDP_CHASSIS_LOCAL_SUBTYPE

let lldp_chassis_id_subtype_of_int =
  function
  | 1 -> Some LLDP_CHASSIS_CHASSIS_COMP_SUBTYPE
  | 2 -> Some LLDP_CHASSIS_INTF_ALIAS_SUBTYPE
  | 3 -> Some LLDP_CHASSIS_PORT_COMP_SUBTYPE
  | 4 -> Some LLDP_CHASSIS_MAC_ADDR_SUBTYPE
  | 5 -> Some LLDP_CHASSIS_NETWORK_ADDR_SUBTYPE
  | 6 -> Some LLDP_CHASSIS_INTF_NAME_SUBTYPE
  | 8 -> Some LLDP_CHASSIS_LOCAL_SUBTYPE
  | _ -> None
  
let lldp_chassis_id_subtype_to_int =
  function
  | LLDP_CHASSIS_CHASSIS_COMP_SUBTYPE -> 1
  | LLDP_CHASSIS_INTF_ALIAS_SUBTYPE -> 2
  | LLDP_CHASSIS_PORT_COMP_SUBTYPE -> 3
  | LLDP_CHASSIS_MAC_ADDR_SUBTYPE -> 4
  | LLDP_CHASSIS_NETWORK_ADDR_SUBTYPE -> 5
  | LLDP_CHASSIS_INTF_NAME_SUBTYPE -> 6
  | LLDP_CHASSIS_LOCAL_SUBTYPE -> 8
  
let lldp_chassis_id_subtype_to_string =
  function
  | LLDP_CHASSIS_CHASSIS_COMP_SUBTYPE -> "LLDP_CHASSIS_CHASSIS_COMP_SUBTYPE"
  | LLDP_CHASSIS_INTF_ALIAS_SUBTYPE -> "LLDP_CHASSIS_INTF_ALIAS_SUBTYPE"
  | LLDP_CHASSIS_PORT_COMP_SUBTYPE -> "LLDP_CHASSIS_PORT_COMP_SUBTYPE"
  | LLDP_CHASSIS_MAC_ADDR_SUBTYPE -> "LLDP_CHASSIS_MAC_ADDR_SUBTYPE"
  | LLDP_CHASSIS_NETWORK_ADDR_SUBTYPE -> "LLDP_CHASSIS_NETWORK_ADDR_SUBTYPE"
  | LLDP_CHASSIS_INTF_NAME_SUBTYPE -> "LLDP_CHASSIS_INTF_NAME_SUBTYPE"
  | LLDP_CHASSIS_LOCAL_SUBTYPE -> "LLDP_CHASSIS_LOCAL_SUBTYPE"
  
type lldp_port_id_subtype =
  LLDP_PORT_INTF_ALIAS_SUBTYPE
  | LLDP_PORT_PORT_COMP_SUBTYPE
  | LLDP_PORT_MAC_ADDR_SUBTYPE
  | LLDP_PORT_NETWORK_ADDR_SUBTYPE
  | LLDP_PORT_INTF_NAME_SUBTYPE
  | LLDP_PORT_AGENT_CIRC_ID_SUBTYPE
  | LLDP_PORT_LOCAL_SUBTYPE

let lldp_port_id_subtype_of_int =
  function
  | 1 -> Some LLDP_PORT_INTF_ALIAS_SUBTYPE
  | 2 -> Some LLDP_PORT_PORT_COMP_SUBTYPE
  | 3 -> Some LLDP_PORT_MAC_ADDR_SUBTYPE
  | 4 -> Some LLDP_PORT_NETWORK_ADDR_SUBTYPE
  | 5 -> Some LLDP_PORT_INTF_NAME_SUBTYPE
  | 6 -> Some LLDP_PORT_AGENT_CIRC_ID_SUBTYPE
  | 7 -> Some LLDP_PORT_LOCAL_SUBTYPE
  | _ -> None
  
let lldp_port_id_subtype_to_int =
  function
  | LLDP_PORT_INTF_ALIAS_SUBTYPE -> 1
  | LLDP_PORT_PORT_COMP_SUBTYPE -> 2
  | LLDP_PORT_MAC_ADDR_SUBTYPE -> 3
  | LLDP_PORT_NETWORK_ADDR_SUBTYPE -> 4
  | LLDP_PORT_INTF_NAME_SUBTYPE -> 5
  | LLDP_PORT_AGENT_CIRC_ID_SUBTYPE -> 6
  | LLDP_PORT_LOCAL_SUBTYPE -> 7
  
let lldp_port_id_subtype_to_string =
  function
  | LLDP_PORT_INTF_ALIAS_SUBTYPE -> "LLDP_PORT_INTF_ALIAS_SUBTYPE"
  | LLDP_PORT_PORT_COMP_SUBTYPE -> "LLDP_PORT_PORT_COMP_SUBTYPE"
  | LLDP_PORT_MAC_ADDR_SUBTYPE -> "LLDP_PORT_MAC_ADDR_SUBTYPE"
  | LLDP_PORT_NETWORK_ADDR_SUBTYPE -> "LLDP_PORT_NETWORK_ADDR_SUBTYPE"
  | LLDP_PORT_INTF_NAME_SUBTYPE -> "LLDP_PORT_INTF_NAME_SUBTYPE"
  | LLDP_PORT_AGENT_CIRC_ID_SUBTYPE -> "LLDP_PORT_AGENT_CIRC_ID_SUBTYPE"
  | LLDP_PORT_LOCAL_SUBTYPE -> "LLDP_PORT_LOCAL_SUBTYPE"
 

type lldp_tvl =
  | Tlv_chassis_id_chassis_comp of string
  | Tlv_chassis_id_intf_alias of string
  | Tlv_chassis_id_port_comp of string
  | Tlv_chassis_id_mac of ethernet_mac
  | Tlv_chassis_id_net of ipv4_addr 
  | Tlv_chassis_id_intf_name of string
  | Tlv_chassis_id_local of string
  | Tlv_port_id_intf_alias of string
  | Tlv_port_id_port_comp of string
  | Tlv_port_id_mac of ethernet_mac
  | Tlv_port_id_net of ipv4_addr
  | Tlv_port_id_intf_name of string
  | Tlv_port_id_circ_id of string
  | Tlv_port_id_local of string
  | Tlv_ttl of int
  | Tlv_end 
  | Tlv of lldp_tlv_types * string
  | Tlv_unk of int * string

let parse_lldp_tlv bits =
  let tlv_type_len = Cstruct.BE.get_uint16 bits 0 in 
  let tlv_type = tlv_type_len lsr 9 in 
  let tlv_len = tlv_type_len land 0x01FF in
  let tlv = 
    match (lldp_tlv_types_of_int tlv_type) with
      | Some(LLDP_TYPE_END) -> Tlv_end 
      | Some(LLDP_TYPE_CHASSIS_ID) -> begin
          let data = Cstruct.to_string (Cstruct.sub bits 3 (tlv_len - 1)) in 
          let chassis_id_subtype = Cstruct.get_uint8 bits 2 in 
            match (lldp_chassis_id_subtype_of_int chassis_id_subtype) with
              | Some(LLDP_CHASSIS_CHASSIS_COMP_SUBTYPE)-> 
                  Tlv_chassis_id_chassis_comp(data)
              | Some(LLDP_CHASSIS_INTF_ALIAS_SUBTYPE)  -> 
                  Tlv_chassis_id_intf_alias(data)
              | Some(LLDP_CHASSIS_PORT_COMP_SUBTYPE)   -> 
                  Tlv_chassis_id_port_comp(data)
              | Some(LLDP_CHASSIS_MAC_ADDR_SUBTYPE)    -> 
                  Tlv_chassis_id_mac(Net.Nettypes.ethernet_mac_of_bytes data)
              | Some(LLDP_CHASSIS_NETWORK_ADDR_SUBTYPE)->
                  let ip = ipv4_addr_of_uint32 
                             (Cstruct.BE.get_uint32 bits 3) in 
                    Tlv_chassis_id_net(ip)
              | Some(LLDP_CHASSIS_INTF_NAME_SUBTYPE)   -> 
                  Tlv_chassis_id_intf_name(data) 
              | Some(LLDP_CHASSIS_LOCAL_SUBTYPE)       -> 
                  Tlv_chassis_id_local(data)
              | None -> 
                  raise (Unparsable(bits))
        end 
      | Some(LLDP_TYPE_PORT_ID) -> begin
           let data = Cstruct.to_string (Cstruct.sub bits 3 (tlv_len - 1)) in 
           let port_id_subtype = Cstruct.get_uint8 bits 2 in 
             match (lldp_port_id_subtype_of_int port_id_subtype) with
               | Some(LLDP_PORT_INTF_ALIAS_SUBTYPE)   ->  
                   Tlv_port_id_intf_alias(data)
               | Some(LLDP_PORT_PORT_COMP_SUBTYPE)    ->  
                   Tlv_port_id_port_comp(data)
               | Some(LLDP_PORT_MAC_ADDR_SUBTYPE)     ->  
                   Tlv_port_id_mac(Net.Nettypes.ethernet_mac_of_bytes data)
               | Some(LLDP_PORT_NETWORK_ADDR_SUBTYPE) ->  
                  let ip = ipv4_addr_of_uint32 
                             (Cstruct.BE.get_uint32 bits 3) in 
                   Tlv_port_id_net(ip)
               | Some(LLDP_PORT_INTF_NAME_SUBTYPE)    ->  
                   Tlv_port_id_intf_name(data)
               | Some(LLDP_PORT_AGENT_CIRC_ID_SUBTYPE)->  
                   Tlv_port_id_circ_id(data)
               | Some(LLDP_PORT_LOCAL_SUBTYPE)        ->  
                   Tlv_port_id_local(data)
               | None -> raise (Unparsable(bits))
        end 
      | Some(LLDP_TYPE_TTL) -> 
          let ttl = Cstruct.BE.get_uint16 bits 3 in 
            Tlv_ttl(ttl)
      | Some(typ) -> 
          let data = Cstruct.to_string (Cstruct.sub bits 3 (tlv_len - 1)) in 
            Tlv(typ, data) 
      | None -> 
          let data = Cstruct.to_string (Cstruct.sub bits 2 tlv_len) in 
            Tlv_unk(tlv_type, data)
  in
    (tlv_len + 2, tlv)

let parse_lldp_tlvs bits =
  (* Ignore ethernet headers for now *)
  let bits = Cstruct.shift bits Ethif.sizeof_ethernet in
  let rec parse_lldp_tlvs_inner bits = 
    match (Cstruct.len bits) with
      | 0 -> []
      | _ -> 
          let (len, tlv) = parse_lldp_tlv bits in 
            if(tlv = Tlv_end) then 
              [tlv]
            else 
              let bits = Cstruct.shift bits len in 
                [tlv] @ (parse_lldp_tlvs_inner bits)
  in 
    parse_lldp_tlvs_inner bits

let set_lldp_tlv_typ_subtyp_data bits typ subtyp data = 
  let typ = typ lsl 9 in 
  let len = ((String.length data) + 1) land 0x1ff in 
  let typ_len = typ + len in 
  let _ = Cstruct.BE.set_uint16 bits 0 typ_len in 
  let _ = Cstruct.set_uint8 bits 2 subtyp in 
  let _ = Cstruct.blit_from_string data 0 bits 3 (String.length data) in
    len + 2

let set_lldp_tlv_typ_data bits typ data = 
  let typ = typ lsl 9 in 
  let len = (String.length data) land 0x1ff in 
  let typ_len = typ + len in 
  let _ = Cstruct.BE.set_uint16 bits 0 typ_len in 
  let _ = Cstruct.blit_from_string data 0 bits 2 (String.length data) in
    len + 2

let marsal_lldp_tlv tlv bits =
  match tlv with
    (* chassis id *)
    | Tlv_chassis_id_chassis_comp(data) -> set_lldp_tlv_typ_subtyp_data bits 1 1 data 
    | Tlv_chassis_id_intf_alias(data) -> set_lldp_tlv_typ_subtyp_data bits 1 2 data 
    | Tlv_chassis_id_port_comp(data) -> set_lldp_tlv_typ_subtyp_data bits 1 3 data
    | Tlv_chassis_id_mac(mac) -> set_lldp_tlv_typ_subtyp_data bits 1 4 
                                  (Net.Nettypes.ethernet_mac_to_bytes mac)
    | Tlv_chassis_id_net(ip) -> 
        let _ = Cstruct.BE.set_uint16 bits 0 0x205 in 
        let _ = Cstruct.set_uint8 bits 2 5 in 
        let _ = Cstruct.BE.set_uint32 bits 3 (Net.Nettypes.ipv4_addr_to_uint32
        ip) in
          7
    | Tlv_chassis_id_intf_name(data) ->  set_lldp_tlv_typ_subtyp_data bits 1 6 data
    | Tlv_chassis_id_local(data) -> set_lldp_tlv_typ_subtyp_data bits 1 8 data
    (* Port id *)
    | Tlv_port_id_intf_alias(data) ->  set_lldp_tlv_typ_subtyp_data bits 2 1 data
    | Tlv_port_id_port_comp(data) ->  set_lldp_tlv_typ_subtyp_data bits 2 2 data
    | Tlv_port_id_mac(mac) ->  set_lldp_tlv_typ_subtyp_data bits 2 3 
                                (Net.Nettypes.ethernet_mac_to_bytes mac)
    | Tlv_port_id_net(ip) -> 
        let _ = Cstruct.BE.set_uint16 bits 0 0x405 in 
        let _ = Cstruct.set_uint8 bits 2 4 in 
        let _ = Cstruct.BE.set_uint32 bits 3 (Net.Nettypes.ipv4_addr_to_uint32
        ip) in
          7
    | Tlv_port_id_intf_name(data) -> set_lldp_tlv_typ_subtyp_data bits 2 5 data 
    | Tlv_port_id_circ_id(data) -> set_lldp_tlv_typ_subtyp_data bits 2 6 data
    | Tlv_port_id_local(data) -> set_lldp_tlv_typ_subtyp_data bits 2 7 data 
    | Tlv_ttl(ttl) -> 
        let _ = Cstruct.BE.set_uint16 bits 0 0x602 in 
        let _ = Cstruct.BE.set_uint16 bits 2 ttl in
          4
    | Tlv_end -> 
        let _ = Cstruct.BE.set_uint16 bits 0 0x000 in 
          2
    | Tlv(typ, data) -> 
        set_lldp_tlv_typ_data bits (lldp_tlv_types_to_int typ) data
    | Tlv_unk (typ, data)  -> set_lldp_tlv_typ_data bits typ data

let marsal_lldp_tlvs mac tlvs bits = 
  let _ = Net.Ethif.set_ethernet_dst "\x01\x80\xc2\x00\x00\x0e" 0 bits in 
  let _ = Net.Ethif.set_ethernet_src (Net.Nettypes.ethernet_mac_to_bytes mac) 
            0 bits in 
  let _ = Net.Ethif.set_ethernet_ethertype bits 0x88cc in 
  let bits = Cstruct.shift bits Ethif.sizeof_ethernet in 
  let rec marsal_lldp_tlvs_inner tlvs bits = 
    match tlvs with 
      | [] -> 0
      | h::t -> 
          let len = marsal_lldp_tlv h bits in 
          let bits = Cstruct.shift bits len in 
          let rest = marsal_lldp_tlvs_inner t bits in 
            len + rest
  in 
    Ethif.sizeof_ethernet + marsal_lldp_tlvs_inner tlvs bits


