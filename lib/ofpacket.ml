(*
 * Copyright (c) 2011 Richard Mortier <mort@cantab.net>
                      Charalampos Rotsos <cr409@cl.cam.ac.uk> 
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
open Printf
open Lwt
open Int32

let sp = Printf.sprintf
let pp = Printf.printf
let ep = Printf.eprintf
  
exception Unparsable of string * Cstruct.t 
exception Unparsed of string * Cstruct.t 
exception Unsupported of string 

let resolve t = Lwt.on_success t (fun _ -> ())
    
let (|>) x f = f x (* pipe *)
let (>>) f g x = g (f x) (* functor pipe *)
let (||>) l f = List.map f l (* element-wise pipe *)

let (+++) x y = Int32.add x y

let (&&&) x y = Int32.logand x y
let (|||) x y = Int32.logor x y
let (^^^) x y = Int32.logxor x y
let (<<<) x y = Int32.shift_left x y
let (>>>) x y = Int32.shift_right_logical x y

let join c l = String.concat c l
let stop (x, bits) = x (* drop remainder to stop parsing and demuxing *) 

type int16 = int

(* XXX of dubious merit - but we don't do arithmetic so prefer the
   documentation benefits for now *)
type uint8  = char
type uint16 = int
type uint32 = int32
type uint64 = int64

let uint8_of_int i = Char.chr i

(* XXX network specific types that should have a proper home *)

type ipv4 = uint32
let ipv4_to_string i =   
  sp "%ld.%ld.%ld.%ld" 
    ((i &&& 0x0_ff000000_l) >>> 24) ((i &&& 0x0_00ff0000_l) >>> 16)
    ((i &&& 0x0_0000ff00_l) >>>  8) ((i &&& 0x0_000000ff_l)       )

type byte = uint8
let byte (i:int) : byte = Char.chr i
let int_of_byte b = int_of_char b
let int32_of_byte b = b |> int_of_char |> Int32.of_int
let int32_of_int (i:int) = Int32.of_int i

type bytes = string
type eaddr = bytes
let bytes_to_hex_string bs = 
  bs |> Array.map (fun b -> sp "%02x." (int_of_byte b))

let eaddr_to_string s = 
  let l = String.length s in
  let hp s i = sp "%02x" (int_of_char s.[i]) in
  String.concat ":" (Array.init l (fun i -> hp s i) |> Array.to_list)

external ipv4_addr_of_uint32: int32 -> Net.Nettypes.ipv4_addr = "%identity"
external ipv4_addr_to_uint32: Net.Nettypes.ipv4_addr -> int32 = "%identity"

let eaddr_is_broadcast s =
  match s with
    | "\xFF\xFF\xFF\xFF\xFF\xFF" -> true
    | _ -> false

let ipv4_addr_of_bytes bs = 
  ((bs.[0] |> int32_of_byte <<< 24) ||| (bs.[1] |> int32_of_byte <<< 16) 
    ||| (bs.[2] |> int32_of_byte <<< 8) ||| (bs.[3] |> int32_of_byte))

(*********************************************************************** *)

(* for readability *)
type vendor = uint32
type queue_id = uint32
type datapath_id = uint64

let contain_exc l v = 
  try
    Some (v ())
  with exn ->
    eprintf "ofpacket %s exn: %s\n%!" l (Printexc.to_string exn); 
    None 

(* 
 * bit manipulation functions for 32-bit integers
 * *)
let int_of_bool = function
  | true -> 1
  | false -> 0
let get_int32_bit f off = (Int32.logand f (shift_left 1l off)) > 0l
let set_int32_bit f off v = 
  logor f (shift_left (Int32.of_int(int_of_bool v)) off)

let get_int32_byte f off = 
  let ret = shift_left (f logand (shift_left 13l off)) off in 
    char_of_int (0x00ff land (Int32.to_int ret))
let set_int32_byte f off v = 
  let value = Int32.of_int ((int_of_char v) lsl off) in
    logor f value

let get_int32_nw_mask f off = 
  let ret = shift_right (logand f (shift_left 0x3fl off)) off in 
    char_of_int (0x003f land (Int32.to_int ret))  
let set_int32_nw_mask f off v = 
  let value = Int32.of_int ((0x3f land v) lsl off) in
    logor f value

let get_int_bit f off = (f land (1 lsl off)) > 0
let set_int_bit f off v = f lor ((int_of_bool v) lsl off)

let marshal_and_sub fn bits =
  let len = fn bits in 
    Cstruct.sub bits 0 len

let marshal_and_shift fn bits =
  let len = fn bits in 
    (len, (Cstruct.shift bits len))

module Header = struct

  cstruct ofp_header {
    uint8_t version;    
    uint8_t typ;   
    uint16_t length;    
    uint32_t xid
  } as big_endian

  (*cenum msg_code {
    HELLO                 =  0;
    ERROR                 =  1;
    ECHO_REQ              =  2;
    ECHO_RESP             =  3;
    VENDOR                =  4;
    FEATURES_REQ          =  5;
    FEATURES_RESP         =  6;
    GET_CONFIG_REQ        =  7;
    GET_CONFIG_RESP       =  8;
    SET_CONFIG            =  9;
    PACKET_IN             = 10;
    FLOW_REMOVED          = 11;
    PORT_STATUS           = 12;
    PACKET_OUT            = 13;
    FLOW_MOD              = 14;
    PORT_MOD              = 15;
    STATS_REQ             = 16;
    STATS_RESP            = 17;
    BARRIER_REQ           = 18;
    BARRIER_RESP          = 19;
    QUEUE_GET_CONFIG_REQ  = 20;
    QUEUE_GET_CONFIG_RESP = 21
  } as uint8_t *)
    type msg_code =
      HELLO
      | ERROR
      | ECHO_REQ
      | ECHO_RESP
      | VENDOR
      | FEATURES_REQ
      | FEATURES_RESP
      | GET_CONFIG_REQ
      | GET_CONFIG_RESP
      | SET_CONFIG
      | PACKET_IN
      | FLOW_REMOVED
      | PORT_STATUS
      | PACKET_OUT
      | FLOW_MOD
      | PORT_MOD
      | STATS_REQ
      | STATS_RESP
      | BARRIER_REQ
      | BARRIER_RESP
      | QUEUE_GET_CONFIG_REQ
      | QUEUE_GET_CONFIG_RESP
    
    let msg_code_of_int =
      function
      | 0 -> Some HELLO
      | 1 -> Some ERROR
      | 2 -> Some ECHO_REQ
      | 3 -> Some ECHO_RESP
      | 4 -> Some VENDOR
      | 5 -> Some FEATURES_REQ
      | 6 -> Some FEATURES_RESP
      | 7 -> Some GET_CONFIG_REQ
      | 8 -> Some GET_CONFIG_RESP
      | 9 -> Some SET_CONFIG
      | 10 -> Some PACKET_IN
      | 11 -> Some FLOW_REMOVED
      | 12 -> Some PORT_STATUS
      | 13 -> Some PACKET_OUT
      | 14 -> Some FLOW_MOD
      | 15 -> Some PORT_MOD
      | 16 -> Some STATS_REQ
      | 17 -> Some STATS_RESP
      | 18 -> Some BARRIER_REQ
      | 19 -> Some BARRIER_RESP
      | 20 -> Some QUEUE_GET_CONFIG_REQ
      | 21 -> Some QUEUE_GET_CONFIG_RESP
      | _ -> None
      
    let msg_code_to_int =
      function
      | HELLO -> 0
      | ERROR -> 1
      | ECHO_REQ -> 2
      | ECHO_RESP -> 3
      | VENDOR -> 4
      | FEATURES_REQ -> 5
      | FEATURES_RESP -> 6
      | GET_CONFIG_REQ -> 7
      | GET_CONFIG_RESP -> 8
      | SET_CONFIG -> 9
      | PACKET_IN -> 10
      | FLOW_REMOVED -> 11
      | PORT_STATUS -> 12
      | PACKET_OUT -> 13
      | FLOW_MOD -> 14
      | PORT_MOD -> 15
      | STATS_REQ -> 16
      | STATS_RESP -> 17
      | BARRIER_REQ -> 18
      | BARRIER_RESP -> 19
      | QUEUE_GET_CONFIG_REQ -> 20
      | QUEUE_GET_CONFIG_RESP -> 21
      
    let msg_code_to_string =
      function
      | HELLO -> "HELLO"
      | ERROR -> "ERROR"
      | ECHO_REQ -> "ECHO_REQ"
      | ECHO_RESP -> "ECHO_RESP"
      | VENDOR -> "VENDOR"
      | FEATURES_REQ -> "FEATURES_REQ"
      | FEATURES_RESP -> "FEATURES_RESP"
      | GET_CONFIG_REQ -> "GET_CONFIG_REQ"
      | GET_CONFIG_RESP -> "GET_CONFIG_RESP"
      | SET_CONFIG -> "SET_CONFIG"
      | PACKET_IN -> "PACKET_IN"
      | FLOW_REMOVED -> "FLOW_REMOVED"
      | PORT_STATUS -> "PORT_STATUS"
      | PACKET_OUT -> "PACKET_OUT"
      | FLOW_MOD -> "FLOW_MOD"
      | PORT_MOD -> "PORT_MOD"
      | STATS_REQ -> "STATS_REQ"
      | STATS_RESP -> "STATS_RESP"
      | BARRIER_REQ -> "BARRIER_REQ"
      | BARRIER_RESP -> "BARRIER_RESP"
      | QUEUE_GET_CONFIG_REQ -> "QUEUE_GET_CONFIG_REQ"
      | QUEUE_GET_CONFIG_RESP -> "QUEUE_GET_CONFIG_RESP"
 
  type h = {
    ver: uint8;
    ty: msg_code;
    len: uint16;
    xid: uint32;
  }

  let get_len = sizeof_ofp_header 

  let parse_header bits = 
    match ((get_ofp_header_version bits), 
      (msg_code_of_int (get_ofp_header_typ bits))) with
      | (1, Some(ty))
        -> let ret = 
          { ver=(char_of_int (get_ofp_header_version bits)); 
             ty; 
             len=(get_ofp_header_length bits); 
             xid=(get_ofp_header_xid bits); } in 
        let _ = Cstruct.shift bits sizeof_ofp_header in 
          ret
    | (_, _) -> raise (Unparsable ("parse_h", bits))
  
  let header_to_string h =
    sp "ver:%d type:%s len:%d xid:0x%08lx" 
      (int_of_byte h.ver) (msg_code_to_string h.ty) h.len h.xid

  let create ?(xid=Random.int32 Int32.max_int) ty len  =
    { ver=byte 1; ty; len; xid }

  let marshal_header h bits = 
    let _ = set_ofp_header_version bits 1 in
    let _ = set_ofp_header_typ bits (msg_code_to_int h.ty) in
    let _ = set_ofp_header_length bits h.len in
    let _ = set_ofp_header_xid bits h.xid in
      sizeof_ofp_header
end

module Queue = struct
  type h = {
    queue_id: queue_id;
  }

  type t = NONE | MIN_RATE of uint16
end

module Port = struct
  type t = 
    | Max | In_port | Table | Normal | Flood | All 
    | Controller | Local | No_port
    | Port of int16
 
  let is_num value = 
    try let _ = int_of_string value in true with _ -> false

  let port_of_int = function
    | 0xff00 -> Max
    | 0xfff8 -> In_port
    | 0xfff9 -> Table
    | 0xfffa -> Normal
    | 0xfffb -> Flood
    | 0xfffc -> All
    | 0xfffd -> Controller
    | 0xfffe -> Local
    | 0xffff -> No_port
    | p      -> Port p
  and int_of_port = function
    | Max        -> 0xff00
    | In_port    -> 0xfff8
    | Table      -> 0xfff9
    | Normal     -> 0xfffa
    | Flood      -> 0xfffb
    | All        -> 0xfffc
    | Controller -> 0xfffd
    | Local      -> 0xfffe
    | No_port    -> 0xffff
    | Port p     -> p
  and string_of_port = function
    | Max        -> sp "MAX"
    | In_port    -> sp "IN_PORT"
    | Table      -> sp "TABLE"
    | Normal     -> sp "NORMAL"
    | Flood      -> sp "FLOOD"
    | All        -> sp "ALL"
    | Controller -> sp "CONTROLLER"
    | Local      -> sp "LOCAL"
    | No_port    -> sp "NO_PORT"
    | Port p     -> sp "PORT(%d)" p
  and port_of_string = function
    | "MAX"                 -> Some(Max)
    | "IN_PORT"             -> Some(In_port)
    | "TABLE"               -> Some(Table)
    | "NORMAL"              -> Some(Normal)
    | "FLOOD"               -> Some(Flood)
    | "ALL"                 -> Some(All)
    | "CONTROLLER"          -> Some(Controller)
    | "LOCAL"               -> Some(Local)   
    | "NO_PORT"             -> Some(No_port) 
    | num when (is_num num) -> Some(Port(int_of_string num))
    | _                     -> None
   
  type config = {
    port_down: bool;
    no_stp: bool;
    no_recv: bool;
    no_recv_stp: bool;
    no_flood: bool;
    no_fwd: bool;
    no_packet_in: bool;
  }

  let parse_config bits = 
    { port_down=(get_int32_bit bits 0); 
    no_stp=(get_int32_bit bits 1); 
    no_recv=(get_int32_bit bits 2); 
    no_recv_stp=(get_int32_bit bits 3); 
    no_flood=(get_int32_bit bits 4);
    no_fwd=(get_int32_bit bits 5); 
    no_packet_in=(get_int32_bit bits 6);}

  let marshal_config config = 
    let ret = 0l in 
    let ret = set_int32_bit ret 0 config.port_down in
    let ret = set_int32_bit ret 1 config.no_stp in
    let ret = set_int32_bit ret 2 config.no_recv in
    let ret = set_int32_bit ret 3 config.no_recv_stp in 
    let ret = set_int32_bit ret 4 config.no_flood in
    let ret = set_int32_bit ret 5 config.no_fwd in 
    let ret = set_int32_bit ret 6 config.no_packet_in in 
      ret 

  let init_port_config = 
    {port_down=false; no_stp=false; no_recv=false; no_recv_stp=false;
    no_flood=false; no_fwd=false; no_packet_in=false; }

  type features = {
    pause_asym: bool;
    pause: bool;
    autoneg: bool;
    fiber: bool;
    copper: bool;
    f_10GB_FD: bool;
    f_1GB_FD: bool;
    f_1GB_HD: bool;
    f_100MB_FD: bool;
    f_100MB_HD: bool;
    f_10MB_FD: bool;
    f_10MB_HD: bool;
  }

  let parse_features bits = 
    { pause_asym=(get_int32_bit bits 11); 
    pause=(get_int32_bit bits 10); 
    autoneg=(get_int32_bit bits 9); 
    fiber=(get_int32_bit bits 8); 
    copper=(get_int32_bit bits 7);
    f_10GB_FD=(get_int32_bit bits 6); 
    f_1GB_FD=(get_int32_bit bits 5); 
    f_1GB_HD=(get_int32_bit bits 4); 
    f_100MB_FD=(get_int32_bit bits 3); 
    f_100MB_HD=(get_int32_bit bits 2);
    f_10MB_FD=(get_int32_bit bits 1); 
    f_10MB_HD=(get_int32_bit bits 0);}

  let init_port_features =
    {pause_asym=false; pause=false; autoneg=false;
    fiber=false; copper=false; f_10GB_FD=false; f_1GB_FD=false;
    f_1GB_HD=false; f_100MB_FD=false; f_100MB_HD=false; f_10MB_FD=false;
    f_10MB_HD=false; };

  type state = {
    link_down: bool;
    stp_listen: bool;
    stp_learn: bool;
    stp_forward: bool;
    stp_block: bool;
  }

  let get_link_down f = (logand f 1l) > 0l
  let get_stp_listen f = (logand f (shift_left 0l 8)) > 0l
  let get_stp_learn f = (logand f (shift_left 1l 8)) > 0l
  let get_stp_forward f = (logand f (shift_left 2l 8)) > 0l
  let get_stp_block f = (logand f (shift_left 3l 8)) > 0l

  (*TODO this parsing is incorrect. use get_int32_bit and I think
   * set_stp_forward is a byte *)
  let set_link_down f v = f logor (Int32.of_int (int_of_bool v))
  let set_stp_listen f v = 
    logor f (shift_left (Int32.of_int (int_of_bool v)) 8)
  let set_stp_learn f v = 
    logor f (shift_left (Int32.of_int (int_of_bool v)) 8)
  let set_stp_forward f v = 
    logor f (shift_left (Int32.of_int (int_of_bool v)) 8)
  let set_stp_block f v = 
    logor f (shift_left (Int32.of_int (int_of_bool v)) 8)

  let parse_state bits = 
    { link_down=(get_link_down bits); 
    stp_listen=(get_stp_listen bits); 
    stp_learn=(get_stp_learn bits); 
    stp_forward=(get_stp_forward bits);
    stp_block=(get_stp_block bits); }

  let init_port_state = 
    {link_down=false; stp_listen=false; stp_learn=false; stp_forward=false;
    stp_block=false; }

  type phy = {
    port_no: uint16;
    hw_addr: eaddr;
    name: string;
    config: config;  
    state: state;    
    curr: features;
    advertised: features;
    supported: features;
    peer: features;
  }

  cstruct ofp_phy_port {
    uint16_t port_no;
    uint8_t hw_addr[6];
    uint8_t name[16];
    uint32_t config;      
    uint32_t state;
    uint32_t curr; 
    uint32_t advertised;    
    uint32_t supported;     
    uint32_t peer
  } as big_endian

  let max_name_len = 16
  let phy_len = 48
  let parse_phy bits = 
    let port_no = (get_ofp_phy_port_port_no bits) in
    let hw_addr=(Cstruct.to_string (get_ofp_phy_port_hw_addr bits)) in 
    let name=(Cstruct.to_string  (get_ofp_phy_port_name bits)) in
    let config = (parse_config (get_ofp_phy_port_config bits)) in 
    let state = (parse_state (get_ofp_phy_port_state bits)) in
    let curr = (parse_features (get_ofp_phy_port_curr bits)) in 
    let advertised = (parse_features (get_ofp_phy_port_advertised bits)) in
    let supported = (parse_features (get_ofp_phy_port_supported bits)) in
    let peer = (parse_features (get_ofp_phy_port_peer bits)) in
    let _ = Cstruct.shift bits sizeof_ofp_phy_port in
      {port_no; hw_addr;  name; config; state; curr; advertised; 
       supported; peer; }

  let parse_phys bits = 
    let rec aux ports bits = 
      match (Cstruct.len bits) with
      | 0 -> ports
      | l when (l >= sizeof_ofp_phy_port ) -> 
          aux ((parse_phy bits) :: ports) 
          (Cstruct.shift bits sizeof_ofp_phy_port)
      | _ -> raise (Unparsable("parse_phys", bits))
    in 
      aux [] bits
  
  let init_port_phy ?(port_no = 0) ?(hw_addr="\x11\x11\x11\x11\x11\x11") 
                      ?(name="") () = 
    {port_no; hw_addr; name; config=init_port_config; 
     state=init_port_state; curr=init_port_features; 
    advertised=init_port_features; supported=init_port_features; 
    peer=init_port_features;}

   let marshal_phy phy bits =
     let _ = set_ofp_phy_port_port_no bits phy.port_no in
     let _ = set_ofp_phy_port_hw_addr phy.hw_addr 0 bits in
     let name = String.make 16 (char_of_int 0) in
     let _ = String.blit phy.name 0 name 0 (String.length phy.name) in 
     let _ = set_ofp_phy_port_name name 0 bits in
     let _ = set_ofp_phy_port_config bits 0l in
     let _ = set_ofp_phy_port_state bits 0l in
     let _ = set_ofp_phy_port_curr bits 0l in
     let _ = set_ofp_phy_port_advertised bits 0l in
     let _ = set_ofp_phy_port_supported bits 0l in
     let _ = set_ofp_phy_port_peer bits 0l in
       Cstruct.shift bits sizeof_ofp_phy_port

  let string_of_phy ph = 
    (sp "port_no:%d,hw_addr:%s,name:%s" 
       ph.port_no (eaddr_to_string ph.hw_addr) ph.name)

  type stats = {
    mutable port_id: uint16;
    mutable rx_packets: uint64;
    mutable tx_packets: uint64;
    mutable rx_bytes: uint64;
    mutable tx_bytes: uint64;
    mutable rx_dropped: uint64;
    mutable tx_dropped: uint64;
    mutable rx_errors: uint64;
    mutable tx_errors: uint64;
    mutable rx_frame_err: uint64;
    mutable rx_over_err: uint64;
    mutable rx_crc_err: uint64;
    mutable collisions: uint64;
  }

  cstruct ofp_port_stats {
    uint16_t port_no;
    uint8_t pad[6];         
    uint64_t rx_packets;     
    uint64_t tx_packets;     
    uint64_t rx_bytes;       
    uint64_t tx_bytes;       
    uint64_t rx_dropped;     
    uint64_t tx_dropped;     
    uint64_t rx_errors;      
    uint64_t tx_errors;      
    uint64_t rx_frame_err;   
    uint64_t rx_over_err;    
    uint64_t rx_crc_err;     
    uint64_t collisions
  } as big_endian 

  let rec parse_port_stats_reply bits = 
    match (Cstruct.len bits) with 
    | 0 -> []
    | _ ->
      let record = [{port_id=(get_ofp_port_stats_port_no bits); 
        rx_packets=(get_ofp_port_stats_rx_packets bits); 
        tx_packets=(get_ofp_port_stats_tx_packets bits); 
        rx_bytes=(get_ofp_port_stats_rx_bytes bits); 
        tx_bytes=(get_ofp_port_stats_tx_bytes bits); 
        rx_dropped=(get_ofp_port_stats_rx_dropped bits); 
        tx_dropped=(get_ofp_port_stats_tx_dropped bits); 
        rx_errors=(get_ofp_port_stats_rx_errors bits); 
        tx_errors=(get_ofp_port_stats_tx_errors bits); 
        rx_frame_err=(get_ofp_port_stats_rx_frame_err bits); 
        rx_over_err=(get_ofp_port_stats_rx_over_err bits); 
        rx_crc_err=(get_ofp_port_stats_rx_crc_err bits); 
        collisions=(get_ofp_port_stats_collisions bits);}] in 
      let _ = Cstruct.shift bits sizeof_ofp_port_stats in
        record @ (parse_port_stats_reply (bits) )
      
  let rec string_of_port_stats_reply ports = 
    match ports with 
      | [] -> ""
      | h::q -> (
        sp "port_no:%d,rx_packets:%Ld,tx_packets:%Ld,rx_bytes:%Ld,\
            tx_bytes:%Ld,rx_dropped:%Ld,tx_dropped:%Ld,rx_errors:%Ld,\
            tx_errors:%Ld,rx_frame_err:%Ld,rx_over_err:%Ld,rx_crc_err:%Ld,\
            collisions:%Ld\n%s" 
          h.port_id
          h.rx_packets h.tx_packets h.rx_bytes h.tx_bytes 
          h.rx_dropped h.tx_dropped h.rx_errors h.tx_errors 
          h.rx_frame_err h.rx_over_err h.rx_crc_err h.collisions
          (string_of_port_stats_reply q))

(*  cenum reason {
    ADD = 0;
    DEL = 1;
    MOD = 2
  } as uint8_t*)

    type reason = ADD | DEL | MOD
    
    let reason_of_int =
      function | 0 -> Some ADD | 1 -> Some DEL | 2 -> Some MOD | _ -> None
      
    let reason_to_int = function | ADD -> 0 | DEL -> 1 | MOD -> 2
      
    let reason_to_string =
      function | ADD -> "ADD" | DEL -> "DEL" | MOD -> "MOD"
      
 
(*  type reason = ADD | DEL | MOD
  let reason_of_int = function
    | 0 -> ADD
    | 1 -> DEL
    | 2 -> MOD
    | _ -> invalid_arg "reason_of_int"
  and int_of_reason = function
    | ADD -> 0
    | DEL -> 1
    | MOD -> 2
  and string_of_reason = function
    | ADD -> sp "ADD"
    | DEL -> sp "DEL"
    | MOD -> sp "MOD"*)

  type status = {
    reason: reason;
    desc: phy;
  }

  cstruct ofp_port_status {
    uint8_t reason;          
    uint8_t pad[7]          
  } as big_endian 

  let create_port_status reason desc =
    ((Header.(create PORT_STATUS (get_len + sizeof_ofp_port_status)) ), 
    {reason; desc;})

  let string_of_status st = 
    (sp "Port status,reason:%s,%s" (reason_to_string st.reason)
       (string_of_phy st.desc) )

  let parse_status bits =
    let reason = 
      match (reason_of_int (get_ofp_port_status_reason bits)) with
      | Some(reason) -> reason 
      | None -> raise(Unparsable("reason_of_int", bits))
    in
    let bits = Cstruct.shift bits sizeof_ofp_port_status in
      {reason; desc=(parse_phy bits)}

  let marshal_port_status ?(xid=0l) status bits =
    let len = Header.get_len + sizeof_ofp_port_status + phy_len in 
    let header = Header.create ~xid Header.PORT_STATUS len in
    let (_, bits) = marshal_and_shift (Header.marshal_header header) bits in
    let _ = set_ofp_port_status_reason bits (reason_to_int status.reason) in 
    let bits = Cstruct.shift bits sizeof_ofp_port_status in
    let _ = marshal_phy status.desc bits in 
      len
end

module Switch = struct
  type capabilities = {
    flow_stats: bool;
    table_stats: bool;
    port_stats: bool;    
    stp: bool;
    ip_reasm: bool;
    queue_stats: bool;    
    arp_match_ip: bool;
      }

  let parse_capabilities bits = 
    { arp_match_ip=(get_int32_bit bits 7); 
    queue_stats=(get_int32_bit bits 6); 
    ip_reasm=(get_int32_bit bits 5); 
    stp=(get_int32_bit bits 3); 
    port_stats=(get_int32_bit bits 2); 
    table_stats=(get_int32_bit bits 1); 
    flow_stats=(get_int32_bit bits 0);}                    

  let marshal_capabilities c = 
    let bits = 0l in
    let _ = set_int32_bit bits 7 c.arp_match_ip in
    let _ = set_int32_bit bits 6 c.queue_stats in
    let _ = set_int32_bit bits 5 c.ip_reasm in
    let _ = set_int32_bit bits 3 c.stp in 
    let _ = set_int32_bit bits 2 c.port_stats in
    let _ = set_int32_bit bits 1 c.table_stats in
    let _ = set_int32_bit bits 0 c.flow_stats in
      bits

  type actions = {
    output: bool;
    set_vlan_id: bool;
    set_vlan_pcp: bool;
    strip_vlan: bool;
    set_dl_src: bool;
    set_dl_dst: bool;
    set_nw_src: bool;
    set_nw_dst: bool;
    set_nw_tos: bool;
    set_tp_src: bool;
    set_tp_dst: bool;
    enqueue: bool;
    vendor: bool;
  }
  
  let parse_actions bits = 
    { output=(get_int32_bit bits 0); 
    set_vlan_id=(get_int32_bit bits 1); 
    set_vlan_pcp=(get_int32_bit bits 2); 
    strip_vlan=(get_int32_bit bits 3);
    set_dl_src=(get_int32_bit bits 4); 
    set_dl_dst=(get_int32_bit bits 5); 
    set_nw_src=(get_int32_bit bits 6); 
    set_nw_dst=(get_int32_bit bits 7);
    set_nw_tos=(get_int32_bit bits 8); 
    set_tp_src=(get_int32_bit bits 9); 
    set_tp_dst=(get_int32_bit bits 10); 
    enqueue=(get_int32_bit bits 11); 
    vendor=(get_int32_bit bits 12);} 
  let marshal_actions action = 
    let bits = 0l in
    let bits = set_int32_bit bits 0 action.output      in  
    let bits = set_int32_bit bits 1 action.set_vlan_id in  
    let bits = set_int32_bit bits 2 action.set_vlan_pcp in  
    let bits = set_int32_bit bits 3 action.strip_vlan  in 
    let bits = set_int32_bit bits 4 action.set_dl_src  in  
    let bits = set_int32_bit bits 5 action.set_dl_dst  in  
    let bits = set_int32_bit bits 6 action.set_nw_src  in  
    let bits = set_int32_bit bits 7 action.set_nw_dst  in 
    let bits = set_int32_bit bits 8 action.set_nw_tos  in  
    let bits = set_int32_bit bits 9 action.set_tp_src  in  
    let bits = set_int32_bit bits 10 action.set_tp_dst in  
    let bits = set_int32_bit bits 11 action.enqueue    in  
    let bits = set_int32_bit bits 12 action.vendor     in  
      bits

  type features = {
    datapath_id: datapath_id;
    n_buffers: uint32;
    n_tables: byte;    
    capabilities: capabilities;
    actions: actions;
    mutable ports: Port.phy list;
  }

  cstruct ofp_switch_features {
    uint64_t datapath_id; 
    uint32_t n_buffers; 
    uint8_t n_tables; 
    uint8_t pad[3]; 
    uint32_t capabilities; 
    uint32_t action
  } as big_endian 

  let rec marshal_phy_ports ports bits =
    match ports with
      | [] -> bits
      | head :: tail ->
        let bits = Port.marshal_phy head bits in
          marshal_phy_ports tail bits
  let get_len t = 
    Header.get_len + sizeof_ofp_switch_features + 
    ((List.length t.ports) * Port.phy_len)


  let marshal_reply_features xid feat bits =
    let ports_count = (List.length feat.ports) in    
    let len = Header.get_len + sizeof_ofp_switch_features +
    ports_count*Port.phy_len in 
    let header = Header.create ~xid Header.FEATURES_RESP len in
    let (_, bits) = marshal_and_shift (Header.marshal_header header) bits in
    let _ = set_ofp_switch_features_datapath_id bits feat.datapath_id in
    let _ = set_ofp_switch_features_n_buffers bits feat.n_buffers in 
    let _ = set_ofp_switch_features_n_tables bits 
              (int_of_char feat.n_tables) in
    let _ = set_ofp_switch_features_capabilities bits 
              (marshal_capabilities feat.capabilities) in
    let _ = set_ofp_switch_features_action bits 
              (marshal_actions feat.actions) in
    let bits = Cstruct.shift bits sizeof_ofp_switch_features in
    let _ = marshal_phy_ports feat.ports bits in 
      len


  let parse_features bits = 
    let datapath_id = get_ofp_switch_features_datapath_id bits in 
    let n_buffers = get_ofp_switch_features_n_buffers bits in
    let n_tables = char_of_int (get_ofp_switch_features_n_tables bits) in 
    let capabilities = parse_capabilities 
                        (get_ofp_switch_features_capabilities bits) in
    let actions = parse_actions (get_ofp_switch_features_action bits) in
    let bits = Cstruct.shift bits sizeof_ofp_switch_features in
    let ports = Port.parse_phys bits in 
      {datapath_id; n_buffers; n_tables; capabilities; actions; ports;}

  type config = {
    drop: bool;
    reasm: bool;
    miss_send_len: uint16;
  }

  let init_switch_config = 
        {drop=true; reasm=true;miss_send_len=1000;}
        
  cstruct ofp_switch_config {
    uint16_t flags;           
    uint16_t miss_send_len 
  } as big_endian 

  let config_get_len = Header.get_len + sizeof_ofp_switch_config

  let marshal_switch_config xid config bits =
    let header = (Header.create  ~xid Header.GET_CONFIG_RESP 
                    (Header.get_len + sizeof_ofp_switch_config)) in
    let (_, bits) = marshal_and_shift (Header.marshal_header header) bits in 
    let _ = set_ofp_switch_config_flags bits 0 in
    let _ = set_ofp_switch_config_miss_send_len bits config.miss_send_len in
      (Header.sizeof_ofp_header + sizeof_ofp_switch_config)

end

module Wildcards = struct
  type t = {
    mutable in_port: bool; 
    mutable dl_vlan: bool;
    mutable dl_src: bool; 
    mutable dl_dst: bool; 
    mutable dl_type: bool; 
    mutable nw_proto: bool; 
    mutable tp_src: bool; 
    mutable tp_dst: bool; 
    mutable nw_src: byte; (* XXX *)
    mutable nw_dst: byte; (* XXX *)
    mutable dl_vlan_pcp: bool;
    mutable nw_tos: bool;
  }
  let in_port_match () = 
    { in_port=false; dl_vlan=true; dl_src=true; 
      dl_dst=true; dl_type=true; nw_proto=true; 
      tp_src=true; tp_dst=true; nw_src=(char_of_int 32); 
      nw_dst=(char_of_int 32); dl_vlan_pcp=true; nw_tos=true;
    }
   let full_wildcard () = 
    { in_port=true; dl_vlan=true; dl_src=true; 
      dl_dst=true; dl_type=true; nw_proto=true; 
      tp_src=true; tp_dst=true; nw_src=(char_of_int 32); 
      nw_dst=(char_of_int 32); dl_vlan_pcp=true; nw_tos=true;
    }
  let exact_match () = 
    { in_port=false; dl_vlan=false; dl_src=false; 
      dl_dst=false; dl_type=false; nw_proto=false; 
      tp_src=false; tp_dst=false; nw_src=(char_of_int 0); 
      nw_dst=(char_of_int 0); dl_vlan_pcp=false; nw_tos=false;
    }
  let l2_match () = 
    { in_port=false;dl_vlan=false;dl_src=false;dl_dst=false;
      dl_type=false;nw_proto=true;tp_src=true;tp_dst=true;
      nw_src=(char_of_int 32);nw_dst=(char_of_int 32);dl_vlan_pcp=false; 
      nw_tos=true
    }
  let l3_match () = 
    { in_port=false;dl_vlan=false;dl_vlan_pcp=false;dl_src=false;
      dl_dst=false;dl_type=false;nw_proto=false;nw_tos=false;
      nw_src=(char_of_int 0);nw_dst=(char_of_int 0);tp_src=true;tp_dst=true;
    }
  let arp_match () = 
    { in_port=false;dl_vlan=false;dl_vlan_pcp=false;dl_src=false;
      dl_dst=false;dl_type=false;nw_proto=false;nw_tos=true;
      nw_src=(char_of_int 32);nw_dst=(char_of_int 32);tp_src=true;tp_dst=true;
    }

  let marshal_wildcard m = 
    let ret = 0l in 
    let ret = set_int32_bit ret 0 m.in_port in
    let ret = set_int32_bit ret 1 m.dl_vlan in 
    let ret = set_int32_bit ret 2 m.dl_src in
    let ret = set_int32_bit ret 3 m.dl_dst in
    let ret = set_int32_bit ret 4 m.dl_type in
    let ret = set_int32_bit ret 5 m.nw_proto in
    let ret = set_int32_bit ret 6 m.tp_src in
    let ret = set_int32_bit ret 7 m.tp_dst in
    let ret = set_int32_nw_mask ret 8 (int_of_char m.nw_src) in
    let ret = set_int32_nw_mask ret 14 (int_of_char m.nw_dst) in
    let ret = set_int32_bit ret 20 m.dl_vlan_pcp in 
    let ret = set_int32_bit ret 21 m.nw_tos in
      ret

  let wildcard_to_string h = 
    sp "in_port:%s,dl_vlan:%s,dl_src:%s,dl_dst:%s,dl_type:%s,\
        nw_proto:%s,tp_src:%s,tp_dst:%s,nw_src:%d,nw_dst:%d,\
        dl_vlan_pcp:%s,nw_tos:%s" 
      (string_of_bool h.in_port) 
      (string_of_bool h.dl_vlan) (string_of_bool h.dl_src)
      (string_of_bool h.dl_dst) (string_of_bool h.dl_type)
      (string_of_bool h.nw_proto) (string_of_bool h.tp_src)
      (string_of_bool h.tp_dst) (int_of_char h.nw_src) 
      (int_of_char h.nw_dst) (string_of_bool h.dl_vlan_pcp) 
      (string_of_bool h.nw_tos)
      
  let parse_wildcards bits = 
    {nw_tos=(get_int32_bit bits 21);
     dl_vlan_pcp=(get_int32_bit bits 20);
     nw_dst=(get_int32_nw_mask bits 14); 
     nw_src=(get_int32_nw_mask bits 8); 
     tp_dst=(get_int32_bit bits 7); 
     tp_src=(get_int32_bit bits 6); 
     nw_proto=(get_int32_bit bits 5); 
     dl_type=(get_int32_bit bits 4); 
     dl_dst=(get_int32_bit bits 3); 
     dl_src=(get_int32_bit bits 2); 
     dl_vlan=(get_int32_bit bits 1); 
     in_port=(get_int32_bit bits 0);}
end     

module Match = struct
  type t = {
    mutable wildcards: Wildcards.t;
    mutable in_port: Port.t;
    mutable dl_src: eaddr;
    mutable dl_dst: eaddr;
    mutable dl_vlan: uint16;
    mutable dl_vlan_pcp: byte;
    mutable dl_type: uint16;
    mutable nw_src: uint32;
    mutable nw_dst: uint32;
    mutable nw_tos: byte;
    mutable nw_proto: byte;
    mutable tp_src: uint16;
    mutable tp_dst: uint16;
  }

  let wildcard () = 
    {wildcards=(Wildcards.full_wildcard ()); in_port=Port.No_port; 
     dl_src="\000\000\000\000\000\000"; 
     dl_dst="\000\000\000\000\000\000";
     dl_vlan=0; dl_vlan_pcp='\000'; dl_type=0; nw_src=0l; nw_dst=0l;
      nw_tos='\000';nw_proto='\000';tp_src=0; tp_dst=0;}

  cstruct ofp_match {
    uint32_t wildcards;        
    uint16_t in_port;          
    uint8_t dl_src[6];
    uint8_t dl_dst[6];
    uint16_t dl_vlan;          
    uint8_t dl_vlan_pcp;       
    uint8_t pad1[1];           
    uint16_t dl_type;          
    uint8_t nw_tos;            
    uint8_t nw_proto;          
    uint8_t pad2[2];           
    uint32_t nw_src;           
    uint32_t nw_dst;           
    uint16_t tp_src;           
    uint16_t tp_dst
  } as big_endian


  let marshal_match m bits = 
    let _ = set_ofp_match_wildcards bits
    (Wildcards.marshal_wildcard m.wildcards) in
    let _ = set_ofp_match_in_port bits (Port.int_of_port m.in_port) in
    let _ = set_ofp_match_dl_src m.dl_src 0 bits in 
    let _ = set_ofp_match_dl_dst m.dl_dst 0 bits in 
    let _ = set_ofp_match_dl_vlan bits m.dl_vlan in
    let _ = set_ofp_match_dl_vlan_pcp bits (int_of_char m.dl_vlan_pcp) in
    let _ = set_ofp_match_dl_type bits m.dl_type in
    let _ = set_ofp_match_nw_tos bits  (int_of_char m.nw_tos) in
    let _ = set_ofp_match_nw_proto bits (int_of_char m.nw_proto) in
    let _ = set_ofp_match_nw_src bits m.nw_src in 
    let _ = set_ofp_match_nw_dst bits m.nw_dst in
    let _ = set_ofp_match_tp_src bits m.tp_src in
    let _ = set_ofp_match_tp_dst bits m.tp_dst in 
      sizeof_ofp_match 

  let parse_match bits = 
    let wildcards = Wildcards.parse_wildcards (get_ofp_match_wildcards bits) in
    let in_port = Port.port_of_int (get_ofp_match_in_port bits) in 
    let dl_src = Cstruct.to_string (get_ofp_match_dl_src bits)  in
    let dl_dst = Cstruct.to_string (get_ofp_match_dl_dst bits) in 
    let dl_vlan = get_ofp_match_dl_vlan bits in 
    let dl_vlan_pcp = char_of_int (get_ofp_match_dl_vlan_pcp bits) in
    let dl_type = get_ofp_match_dl_type bits in
    let nw_tos = char_of_int (get_ofp_match_nw_tos bits) in
    let nw_proto = char_of_int (get_ofp_match_nw_proto bits) in
    let nw_src = get_ofp_match_nw_src bits in 
    let nw_dst = get_ofp_match_nw_dst bits in
    let tp_src = get_ofp_match_tp_src bits in
    let tp_dst = get_ofp_match_tp_dst bits in 
    let _ = Cstruct.shift bits sizeof_ofp_match  in 
      {wildcards; in_port; dl_src; dl_dst; dl_vlan; dl_vlan_pcp; 
       dl_type; nw_tos; nw_proto; nw_src; nw_dst; tp_src; tp_dst;}

  (* Check if the flow object is include in flow_def match *)
  let null_eaddr = "\x00\x00\x00\x00\x00\x00"
  let create_flow_match wildcards
      ?(in_port = 0) ?(dl_src=null_eaddr) ?(dl_dst=null_eaddr) 
      ?(dl_vlan=0xffff) ?(dl_vlan_pcp=(char_of_int 0)) ?(dl_type=0) 
      ?(nw_tos=(char_of_int 0)) 
      ?(nw_proto=(char_of_int 0)) 
      ?(nw_src=(Int32.of_int 0)) ?(nw_dst=(Int32.of_int 0)) 
      ?(tp_src=0) ?(tp_dst=0) 
      () = 
    { wildcards; in_port=(Port.port_of_int in_port); 
      dl_src; dl_dst; dl_vlan; dl_vlan_pcp; dl_type; 
      nw_src; nw_dst; nw_tos; nw_proto; tp_src; tp_dst; 
    }

  let translate_port m p = 
    {wildcards=m.wildcards; in_port=p; dl_src=m.dl_src; dl_dst=m.dl_dst;
     dl_vlan=m.dl_vlan; dl_vlan_pcp=m.dl_vlan_pcp; dl_type=m.dl_type; 
     nw_tos=m.nw_tos; nw_proto=m.nw_proto; nw_src=m.nw_src; nw_dst=m.nw_dst;
     tp_src=m.tp_src; tp_dst=m.tp_dst;} 

  cstruct dl_header {
    uint8_t   dl_dst[6];
    uint8_t   dl_src[6]; 
    uint16_t  dl_type 
  } as big_endian

  cstruct arphdr {
    uint16_t ar_hrd;         
    uint16_t ar_pro;         
    uint8_t ar_hln;              
    uint8_t ar_pln;              
    uint16_t ar_op;          
    uint8_t ar_sha[6];  
    uint32_t nw_src;
    uint8_t ar_tha[6];  
    uint32_t nw_dst 
  } as big_endian

  cstruct nw_header {
    uint8_t        hlen_version;
    uint8_t        nw_tos;
    uint16_t       total_len;
    uint8_t        pad[5];
    uint8_t        nw_proto; 
    uint16_t       csum;
    uint32_t       nw_src; 
    uint32_t       nw_dst
  } as big_endian 

  cstruct tp_header {
    uint16_t tp_src;
    uint16_t tp_dst
  } as big_endian 

  cstruct icmphdr {
    uint8_t typ;
    uint8_t code;
    uint16_t checksum
  } as big_endian

  let raw_packet_to_match in_port bits =
    let dl_dst = Cstruct.to_string (get_dl_header_dl_dst bits) in 
    let dl_src = Cstruct.to_string (get_dl_header_dl_src bits) in
    let dl_type = get_dl_header_dl_type bits in
    let bits = Cstruct.shift bits sizeof_dl_header in 
    match (dl_type) with
    | 0x0800 -> begin
      let nw_src = get_nw_header_nw_src bits in 
      let nw_dst = get_nw_header_nw_dst bits in 
      let nw_proto = get_nw_header_nw_proto bits in 
      let nw_tos = char_of_int (get_nw_header_nw_tos bits) in 
      let len = (get_nw_header_hlen_version bits) land 0xf in 
      let bits = Cstruct.shift bits (len*4) in
        match (nw_proto) with
        | 17 
        | 6 ->
          {wildcards=(Wildcards.exact_match ()); 
          in_port; dl_src; dl_dst; dl_vlan=0xffff;
          dl_vlan_pcp=(char_of_int 0);dl_type; nw_src; 
          nw_dst; nw_tos; 
          nw_proto=(char_of_int nw_proto); tp_src=(get_tp_header_tp_src bits);
          tp_dst=(get_tp_header_tp_dst bits);}
        | 1 ->
          { wildcards =(Wildcards.exact_match ()); 
          in_port;dl_src; dl_dst; dl_vlan=0xffff;
          dl_vlan_pcp=(char_of_int 0);dl_type; 
          nw_src; nw_dst; nw_tos; 
          nw_proto=(char_of_int nw_proto); tp_src=(get_icmphdr_typ bits); 
          tp_dst=(get_icmphdr_code bits); }        
        | _ ->
          { wildcards =(Wildcards.l3_match ()); 
          in_port;dl_src; dl_dst; dl_vlan=0xffff;
          dl_vlan_pcp=(char_of_int 0);dl_type; 
          nw_src; nw_dst; nw_tos; 
          nw_proto=(char_of_int nw_proto); tp_src=0; tp_dst=0; }
      end
    | 0x0806 ->
        {wildcards=(Wildcards.arp_match ()); 
        in_port; dl_src; dl_dst; dl_type;
        dl_vlan=0xffff; dl_vlan_pcp=(char_of_int 0);
        nw_src=(get_arphdr_nw_src bits); 
        nw_dst=(get_arphdr_nw_dst bits); 
        nw_proto=( char_of_int (get_arphdr_ar_op bits)); 
        nw_tos=(char_of_int 0); tp_src=0; tp_dst=0}
    | _ ->  
      {wildcards=(Wildcards.l2_match ()); 
      in_port; dl_src; dl_dst; dl_type;
      dl_vlan=0xffff; dl_vlan_pcp=(char_of_int 0);
      nw_src=0l; nw_dst=0l; 
      nw_tos=(char_of_int 0); nw_proto=(char_of_int 0); 
      tp_src=0; tp_dst=0}

  let print_field flag name value = 
    if (flag) then
      ""
    else
      sprintf "%s:%s," name value 

  let match_to_string m =
    sprintf 
      "%s%s%s%s%s%s%s%s%s%s%s%s"
      (print_field m.wildcards.Wildcards.in_port "in_port" (Port.string_of_port m.in_port))
      (print_field m.wildcards.Wildcards.dl_src "dl_src" (eaddr_to_string m.dl_src))
      (print_field m.wildcards.Wildcards.dl_dst "dl_dst" (eaddr_to_string m.dl_dst))
      (print_field m.wildcards.Wildcards.dl_vlan "dl_vlan" (string_of_int m.dl_vlan))
      (print_field m.wildcards.Wildcards.dl_vlan_pcp "dl_pcp" 
        (string_of_int (int_of_char m.dl_vlan_pcp) ))
      (print_field m.wildcards.Wildcards.dl_type "dl_type" (string_of_int m.dl_type))
     (print_field (m.wildcards.Wildcards.nw_src >= '\x20') "nw_src" 
        (sprintf "%s/%d" (Net.Nettypes.ipv4_addr_to_string (ipv4_addr_of_uint32 m.nw_src))
          (int_of_char m.wildcards.Wildcards.nw_src) ))
      (print_field (m.wildcards.Wildcards.nw_dst >= '\x20') "nw_dst" 
        (sprintf "%s/%d" (Net.Nettypes.ipv4_addr_to_string (ipv4_addr_of_uint32 m.nw_dst) )
          (int_of_char m.wildcards.Wildcards.nw_dst) ))
      (print_field m.wildcards.Wildcards.nw_tos "nw_tos" (string_of_int (int_of_char m.nw_tos)))
      (print_field m.wildcards.Wildcards.nw_proto "nw_proto" (string_of_int (int_of_char m.nw_proto)))
      (print_field m.wildcards.Wildcards.tp_src "tp_src" (string_of_int m.tp_src))
      (print_field m.wildcards.Wildcards.tp_dst "tp_dst" (string_of_int m.tp_dst))

  let flow_match_compare flow flow_def wildcard =
(*  Printf.printf "comparing flows %s \n%s\n%s \n%!" (Wildcards.wildcard_to_string wildcard) 
      (match_to_string flow)  (match_to_string flow_def); 
    Printf.printf "in_port:%s,dl_vlan:%s,dl_src:%s(%d %d),dl_dst:%s,dl_type:%s,\
        nw_proto:%s,tp_src:%s(%d %d),tp_dst:%s,nw_src:%s,nw_dst:%s,\
        dl_vlan_pcp:%s,nw_tos:%s\n%!" 
       (string_of_bool ((wildcard.Wildcards.in_port)|| (flow.in_port=flow_def.in_port)) )
       (string_of_bool ((wildcard.Wildcards.dl_vlan) || (flow.dl_vlan == flow_def.dl_vlan)) )
       (string_of_bool ((wildcard.Wildcards.dl_src)  || (flow.dl_src = flow_def.dl_src)) )
        (String.length flow.dl_src) (String.length flow_def.dl_src)
       (string_of_bool ((wildcard.Wildcards.dl_dst)  || (flow.dl_dst = flow_def.dl_dst)) )
       (string_of_bool ((wildcard.Wildcards.dl_type) || (flow.dl_type== flow_def.dl_type)) )
       (string_of_bool ((wildcard.Wildcards.nw_proto)|| (flow.nw_proto==flow_def.nw_proto)) )
       (string_of_bool ((wildcard.Wildcards.tp_src)  || (flow.tp_src == flow_def.tp_src)) )
        flow.tp_src flow_def.tp_src
       (string_of_bool ((wildcard.Wildcards.tp_dst)  || (flow.tp_dst == flow_def.tp_dst)) )
       (string_of_bool ((wildcard.Wildcards.nw_src >= '\x20') || 
          (Int32.shift_right_logical flow.nw_src (int_of_char wildcard.Wildcards.nw_src)) =
          (Int32.shift_right_logical flow_def.nw_src (int_of_char wildcard.Wildcards.nw_src))) )
       (string_of_bool ((wildcard.Wildcards.nw_src >= '\x20') ||
          (Int32.shift_right_logical flow.nw_dst (int_of_char wildcard.Wildcards.nw_dst)) =
          (Int32.shift_right_logical flow_def.nw_dst (int_of_char wildcard.Wildcards.nw_dst))) )
      (string_of_bool ((wildcard.Wildcards.nw_tos)  || (flow.nw_tos == flow_def.nw_tos)) )
      (string_of_bool ((wildcard.Wildcards.dl_vlan_pcp) || flow.dl_vlan_pcp ==
              flow_def.dl_vlan_pcp));*)

    (((wildcard.Wildcards.in_port)|| ((Port.int_of_port flow.in_port) == (Port.int_of_port flow_def.in_port))) && 
(*      ((wildcard.Wildcards.dl_vlan) || (flow.dl_vlan == flow_def.dl_vlan))
    *      &&*)
      ((wildcard.Wildcards.dl_src)  || (flow.dl_src = flow_def.dl_src)) &&
      ((wildcard.Wildcards.dl_dst)  || (flow.dl_dst = flow_def.dl_dst)) &&
      ((wildcard.Wildcards.dl_type) || (flow.dl_type== flow_def.dl_type)) &&
      ((wildcard.Wildcards.nw_proto)|| (flow.nw_proto==flow_def.nw_proto)) &&
      ((wildcard.Wildcards.tp_src)  || (flow.tp_src == flow_def.tp_src)) &&
      ((wildcard.Wildcards.tp_dst)  || (flow.tp_dst == flow_def.tp_dst)) &&
      ((wildcard.Wildcards.nw_src >= '\x20') ||
        (Int32.shift_right_logical flow.nw_src (int_of_char wildcard.Wildcards.nw_src)) =
        (Int32.shift_right_logical flow_def.nw_src (int_of_char wildcard.Wildcards.nw_src))) &&
      ((wildcard.Wildcards.nw_dst >= '\x20') ||
        (Int32.shift_right_logical flow.nw_dst (int_of_char wildcard.Wildcards.nw_dst)) =
        (Int32.shift_right_logical flow_def.nw_dst (int_of_char wildcard.Wildcards.nw_dst))) &&
      ((wildcard.Wildcards.nw_tos)  || (flow.nw_tos == flow_def.nw_tos)) &&
      ((wildcard.Wildcards.dl_vlan_pcp) || flow.dl_vlan_pcp ==
              flow_def.dl_vlan_pcp))

end

module Flow = struct
  type action = 
    | Output of (Port.t * int)
    | Set_vlan_vid of int 
    | Set_vlan_pcp of int 
    | STRIP_VLAN 
    | Set_dl_src of eaddr
    | Set_dl_dst of eaddr
    | Set_nw_src of ipv4 
    | Set_nw_dst of ipv4
    | Set_nw_tos of byte 
    | Set_tp_src of int16 
    | Set_tp_dst of int16
    | Enqueue of Port.t * uint32
    | VENDOR_ACT 

  let action_of_int = function
    |  0 -> Output((Port.port_of_int 0), 0)
    |  1 -> Set_vlan_vid(0xffff)
    |  2 -> Set_vlan_pcp(0)
    |  3 -> STRIP_VLAN 
    |  4 -> Set_dl_src("\xff\xff\xff\xff\xff\xff")
    |  5 -> Set_dl_dst("\xff\xff\xff\xff\xff\xff") 
    |  6 -> Set_nw_src(0xFFFFFFFFl)
    |  7 -> Set_nw_dst(0xFFFFFFFFl)
    |  8 -> Set_nw_tos(char_of_int 0) 
    |  9 -> Set_tp_src (0)
    | 10 -> Set_tp_dst (0)
    | 11 -> Enqueue ((Port.port_of_int 0), 0l)
    | 0xffff -> VENDOR_ACT
    | a -> invalid_arg (sp "action_of_int %04x" a)
  and int_of_action = function 
    | Output _     -> 0 
    | Set_vlan_vid _ -> 1 
    | Set_vlan_pcp _ -> 2
    | STRIP_VLAN -> 3
    | Set_dl_src _ -> 4
    | Set_dl_dst _ -> 5
    | Set_nw_src _ -> 6
    | Set_nw_dst _ -> 7
    | Set_nw_tos _ -> 8
    | Set_tp_src _ -> 9
    | Set_tp_dst _ -> 10
    | Enqueue (_,_) -> 11
    | VENDOR_ACT -> 0xffff
  and string_of_action = function
    | Output (port, max_len) 
      -> sp "OUTPUT %d %d " (Port.int_of_port port) max_len
    | Set_vlan_vid vlan -> sp "SET_VLAN_VID %d" vlan
    | Set_vlan_pcp (pcp) -> sp "SET_VLAN_PCP %d" pcp
    | STRIP_VLAN   -> sp "STRIP_VLAN"
    | Set_dl_src(eaddr)   -> (sp "SET_DL_SRC %s" (eaddr_to_string eaddr))
    | Set_dl_dst(eaddr)   -> (sp "SET_DL_DST %s" (eaddr_to_string eaddr))
    | Set_nw_src (ip) -> sp "SET_NW_SRC %s" (ipv4_to_string ip)
    | Set_nw_dst (ip) -> sp "SET_NW_DST %s" (ipv4_to_string ip)
    | Set_nw_tos (tos) -> sp "SET_NW_TOS %d" (int_of_char tos)
    | Set_tp_src (port) -> sp "SET_TP_SRC %d" port
    | Set_tp_dst (port) -> sp "SET_TP_DST %d" port
    | Enqueue (port, queue) -> sp "ENQUEUE %s:%ld" (Port.string_of_port port)
    queue
    | VENDOR_ACT   -> sp "VENDOR"

  let rec string_of_actions = function 
    | [] -> ""
    | head::tail -> (string_of_action head) ^ "," ^ (string_of_actions tail) 
  let len_of_action = function
    | Set_dl_src(_) -> 16
    | Set_dl_dst(_) -> 16
    | Enqueue(_, _) -> 16
    | _            -> 8  
  let rec actions_len = function
    | [] -> 0
    | action::actions -> (len_of_action action) + (actions_len actions)

 cstruct ofp_action_output {
   uint16_t typ;
   uint16_t len;
   uint16_t port;
   uint16_t max_len
 } as big_endian 

 cstruct ofp_action_vlan_vid {
   uint16_t typ;          
   uint16_t len;           
   uint16_t vlan_vid;      
   uint8_t pad[2]
 } as big_endian

 cstruct ofp_action_vlan_pcp {
   uint16_t typ;
   uint16_t len;           
   uint8_t vlan_pcp;       
   uint8_t pad[3]
 } as big_endian

 cstruct ofp_action_dl_addr {
   uint16_t typ; 
   uint16_t len;          
   uint8_t dl_addr[6];
   uint8_t pad[6]
 } as big_endian 

 cstruct ofp_action_nw_addr {
   uint16_t typ;
   uint16_t len; 
   uint32_t nw_addr
 } as big_endian

 cstruct ofp_action_tp_port {
   uint16_t typ;         
   uint16_t len;          
   uint16_t tp_port;      
   uint8_t pad[2]
 } as big_endian

 cstruct ofp_action_nw_tos {
   uint16_t typ;
   uint16_t len; 
   uint8_t nw_tos; 
   uint8_t pad[3]
 } as big_endian

 cstruct ofp_action_enqueue {
   uint16_t typ;
   uint16_t len;
   uint16_t port;
   uint8_t pad[6]; 
   uint32_t queue_id
 } as big_endian 

 cstruct ofp_action_header {
   uint16_t typ;
   uint16_t len;
   uint8_t pad[4]
 } as big_endian

  let marshal_action m bits =
    match m with  
    | Output (port, max_len) ->
      let _ = set_ofp_action_output_typ bits (int_of_action m) in
      let _ = set_ofp_action_output_len bits 8 in
      let _ = set_ofp_action_output_port bits (Port.int_of_port port) in
      let _ = set_ofp_action_output_max_len bits max_len in
        sizeof_ofp_action_output
    | Set_vlan_vid (vlan) -> 
      let _ = set_ofp_action_vlan_vid_typ bits (int_of_action m) in
      let _ = set_ofp_action_vlan_vid_len bits 8 in
      let _ = set_ofp_action_vlan_vid_vlan_vid bits vlan in
        sizeof_ofp_action_vlan_vid
    | Set_vlan_pcp (pcp) ->
      let _ = set_ofp_action_vlan_pcp_typ bits (int_of_action m) in
      let _ = set_ofp_action_vlan_pcp_len bits 8 in
      let _ = set_ofp_action_vlan_pcp_vlan_pcp bits pcp in
        sizeof_ofp_action_vlan_pcp
    | STRIP_VLAN -> 
      let _ = set_ofp_action_vlan_pcp_typ bits (int_of_action m) in
      let _ = set_ofp_action_vlan_pcp_len bits 8 in
        sizeof_ofp_action_vlan_pcp
    | Set_dl_src(eaddr) 
    | Set_dl_dst(eaddr) ->
      let _ = set_ofp_action_dl_addr_typ bits (int_of_action m) in
      let _ = set_ofp_action_dl_addr_len bits 16 in
      let _ = set_ofp_action_dl_addr_dl_addr eaddr 0 bits in 
        sizeof_ofp_action_dl_addr
    | Set_nw_src (ip) 
    | Set_nw_dst (ip) -> 
      let _ = set_ofp_action_nw_addr_typ bits (int_of_action m) in
      let _ = set_ofp_action_nw_addr_len bits 8 in
      let _ = set_ofp_action_nw_addr_nw_addr bits ip in
        sizeof_ofp_action_nw_addr    
    | Set_nw_tos (tos) -> 
      let _ = set_ofp_action_nw_tos_typ bits (int_of_action m) in
      let _ = set_ofp_action_nw_tos_len bits 8 in
      let _ = set_ofp_action_nw_tos_nw_tos bits (int_of_char tos) in
        sizeof_ofp_action_nw_tos
    | Set_tp_src (port) 
    | Set_tp_dst (port) ->
      let _ = set_ofp_action_tp_port_typ bits (int_of_action m) in
      let _ = set_ofp_action_tp_port_len bits 8 in
      let _ = set_ofp_action_tp_port_tp_port bits port in
        sizeof_ofp_action_nw_tos        
    | Enqueue (port, queue) -> 
      let _ = set_ofp_action_enqueue_typ bits (int_of_action m) in
      let _ = set_ofp_action_enqueue_len bits 16 in
      let _ = set_ofp_action_enqueue_port bits (Port.int_of_port port) in
      let _ = set_ofp_action_enqueue_queue_id bits queue in
        sizeof_ofp_action_enqueue 
    | _ -> failwith "Unsupported action" 

  let rec marshal_actions actions bits =
    match actions with
    | [] -> 0
    | action :: tail -> 
      let (len, bits) = marshal_and_shift (marshal_action action) bits in 
        len + (marshal_actions tail bits)

 cstruct ofp_action_header {
   uint16_t typ;
   uint16_t len;
   uint8_t pad[4]
 } as big_endian

  let parse_action bits = 
    match (action_of_int (get_ofp_action_header_typ bits)) with
    | Output (_, _) ->
      let port = Port.port_of_int (get_ofp_action_output_port bits) in 
      let max_len = get_ofp_action_output_max_len bits in
        (sizeof_ofp_action_output, Output(port, max_len))
    | Set_vlan_vid( _ ) -> 
      let vlan = get_ofp_action_vlan_vid_vlan_vid bits in 
        (sizeof_ofp_action_vlan_vid, Set_vlan_vid(vlan))
    | Set_vlan_pcp _ ->
      let pcp = get_ofp_action_vlan_pcp_vlan_pcp bits in 
        (sizeof_ofp_action_vlan_pcp, Set_vlan_pcp(pcp))
    | STRIP_VLAN -> 
      (sizeof_ofp_action_header, STRIP_VLAN)
    | Set_dl_src( _ ) ->
      let eaddr = Cstruct.to_string 
                    (get_ofp_action_dl_addr_dl_addr bits) in 
        (sizeof_ofp_action_dl_addr, Set_dl_src(eaddr))
    | Set_dl_dst( _ ) -> 
      let eaddr = Cstruct.to_string 
                    (get_ofp_action_dl_addr_dl_addr bits) in 
        (sizeof_ofp_action_dl_addr, Set_dl_dst(eaddr))
    | Set_nw_src( _ ) ->
      let ip = get_ofp_action_nw_addr_nw_addr bits in
        (sizeof_ofp_action_nw_addr, Set_nw_src( ip ))
    | Set_nw_dst( _ ) ->
      let ip = get_ofp_action_nw_addr_nw_addr bits in
        (sizeof_ofp_action_nw_addr, Set_nw_dst( ip ))
    | Set_nw_tos( _ ) ->
      let tos = char_of_int (get_ofp_action_nw_tos_nw_tos bits) in
        (sizeof_ofp_action_nw_tos, Set_nw_tos( tos ))
    | Set_tp_src( _ ) ->
      let port = get_ofp_action_tp_port_tp_port bits in
        (sizeof_ofp_action_tp_port, Set_tp_src(port))
    | Set_tp_dst( _ ) ->
      let port = get_ofp_action_tp_port_tp_port bits in
        (sizeof_ofp_action_tp_port, Set_tp_dst(port))
    | Enqueue (_ , _) ->
      let port = Port.port_of_int (get_ofp_action_enqueue_port bits) in
      let queue = get_ofp_action_enqueue_queue_id bits in
        (sizeof_ofp_action_enqueue, Enqueue( port, queue ))
    | _ -> raise(Unparsable ("parse_action", bits)) 

  let rec parse_actions bits =
    match (Cstruct.len bits) with
    | 0 -> (0, [])
    | l when ((l mod 8) = 0) ->
      let (len, action) = parse_action bits in 
      let bits = Cstruct.shift bits len in 
      let (len_rest, actions) = parse_actions bits in 
        (len + len_rest, [action] @ actions)
    | _ -> 
        printf "len of action cstruct %d\n%!" (Cstruct.len bits); 
        raise (Unparsable("parse_actions", bits))

  type reason = IDLE_TIMEOUT | HARD_TIMEOUT | DELETE
  let reason_of_int = function
    | 0 -> IDLE_TIMEOUT
    | 1 -> HARD_TIMEOUT
    | 2 -> DELETE
    | _ -> invalid_arg "reason_of_int"
  and int_of_reason = function
    | IDLE_TIMEOUT -> 0
    | HARD_TIMEOUT -> 1
    | DELETE -> 2
  and string_of_reason = function
    | IDLE_TIMEOUT -> 0
    | HARD_TIMEOUT -> 1
    | DELETE -> 2

  type stats = {
    mutable table_id: byte;
    mutable of_match: Match.t;
    mutable duration_sec: uint32;
    mutable duration_nsec: uint32;
    mutable priority: uint16;
    mutable idle_timeout: uint16;
    mutable hard_timeout: uint16;
    mutable cookie: uint64;
    mutable packet_count: uint64;
    mutable byte_count: uint64;
    mutable action: action list;
  }

  cstruct ofp_flow_stats {
    uint16_t length;       
    uint8_t table_id;      
    uint8_t pad;
    uint8_t flow_match[40]; 
    uint32_t duration_sec; 
    uint32_t duration_nsec;
    uint16_t priority;     
    uint16_t idle_timeout; 
    uint16_t hard_timeout; 
    uint8_t pad2[6];       
    uint64_t cookie;       
    uint64_t packet_count; 
    uint64_t byte_count
  } as big_endian 

  let parse_flow_stat bits =
    let table_id = char_of_int (get_ofp_flow_stats_table_id bits) in
    let of_match = Match.parse_match 
                    (get_ofp_flow_stats_flow_match bits) in 
    let duration_sec =  get_ofp_flow_stats_duration_sec bits in
    let duration_nsec = get_ofp_flow_stats_duration_nsec bits in
    let priority = get_ofp_flow_stats_priority bits in 
    let idle_timeout = get_ofp_flow_stats_idle_timeout bits in
    let hard_timeout = get_ofp_flow_stats_hard_timeout bits in
    let cookie = get_ofp_flow_stats_cookie bits in 
    let packet_count = get_ofp_flow_stats_packet_count bits in
    let byte_count = get_ofp_flow_stats_byte_count bits in
    let bits = Cstruct.shift bits sizeof_ofp_flow_stats in
    let (action_len, action) = parse_actions bits in 
      ( sizeof_ofp_flow_stats + action_len, 
        {table_id; of_match; duration_sec;
        duration_nsec; priority; idle_timeout; hard_timeout;
        cookie; packet_count;byte_count;action;} )
  
  let rec parse_flow_stats bits =
    (* A recursive function to parse each entry *) 
    match (Cstruct.len bits) with
    | 0 -> []
    | l when ((get_ofp_flow_stats_length bits) > l) ->
      let (len, flow) = parse_flow_stat bits in 
      let bits = Cstruct.shift bits len in 
        [flow] @ (parse_flow_stats bits)
    | _ -> raise (Unparsable("parse_flow_stats", bits))
   
  let string_of_flow_stat flow = 
    sp "table_id:%d,%s,duration:%ld.%ld,priority:%d,idle:%d,hard:%d,\
        cookie:%Ld,packets:%Ld,bytes:%Ld" 
      (int_of_char flow.table_id) (Match.match_to_string flow.of_match)
      flow.duration_sec flow.duration_nsec
      flow.priority flow.idle_timeout flow.hard_timeout flow.cookie 
      flow.packet_count flow.byte_count

  let flow_stats_len flow = 
    sizeof_ofp_flow_stats + (actions_len flow.action)

  let marshal_flow_stat flow bits = 
    let _ = set_ofp_flow_stats_length bits (flow_stats_len flow) in
    let _ = set_ofp_flow_stats_table_id bits (int_of_char flow.table_id) in
    let _ = Match.marshal_match flow.of_match  
                    (get_ofp_flow_stats_flow_match bits) in 
    let _ = set_ofp_flow_stats_duration_sec bits flow.duration_sec in
    let _ = set_ofp_flow_stats_duration_nsec bits flow.duration_nsec in
    let _ = set_ofp_flow_stats_priority bits flow.priority in 
    let _ = set_ofp_flow_stats_idle_timeout bits flow.idle_timeout in
    let _ = set_ofp_flow_stats_hard_timeout bits flow.hard_timeout in
    let _ = set_ofp_flow_stats_cookie bits flow.cookie in 
    let _ = set_ofp_flow_stats_packet_count bits flow.packet_count in
    let _ = set_ofp_flow_stats_byte_count bits flow.byte_count in
    let bits = Cstruct.shift bits sizeof_ofp_flow_stats in
    let (action_len, bits) = marshal_and_shift (marshal_actions flow.action) 
                              bits in
      (sizeof_ofp_flow_stats + action_len)

  let rec marshal_flow_stats flows bits =
    match flows with
    | [] -> 0
    | flow :: flows -> begin
      let (len, bits) = marshal_and_shift (marshal_flow_stat flow) bits in 
      let rest_len = marshal_flow_stats flows bits in 
        (len + rest_len)
    end
end

module Packet_out = struct
  type t = {
    buffer_id: uint32;
    in_port: Port.t;
    actions: Flow.action list;
    data : Cstruct.t;
  }

  cstruct ofp_packet_out {
    uint32_t buffer_id;      
    uint16_t in_port;        
    uint16_t actions_len
  } as big_endian

  let packet_out_to_string p = 
    sp "Packet_out: buffer_id:%ld in_port:%s actions:%s"
      p.buffer_id (Port.string_of_port p.in_port) (Flow.string_of_actions p.actions)

  let parse_packet_out bits = 
    let buffer_id = get_ofp_packet_out_buffer_id bits in
    let in_port = Port.port_of_int (get_ofp_packet_out_in_port bits) in
    let act_len = get_ofp_packet_out_actions_len bits in 
    let bits = Cstruct.shift bits sizeof_ofp_packet_out in
    let action_bits =  Cstruct.sub bits 0 act_len in
    let (_, actions) = Flow.parse_actions action_bits in
    let data = Cstruct.shift bits act_len in
      { buffer_id; in_port; actions; data; }

  let create ?(xid=0l) ?(buffer_id =(-1l)) ?(actions = [] ) 
      ~data ~in_port () =
    {buffer_id; in_port; actions; data;} 

  let get_len t = Header.sizeof_ofp_header + sizeof_ofp_packet_out + 
                (Flow.actions_len t.actions) + (Cstruct.len t.data) 

  let marshal_packet_out ?(xid=Random.int32 Int32.max_int) m bits =
    let size = get_len m in
    let of_header=Header.(create ~xid PACKET_OUT size) in
    let (ofp_len, bits) = marshal_and_shift (Header.marshal_header of_header)
    bits in
    let _ = set_ofp_packet_out_buffer_id bits m.buffer_id in
    let _ = set_ofp_packet_out_in_port bits (Port.int_of_port m.in_port) in
    let _ = set_ofp_packet_out_actions_len bits (Flow.actions_len m.actions) in
    let bits = Cstruct.shift bits sizeof_ofp_packet_out in
    let (act_len, bits) = marshal_and_shift (Flow.marshal_actions m.actions) bits in
    let _ = Cstruct.blit m.data 0 bits 0 (Cstruct.len m.data) in
      size

end

module Packet_in = struct
  type reason = NO_MATCH | ACTION
  let reason_of_int = function
    | 0 -> NO_MATCH
    | 1 -> ACTION
    | _ -> invalid_arg "reason_of_int"
  and int_of_reason = function
    | NO_MATCH -> 0
    | ACTION   -> 1
  and string_of_reason = function
    | NO_MATCH -> sp "NO_MATCH"
    | ACTION   -> sp "ACTION"

  type t = {
    buffer_id: uint32;
    in_port: Port.t;
    reason: reason;
    data: Cstruct.t;
  }

 cstruct ofp_packet_in {
   uint32_t buffer_id;     
   uint16_t total_len;     
   uint16_t in_port;       
   uint8_t reason;         
   uint8_t pad
  } as big_endian

  let parse_packet_in bits =
    let buffer_id = get_ofp_packet_in_buffer_id bits in
    let total_len = get_ofp_packet_in_total_len bits in
    let in_port = Port.port_of_int (get_ofp_packet_in_in_port bits) in
    let reason = reason_of_int (get_ofp_packet_in_reason bits) in
    let data = Cstruct.sub bits sizeof_ofp_packet_in total_len in
      { buffer_id; in_port; reason; data}
  
  let packet_in_to_string p = 
    sp "Packet_in: buffer_id:%ld in_port:%s reason:%s"
      p.buffer_id (Port.string_of_port p.in_port) (string_of_reason p.reason)

  let get_len t = Header.get_len + sizeof_ofp_packet_in +
                    (Cstruct.len t.data)

  let create_pkt_in ?(buffer_id=(-1l)) ~in_port ~reason ~data =
    let pkt_in = {buffer_id; in_port; reason; data;} in
    let h = Header.create Header.PACKET_IN (get_len pkt_in) in 
      (h, pkt_in)

  let marshal_pkt_in ?(xid=(Random.int32 Int32.max_int)) ?(data_len=0)
        t bits =
      let data_len = 
        if (data_len = 0) then
          Cstruct.len t.data 
        else (
          if (data_len < (Cstruct.len t.data)) then
            data_len
          else 
            Cstruct.len t.data 
          )
      in 
      let h = Header.create ~xid Header.PACKET_IN (Header.sizeof_ofp_header +
                        sizeof_ofp_packet_in + data_len) in 
      let (ofp_len, bits) = marshal_and_shift (Header.marshal_header h) bits in
      let _ = set_ofp_packet_in_buffer_id bits t.buffer_id in
      let _ = set_ofp_packet_in_total_len bits data_len in
      let _ = set_ofp_packet_in_in_port bits (Port.int_of_port t.in_port) in 
      let _ = set_ofp_packet_in_reason bits  (int_of_reason t.reason) in
      let _ = Cstruct.blit t.data 0 bits sizeof_ofp_packet_in 
                data_len in 
        ofp_len + sizeof_ofp_packet_in + data_len
end


(* this is a message only from the controller to the witch so
 * we can allow to parse inline the packet *)
module Flow_mod = struct
  type command = ADD | MODIFY | MODIFY_STRICT | DELETE | DELETE_STRICT
  let command_of_int = function
    | 0 -> ADD
    | 1 -> MODIFY
    | 2 -> MODIFY_STRICT
    | 3 -> DELETE
    | 4 -> DELETE_STRICT
    | _ -> invalid_arg "command_of_int"
  and int_of_command = function
    | ADD -> 0
    | MODIFY -> 1
    | MODIFY_STRICT -> 2
    | DELETE -> 3
    | DELETE_STRICT -> 4
  and string_of_command = function
    | ADD -> sp "ADD"
    | MODIFY -> sp "MODIFY"
    | MODIFY_STRICT -> sp "MODIFY_STRICT"
    | DELETE -> sp "DELETE"
    | DELETE_STRICT -> sp "DELETE_STRICT"

  type flags = {
    send_flow_rem: bool;
    emerg: bool;
    overlap: bool;
  }

  let marshal_flags flags =
    let ret = 0 in 
    let ret = set_int_bit ret 0 flags.send_flow_rem in 
    let ret = set_int_bit ret 1 flags.overlap in 
    let ret = set_int_bit ret 2 flags.emerg in 
      ret

  let parse_flags bits =
    let send_flow_rem = get_int_bit bits 0 in 
    let overlap = get_int_bit bits 1 in 
    let emerg = get_int_bit bits 2 in 
      {send_flow_rem; overlap; emerg; }

  type t = {
    mutable of_match: Match.t;
    cookie: uint64;
    command: command;
    mutable idle_timeout: uint16;
    mutable hard_timeout: uint16;
    mutable priority: uint16;
    buffer_id: int32;
    out_port: Port.t;
    flags: flags;
    mutable actions: Flow.action list;
  }
  
  cstruct ofp_flow_mod {
    uint64_t cookie;         
    uint16_t command;        
    uint16_t idle_timeout;   
    uint16_t hard_timeout;   
    uint16_t priority;       
    uint32_t buffer_id;      
    uint16_t out_port;       
    uint16_t flags
  } as big_endian

  let create flow_match cookie command ?(priority = 0) 
      ?(idle_timeout = 60) ?(hard_timeout = 0)
      ?(buffer_id =  -1 ) ?(out_port = Port.No_port) 
      ?(flags ={send_flow_rem=false;emerg=false;overlap=false;}) actions () =
    {of_match=flow_match; cookie; command=command; idle_timeout; hard_timeout; 
    priority; buffer_id=(Int32.of_int buffer_id); out_port;flags; actions;}

  let marshal_flow_mod ?(xid=(Random.int32 Int32.max_int)) m bits =
    let len = Header.sizeof_ofp_header + Match.sizeof_ofp_match + 
              sizeof_ofp_flow_mod + (Flow.actions_len m.actions) in
    let header = Header.create ~xid Header.FLOW_MOD len in 
    let (_, bits) = marshal_and_shift (Header.marshal_header header) bits in 
    let (_, bits) = marshal_and_shift (Match.marshal_match m.of_match) bits in 
    let _ = set_ofp_flow_mod_cookie bits m.cookie in 
    let _ = set_ofp_flow_mod_command bits (int_of_command m.command) in
    let _ = set_ofp_flow_mod_idle_timeout bits m.idle_timeout in 
    let _ = set_ofp_flow_mod_hard_timeout bits m.hard_timeout in
    let _ = set_ofp_flow_mod_priority bits m.priority in
    let _ = set_ofp_flow_mod_buffer_id bits m.buffer_id in
    let _ = set_ofp_flow_mod_out_port bits (Port.int_of_port m.out_port) in
    let _ = set_ofp_flow_mod_flags bits (marshal_flags m.flags) in 
    let bits = Cstruct.shift bits sizeof_ofp_flow_mod in 
    let _ = marshal_and_shift (Flow.marshal_actions m.actions) bits in  
      len

  let flow_mod_to_string t = 
    sp "cookie:%08Lx, command:%s, idle:%d, hard:%d, priority:%d, buffer_id:%ld, \
        port:%s, match:[%s] actions:[%s]"
      t.cookie (string_of_command t.command) t.idle_timeout t.hard_timeout
      t.priority t.buffer_id (Port.string_of_port t.out_port) 
      (Match.match_to_string t.of_match) (Flow.string_of_actions t.actions) 

  let parse_flow_mod bits = 
    let of_match = Match.parse_match bits in
    let bits = Cstruct.shift bits Match.sizeof_ofp_match in
    let cookie = get_ofp_flow_mod_cookie bits in 
    let command = command_of_int (get_ofp_flow_mod_command bits) in
    let idle_timeout = get_ofp_flow_mod_idle_timeout bits in 
    let hard_timeout = get_ofp_flow_mod_hard_timeout bits in 
    let priority = get_ofp_flow_mod_priority bits in 
    let buffer_id = get_ofp_flow_mod_buffer_id bits in 
    let out_port = Port.port_of_int (get_ofp_flow_mod_out_port bits) in 
    let flags = parse_flags (get_ofp_flow_mod_flags bits) in
    let bits = Cstruct.shift bits sizeof_ofp_flow_mod in 
    let (_, actions) = Flow.parse_actions bits in 
      {of_match; cookie; command; idle_timeout; hard_timeout;
      priority; buffer_id; out_port; 
      flags; actions; } 

end

module Flow_removed = struct
  type reason = IDLE_TIMEOUT | HARD_TIMEOUT | DELETE
  let reason_of_int = function
    | 0 -> IDLE_TIMEOUT
    | 1 -> HARD_TIMEOUT
    | 2 -> DELETE
    | _ -> invalid_arg "reason_of_int"
  and int_of_reason = function
    | IDLE_TIMEOUT -> 0
    | HARD_TIMEOUT -> 1
    | DELETE -> 2
  and string_of_reason = function
    | IDLE_TIMEOUT -> "IDLE_TIMEOUT"
    | HARD_TIMEOUT -> "HARD_TIMEOUT"
    | DELETE -> "DELETE"

  cstruct ofp_flow_removed {
    uint64_t cookie;         
    uint16_t priority;       
    uint8_t reason;          
    uint8_t pad[1];          
    uint32_t duration_sec;   
    uint32_t duration_nsec;  
    uint16_t idle_timeout;   
    uint8_t pad2[2];         
    uint64_t packet_count;
    uint64_t byte_count
  } as big_endian

  type t = { 
    of_match:Match.t;
    cookie:uint64;
    priority:uint16;
    reason:reason;
    duration_sec:uint32;
    duration_nsec:uint32;
    idle_timeout:uint16;
    packet_count:uint64;
    byte_count:uint64;
  }

  let flow_to_flow_removed ?(reason=DELETE) ~duration_sec ~duration_nsec 
        ~packet_count ~byte_count t = 
    {of_match=t.Flow_mod.of_match; cookie=t.Flow_mod.cookie;
     priority=t.Flow_mod.priority; reason; duration_sec; duration_nsec; 
     idle_timeout=t.Flow_mod.idle_timeout; packet_count; byte_count; }

  let parse_flow_removed bits =
    let of_match = Match.parse_match bits in 
    let cookie = get_ofp_flow_removed_cookie bits in 
    let priority = get_ofp_flow_removed_priority bits in 
    let reason = reason_of_int (get_ofp_flow_removed_reason bits ) in 
    let duration_sec = get_ofp_flow_removed_duration_sec bits in 
    let duration_nsec = get_ofp_flow_removed_duration_nsec bits in 
    let idle_timeout = get_ofp_flow_removed_idle_timeout bits in 
    let packet_count = get_ofp_flow_removed_packet_count bits in 
    let byte_count = get_ofp_flow_removed_byte_count bits in 
    let _ =  Cstruct.shift bits sizeof_ofp_flow_removed  in 
      {of_match; cookie; priority; reason; duration_sec; duration_nsec; 
      idle_timeout; packet_count; byte_count;}

  let get_len = Header.sizeof_ofp_header + Match.sizeof_ofp_match + 
                  sizeof_ofp_flow_removed

  let marshal_flow_removed ?(xid=(Random.int32 Int32.max_int)) m bits =
    let len = get_len in
    let header = Header.create ~xid Header.FLOW_REMOVED len in 
    let (_, bits) = marshal_and_shift (Header.marshal_header header) bits in 
    let (_, bits) = marshal_and_shift (Match.marshal_match m.of_match) bits in 
    let _ = set_ofp_flow_removed_cookie bits m.cookie in 
    let _ = set_ofp_flow_removed_priority bits m.priority in 
    let _ = set_ofp_flow_removed_reason bits (int_of_reason m.reason) in 
    let _ = set_ofp_flow_removed_duration_sec bits m.duration_sec in 
    let _ = set_ofp_flow_removed_duration_nsec bits m.duration_nsec in 
    let _ = set_ofp_flow_removed_idle_timeout bits m.idle_timeout in 
    let _ = set_ofp_flow_removed_packet_count bits m.packet_count in 
    let _ = set_ofp_flow_removed_byte_count bits m.byte_count in
      len

  let string_of_flow_removed m = 
    sp "flow:%s cookie:%s priority:%d reason:%s duration:%s.%s ifle_timeout:%d packet:%s bytes:%s"   
      (Match.match_to_string m.of_match) (Int64.to_string m.cookie) 
      m.priority (string_of_reason m.reason) (Int32.to_string m.duration_sec) 
      (Int32.to_string m.duration_nsec)  m.idle_timeout (Int64.to_string m.packet_count)
      (Int64.to_string  m.byte_count)
end

module Port_mod = struct
  type t = {
    port_no: Port.t;
    hw_addr: eaddr;
    config: Port.config;
    mask: Port.config;
    advertise: Port.features;
  }
end

module Stats = struct
  type table_id = All | Emergency | Table of uint8
  let table_id_of_int = function
    | 0xff -> All
    | 0xfe -> Emergency
    | i when ((0 <= i) && (i < 0xfe)) -> Table (byte i)
    | _ -> invalid_arg "table_id_of_int"
  and int_of_table_id = function
    | All -> 0xff
    | Emergency -> 0xfe
    | Table i -> int_of_byte i
  and string_of_table_id = function
    | All -> sp "All"
    | Emergency -> sp "Emergency"
    | Table i -> sp "Table (%d)" (int_of_byte i)

 
  type aggregate = {
    packet_count: uint64;
    byte_count: uint64;
    flow_count: uint32;
  }

  type table = {
    mutable table_id: table_id;
    mutable name: string;
    mutable wildcards: Wildcards.t;
    mutable max_entries: uint32;
    mutable active_count: uint32;
    mutable lookup_count: uint64;
    mutable matched_count: uint64;
  }

  type queue = {
    port_no: uint16;
    queue_id: uint32;
    tx_bytes: uint64;
    tx_packets: uint64;
    tx_errors: uint64;
  }

  type desc = {
    imfr_desc: bytes;
    hw_desc: bytes;
    sw_desc: bytes;
    serial_num: bytes;
    dp_desc: bytes;
  }

  type req_hdr = {
    ty : uint16;
    flags: uint16;
  }

  type port_counter = {
    rx_packets: uint64;
    tx_packets: uint64;
    rx_bytes: uint64;
    tx_bytes: uint64;
    rx_drops: uint64;
    tx_drops: uint64;
    rx_errors: uint64;
    tx_errors: uint64;
    rx_alignment_errors: uint64;
    rx_overrun_errors: uint64;
    rx_crc_errors: uint64;
    n_collisions: uint64;
  }
  
  type stats_type = DESC | FLOW | AGGREGATE | TABLE | PORT | QUEUE | VENDOR

  let int_of_req_type = function 
      | DESC -> 0
      | FLOW -> 1
      | AGGREGATE -> 2
      | TABLE -> 3
      | PORT -> 4
      | QUEUE -> 5
      | VENDOR -> 6
  let req_type_of_int = function 
      | 0 -> DESC     
      | 1 -> FLOW     
      | 2 -> AGGREGATE
      | 3 -> TABLE    
      | 4 -> PORT     
      | 5 -> QUEUE    
      | 6 -> VENDOR  
      | v -> raise(Unsupported("req_type_of_int"))
        
 cstruct ofp_stats_request {
    uint16_t typ;
    uint16_t flags
  } as big_endian

  cstruct ofp_flow_stats_request {
    uint8_t table_id;        
    uint8_t pad;             
    uint16_t out_port              
  } as big_endian

  cstruct ofp_queue_stats_request {
    uint16_t port_no;
    uint8_t pad[2];
    uint32_t queue_id 
  } as big_endian

  cstruct ofp_port_stats_request {
    uint16_t port_no;        
    uint8_t pad[6]
  } as big_endian

    let get_len = function
    | TABLE
    | DESC -> Header.sizeof_ofp_header + sizeof_ofp_stats_request
    | AGGREGATE
    | FLOW -> 
        (Header.sizeof_ofp_header + sizeof_ofp_stats_request + 
        Match.sizeof_ofp_match + sizeof_ofp_flow_stats_request )
    | QUEUE ->  (Header.sizeof_ofp_header + sizeof_ofp_stats_request + 
                  sizeof_ofp_queue_stats_request)
    | PORT -> (Header.sizeof_ofp_header + sizeof_ofp_stats_request +
                sizeof_ofp_port_stats_request)
    | _ -> (Header.sizeof_ofp_header + 4)

   let create_desc_stat_req ?(xid=(Random.int32 Int32.max_int)) bits =  
    let len = get_len DESC in 
    let header = Header.create ~xid Header.STATS_REQ len in 
    let _ = Header.marshal_header header bits in
    let bits = Cstruct.shift bits Header.sizeof_ofp_header in 
    let _ = set_ofp_stats_request_typ bits (int_of_req_type DESC) in 
    let _ = set_ofp_stats_request_flags bits 0 in 
      len

  let create_flow_stat_req flow_match ?(table_id=All) ?(out_port=(Port.No_port))
        ?(xid=(Random.int32 Int32.max_int)) bits = 
    let len = get_len FLOW in  
    let header = Header.create ~xid Header.STATS_REQ len in 
    let _ = Header.marshal_header header bits in
    let bits = Cstruct.shift bits Header.sizeof_ofp_header in 
    let _ = set_ofp_stats_request_typ bits (int_of_req_type FLOW) in 
    let _ = set_ofp_stats_request_flags bits 0 in 
    let bits = Cstruct.shift bits sizeof_ofp_stats_request in 
    let _ = Match.marshal_match flow_match bits in 
    let bits = Cstruct.shift bits Match.sizeof_ofp_match in 
    let _ = set_ofp_flow_stats_request_table_id bits (int_of_table_id table_id) in 
    let _ = set_ofp_flow_stats_request_out_port bits (Port.int_of_port out_port) in 
      len

  let create_aggr_flow_stat_req flow_match ?(table_id=All) ?(out_port=Port.No_port) 
      ?(xid=(Random.int32 Int32.max_int)) bits = 
    let len = get_len AGGREGATE in 
    let header = Header.create ~xid Header.STATS_REQ len in  
    let _ = Header.marshal_header header bits in 
    let bits = Cstruct.shift bits Header.sizeof_ofp_header in 
    let _ = set_ofp_stats_request_typ bits (int_of_req_type AGGREGATE) in 
    let _ = set_ofp_stats_request_flags bits 0 in 
    let bits = Cstruct.shift bits sizeof_ofp_stats_request in 
    let _ = Match.marshal_match flow_match bits in 
    let bits = Cstruct.shift bits Match.sizeof_ofp_match in 
    let _ = set_ofp_flow_stats_request_table_id bits (int_of_table_id table_id) in 
    let _ = set_ofp_flow_stats_request_out_port bits (Port.int_of_port out_port) in 
      len

(*  struct ofp_vendor_header {
    uint32_t vendor;         
  } as big_endian

  let create_vendor_stat_req ?(xid=(Random.int32 Int32.max_int)) () = 
    let header = (Header.build_h (Header.create Header.STATS_REQ (get_len VENDOR) snd_xid)) in 
    BITSTRING{(header):(Header.get_len * 8):bitstring; (int_of_req_type
                                                          DESC):16;0:16}*)
  let create_table_stat_req ?(xid=(Random.int32 Int32.max_int)) bits =
    let len = get_len TABLE in 
    let header = Header.create ~xid Header.STATS_REQ len in 
    let _ = Header.marshal_header header bits in 
    let bits = Cstruct.shift bits Header.sizeof_ofp_header in 
    let _ = set_ofp_stats_request_typ bits (int_of_req_type TABLE) in 
    let _ = set_ofp_stats_request_flags bits 0 in
      len

 let create_queue_stat_req ?(xid=(Random.int32 Int32.max_int))
      ?(queue_id=0xffffffffl) ?(port=Port.No_port) bits =
    let len = get_len QUEUE in 
    let header = Header.create ~xid Header.STATS_REQ len in 
    let _ = Header.marshal_header header bits in 
    let bits = Cstruct.shift bits sizeof_ofp_stats_request in 
    let _ = set_ofp_stats_request_typ bits (int_of_req_type QUEUE) in 
    let _ = set_ofp_stats_request_flags bits 0 in
    let bits = Cstruct.shift bits sizeof_ofp_stats_request in 
    let _ = set_ofp_queue_stats_request_port_no bits (Port.int_of_port port) in 
    let _ = set_ofp_queue_stats_request_queue_id bits queue_id in 
      len 

 let create_port_stat_req ?(xid=(Random.int32 Int32.max_int)) 
        ?(port=Port.No_port) bits =
    let len = get_len PORT in 
    let header = Header.create ~xid Header.STATS_REQ len in 
    let _ = Header.marshal_header header bits in 
    let bits = Cstruct.shift bits sizeof_ofp_stats_request in 
    let _ = set_ofp_stats_request_typ bits (int_of_req_type PORT) in 
    let _ = set_ofp_stats_request_flags bits 0 in
    let bits = Cstruct.shift bits sizeof_ofp_stats_request in 
    let _ = set_ofp_port_stats_request_port_no bits (Port.int_of_port port) in 
      len

  type req = 
    | Desc_req of req_hdr
    | Flow_req of req_hdr *  Match.t * table_id * Port.t
    | Aggregate_req of req_hdr * Match.t * table_id * Port.t
    | Table_req of req_hdr
    | Port_req of req_hdr * Port.t
    | Queue_req of req_hdr * Port.t * queue_id
    | Vendor_req of req_hdr

  let marshal_stats_req ?(xid=Random.int32 Int32.max_int) req bits = 
      match req with
      | Desc_req _ -> create_desc_stat_req ~xid bits 
      | Table_req _ -> create_table_stat_req ~xid bits 
      | Flow_req (_, m, table_id, out_port) -> 
          create_flow_stat_req m ~table_id ~out_port ~xid bits 
      | Aggregate_req (_, m, table_id, out_port) -> 
          create_aggr_flow_stat_req m ~table_id ~out_port ~xid bits
      | Port_req (_, port) -> 
          create_port_stat_req ~xid ~port bits 
      | Queue_req (_, port, queue_id) -> 
          create_queue_stat_req ~xid ~queue_id ~port bits
(*      | Vendor_req _ -> failwith "Vendor queue req not supported" *)


  let parse_stats_req bits =
    let flags = get_ofp_stats_request_flags bits in  
    let ty = get_ofp_stats_request_typ bits in 
    let _ = Cstruct.shift bits sizeof_ofp_stats_request in 
    let req = {ty; flags;} in 
    match (req_type_of_int ty) with
    | DESC -> Desc_req(req)
    | FLOW -> 
      let of_match = Match.parse_match bits in 
      let table_id = table_id_of_int (get_ofp_flow_stats_request_table_id bits) in
      let port_id = Port.port_of_int (get_ofp_flow_stats_request_out_port bits) in 
      let _ = Cstruct.shift bits sizeof_ofp_flow_stats_request in 
        Flow_req(req, of_match, table_id, port_id)
    | AGGREGATE -> 
      let of_match = Match.parse_match bits in 
      let table_id = table_id_of_int (get_ofp_flow_stats_request_table_id bits) in
      let port_id = Port.port_of_int (get_ofp_flow_stats_request_out_port bits) in 
      let _ = Cstruct.shift bits sizeof_ofp_flow_stats_request in
        Aggregate_req(req, of_match, table_id, port_id )
    | TABLE -> Table_req(req)
    | PORT -> 
      let port_id = Port.port_of_int (get_ofp_port_stats_request_port_no bits) in 
      let _ = Cstruct.shift bits sizeof_ofp_port_stats_request in 
        Port_req(req, port_id)
    | _ -> raise (Unparsable ("parse_stats_req", bits))

  type resp_hdr = {
    st_ty: stats_type;
    more_to_follow: bool;
  }

  let int_of_stats_type = function
    | DESC -> 0
    | FLOW -> 1
    | AGGREGATE -> 2 
    | TABLE -> 3 
    | PORT -> 4
    | QUEUE -> 5
    | VENDOR -> 0xffff

  let stats_type_of_int = function
    | 0 -> DESC
    | 1 -> FLOW
    | 2 -> AGGREGATE
    | 3 -> TABLE
    | 4 -> PORT
    | 5 -> QUEUE
    | 0xffff -> VENDOR
    | _ -> invalid_arg "stats type invalid int"

  type resp = 
    | Desc_resp of resp_hdr * desc
    | Flow_resp of resp_hdr * (Flow.stats list)
    | Aggregate_resp of resp_hdr * aggregate
    | Table_resp of resp_hdr * (table list)
    | Port_resp of resp_hdr * (Port.stats list)
    | Queue_resp of resp_hdr * (queue list)
    | Vendor_resp of resp_hdr

  cstruct ofp_table_stats {
    uint8_t table_id;      
    uint8_t pad[3];        
    uint8_t name[32]; 
    uint32_t wildcards;    
    uint32_t max_entries;  
    uint32_t active_count; 
    uint64_t lookup_count; 
    uint64_t matched_count
  } as big_endian 

  let rec parse_table_stats_reply bits =
    match (Cstruct.len bits ) with 
    | l -> 
      let table_id = table_id_of_int (get_ofp_table_stats_table_id bits) in 
      let name = get_ofp_table_stats_name bits in 
      let name = Cstruct.copy name 0 (Cstruct.len name) in 
      let wildcards = Wildcards.parse_wildcards (get_ofp_table_stats_wildcards
      bits) in
      let max_entries = get_ofp_table_stats_max_entries bits in 
      let active_count = get_ofp_table_stats_active_count bits in 
      let lookup_count = get_ofp_table_stats_lookup_count bits in 
      let matched_count = get_ofp_table_stats_matched_count bits in 
      let ret = {table_id; name; wildcards; max_entries; active_count; lookup_count;
                  matched_count;} in
      let _ = Cstruct.shift bits sizeof_ofp_table_stats in 
        [ret] @ (parse_table_stats_reply bits)
    | 0 -> []
          
  let rec string_of_table_stats_reply tables =
    match tables with
      | [] -> ""
      | h::q -> sp "table_id:%s,name:%s,wildcard:%s,max_entries:%ld,
          active_count:%ld,lookup_count:%Ld,matched_count:%Ld\n%s" 
        (string_of_table_id h.table_id) h.name 
        (Wildcards.wildcard_to_string h.wildcards) 
        h.max_entries h.active_count h.lookup_count 
        h.matched_count (string_of_table_stats_reply q) 
  
  cstruct ofp_stats_reply {
    uint16_t typ; 
    uint16_t flags
  } as big_endian

  cstruct ofp_desc_stats {
    uint8_t mfr_desc[256];    
    uint8_t hw_desc[256];     
    uint8_t sw_desc[256];     
    uint8_t serial_num[32];
    uint8_t dp_desc[256]
  } as big_endian

  cstruct ofp_aggregate_stats_reply {
    uint64_t packet_count;    
    uint64_t byte_count; 
    uint32_t flow_count; 
    uint8_t pad[4]
  } as big_endian 

  cstruct ofp_port_stats {
    uint16_t port_no;
    uint8_t pad[6];       
    uint64_t rx_packets;  
    uint64_t tx_packets;  
    uint64_t rx_bytes;    
    uint64_t tx_bytes;    
    uint64_t rx_dropped;  
    uint64_t tx_dropped;  
    uint64_t rx_errors;   
    uint64_t tx_errors;   
    uint64_t rx_frame_err;
    uint64_t rx_over_err; 
    uint64_t rx_crc_err;  
    uint64_t collisions
  } as big_endian

  let parse_stats_resp bits =
    let typ = stats_type_of_int  (get_ofp_stats_reply_typ bits) in 
    let more_to_follow = ((get_ofp_stats_reply_flags bits) = 1) in 
    let resp = {st_ty=typ;more_to_follow;} in 
    let _ = Cstruct.shift bits sizeof_ofp_stats_reply in 

    match typ with
    | DESC -> 
      let imfr_desc = Cstruct.copy (get_ofp_desc_stats_mfr_desc bits) 0 256 in 
      let hw_desc= Cstruct.copy (get_ofp_desc_stats_hw_desc bits) 0 256 in 
      let sw_desc = Cstruct.copy (get_ofp_desc_stats_sw_desc bits) 0 256 in
      let serial_num = Cstruct.copy (get_ofp_desc_stats_serial_num bits)
                          0 32 in
      let dp_desc = Cstruct.copy (get_ofp_desc_stats_dp_desc bits) 0 256
      in 
        Desc_resp(resp, {imfr_desc; hw_desc; sw_desc; serial_num; dp_desc;})

    | FLOW -> 
      let flows = Flow.parse_flow_stats bits in 
        Flow_resp (resp, flows)
    | AGGREGATE -> 
      let packet_count = get_ofp_aggregate_stats_reply_packet_count bits in 
      let byte_count = get_ofp_aggregate_stats_reply_byte_count bits in 
      let flow_count = get_ofp_aggregate_stats_reply_flow_count bits in 
      let _ = Cstruct.shift bits sizeof_ofp_aggregate_stats_reply in 
        Aggregate_resp(resp, {packet_count; byte_count; flow_count;})
    | TABLE -> 
      let table_stats = parse_table_stats_reply bits in 
        Table_resp(resp, table_stats)
    | PORT -> 
      let port_stats = Port.parse_port_stats_reply bits in 
        Port_resp(resp, port_stats )
    | VENDOR -> Vendor_resp(resp) 
    | QUEUE -> raise(Unparsable("parse_stats_resp QUEUE", bits))

  let resp_get_len = function
    | Desc_resp(_, _) -> Header.sizeof_ofp_header + sizeof_ofp_desc_stats 
    | Flow_resp (_, f) ->
      let flow_len = List.fold_right 
        (fun f l -> l + (Flow.flow_stats_len f) ) f 0 in 
        Header.get_len + sizeof_ofp_stats_reply +flow_len
    | Aggregate_resp _ ->
      Header.get_len + sizeof_ofp_stats_reply +
                  sizeof_ofp_aggregate_stats_reply
     | Table_resp (_, tables) -> 4 + (List.length tables) *(1+3+32+4+4+4+8+8)
    | _ -> failwith "resp_get_len"

  let marshal_stats_resp xid resp bits =
    match resp with 
    | Desc_resp(resp_hdr, desc) ->
      let len = (Header.sizeof_ofp_header + sizeof_ofp_stats_reply +
                  sizeof_ofp_desc_stats) in 
      let of_header = Header.create ~xid Header.STATS_RESP len in 
      let (ofp_len, bits) = marshal_and_shift (Header.marshal_header of_header)
                              bits in
      let _ = set_ofp_stats_reply_typ bits (int_of_stats_type DESC) in 
      let _ = set_ofp_stats_reply_flags bits (int_of_bool
      resp_hdr.more_to_follow) in 
      let bits = Cstruct.shift bits sizeof_ofp_stats_reply in 
      let _ = set_ofp_desc_stats_mfr_desc desc.imfr_desc 0 bits in  
      let _ = set_ofp_desc_stats_hw_desc desc.hw_desc 0 bits in  
      let _ = set_ofp_desc_stats_sw_desc desc.sw_desc 0 bits in  
      let _ = set_ofp_desc_stats_serial_num desc.serial_num 0 bits in  
      let _ = set_ofp_desc_stats_dp_desc desc.dp_desc 0 bits in  
        len        
    | Flow_resp(resp_h, flows) ->
      let flow_len = List.fold_right 
        (fun f l -> l + (Flow.flow_stats_len f) ) flows 0 in 
      let len = (Header.sizeof_ofp_header + sizeof_ofp_stats_reply +
                  flow_len) in
      let of_header = Header.create ~xid Header.STATS_RESP len in 
      let (ofp_len, bits) = marshal_and_shift (Header.marshal_header of_header)
                              bits in
      let _ = set_ofp_stats_reply_typ bits (int_of_stats_type FLOW) in 
      let _ = set_ofp_stats_reply_flags bits (int_of_bool
      resp_h.more_to_follow) in 
      let bits = Cstruct.shift bits sizeof_ofp_stats_reply in 
      let (flows_len, bits) = marshal_and_shift (Flow.marshal_flow_stats
      flows) bits in 
        (Header.sizeof_ofp_header + sizeof_ofp_stats_reply +
        flows_len)
    | Aggregate_resp(resp, stats) ->
      let len = Header.sizeof_ofp_header + sizeof_ofp_stats_reply +
                  sizeof_ofp_aggregate_stats_reply in
      let of_header = Header.create ~xid Header.STATS_RESP len in 
      let (ofp_len, bits) = marshal_and_shift (Header.marshal_header of_header)
                              bits in
      let _ = set_ofp_stats_reply_typ bits (int_of_stats_type AGGREGATE) in 
      let _ = set_ofp_stats_reply_flags bits (int_of_bool
                resp.more_to_follow) in 
      let bits = Cstruct.shift bits sizeof_ofp_stats_reply in 
      let _ = set_ofp_aggregate_stats_reply_packet_count bits stats.packet_count in 
      let _ = set_ofp_aggregate_stats_reply_byte_count bits stats.byte_count in 
      let _ = set_ofp_aggregate_stats_reply_flow_count bits stats.flow_count in 
        len
(*    | Table_resp(resp_hdr, tables)
      -> let tbl_bitstring = (List.map (fun tbl -> BITSTRING{ (int_of_table_id tbl.table_id):8; 0:24; 
          (Printf.sprintf "%s%s" tbl.name (String.make (32-(String.length tbl.name)) (Char.chr 0))):32*8:string;
            (Wildcards.wildcard_to_bitstring tbl.wildcards):32:bitstring; tbl.max_entries:32;
            tbl.active_count:32; tbl.lookup_count:64; tbl.matched_count:64}) tables) in 
      Bitstring.concat ([(BITSTRING{(int_of_stats_type
      resp_hdr.st_ty):16;0:16});] @ tbl_bitstring) *)
    | _  -> raise (Unparsed ("STATS_RESP", bits))

  let rec string_of_flow_stats flows = 
    match flows with 
        [] -> ""
      | flow::flow_list -> 
        sp "%s\n%s" (Flow.string_of_flow_stat flow) 
          (string_of_flow_stats flow_list)

  let string_of_stats stats =
    match stats with
        Desc_resp(hdr, desc) ->
          (sp "Stats Desc %s %s %s %s %s" desc.imfr_desc desc.hw_desc
             desc.sw_desc desc.serial_num desc.dp_desc)
      | Flow_resp(hdr, flows) -> 
        (sp "Stats Flows %s" (string_of_flow_stats flows))
      | Aggregate_resp (hdr, aggr) ->
        (sp "Aggr flow stats %Ld %Ld %ld" aggr.packet_count
           aggr.byte_count aggr.flow_count)
      | Port_resp(resp, ports) ->
        (sp "port stats %s" (Port.string_of_port_stats_reply ports))
      | Table_resp(resp, tables) ->
        (sp "table stats %s" ) (string_of_table_stats_reply tables)
      | Vendor_resp(resp) ->
        (sp "vendor resp")
      | _ -> invalid_arg "Invalide stats resp object"

end

type error_code = 
  | HELLO_INCOMPATIBLE
  | HELLO_EPERM
  | REQUEST_BAD_VERSION
  | REQUEST_BAD_TYPE
  | REQUEST_BAD_STAT
  | REQUEST_BAD_VENDOR
  | REQUEST_BAD_SUBTYPE
  | REQUEST_REQUEST_EPERM
  | REQUEST_BAD_LEN
  | REQUEST_BUFFER_EMPTY
  | REQUEST_BUFFER_UNKNOWN
  | ACTION_BAD_TYPE
  | ACTION_BAD_LEN
  | ACTION_BAD_VENDOR
  | ACTION_BAD_VENDOR_TYPE
  | ACTION_BAD_OUT_PORT
  | ACTION_BAD_ARGUMENT
  | ACTION_EPERM
  | ACTION_TOO_MANY
  | ACTION_BAD_QUEUE
  | FLOW_MOD_ALL_TABLES_FULL
  | FLOW_MOD_OVERLAP
  | FLOW_MOD_EPERM
  | FLOW_MOD_EMERG_TIMEOUT
  | FLOW_MOD_BAD_COMMAND
  | FLOW_MOD_UNSUPPORTED
  | PORT_MOD_BAD_PORT
  | PORT_MOD_BAD_HW_ADDR
  | QUEUE_OP_BAD_PORT
  | QUEUE_OP_BAD_QUEUE
  | QUEUE_OP_EPERM	
let error_code_of_int = function
  | 0x00000000 -> HELLO_INCOMPATIBLE
  | 0x00000001 -> HELLO_EPERM
  | 0x00010000 -> REQUEST_BAD_VERSION
  | 0x00010001 -> REQUEST_BAD_TYPE
  | 0x00010002 -> REQUEST_BAD_STAT
  | 0x00010003 -> REQUEST_BAD_VENDOR
  | 0x00010004 -> REQUEST_BAD_SUBTYPE
  | 0x00010005 -> REQUEST_REQUEST_EPERM
  | 0x00010006 -> REQUEST_BAD_LEN
  | 0x00010007 -> REQUEST_BUFFER_EMPTY
  | 0x00010008 -> REQUEST_BUFFER_UNKNOWN
  | 0x00020000 -> ACTION_BAD_TYPE
  | 0x00020001 -> ACTION_BAD_LEN
  | 0x00020002 -> ACTION_BAD_VENDOR
  | 0x00020003 -> ACTION_BAD_VENDOR_TYPE
  | 0x00020004 -> ACTION_BAD_OUT_PORT
  | 0x00020005 -> ACTION_BAD_ARGUMENT
  | 0x00020006 -> ACTION_EPERM
  | 0x00020007 -> ACTION_TOO_MANY
  | 0x00020008 -> ACTION_BAD_QUEUE
  | 0x00030000 -> FLOW_MOD_ALL_TABLES_FULL
  | 0x00030001 -> FLOW_MOD_OVERLAP
  | 0x00030002 -> FLOW_MOD_EPERM
  | 0x00030003 -> FLOW_MOD_EMERG_TIMEOUT
  | 0x00030004 -> FLOW_MOD_BAD_COMMAND
  | 0x00030005 -> FLOW_MOD_UNSUPPORTED
  | 0x00040000 -> PORT_MOD_BAD_PORT
  | 0x00040001 -> PORT_MOD_BAD_HW_ADDR
  | 0x00050000 -> QUEUE_OP_BAD_PORT
  | 0x00050001 -> QUEUE_OP_BAD_QUEUE
  | 0x00050002 -> QUEUE_OP_EPERM	
  | _ -> invalid_arg "error_code_of_int"
and int_of_error_code = function
  | HELLO_INCOMPATIBLE       -> 0x000000000l
  | HELLO_EPERM              -> 0x000000001l
  | REQUEST_BAD_VERSION      -> 0x000100000l
  | REQUEST_BAD_TYPE         -> 0x000100001l
  | REQUEST_BAD_STAT         -> 0x000100002l
  | REQUEST_BAD_VENDOR       -> 0x000100003l
  | REQUEST_BAD_SUBTYPE      -> 0x000100004l
  | REQUEST_REQUEST_EPERM    -> 0x000100005l
  | REQUEST_BAD_LEN          -> 0x000100006l
  | REQUEST_BUFFER_EMPTY     -> 0x000100007l
  | REQUEST_BUFFER_UNKNOWN   -> 0x000100008l
  | ACTION_BAD_TYPE          -> 0x000200000l
  | ACTION_BAD_LEN           -> 0x000200001l
  | ACTION_BAD_VENDOR        -> 0x000200002l
  | ACTION_BAD_VENDOR_TYPE   -> 0x000200003l
  | ACTION_BAD_OUT_PORT      -> 0x000200004l
  | ACTION_BAD_ARGUMENT      -> 0x000200005l
  | ACTION_EPERM             -> 0x000200006l
  | ACTION_TOO_MANY          -> 0x000200007l
  | ACTION_BAD_QUEUE         -> 0x000200008l
  | FLOW_MOD_ALL_TABLES_FULL -> 0x000300000l
  | FLOW_MOD_OVERLAP         -> 0x000300001l
  | FLOW_MOD_EPERM           -> 0x000300002l
  | FLOW_MOD_EMERG_TIMEOUT   -> 0x000300003l
  | FLOW_MOD_BAD_COMMAND     -> 0x000300004l
  | FLOW_MOD_UNSUPPORTED     -> 0x000300005l
  | PORT_MOD_BAD_PORT        -> 0x000400000l
  | PORT_MOD_BAD_HW_ADDR     -> 0x000400001l
  | QUEUE_OP_BAD_PORT        -> 0x000500000l
  | QUEUE_OP_BAD_QUEUE       -> 0x000500001l
  | QUEUE_OP_EPERM           -> 0x000500002l
and string_of_error_code = function
  | HELLO_INCOMPATIBLE       -> sp "HELLO_INCOMPATIBLE"
  | HELLO_EPERM              -> sp "HELLO_EPERM"
  | REQUEST_BAD_VERSION      -> sp "REQUEST_BAD_VERSION"
  | REQUEST_BAD_TYPE         -> sp "REQUEST_BAD_TYPE"
  | REQUEST_BAD_STAT         -> sp "REQUEST_BAD_STAT"
  | REQUEST_BAD_VENDOR       -> sp "REQUEST_BAD_VENDOR"
  | REQUEST_BAD_SUBTYPE      -> sp "REQUEST_BAD_SUBTYPE"
  | REQUEST_REQUEST_EPERM    -> sp "REQUEST_REQUEST_EPERM"
  | REQUEST_BAD_LEN          -> sp "REQUEST_BAD_LEN"
  | REQUEST_BUFFER_EMPTY     -> sp "REQUEST_BUFFER_EMPTY"
  | REQUEST_BUFFER_UNKNOWN   -> sp "REQUEST_BUFFER_UNKNOWN"
  | ACTION_BAD_TYPE          -> sp "ACTION_BAD_TYPE"
  | ACTION_BAD_LEN           -> sp "ACTION_BAD_LEN"
  | ACTION_BAD_VENDOR        -> sp "ACTION_BAD_VENDOR"
  | ACTION_BAD_VENDOR_TYPE   -> sp "ACTION_BAD_VENDOR_TYPE"
  | ACTION_BAD_OUT_PORT      -> sp "ACTION_BAD_OUT_PORT"
  | ACTION_BAD_ARGUMENT      -> sp "ACTION_BAD_ARGUMENT"
  | ACTION_EPERM             -> sp "ACTION_EPERM"
  | ACTION_TOO_MANY          -> sp "ACTION_TOO_MANY"
  | ACTION_BAD_QUEUE         -> sp "ACTION_BAD_QUEUE"
  | FLOW_MOD_ALL_TABLES_FULL -> sp "FLOW_MOD_ALL_TABLES_FULL"
  | FLOW_MOD_OVERLAP         -> sp "FLOW_MOD_OVERLAP"
  | FLOW_MOD_EPERM           -> sp "FLOW_MOD_EPERM"
  | FLOW_MOD_EMERG_TIMEOUT   -> sp "FLOW_MOD_EMERG_TIMEOUT"
  | FLOW_MOD_BAD_COMMAND     -> sp "FLOW_MOD_BAD_COMMAND"
  | FLOW_MOD_UNSUPPORTED     -> sp "FLOW_MOD_UNSUPPORTED"
  | PORT_MOD_BAD_PORT        -> sp "PORT_MOD_BAD_PORT"
  | PORT_MOD_BAD_HW_ADDR     -> sp "PORT_MOD_BAD_HW_ADDR"
  | QUEUE_OP_BAD_PORT        -> sp "QUEUE_OP_BAD_PORT"
  | QUEUE_OP_BAD_QUEUE       -> sp "QUEUE_OP_BAD_QUEUE"
  | QUEUE_OP_EPERM           -> sp "QUEUE_OP_EPERM"

  cstruct ofp_error_msg {
    uint16_t typ;
    uint16_t code
  } as big_endian

let marshal_error errornum data xid bits = 
    let req_len = Cstruct.len data in
    let req_h = Header.create ~xid Header.ERROR  
    (Header.get_len + sizeof_ofp_error_msg + req_len) in
    let (len, bits) = marshal_and_shift (Header.marshal_header req_h) bits in
    let errornum = int_of_error_code errornum in 
    let _ = set_ofp_error_msg_typ bits 
              (Int32.to_int (Int32.shift_left errornum 16)) in 
    let _ = set_ofp_error_msg_code bits 
              (Int32.to_int (Int32.logand errornum 0xffff0000l)) in
    let bits = Cstruct.shift bits sizeof_ofp_error_msg in 
    let _ = Cstruct.blit data 0 bits 0 (Cstruct.len data) in 
      (Header.get_len + sizeof_ofp_error_msg + req_len)

let build_features_req xid bits = 
  Header.marshal_header (Header.(create ~xid FEATURES_REQ 8)) bits

let build_echo_resp h bits =
  let len = Header.get_len in 
  let _ = 
    Header.(marshal_header 
              (create ~xid:h.xid ECHO_RESP len ) bits) in
   len

type t =
  | Hello of Header.h
  | Error of Header.h  * error_code * Cstruct.t
  | Echo_req of Header.h 
  | Echo_resp of Header.h 
  | Vendor of Header.h  * vendor * Cstruct.t

  | Features_req of Header.h
  | Features_resp of Header.h  * Switch.features
  | Get_config_req of Header.h 
  | Get_config_resp of Header.h  * Switch.config    
  | Set_config of Header.h  * Switch.config    

  | Packet_in of Header.h  * Packet_in.t
  | Flow_removed of Header.h  * Flow_removed.t
  | Port_status of Header.h  * Port.status

  | Packet_out of Header.h  * Packet_out.t (* Cstruct.t *)
  | Flow_mod of Header.h  * Flow_mod.t
  | Port_mod of Header.h  * Port_mod.t

  | Stats_req of Header.h  * Stats.req
  | Stats_resp of Header.h  * Stats.resp

  | Barrier_req of Header.h 
  | Barrier_resp of Header.h 

  | Queue_get_config_req of Header.h * Port.t
  | Queue_get_config_resp of Header.h * Port.t * Queue.t array

let parse h bits =
  Header.(match h.ty with
    | HELLO -> Hello (h)
    | ERROR -> raise (Unparsed ("ERROR", bits))
    | ECHO_REQ -> Echo_req h
    | ECHO_RESP -> Echo_resp h
    | VENDOR -> raise (Unparsed ("VENDOR", bits))
    | FEATURES_REQ -> Features_req (h)
    | FEATURES_RESP -> Features_resp (h, Switch.parse_features bits)
    | GET_CONFIG_REQ -> Get_config_req(h)
    | GET_CONFIG_RESP -> raise (Unparsed ("GET_CONFIG_RESP", bits))
    | SET_CONFIG -> raise (Unparsed ("SET_CONFIG", bits))
    | PACKET_IN -> Packet_in (h, Packet_in.parse_packet_in bits)
    | PORT_STATUS -> Port_status(h, (Port.parse_status bits)) 
    | FLOW_REMOVED -> Flow_removed(h, (Flow_removed.parse_flow_removed bits))
(*     | FLOW_MOD -> raise (Unparsed ("GET_CONFIG_RESP", bits)) *)
    | PACKET_OUT -> Packet_out (h, (Packet_out.parse_packet_out bits) ) 
    | FLOW_MOD -> Flow_mod(h, (Flow_mod.parse_flow_mod bits)) 
    | STATS_REQ -> Stats_req(h, (Stats.parse_stats_req bits))
    | STATS_RESP -> Stats_resp (h, (Stats.parse_stats_resp bits))
    | BARRIER_REQ -> Barrier_req(h)
    | BARRIER_RESP -> Barrier_resp(h)
    | _ -> raise (Unparsed ("_", bits))
  )

let to_string  = function
  | Features_req h
  | Get_config_req h
  | Barrier_req h
  | Barrier_resp h
  | Echo_req h
  | Echo_resp h
  | Get_config_req h
  | Get_config_resp (h, _)
  | Set_config (h, _) 
  | Flow_removed (h, _) 
  | Packet_in (h, _) 
  | Features_resp (h, _) 
  | Port_status (h, _) 
  | Stats_req (h, _)  
  | Stats_resp (h, _) 
  | Error (h, _, _) 
  | Packet_out (h, _)  
  | Flow_mod (h, _)  
  | Hello h -> Header.header_to_string h 
  | _ -> failwith "Unsupported message" 

let marshal msg =
  let marshal = 
    match msg with
        | Features_req h
        | Get_config_req h
        | Barrier_req h
        | Barrier_resp h
        | Echo_req h
        | Echo_resp h
        | Get_config_req h
        | Hello h -> Header.marshal_header h 
        | Flow_removed (h, frm) ->
            Flow_removed.marshal_flow_removed ~xid:(h.Header.xid) frm
        | Packet_in (h, pkt_in) -> 
            Packet_in.marshal_pkt_in ~xid:h.Header.xid pkt_in
        | Features_resp (h, p) ->
            Switch.marshal_reply_features h.Header.xid p
        | Get_config_resp (h, c)
        | Set_config (h, c) -> 
            Switch.marshal_switch_config h.Header.xid c        
        | Port_status (h, p) ->
            Port.marshal_port_status ~xid:h.Header.xid p 
        | Stats_req (h, p) -> 
            Stats.marshal_stats_req ~xid:h.Header.xid p
        | Stats_resp (h, p) ->
            Stats.marshal_stats_resp h.Header.xid p
        | Error (h, err, bits) ->
            marshal_error err bits h.Header.xid
        | Packet_out (h, p) -> 
            Packet_out.marshal_packet_out ~xid:h.Header.xid p
        | Flow_mod (h, fm) -> 
            Flow_mod.marshal_flow_mod ~xid:h.Header.xid fm
        | _ -> failwith "Unsupported message" 
    in
(*
  | Vendor of Header.h  * vendor * Cstruct.t
  | Port_mod of Header.h  * Port_mod.t
  | Queue_get_config_req of Header.h * Port.t
  | Queue_get_config_resp of Header.h * Port.t * Queue.t array
 *)
    marshal_and_sub marshal (OS.Io_page.to_cstruct (OS.Io_page.get ()))
