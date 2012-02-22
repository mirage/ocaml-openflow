open Bitstring
open Checksum
open Printf

let gw_ip =  0x0a000202l
let gw_mac = "\x52\x54\x00\x12\x35\x02"
let local_ip = 0x0a00020fl
let local_mac = "\x08\x00\x27\xbb\x59\x1e"

let get_tcp_packet_payload data = 
  bitmatch data with 
    | {_:116:bitstring; ihl:4; _:((ihl*32)-8):bitstring;
         _:96:bitstring; tcp_len:4;
         _:((tcp_len*32) - 100):bitstring; tcp_body:-1:bitstring } ->
        (Printf.printf "get_tcp_packet_payload matched packet\n%!");
        (Bitstring.hexdump_bitstring Pervasives.stdout tcp_body);
        tcp_body
    | { _ } -> 
        (Bitstring.hexdump_bitstring Pervasives.stdout data;
        Printf.printf "get_tcp_packet_payload failed to parse\n%!";
         Bitstring.empty_bitstring)

let gen_server_syn data new_isn new_dst_port m = 
    bitmatch data with 
      | {_:48:bitstring; _:48:bitstring; header:20:bitstring; 
         ihl:4;  tos:8; tlen:16; ipid:16; flags:3; fragoff:13;
         ttl:8; proto:8; _:16; nw_src:32; nw_dst:32;
         header2:(ihl-5)*32:bitstring;src_port:16; dst_port:16; isn:32; 
         header3:64:bitstring; 
         checksum:16; tcp_body:-1:bitstring } ->
            let ip_chk = Checksum.ones_complement (BITSTRING { 4:4; ihl:4; tos:8; 
                tlen:16; ipid:16; flags:3; fragoff:13; ttl:8; proto:8; 0:16; 
                gw_ip:32; local_ip:32}) 
            in 
            let tcp_chk = 
                (Checksum.ones_complement (BITSTRING{gw_ip:32; local_ip:32; 0:8; 6:8; 
                    (((Bitstring.bitstring_length tcp_body)/8) + 18):16; 
                     src_port:16; new_dst_port:16; new_isn:32; 
                     header3:64:bitstring; 0:16;                     
                     tcp_body:(Bitstring.bitstring_length tcp_body):bitstring}))
            in
              BITSTRING{local_mac:48:string; gw_mac:48:string;
                        header:20:bitstring; ihl:4; 
                        tos:8; tlen:16; ipid:16; flags:3; fragoff:13;
                        ttl:8; proto:8; ip_chk:16:littleendian; gw_ip:32; local_ip:32; 
                        header2:(ihl-5)*32:bitstring; src_port:16; new_dst_port:16; 
                        new_isn:32; header3:64:bitstring;  tcp_chk:16:littleendian;
                        tcp_body:(Bitstring.bitstring_length tcp_body):bitstring}
      | { _ } -> invalid_arg("gen_server_syn input packet is not TCP") 

let gen_server_ack isn ack dst_port src_port m =
  let eth_hdr = BITSTRING{local_mac:48:string; gw_mac:48:string; 0x0800:16} in 
  let ip_chk = Checksum.ones_complement (BITSTRING { 4:4; 5:4; 0:8; 40:16; 0:16; 0:3; 
        0:13; 64:8; 6:8; 0:16; gw_ip:32; local_ip:32}) in
  let ipv4_hdr = BITSTRING { 4:4; 5:4; 0:8; 40:16; 0:16; 0:3; 0:13; 64:8; 6:8; 
                             ip_chk:16:littleendian; gw_ip:32; local_ip:32} in
  let tcp_chk = 
    (Checksum.ones_complement (BITSTRING{gw_ip:32; local_ip:32; 0:8; 6:8; 
        20:16; src_port:16; dst_port:16; isn:32;  ack:32; 5:4;
        0:6; false:1; true:1; false:1; false:1; false:1; false:1; 0xffff:16;0:16;
        0:16})) in 
  let tcp_hdr = BITSTRING {src_port:16; dst_port:16; isn:32; ack:32; 5:4;
        0:6; false:1; true:1; false:1; false:1; false:1; false:1; 
        0xffff:16; tcp_chk:16:littleendian; 0:16 } in  
    Bitstring.concat [eth_hdr; ipv4_hdr; tcp_hdr;] 

let gen_server_synack isn ack dst_port src_port src_ip =
  let eth_hdr = BITSTRING{local_mac:48:string; gw_mac:48:string; 0x0800:16} in 
  let ip_chk = Checksum.ones_complement (BITSTRING { 4:4; 5:4; 0:8; 40:16; 0:16; 0:3; 
        0:13; 64:8; 6:8; 0:16; src_ip:32; local_ip:32}) in
  let ipv4_hdr = BITSTRING { 4:4; 5:4; 0:8; 40:16; 0:16; 0:3; 0:13; 64:8; 6:8; 
                             ip_chk:16:littleendian; src_ip:32; local_ip:32} in
  let tcp_chk = 
    (Checksum.ones_complement (BITSTRING{src_ip:32; local_ip:32; 0:8; 6:8; 
        20:16; src_port:16; dst_port:16; isn:32;  ack:32; 5:4;
        0:6; false:1; true:1; false:1; false:1; true:1; false:1; 0xffff:16;0:16;
        0:16})) in 
  let tcp_hdr = BITSTRING {src_port:16; dst_port:16; isn:32; ack:32; 5:4;
        0:6; false:1; true:1; false:1; false:1; true:1; false:1; 
        0xffff:16; tcp_chk:16:littleendian; 0:16 } in  
    Bitstring.concat [eth_hdr; ipv4_hdr; tcp_hdr;] 

let gen_tcp_data_pkt isn ack dst_port src_port data =
  let eth_hdr = BITSTRING{local_mac:48:string; gw_mac:48:string; 0x0800:16} in 
  let ip_chk = Checksum.ones_complement (BITSTRING { 4:4; 5:4; 0:8; 
        (40 + ((Bitstring.bitstring_length data)/8)):16; 0:16; 0:3; 
        0:13; 64:8; 6:8; 0:16; gw_ip:32; local_ip:32}) in
  let ipv4_hdr = BITSTRING { 4:4; 5:4; 0:8; 
    (40 + ((Bitstring.bitstring_length data)/8)):16; 0:16; 0:3; 0:13; 
    64:8; 6:8; ip_chk:16:littleendian; gw_ip:32; local_ip:32} in
  let tcp_chk = 
    (Checksum.ones_complement (BITSTRING{gw_ip:32; local_ip:32; 0:8; 6:8; 
        (20+((Bitstring.bitstring_length data)/8)):16; src_port:16; dst_port:16; 
        isn:32;  ack:32; 5:4; 0:6; false:1; true:1; false:1; false:1; false:1; 
        false:1; 0xffff:16;0:16; 0:16; 
        data:(Bitstring.bitstring_length data):bitstring})) in 
  let tcp_hdr = BITSTRING {src_port:16; dst_port:16; isn:32; ack:32; 5:4;
        0:6; false:1; true:1; false:1; false:1; false:1; false:1; 
        0xffff:16; tcp_chk:16:littleendian; 0:16;
        data:(Bitstring.bitstring_length data):bitstring} in  
    Bitstring.concat [eth_hdr; ipv4_hdr; tcp_hdr;] 
