(*
 * Copyright (c) 2011 Richard Mortier <mort@cantab.net>
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
open Net
open Nettypes

module OP = Ofpacket

let sp = Printf.sprintf
let pr = Printf.printf
let ep = Printf.eprintf
let cp = OS.Console.log

(* XXX should really standardise these *)
type uint16 = OP.uint16
type uint32 = OP.uint32
type uint64 = OP.uint64
type byte   = OP.byte
type eaddr  = OP.eaddr

type port = uint16
type cookie = uint64

type device = string (* XXX placeholder! *)

let resolve t = Lwt.on_success t (fun _ -> ())

module Entry = struct
  type table_counter = {
    n_active: uint32;
    n_lookups: uint64;
    n_matches: uint64;
  }

  type flow_counter = {
   mutable n_packets: uint64;
   mutable n_bytes: uint64;

   mutable priority: uint16;
   mutable cookie: int64;
   mutable insert_secs: uint32;
   mutable insert_nsecs: uint32;
   mutable last_secs: uint32;
   mutable last_nsec: uint32;
   mutable idle_timeout: int;
   mutable hard_timeout:int;
  }

  type port_counter = {
    mutable rx_packets: uint64;
    mutable tx_packets: uint64;
    mutable rx_bytes: uint64;
    mutable tx_bytes: uint64;
    mutable rx_drops: uint64;
    mutable tx_drops: uint64;
    mutable rx_errors: uint64;
    mutable tx_errors: uint64;
    mutable rx_alignment_errors: uint64;
    mutable rx_overrun_errors: uint64;
    mutable rx_crc_errors: uint64;
    mutable n_collisions: uint64;
  }

  type queue_counter = {
    tx_queue_packets: uint64;
    tx_queue_bytes: uint64;
    tx_queue_overrun_errors: uint64;
  }

  type counters = {
    per_table: table_counter list;
    per_flow: flow_counter list;
    per_port: port_counter list;
    per_queue: queue_counter list;
  }

  let init_flow_counters () =
    {n_packets=(Int64.of_int 0);
    n_bytes= (Int64.of_int 0);
    priority= 0; cookie= (Int64.of_int 0); 
    insert_secs=(Int32.of_float (OS.Clock.time()));
    insert_nsecs=0l;
    last_secs=(Int32.of_float (OS.Clock.time())); 
    last_nsec=0l;
        idle_timeout=0; hard_timeout=0;}
  let init_port_counters () ={
    rx_packets=0L; tx_packets=0L;
    rx_bytes=0L; tx_bytes=0L;
    rx_drops=0L; tx_drops=0L;
    rx_errors=0L; tx_errors=0L;
    rx_alignment_errors=0L;
    rx_overrun_errors=0L;
    rx_crc_errors=0L; n_collisions=0L;}
  
  type t = { 
    (* fields: OP.Match.t list; *)
    counters: flow_counter;
    actions: OP.Flow.action list;
  }

end

module Table = struct
  type t = {
    

    tid: cookie;

    (* This stores entries as they arrive *)
    mutable entries: (OP.Match.t, Entry.t ref) Hashtbl.t;

    (* This stores only exact match entries.*)
    (* TODO delete an entry from both tables *)
    mutable cache : (OP.Match.t, Entry.t ref) Hashtbl.t;
    mutable lookup: uint64;
    mutable missed: uint64;


  }
end

module Switch = struct
  type port = {
    (* details: OP.Port.phy; *)
    port_id: int;
    port_name: string;
    mgr: Net.Manager.t;
    counter: Entry.port_counter;
    (* device: device; *)
    (* port_id: (OS.Netif.id, int) Hashtbl.t; *)
    
  }

  type stats = {
    mutable n_frags: uint64;
    mutable n_hits: uint64;
    mutable n_missed: uint64;
    mutable n_lost: uint64;
  }

  type lookup_ret = 
         Found of Entry.t ref
       | NOT_FOUND

      type t = {
        (* Mapping Netif objects to ports *)
        mutable ports: (OS.Netif.id, port ref) Hashtbl.t;

        (* Mapping port ids to port numbers *)
        mutable int_ports: (int, port ref) Hashtbl.t;
        mutable port_feat : OP.Port.phy list;
        mutable controllers: (Net.Channel.t) list;
        table: Table.t;
        stats: stats;
        p_sflow: uint32; (** probability for sFlow sampling *)
        mutable errornum : uint32; 
        mutable portnum : int;
      }

      let bitstring_of_port port = 
        BITSTRING{ port.port_id:16; 0L:48; 
            port.counter.Entry.rx_packets:64; 
            port.counter.Entry.tx_packets:64; (* Receive/transmit bytes *)
            port.counter.Entry.rx_bytes:64; 
            port.counter.Entry.tx_bytes:64; 
            port.counter.Entry.rx_drops:64; 
            port.counter.Entry.tx_drops:64; 
            port.counter.Entry.rx_errors:64; 
            port.counter.Entry.tx_errors:64; 
            port.counter.Entry.rx_alignment_errors:64; 
            port.counter.Entry.rx_overrun_errors:64; 
            port.counter.Entry.rx_crc_errors:64; (* Receive/transmit packets *)
            port.counter.Entry.n_collisions:64}

      let forward_frame st in_port port frame pkt_size =
      Printf.printf "Outputing frame to port %s\n" (OP.Port.string_of_port
       port);
       if (frame == Bitstring.empty_bitstring) then 
           return ()
       else (
           match port with 
           | OP.Port.Port(port) -> 
                   if Hashtbl.mem st.int_ports port then(
                       let out_p = (!( Hashtbl.find st.int_ports port))  in
                       Net.Manager.send_raw out_p.mgr out_p.port_name [frame];
                       return ())
                   else
                    return (Printf.printf "Port %d not registered \n" port)
           | OP.Port.No_port -> return ()
           | OP.Port.Flood |OP.Port.All ->
                   return (Hashtbl.iter (fun port_id port -> 
                       if(port_id != (OP.Port.int_of_port in_port)) then
                           (Printf.printf "Sending packet on %d %d %s bits %d\n%!" port_id
                           (OP.Port.int_of_port in_port) (!port).port_name
                           (Bitstring.bitstring_length frame);
                           resolve(Net.Manager.send_raw (!port).mgr (!port).port_name
                           [frame]))
                   else
                       (Printf.printf "Not sending packet on %d %d\n%!" port_id
                       (OP.Port.int_of_port in_port);
                       ())
                       ) st.int_ports)
           | OP.Port.In_port ->
                   let port = (OP.Port.int_of_port in_port) in 
                   if Hashtbl.mem st.int_ports port then
                       let out_p = (!(Hashtbl.find st.int_ports port))  in
                       (Net.Manager.send_raw out_p.mgr out_p.port_name [frame];
                       return ())
                       else
                           return (Printf.printf "Port %d not registered \n" port)
                           (*           | Table
                            *           | Normal
                            *           | Controller -> generate a packet out. 
                            *           | Local -> can I inject this frame to the network
                            *           stack?  *)
           | _ -> return (Printf.printf "Not implemented output port\n")
       )

  let rec set_frame_bits frame start len bits = 
    match len with 
        (*TODO: Make the pattern match more accurate, read the match syntax*)
      | 0 -> return ()
      | len ->
        Bitstring.put frame (start + len - 1) (Bitstring.get bits (len - 1));
        set_frame_bits frame start (len-1) bits


   let rec apply_of_actions st in_port actions frame = 
    match actions with 
      | [] -> return ()
      | head :: actions ->
        match head with
          | OP.Flow.Output (port, pkt_size) ->
              forward_frame st in_port port frame pkt_size; 
              apply_of_actions st in_port actions frame
          | OP.Flow.Set_dl_src(eaddr) ->
             (* Printf.printf "setting src mac addr to %s\n" (OP.eaddr_to_string
              * eaddr); *)
              set_frame_bits frame 48 48 (OP.bitstring_of_eaddr eaddr);
              apply_of_actions st in_port actions frame
          | OP.Flow.Set_dl_dst(eaddr) ->
             (* Printf.printf "setting dst mac addr to %s\n" (OP.eaddr_to_string
              * eaddr); *)
              set_frame_bits frame 0 48 (OP.bitstring_of_eaddr eaddr);
              apply_of_actions st in_port actions frame
          | _ ->
              (Printf.printf "Unsupported action\n");
              apply_of_actions st in_port actions frame

   let errornum = ref 1 

   let lookup_flow st of_match =
       (* Check first the match table cache *)
        if (Hashtbl.mem st.table.Table.cache of_match ) then (
            let entry = (Hashtbl.find st.table.Table.cache of_match) in
                Found(entry) 
        ) else (
            let ret_lst = ref [] in 
            let lookup_flow flow entry =
                if (OP.Match.flow_match_compare of_match flow
                flow.OP.Match.wildcards) then (
                    Printf.printf "Found flow %s in cache \n%!"
                    (OP.Match.match_to_string of_match);
                    ret_lst := (!ret_lst) @ [entry]
                )
            in
                Hashtbl.iter lookup_flow st.table.Table.entries;
                if (List.length (!ret_lst) == 0) then 
                    NOT_FOUND
                else ( 
                    Printf.printf "Found flow %s in table\n%!"
                    (OP.Match.match_to_string of_match);
                    let flow_ref = (List.hd (!ret_lst)) in 
                    Hashtbl.add st.table.Table.cache of_match flow_ref;
                    Found(flow_ref)
                )
        )

        (* Check the wilcard card table *)
 
end

let st = Switch.(
  { ports = (Hashtbl.create 0); int_ports = (Hashtbl.create 0);
  table = Table.({ tid = 0_L; entries = (Hashtbl.create 0); cache = (Hashtbl.create 0); 
        missed = 0L; lookup=0L;});
    stats = { n_frags=0_L; n_hits=0_L; n_missed=0_L; n_lost=0_L };
    p_sflow = 0_l; controllers=[]; port_feat = []; errornum = 0l;
    portnum=0;
  })

let add_flow tuple actions =
    Printf.printf "adding flow %s %s\n%!" (OP.Match.match_to_string tuple) (OP.Flow.string_of_actions actions);
  if (Hashtbl.mem st.Switch.table.Table.entries tuple) then
    Printf.printf "Tuple already exists" 
  else
    Hashtbl.add st.Switch.table.Table.entries tuple (ref Entry.({actions; counters=(init_flow_counters ())}))


let process_frame intf_name frame = 
  (* roughly,
   * + examine frame
   * + extract OF headers
   * + match against st.table
   *   + if match, update counters, execute actions
   *   + else, forward to controller/drop, depending on config
   *)
  if (Hashtbl.mem st.Switch.ports intf_name ) then
    let p = (!(Hashtbl.find st.Switch.ports intf_name)) in
    let in_port = (OP.Port.port_of_int p.Switch.port_id) in (* Hashtbl.find   in *)
    let tupple = (OP.Match.parse_from_raw_packet in_port frame ) in
    (* Update port rx statistics *)
    p.Switch.counter.Entry.rx_packets <- (Int64.add p.Switch.counter.Entry.rx_packets 1L); 
    p.Switch.counter.Entry.rx_bytes <- (Int64.add p.Switch.counter.Entry.rx_bytes 
    (Int64.of_int ((Bitstring.bitstring_length frame)/8)));

    (* Lookup packet flow to existing flows in table *)
    let entry = (Switch.lookup_flow st tupple) in 
    match entry with 
    | Switch.NOT_FOUND ->
            (* Update missed counter *)
            st.Switch.table.Table.missed <- (Int64.add
            st.Switch.table.Table.missed 1L);
(*
            let addr = "\x11\x11\x11\x11\x11\x11" in 
            add_flow tupple [(OP.Flow.Set_dl_src (addr));
            (OP.Flow.Set_dl_dst (addr));
            (OP.Flow.Output ((OP.Port.port_of_int 2),  2000)) ; ];

*)
            let pkt_in = (OP.Packet_in.bitstring_of_pkt_in ~port:in_port
            ~reason:OP.Packet_in.NO_MATCH ~bits:frame ()) in 
            return (List.iter (fun t -> (Channel.write_bitstring t pkt_in) >>
            Channel.flush t; ()) st.Switch.controllers)
                (* generate a packet in event *)
    | Switch.Found(entry) ->
            st.Switch.table.Table.lookup <- (Int64.add
            st.Switch.table.Table.lookup 1L);
            (!entry).Entry.counters.Entry.n_packets <- (Int64.add
            (!entry).Entry.counters.Entry.n_packets 1L);
            (!entry).Entry.counters.Entry.n_bytes <- (Int64.add
            (!entry).Entry.counters.Entry.n_bytes  (Int64.of_int
            ((Bitstring.bitstring_length frame)/8)));
            (!entry).Entry.counters.Entry.last_secs = (Int32.of_float (OS.Clock.time ()));
            Switch.apply_of_actions st tupple.OP.Match.in_port (!entry).Entry.actions frame
  else 
      return ()


let add_port sw mgr intf = 
  Net.Manager.intercept intf process_frame;
  st.Switch.portnum <- st.Switch.portnum + 1;
  Printf.printf "Adding port %d (%s)\n %!" st.Switch.portnum
  (Net.Manager.get_intf intf); 
  let port =  (ref Switch.({port_id= st.Switch.portnum; mgr=mgr; 
                       port_name=(Net.Manager.get_intf intf);
                        counter=(Entry.init_port_counters ())})) in  
    Hashtbl.add sw.Switch.int_ports st.Switch.portnum port; 
    Hashtbl.add sw.Switch.ports (!port).Switch.port_name port;
    sw.Switch.port_feat <- sw.Switch.port_feat @ 
    [(OP.Port.init_port_phy ~port_no:st.Switch.portnum ~name:((!port).Switch.port_name) () )]

let process_openflow st =
  (* this is processing from a Channel, so roughly
   * + read OpenFlow message off Channel,
   * + match message and handle accordingly, possibly emitting one or more
   *   messages as a result
   *)

  return ()

type endhost = {
  ip: Nettypes.ipv4_addr;
  port: int;
}

let process_of_packet state (remote_addr, remote_port) ofp t bits = 
    (* let ep = { ip=remote_addr; port=remote_port } in *)
    match ofp with
      | OP.Hello (h, _) (* Reply to HELLO with a HELLO and a feature request *)
        -> (cp "HELLO";
            Channel.write_bitstring t (OP.Header.build_h h) 
            >> Channel.write_bitstring t (OP.build_features_req 0_l) 
            >> Channel.flush t)
      | OP.Echo_req (h, bs)  (* Reply to ECHO requests *)
        -> (cp "ECHO_REQ";
            Channel.write_bitstring t (OP.build_echo_resp h bs)
            >> Channel.flush t)
      | OP.Features_req (h)  
      -> (cp "FEAT_REQ";
          Channel.write_bitstring t 
           (OP.Switch.gen_reply_features h Int64.one st.Switch.port_feat)
           >> Channel.flush t)
      | OP.Stats_req(h, req) 
        -> (
            let xid = h.OP.Header.xid in 
            cp "STATS_REQ\n%!";
          ( match req with
            | OP.Stats.Desc_req(req) ->
                let desc = "Mirage" in 
                let resp_h = (OP.Header.create OP.Header.STATS_RESP 
                  (OP.Header.get_len+  4 + 256 + 256 + 256 + 32 + 256) 
                  h.OP.Header.xid) in 
                let desc_data = BITSTRING{ (OP.Header.build_h resp_h):OP.Header.get_len*8:bitstring; 0:16; 0:16;
                  (Printf.sprintf "%s%s" desc (String.make (256-(String.length desc)) (Char.chr 0))):256*8:string;
                  (Printf.sprintf "%s%s" desc (String.make (256-(String.length desc)) (Char.chr 0))):256*8:string;
                  (Printf.sprintf "%s%s" desc (String.make (256-(String.length desc)) (Char.chr 0))):256*8:string;
                  (Printf.sprintf "%s%s" "0.1" (String.make 29 (Char.chr 0))):32*8:string;
                  (Printf.sprintf "%s%s" desc (String.make (256-(String.length desc)) (Char.chr 0))):256*8:string } in
                Channel.write_bitstring t desc_data >>
                Channel.flush t 
            | OP.Stats.Flow_req(req_h, of_match, table_id, out_port) ->
                let flow_lst_resp = ref ([(BITSTRING{1:16; 0:16})]) in
                let flow_lst_len = ref 32 in 
                let match_flows of_match key value =
                    if (OP.Match.flow_match_compare key of_match of_match.OP.Match.wildcards) then (
                        let actions = (OP.Flow.bitstring_of_actions value.Entry.actions) in 
                        let flow = (BITSTRING{(
                            2+1+1+(OP.Match.get_len)+4+4+2+2+2+6+8+8+8+0+((Bitstring.bitstring_length actions)/8)):16; 
                            1:8; 0:8; 
                            (OP.Match.match_to_bitstring key):OP.Match.get_len*8:bitstring;
                            (Int32.sub (Int32.of_float (OS.Clock.time()))
                            value.Entry.counters.Entry.insert_secs):32; 
                            0l:32; (*Make this more accurate *)
                            value.Entry.counters.Entry.idle_timeout:16; 
                            value.Entry.counters.Entry.hard_timeout:16; 
                            value.Entry.counters.Entry.priority:16; 
                            0L:48; 
                            value.Entry.counters.Entry.cookie:64; 
                            value.Entry.counters.Entry.n_packets:64;
                            value.Entry.counters.Entry.n_bytes:64; 
                            actions:(Bitstring.bitstring_length actions):bitstring}) in
                        flow_lst_len := (!flow_lst_len) + (Bitstring.bitstring_length flow);
                        flow_lst_resp := (!flow_lst_resp) @ [flow]
                    )
                in
                    Hashtbl.iter (fun key value -> match_flows of_match key (!value)) st.Switch.table.Table.entries;
                    let resp_h = (OP.Header.create OP.Header.STATS_RESP (OP.Header.get_len + ((!flow_lst_len)/8)) 
                        h.OP.Header.xid) in
                    let desc_data = (Bitstring.concat ([(OP.Header.build_h resp_h)] @ (!flow_lst_resp))) in 
                    Channel.write_bitstring t desc_data >>  
                    Channel.flush t
            | OP.Stats.Aggregate_req (req_h, of_match, table, port) -> 
                let aggr_flow_bytes = ref 0L in
                let aggr_flow_pkts = ref 0L in
                let aggr_flows = ref 0l in
                
                let match_flows_aggr of_match key value =
                     if (OP.Match.flow_match_compare key of_match of_match.OP.Match.wildcards) then (
                         aggr_flows := Int32.add (!aggr_flows) 1l ;
                         aggr_flow_bytes := Int64.add (!aggr_flow_bytes) 
                         value.Entry.counters.Entry.n_bytes; 
                         aggr_flow_pkts := Int64.add (!aggr_flow_pkts)
                         value.Entry.counters.Entry.n_packets
                     ) in 

                Hashtbl.iter (fun key value -> match_flows_aggr of_match key (!value))
                    st.Switch.table.Table.entries;
                let resp_h = (OP.Header.create OP.Header.STATS_RESP
                    (OP.Header.get_len + 4 + 8 + 8 +8) h.OP.Header.xid) in
                let data_snd = (BITSTRING{ (OP.Header.build_h resp_h):OP.Header.get_len*8:bitstring; 
                    2:16; 0:16; (!aggr_flow_bytes):64; (!aggr_flow_pkts):64; (!aggr_flows):32; 0l:32}) in 
                Channel.write_bitstring t data_snd >> 
                Channel.flush t
            | OP.Stats.Table_req(req) ->
                let resp_h = (OP.Header.create OP.Header.STATS_RESP
                    (OP.Header.get_len + 4 + 1+3+32+4+4+4+8+8) 
                    h.OP.Header.xid) in                    
                let name = "flow_table" in 
                let desc_data =  BITSTRING{ 
                    (OP.Header.build_h resp_h):OP.Header.get_len*8:bitstring; 
                    3:16; 0:16; 1:8; 0:24;
                    (Printf.sprintf "%s%s" name (String.make (32-(String.length name)) 
                                                (Char.chr 0))):32*8:string;
                    (OP.Wildcards.wildcard_to_bitstring OP.Wildcards.full_wildcard):32:bitstring; 
                    0xFFFFFl:32; (Int32.of_int (Hashtbl.length
                    st.Switch.table.Table.entries)):32; 0L:64; 0L:64} in
                Channel.write_bitstring t desc_data >>  
                Channel.flush t
            | OP.Stats.Port_req(req_h, port) ->
                match port with
                | OP.Port.No_port -> ( 
                    let resp_h = (OP.Header.create OP.Header.STATS_RESP
                        (OP.Header.get_len + 4 + 104*(Hashtbl.length st.Switch.ports)) h.OP.Header.xid) in
                    let ports_data = ref [] in 
                        Hashtbl.iter (fun _ port -> ports_data := (!ports_data) @
                        [(Switch.bitstring_of_port (!port))]) st.Switch.ports;
                    let desc_data =  (Bitstring.concat 
                        ([(OP.Header.build_h resp_h); (BITSTRING{4:16; 0:16});]@(!ports_data)))  in
                    Channel.write_bitstring t desc_data >>  
                    Channel.flush t)
                | _ -> (
                    if ( not (Hashtbl.mem st.Switch.int_ports
                    (OP.Port.int_of_port port))) then
                        Channel.write_bitstring t (OP.bitstring_of_error
                        OP.QUEUE_OP_BAD_PORT bits  h.OP.Header.xid) >>  Channel.flush t
                    else (
                        let out_p = (!( Hashtbl.find st.Switch.int_ports (OP.Port.int_of_port port)))  in
                        let resp_h = (OP.Header.create OP.Header.STATS_RESP
                            (OP.Header.get_len + 4 + 104) h.OP.Header.xid) in
                         Channel.write_bitstring t (Bitstring.concat 
                        ([(OP.Header.build_h resp_h); (BITSTRING{4:16; 0:16});
                        Switch.bitstring_of_port out_p])) >>
                         Channel.flush t
                    )
                )
          | _ ->  
                Channel.write_bitstring t (OP.bitstring_of_error OP.REQUEST_BAD_TYPE bits xid) >>
                Channel.flush t
          )
        )          
      | OP.Get_config_req(h) 
        -> let resp = OP.Switch.init_switch_config in
            Channel.write_bitstring t (OP.Switch.bitstring_of_switch_config 
                                         h.OP.Header.xid resp) >>
            Channel.flush t
      | OP.Barrier_req(h) ->
            let resp_h = (OP.Header.create OP.Header.BARRIER_RESP
                        (OP.Header.get_len) h.OP.Header.xid) in
                 Channel.write_bitstring t (OP.Header.build_h resp_h) >>
                  Channel.flush t
      | OP.Packet_out(h, pkt) ->
              Printf.printf "packet out received with actions : %s \n%!"
              (OP.Flow.string_of_actions pkt.OP.Packet_out.actions);
              (Switch.apply_of_actions st pkt.OP.Packet_out.in_port
              pkt.OP.Packet_out.actions pkt.OP.Packet_out.data )
      | OP.Flow_mod(h,fm) 
        -> Printf.printf "Flow modification received\n";
           let of_match = fm.OP.Flow_mod.of_match in 
           let of_actions = fm.OP.Flow_mod.actions in
            (Printf.printf "need to insert rule %s actions %s" 
               (OP.Match.match_to_string of_match) 
               (OP.Flow.string_of_actions fm.OP.Flow_mod.actions));
            add_flow of_match of_actions; 
             (* if(Hashtbl.mem of_match st.Switch. ) *)
           return ()
      | _ -> 
            Channel.write_bitstring t 
            (OP.bitstring_of_error OP.REQUEST_BAD_TYPE bits  1l) >>
            Channel.flush t
  

let rec rd_data len t = 
  match len with
    | 0 -> return Bitstring.empty_bitstring
    | _ -> lwt data = (Channel.read_some ~len:len t) in 
           let nbytes = ((Bitstring.bitstring_length data)/8) in
           lwt more_data = (rd_data (len - nbytes) t) in
           return (Bitstring.concat [ data; more_data ])


let listen mgr loc init =
  init mgr st; 
  let controller (remote_addr, remote_port) t =
    let rs = Nettypes.ipv4_addr_to_string remote_addr in
    Log.info "OpenFlow Controller" "+ %s:%d" rs remote_port; 
      st.Switch.controllers <- (st.Switch.controllers @ [t]);
    let rec echo () =
      try_lwt
        lwt hbuf = Channel.read_some ~len:OP.Header.get_len t in
          ((Bitstring.bitstring_length hbuf)/8); 
        let ofh  = OP.Header.parse_h hbuf in
        let dlen = ofh.OP.Header.len - OP.Header.get_len in 
        lwt dbuf = rd_data dlen t in
        let ofp  = OP.parse ofh dbuf in
        process_of_packet st (remote_addr, remote_port) ofp t
        (Bitstring.concat [hbuf; dbuf]) 
        >> echo ()
      with
        | Nettypes.Closed -> 
            (* TODO Need to remove the t from st.Switch.controllers *)
            return ()
        | OP.Unparsed (m, bs) -> (pr "# unparsed! m=%s\n %!" m); echo ()

    in echo () 
  in
  Channel.listen mgr (`TCPv4 (loc, controller))

