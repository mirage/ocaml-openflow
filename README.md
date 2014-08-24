Mirage OpenFlow Implementation
==============================

OpenFlow is a switching standard and open protocol  enabling
distributed control of the flow tables contained within Ethernet
switches in a network. Each OpenFlow switch has three parts:

+ A **datapath**, containing a *flow table*, associating set of
  *actions* with each flow entry;
+ A **secure channel**, connecting to a controller; and
+ The **OpenFlow protocol**, used by the controller to talk to
  switches.

Following this standard model, the implementation comprises three parts:

* `Openflow` library, contains a complete parsing library in pure Ocaml and a
  minimal controller library using an event-driven model.
* `Openflow.switch` library, provides a skeleton OpenFlow switch supporting most
  elementary switch functionality.
* `Openflow.flv` library, implements a basic FLowVisor reimplementation in
  ocaml.

__N.B.__ _There are two versions of the OpenFlow protocol: v1.0.0 (`0x01` on
the wire) and v1.1.0 (`0x02` on the wire).  The implementation supports wire
protocol `0x01` as this is what is implemented in [Open vSwitch][ovs-1.2],
used for debugging._

Openflow.Ofpacket
-----------

The file begins with some utility functions, operators, types.  The
bulk of the code is organised following the v1.0.0
[protocol specification][of-1.0], as implemented by
[Open vSwitch v1.2][ovs-1.2].  Each set of messages is contained
within its own module, most of which contain a type `t` representing
the entity named by the module, plus relevant parsers to convert a
bitstring to a type (`parse_*`) and pretty printers for the type
(`string_of_*`).  At the end of the file, in the root `Ofpacket`
module scope, are definitions for interacting with the protocol as a
whole, e.g., error codes, OpenFlow message types and standard header,
root OpenFlow parser, OpenFlow packet builders.

### Queue, Port, Switch

The `Queue` module is really a placeholder currently.  OpenFlow
defines limited quality-of-service support via a simple queueing
mechanism.  Flows are mapped to queues attached to ports, and each
queue is then configured as desired.  The specification currently
defines just a minimum rate, although specific implementations may
provide more.

The `Port` module wraps several port related elements:

+ _t_, where that is either simply the index of the port in the
  switch, or the special indexes (> 0xff00) representing the
  controller, flooding, etc.
+ _config_, a specific port's configuration (up/down, STP
  supported, etc).
+ _features_, a port's feature set (rate, fiber/copper,
  etc).
+ _state_, a port's current state (up/down, STP learning mode, etc).
+ _phy_, a port's physical details (index, address, name, etc).
+ _stats_, current statistics  of the port (packet and byte counters,
  collisions, etc).
+ _reason_ and _status_, for reporting changes to a port's
  configuration; _reason_ is one of `ADD|DEL|MOD`.

Finally, `Switch` wraps elements pertaining to a whole switch, that is
a collection of ports, tables (including the _group table_), and the
connection to the controller.

+ _capabilities_, the switch's capabilities in terms of supporting IP
  fragment reassembly, various statistics, etc.
+ _action_, the types of action the switch's ports support (setting
  various fields, etc).
+ _features_, the switch's id, number of buffers, tables, port list etc.
+ _config_, for masking against handling of IP fragments: no special
  handling, drop, reassemble.

### Wildcards, Match, Flow

The `Wildcards` and `Match` modules both simply wrap types
respectively representing the fields to wildcard in a flow match, and
the flow match specification itself.

The `Flow` module then contains structures representing:

+ _t_, the flow itself (its age, activity, priority, etc); and
+ _stats_, extended statistics association with a flow identified by a
  64 bit  `cookie`.

### Packet_in, Packet_out

These represent messages associated with receipt or transmission of a
packet in response to a controller initiated action.

`Packet_in` is used where a packet arrives at the switch and is
forwarded to the controller, either due to lack of matching entry, or
an explicit action.

`Packet_out` contains the structure used by the controller to indicate
to the switch that a packet it has been buffering must now have some
actions performed on it, typically culminating in it being forward out
of one or more ports.

### Flow_mod, Port_mod

These represent modification messages to existing flow and port state
in the switch.

### Stats

Finally, the `Stats` module contains structures representing the
different statistics messages available through OpenFlow, as well as
the request and response messages that transport them.

[of-1.0]: http://www.openflow.org/documents/openflow-spec-v1.0.0.pdf
[of-1.1]: http://www.openflow.org/documents/openflow-spec-v1.1.0.pdf
[ovs-1.2]: http://openvswitch.org/releases/openvswitch-1.2.2.tar.gz

Openflow.Ofsocket
-------------

A simple module to create an openflow channel abstraction over a serires of
different transport mechanisms. At the moment the library contains support of
Channel.t connections and Lwt_stream streams. The protocol ensures to read from
the socket full Openflow pdus and transform them to appropriate Ofpacket
structures.

Openflow.Ofontroller
-------------

Initially modelled after [NOX][], this is a skeleton controller
that provides a simple event based wrapper around the OpenFlow
protocol.  It currently provides the minimal set of  events
corresponding to basic switch operation:

+ `DATAPATH_JOIN`, representing the connection of a datapath  to the
  controller, i.e., notification of the existence of a switch.
+ `DATAPATH_LEAVE`, representing the disconnection of a datapath from
  the controller, i.e., notification of the destruction of a switch.
+ `PACKET_IN`, representing the forwarding of a packet to the
  controller, whether through an explicit action corresponding to a
  flow match, or simply as the default when flow match is found.
+ `FLOW_REMOVED`, i.e., representing the switch notification regarding the
  removal of a flow from the flow table.
+ `FLOW_STATS_REPLY`, i.e., represents the replies transmitted by the switch
  after a flow_stats_req.
+ `AGGR_FLOW_STATS_REPLY`, i.e., representing the reply transmitted by the switch
to an aggr_flow_stats_req.
+ ` DESC_STATS_REPLY`, i.e., representing the reply of a switch to desc_stats
  request.
+ `PORT_STATS_REPLY`, i.e., representing the replt of a switch to a port_stats
  request providing port level counter and the state of the switch.
+ `TABLE_STATS_REPLY`, i.e., representing the reply of a switch to a
  table_stats request.
+ `PORT_STATUS_REPLY`, i.e., representing the notification send by the switch
  when the state of a port of the switch is changed.

The controller state is mutable and modelled as:

+ A list of callbacks per event, each taking the current state, the
  originating datapath, and the event;
+ Mappings from switch (`datapath_id`) to a Mirage communications
  channel (`Channel.t`); and

The main work of the controller is carried out in `process_of_packet`
which processes each received packet within the context given by the
current state of the switch: this is where the OpenFlow state machine
is implemented.

The controller entry point is via the `listen`, `local_connect` or `connect`
function which effectively creates a receiving channel to parse OpenFlow
packets, and pass them to `process_of_packet` which handles a range of standard
protocol-level interactions, e.g., `ECHO_REQ`, `FEATURES_RESP`, generating
Mirage events as appropriate.  Specifically, `controller` is passed as callback
to the respective connection method, and recursively evaluates `read_packet` to
read the incoming packet and pass it to `process_of_packet`.

[nox]: http://noxrepo.org/


Openflow.Switch.Ofswitch
---------

An OpenFlow _switch_ or _datapath_ consists of a _flow table_, a _group table_
(in later versions, not supported in v1.0.0), and a _channel_ back to the
controller.  Communication over the channel is via the OpenFlow protocol, and is
how the controller manages the switch.

In short, each table contains flow entries consisting of _match fields_,
_counters_, and _instructions_ to apply to packets.  Starting with the first
flow table, if an incoming packet matches an entry, the counters are updated
and the instructions carried out.  If no entry in the first table matches,
(part of) the packet is forwarded to the controller, or it is dropped, or it
proceeds to the next flow table.

At the current point the switch doesn't support any queue principles.

Skeleton code is as follows:

### Entry

Represents a single flow table entry.  Each entry consists of:

+ _counters_, to keep statistics per-table, -flow, -port, -queue
  (`Entry.table_counter list`, `Entry.flow_counter list`, `Entry.port_counter
  list`, `Entry.queue_counter list`); and
+ _actions_, to perform on packets matching the fields (`Entry.action list`).

### Table

A simple module representing a table of flow entries.  Currently just an id
(`tid`) , a hashtbl of entries (`(OP.Match.t, Entry.t) Hashtbl.t`), a list of
exact match entries to reduce the lookup time for wildcard entries and a the
table counter.

### Switch

Encapsulating the switch (or datapath) itself.  Currently defines a _port_ as:

+ _details_, a physical port configuration (`Ofpacket.Port.phy`); and
+ _device_, some handle to the physical device (mocked out as a `string`).

The switch is then modelled as:

+ _ports_, a list of physical ports (`Switch.port list`);
+ _table_, the table of flow entries for this switch;
+ _stats_, a set of per-switch counters (`Switch.stats`); and
+ *p_sflow*, the probability in use when sFlow sampling.

Note that the vocabulary of a number of these changes with v1.1.0, in addition
to the table structure becoming more complex (support for chains of tables,
forwarding to tables, and the group table).

Questions/Notes
---------------

What's the best way to structure the controller so that application code can
introduce generation and consumption of new events?  NOX permits this within a
single event-handling framework -- is this simply out-of-scope here, or should
we have a separate event-based programming framework available, or is there a
straightforward Ocaml-ish way to incorporate this into the OpenFlow
Controller?

What's the best way to expose parsing as a separate activity to reading data
off the wire?  Specifically, I'd really like to reuse functions from
`Net.Ethif`, `Net.Ipv4`, etc to recover structure from the bitstring without
need to have `OfPacket.Match.parse_from_raw_packet`.  Previously I have found
having parsers that return structured data and then wrapping up the packet
structure as a nested type, e.g., `PCAP(pcaph, ETH(ethh, IPv4(iph, payload)))`
or `...TCP(tcph, payload))))` worked well, permitting fairly natural pattern
matching.  The depth to which the packet was deumltiplexed was controlled by a
parameter to the entry-point parser.

The `Switch` design is almost certainly very inefficient, and needs working
on.  This is waiting on implementation -- although sketched out, waiting on
network driver model to actually be able to get hold of physical devices and
frames.  When we can, also need to consider how to control packet parsing, and
demultiplexing of frames for switching from frames comprising the TCP stream
carrying the controller channel.  Ideally, it would be transparent to have
a `Channel` for the controller's OpenFlow messages  _and_ a per-device frame
handler for everything else.  That is, Mirage would do the necessary
demultiplexing -- but only what's necessary -- passing non-OpenFlow frames to
the switch to be matched, but reassembling the TCP flow carrying the
controller's OpenFlow traffic.
