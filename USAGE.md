API
===

The source code contains 3 main libraries: Openflow, Openflow.Switch and
Openflow.Flv.

The Openflow module contains all the code to parse, print and generate openflow
messages, as well as, a basic openflow control platform.  The ofcontroller
implements an openflow controller library. The library is event driven.  The
programmer can access any openflow message by registering event callback during
the init phase of the code for every connected switch. The parsing uses cstruct.t objects.

The Openflow.Switch module implements an openflow switch. The module exposes a simple API through
which a user can add and remove ports to the controller and run the default openflow
processing switch functionality. In addition, the module contains an
out-of-channel mechanism to modify the state of the switch using a json-rpc
mechanism and insert, delete and view flow entries or enable or disable network
ports. Finally, the switch provides a standalone mode, when the controller
becomes unavailable, using a local learning switch logic, implemented in module
Openflow.Switch.Ofswitch_standalone. Standalone functionality can be initiated
through the Ofswitch.standalone_connect method.

Additionally the library contains a small number of helper functions that enhance the
functionality of openflow application. Ofswitch_config is a daemon that exposes a json
API through which other apps can have configure the ports of the switch and access the
content of the datapath table, using a simple tcp socket. Ofswitch_standalone is a minimum
learning switch implementation over openflow that can be enabled on the switch module when
no controller is accessible.

The Openflow.Flv library reimplements the functionality provided by the flowvisor
switch virtualisation software. FLowvisor is able to aggregate multiple switches
and expose them to controller as a single switch, aggregating all the ports of
the switches. The module provides elementary slicing
functionality using wildcards. Additionally, the module implements a simple
switch topology discovery mechanism using the lldp protocol. The functionality
of this module is currently experimental and the library is not fully
functional (e.g. OP.Port.Flood output actions are not supported by the module ).

Programs
=======

The source code of the library contains a number of small appliances that provide simple
examples over the functionality of the library.

lwt_switch
================

This is a unix backend implementation of an openflow switch. The application exposes both
the json config web service and uses the standalone embedded controller. The application
tries to connect to locahost in order to connect to controller and also run the json-based
configuration daemon on port 6634.

lwt_controller
==========

An openflow controller that implements a simple openflow-based learning switch.
The program listens on port 6633.

ofswitch_ctrl
============

This is a simple implementation of a configuration client for the switch code.
The application has a lot of similarities with the syntax of the ovs-vsctl code.
Users can access and modify the state of the switch with the following command
line parameters:

* dump-flows intf tupple: this command will match all flows on the forwardign
  table of the switch and return a dump of the matching flows to the
  provided tupple.
* del-flows intf tupple: delete matching flows.
* add-flows intf tupple: adding a tupple to the flow table.
* add-port intf network_device : adding a port under the control of the openflow switch.
* del-port intf network_device : remove a port from the control of the switch.

ofswitch.xen
===============

A unikernel appliance of the lwt_switch for the xen backend.

ofcontroller.xen
==============

A unikernel application of the lwt_controller for the xen backend.
