API
===

The source code contains 3 main modules: ofpacket, ofswitch and ofcontroller.

The ofpacket module contains all the code to parse, print and generate openflow messages.
The parsing uses cstruct.t objects. 

The ofcontroller implements an openflow controller library. The library is event driven.
The programmer can access any openflow message by registering event callback during 
the init phase of the code for every connected switch. 

The ofswitch code implements an openflow switch. The module exposes a simple API through
which a user can add and remove ports to the controller and run the default openflow 
processing switch functionality. 

Additionally the library contains a small number of helper functions that enhance the 
functionality of openflow application. Ofswitch_config is a daemon that exposes a json
API through which other apps can have configure the ports of the switch and access the
content of the datapath table, using a simple tcp socket. Ofswitch_standalone is a minimum
learning switch implementation over openflow that can be enabled on the switch module when
no controller is accessible. 

Programs
=======

The source code of the library contains a number of small appliances that provide simple
examples over the functionality of the library. 

basic_switch_lwt
================

This is a unix backend implementation of an openflow switch. The application exposes both
the json config web service and uses the standalone embedded controller. The application 
tries to connect to locahost in order to connect to controller and also run the json-based
configuration daemon on port 6634. 

ovs-vsctl
=========

This is a simple implementation of a configuration client for the switch code. The application 
has a lot of similarities with the syntax of the ovs-vsctl code. Uses can access and modify the
state of the switch with the following command line parameters:

* dump-flows intf tupple: this command will match all flows on the forwardign table of the switch
and return a dump of the matching flows to the provided tupple.
* del-flows intf tupple: delete matching flows.
* add-flows intf tupple: adding a tupple to the flow table.
* add-port intf network_device : adding a port under the control of the openflow switch.
* del-port intf network_device : remove a port from the control of the switch. 

learning_switch
===============

An openflow controller that implements a simple openflow-based learning switch. The program listens
on port 6633. 
