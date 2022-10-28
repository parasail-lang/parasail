------------------------------------------------------------------------------
--                          ZMQ Zyre interface                              --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation. See           --
-- documentation/COPYING3 and documentation/GCC_RUNTIME3_1 for details.     --
------------------------------------------------------------------------------

--  Package defining an interface to the Zyre/ZeroMQ libraries

with ZMQ.String_Zlists;
with ZMQ.Zmsgs;
with ZMQ.Zsocks;
with Interfaces;
private with zyre_library_h;
package ZMQ.Zyre is
   --  Interfaces to Zyre operations

   type Zyre_Node is limited private;

   --  Move a node handle from one Zyre_Node object to another.
   procedure Zyre_Node_Move (From : in out Zyre_Node; To : in out Zyre_Node);

   --  Constructor, creates a new Zyre node. Note that until you start the
   --  node it is silent and invisible to other nodes on the network.
   --  The node name is provided to other nodes during discovery. If you
   --  specify NULL, Zyre generates a randomized node name from the UUID.
   --  ZYRE_EXPORT zyre_t *
   --      zyre_new (const char *name);
   function Zyre_New (Name : String) return Zyre_Node;
   
   --  Destructor, destroys a Zyre node. When you destroy a node, any
   --  messages it is sending or receiving will be discarded.
   --  ZYRE_EXPORT void
   --      zyre_destroy (zyre_t **self_p);
   procedure Zyre_Destroy (Self : in out Zyre_Node);
   
   --  Return our node UUID string, after successful initialization
   --  ZYRE_EXPORT const char *
   --      zyre_uuid (zyre_t *self);
   function Zyre_UUID (Self : Zyre_Node) return String;
   
   --  Return our node name, after successful initialization. First 6
   --  characters of UUID by default.
   --  ZYRE_EXPORT const char *
   --      zyre_name (zyre_t *self);
   function Zyre_Name (Self : Zyre_Node) return String;
   
   --  Set the public name of this node overriding the default. The name is
   --  provide during discovery and come in each ENTER message.
   --  ZYRE_EXPORT void
   --      zyre_set_name (zyre_t *self, const char *name);
   procedure Set_Zyre_Name (Self : in out Zyre_Node; Name : String);
   
   --  Set node header; these are provided to other nodes during discovery
   --  and come in each ENTER message.
   --  ZYRE_EXPORT void
   --      zyre_set_header (zyre_t *self, const char *name,
   --         const char *format, ...) CHECK_PRINTF (3);
   procedure Zyre_Set_Header (Self : in out Zyre_Node; Name : String;
                              Header : String);

   --  Set verbose mode; this tells the node to log all traffic as well as
   --  all major events.
   --  ZYRE_EXPORT void
   --      zyre_set_verbose (zyre_t *self);
   procedure Zyre_Set_Verbose (Self : in out Zyre_Node);
   
   --  Set UDP beacon discovery port; defaults to 5670, this call overrides
   --  that so you can create independent clusters on the same network, for
   --  e.g. development vs. production. Has no effect after zyre_start().
   --  ZYRE_EXPORT void
   --      zyre_set_port (zyre_t *self, int port_nbr);
   procedure Zyre_Set_Port (Self : in out Zyre_Node; Port_Nbr : Integer);
   
   --  Set the peer evasiveness timeout, in milliseconds. Default is 5000.
   --  This can be tuned in order to deal with expected network conditions
   --  and the response time expected by the application. This is tied to
   --  the beacon interval and rate of messages received.
   --  ZYRE_EXPORT void
   --      zyre_set_evasive_timeout (zyre_t *self, int interval);
   procedure Zyre_Set_Evasive_Timeout
     (Self : in out Zyre_Node; Interval : Integer);
   
   --  Set the peer silence timeout, in milliseconds. Default is 5000.
   --  This can be tuned in order to deal with expected network conditions
   --  and the response time expected by the application. This is tied to
   --  the beacon interval and rate of messages received.
   --  Silence is triggered one second after the timeout if peer has not
   --  answered ping and has not sent any message.
   --  NB: this is currently redundant with the evasiveness timeout. Both
   --  affect the same timeout value.
   --  ZYRE_EXPORT void
   --      zyre_set_silent_timeout (zyre_t *self, int interval);
   procedure Zyre_Set_Silent_Timeout
     (Self : in out Zyre_Node; Interval : Integer);
   
   --  Set the peer expiration timeout, in milliseconds. Default is 30000.
   --  This can be tuned in order to deal with expected network conditions
   --  and the response time expected by the application. This is tied to
   --  the beacon interval and rate of messages received.
   --  ZYRE_EXPORT void
   --      zyre_set_expired_timeout (zyre_t *self, int interval);
   procedure Zyre_Set_Expired_Timeout
     (Self : in out Zyre_Node; Interval : Integer);
   
   --  Set UDP beacon discovery interval, in milliseconds. Default is instant
   --  beacon exploration followed by pinging every 1,000 msecs.
   --  ZYRE_EXPORT void
   --      zyre_set_interval (zyre_t *self, size_t interval);
   procedure Zyre_Set_Interval
     (Self : in out Zyre_Node; Interval : Long_Integer);
   
   --  Set network interface for UDP beacons. If you do not set this, CZMQ will
   --  choose an interface for you. On boxes with several interfaces you should
   --  specify which one you want to use, or strange things can happen.
   --  ZYRE_EXPORT void
   --      zyre_set_interface (zyre_t *self, const char *value);
   procedure Zyre_Set_Interface
     (Self : in out Zyre_Node; Value : String);
   
   --  By default, Zyre binds to an ephemeral TCP port and broadcasts the local
   --  host name using UDP beaconing. When you call this method, Zyre will use
   --  gossip discovery instead of UDP beaconing. You MUST set-up the gossip
   --  service separately using zyre_gossip_bind() and _connect(). Note the
   --  endpoint MUST be valid for both bind and connect operations. You can use
   --  inproc://, ipc://, or tcp:// transports (for tcp://, use an IP address
   --  that is meaningful to remote as well as local nodes). Returns 0 if
   --  the bind was successful, else -1.
   --  ZYRE_EXPORT int
   --      zyre_set_endpoint (zyre_t *self, const char *format, ...)
   --        CHECK_PRINTF (2);
   procedure Zyre_Set_Endpoint
     (Self : in out Zyre_Node; Value : String);
   
   --  Set-up gossip discovery of other nodes. At least one node in the cluster
   --  must bind to a well-known gossip endpoint, so other nodes can connect to
   --  it. Note that gossip endpoints are completely distinct from Zyre node
   --  endpoints, and should not overlap (they can use the same transport).
   --  ZYRE_EXPORT void
   --      zyre_gossip_bind (zyre_t *self, const char *format, ...)
   --         CHECK_PRINTF (2);
   procedure Zyre_Gossip_Bind
     (Self : in out Zyre_Node; Endpoint : String);
   
   --  Set-up gossip discovery of other nodes. A node may connect to multiple
   --  other nodes, for redundancy paths. For details of the gossip network
   --  design, see the CZMQ zgossip class.
   --  ZYRE_EXPORT void
   --      zyre_gossip_connect (zyre_t *self, const char *format, ...)
   --        CHECK_PRINTF (2);
   procedure Zyre_Gossip_Connect
     (Self : in out Zyre_Node; Endpoint : String);
   
   subtype Return_Status is Integer range -1 .. 0;

   --  Start node, after setting header values. When you start a node it
   --  begins discovery and connection. Returns 0 if OK, -1 if it wasn't
   --  possible to start the node.
   --  ZYRE_EXPORT int
   --      zyre_start (zyre_t *self);
   function Zyre_Start (Self : in out Zyre_Node) return Return_Status;
   
   --  Stop node; this signals to other peers that this node will go away.
   --  This is polite; however you can also just destroy the node without
   --  stopping it.
   --  ZYRE_EXPORT void
   --      zyre_stop (zyre_t *self);
   procedure Zyre_Stop (Self : in out Zyre_Node);
   
   --  Join a named group; after joining a group you can send messages to
   --  the group and all Zyre nodes in that group will receive them.
   --  ZYRE_EXPORT int
   --      zyre_join (zyre_t *self, const char *group);
   function Zyre_Join (Self : in out Zyre_Node; Group : String)
     return Return_Status;
   
   --  Leave a group
   --  ZYRE_EXPORT int
   --      zyre_leave (zyre_t *self, const char *group);
   function Zyre_Leave (Self : in out Zyre_Node; Group : String)
     return Return_Status;
   
   --  Receive next message from network; the message may be a control
   --  message (ENTER, EXIT, JOIN, LEAVE) or data (WHISPER, SHOUT).
   --  Returns zmsg_t object, or NULL if interrupted
   --  Caller owns return value and must destroy it when done.
   --  ZYRE_EXPORT zmsg_t *
   --      zyre_recv (zyre_t *self);
   function Zyre_Recv (Self : in out Zyre_Node) return Zmsgs.Zmsg;
   
   --  Send message to single peer, specified as a UUID string
   --  Destroys message after sending
   --  ZYRE_EXPORT int
   --      zyre_whisper (zyre_t *self, const char *peer, zmsg_t **msg_p);
   function Zyre_Whisper
     (Self : in out Zyre_Node; Peer : String; Msg_P : in out Zmsgs.Zmsg)
     return Return_Status;
   
   --  Send message to a named group
   --  Destroys message after sending
   --  ZYRE_EXPORT int
   --      zyre_shout (zyre_t *self, const char *group, zmsg_t **msg_p);
   function Zyre_Shout
     (Self : in out Zyre_Node; Group : String; Msg_P : in out Zmsgs.Zmsg)
     return Return_Status;
   
   --  Send formatted string to a single peer specified as UUID string
   --  ZYRE_EXPORT int
   --      zyre_whispers (zyre_t *self, const char *peer,
   --        const char *format, ...) CHECK_PRINTF (3);
   function Zyre_Whispers
     (Self : in out Zyre_Node; Peer : String; Msg : String)
     return Return_Status;
   
   --  Send formatted string to a named group
   --  ZYRE_EXPORT int
   --      zyre_shouts (zyre_t *self, const char *group,
   --        const char *format, ...) CHECK_PRINTF (3);
   function Zyre_Shouts
     (Self : in out Zyre_Node; Group : String; Msg : String)
     return Return_Status;
   
   --  Return zlist of current peer ids.
   --  Caller owns return value and must destroy it when done.
   --  ZYRE_EXPORT zlist_t *
   --      zyre_peers (zyre_t *self);
   function Zyre_Peers (Self : Zyre_Node) return String_Zlists.Zlist;
   
   --  Return zlist of current peers of this group.
   --  Caller owns return value and must destroy it when done.
   --  ZYRE_EXPORT zlist_t *
   --      zyre_peers_by_group (zyre_t *self, const char *name);
   function Zyre_Peers_By_Group
     (Self : Zyre_Node; Name : String) return String_Zlists.Zlist;
   
   --  Return zlist of currently joined groups.
   --  Caller owns return value and must destroy it when done.
   --  ZYRE_EXPORT zlist_t *
   --      zyre_own_groups (zyre_t *self);
   function Zyre_Own_Groups (Self : Zyre_Node) return String_Zlists.Zlist;
   
   --  Return zlist of groups known through connected peers.
   --  Caller owns return value and must destroy it when done.
   --  ZYRE_EXPORT zlist_t *
   --      zyre_peer_groups (zyre_t *self);
   function Zyre_Peer_Groups (Self : Zyre_Node) return String_Zlists.Zlist;
   
   --  Return the endpoint of a connected peer.
   --  Returns empty string if peer does not exist.
   --  Caller owns return value and must destroy it when done.
   --  ZYRE_EXPORT char *
   --      zyre_peer_address (zyre_t *self, const char *peer);
   function Zyre_Peer_Address
     (Self : Zyre_Node; Name : String) return String;
   
   --  Return the value of a header of a conected peer.
   --  Returns null if peer or key doesn't exits.
   --  Caller owns return value and must destroy it when done.
   --  ZYRE_EXPORT char *
   --      zyre_peer_header_value (zyre_t *self, const char *peer,
   --         const char *name);
   function Zyre_Peer_Header_Value
     (Self : Zyre_Node; Peer : String; Name : String) return String;
   
   --  Return socket for talking to the Zyre node, for polling
   --  ZYRE_EXPORT zsock_t *
   --      zyre_socket (zyre_t *self);
   function Zyre_Socket (Self : Zyre_Node) return Zsocks.Zsock;
   
   --  Print zyre node information to stdout
   --  ZYRE_EXPORT void
   --      zyre_print (zyre_t *self);
   procedure Zyre_Print (Self : Zyre_Node);
   
   --  Return the Zyre version for run-time API detection; returns
   --  major * 10000 + minor * 100 + patch, as a single integer.
   --  ZYRE_EXPORT uint64_t
   --      zyre_version (void);
   function Zyre_Version return Interfaces.Unsigned_64;
   
   --  Self test of this class.
   --  ZYRE_EXPORT void
   --      zyre_test (bool verbose);
   procedure Zyre_Test (Verbose : Boolean);
 
private
   type Zyre_Node is record
      Raw : access zyre_library_h.zyre_t;
   end record;
end ZMQ.Zyre;
