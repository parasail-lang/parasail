pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
limited with czmq_library_h;
with System;
with Interfaces.C.Extensions;

package zgossip_h is

  --  =========================================================================
  --    zgossip - zgossip server
  --    ** WARNING *************************************************************
  --    THIS SOURCE FILE IS 100% GENERATED. If you edit this file, you will lose
  --    your changes at the next build cycle. This is great for temporary printf
  --    statements. DO NOT MAKE ANY CHANGES YOU WISH TO KEEP. The correct places
  --    for commits are:
  --     * The XML model used for this code generation: zgossip.xml, or
  --     * The code generation script that built this file: zproto_server_c
  --    ************************************************************************
  --    Copyright (c) the Contributors as noted in the AUTHORS file.
  --    This file is part of CZMQ, the high-level C binding for 0MQ:
  --    http://czmq.zeromq.org.
  --    This Source Code Form is subject to the terms of the Mozilla Public
  --    License, v. 2.0. If a copy of the MPL was not distributed with this
  --    file, You can obtain one at http://mozilla.org/MPL/2.0/.
  --    =========================================================================
  -- 

  --  @interface
  --  To work with zgossip, use the CZMQ zactor API:
  --  Create new zgossip instance, passing logging prefix:
  --      zactor_t *zgossip = zactor_new (zgossip, "myname");
  --  Destroy zgossip instance
  --      zactor_destroy (&zgossip);
  --  Enable verbose logging of commands and activity:
  --      zstr_send (zgossip, "VERBOSE");
  --  Bind zgossip to specified endpoint. TCP endpoints may specify
  --  the port number as "*" to acquire an ephemeral port:
  --      zstr_sendx (zgossip, "BIND", endpoint, NULL);
  --  Return assigned port number, specifically when BIND was done using an
  --  an ephemeral port:
  --      zstr_sendx (zgossip, "PORT", NULL);
  --      char *command, *port_str;
  --      zstr_recvx (zgossip, &command, &port_str, NULL);
  --      assert (streq (command, "PORT"));
  --  Specify configuration file to load, overwriting any previous loaded
  --  configuration file or options:
  --      zstr_sendx (zgossip, "LOAD", filename, NULL);
  --  Set configuration path value:
  --      zstr_sendx (zgossip, "SET", path, value, NULL);
  --  Save configuration data to config file on disk:
  --      zstr_sendx (zgossip, "SAVE", filename, NULL);
  --  Send zmsg_t instance to zgossip:
  --      zactor_send (zgossip, &msg);
  --  Receive zmsg_t instance from zgossip:
  --      zmsg_t *msg = zactor_recv (zgossip);
  --  This is the zgossip constructor as a zactor_fn:
   procedure zgossip (pipe : access czmq_library_h.zsock_t; args : System.Address)  -- /homes/taft/_distrib/include/zgossip.h:84
   with Import => True, 
        Convention => C, 
        External_Name => "zgossip";

  --  Self test of this class
   procedure zgossip_test (verbose : Extensions.bool)  -- /homes/taft/_distrib/include/zgossip.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "zgossip_test";

  --  @end
end zgossip_h;
