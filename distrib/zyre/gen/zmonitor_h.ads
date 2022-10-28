pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
limited with czmq_library_h;
with System;
with Interfaces.C.Extensions;

package zmonitor_h is

  --  =========================================================================
  --    zmonitor - socket event monitor
  --    Copyright (c) the Contributors as noted in the AUTHORS file.
  --    This file is part of CZMQ, the high-level C binding for 0MQ:
  --    http://czmq.zeromq.org.
  --    This Source Code Form is subject to the terms of the Mozilla Public
  --    License, v. 2.0. If a copy of the MPL was not distributed with this
  --    file, You can obtain one at http://mozilla.org/MPL/2.0/.
  --    =========================================================================
  -- 

  --  @interface
  --  Create new zmonitor actor instance to monitor a zsock_t socket:
  --      zactor_t *monitor = zactor_new (zmonitor, mysocket);
  --  Destroy zmonitor instance.
  --      zactor_destroy (&monitor);
  --  Enable verbose logging of commands and activity.
  --      zstr_send (monitor, "VERBOSE");
  --  Listen to monitor event type (zero or types, ending in NULL):
  --      zstr_sendx (monitor, "LISTEN", type, ..., NULL);
  --  
  --      Events:
  --      CONNECTED
  --      CONNECT_DELAYED
  --      CONNECT_RETRIED
  --      LISTENING
  --      BIND_FAILED
  --      ACCEPTED
  --      ACCEPT_FAILED
  --      CLOSED
  --      CLOSE_FAILED
  --      DISCONNECTED
  --      MONITOR_STOPPED
  --      ALL
  --  Start monitor; after this, any further LISTEN commands are ignored.
  --      zstr_send (monitor, "START");
  --      zsock_wait (monitor);
  --  Receive next monitor event:
  --      zmsg_t *msg = zmsg_recv (monitor);
  --  This is the zmonitor constructor as a zactor_fn; the argument can be
  --  a zactor_t, zsock_t, or libzmq void * socket:
   procedure zmonitor (pipe : access czmq_library_h.zsock_t; sock : System.Address)  -- /homes/taft/_distrib/include/zmonitor.h:63
   with Import => True, 
        Convention => C, 
        External_Name => "zmonitor";

  --  Selftest
   procedure zmonitor_test (verbose : Extensions.bool)  -- /homes/taft/_distrib/include/zmonitor.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "zmonitor_test";

  --  @end
end zmonitor_h;
