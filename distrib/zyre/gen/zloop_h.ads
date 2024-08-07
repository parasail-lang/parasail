pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
limited with czmq_library_h;
with System;
limited with zmq_h;
with stddef_h;
with Interfaces.C.Extensions;

package zloop_h is

   --  arg-macro: procedure zloop_set_tolerant (s, i)
   --    zloop_poller_set_tolerant(s,i)
  --  =========================================================================
  --    zloop - event-driven reactor
  --    Copyright (c) the Contributors as noted in the AUTHORS file.
  --    This file is part of CZMQ, the high-level C binding for 0MQ:
  --    http://czmq.zeromq.org.
  --    This Source Code Form is subject to the terms of the Mozilla Public
  --    License, v. 2.0. If a copy of the MPL was not distributed with this
  --    file, You can obtain one at http://mozilla.org/MPL/2.0/.
  --    =========================================================================
  -- 

  --  @warning THE FOLLOWING @INTERFACE BLOCK IS AUTO-GENERATED BY ZPROJECT
  --  @warning Please edit the model at "api/zloop.api" to make changes.
  --  @interface
  --  This is a stable class, and may not change except for emergencies. It
  --  is provided in stable builds.
  -- Callback function for reactor socket activity
   --  skipped function type zloop_reader_fn

  -- Callback function for reactor events (low-level)
   --  skipped function type zloop_fn

  -- Callback for reactor timer events
   --  skipped function type zloop_timer_fn

  --  Create a new zloop reactor
   function zloop_new return access czmq_library_h.zloop_t  -- /homes/taft/_distrib/include/zloop.h:40
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_new";

  --  Destroy a reactor
   procedure zloop_destroy (self_p : System.Address)  -- /homes/taft/_distrib/include/zloop.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_destroy";

  --  Register socket reader with the reactor. When the reader has messages,
  --  the reactor will call the handler, passing the arg. Returns 0 if OK, -1
  --  if there was an error. If you register the same socket more than once,
  --  each instance will invoke its corresponding handler.
   function zloop_reader
     (self : access czmq_library_h.zloop_t;
      sock : access czmq_library_h.zsock_t;
      handler : access function
        (arg1 : access czmq_library_h.zloop_t;
         arg2 : access czmq_library_h.zsock_t;
         arg3 : System.Address) return int;
      arg : System.Address) return int  -- /homes/taft/_distrib/include/zloop.h:51
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_reader";

  --  Cancel a socket reader from the reactor. If multiple readers exist for
  --  same socket, cancels ALL of them.
   procedure zloop_reader_end (self : access czmq_library_h.zloop_t; sock : access czmq_library_h.zsock_t)  -- /homes/taft/_distrib/include/zloop.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_reader_end";

  --  Configure a registered reader to ignore errors. If you do not set this,
  --  then readers that have errors are removed from the reactor silently.
   procedure zloop_reader_set_tolerant (self : access czmq_library_h.zloop_t; sock : access czmq_library_h.zsock_t)  -- /homes/taft/_distrib/include/zloop.h:61
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_reader_set_tolerant";

  --  Register low-level libzmq pollitem with the reactor. When the pollitem
  --  is ready, will call the handler, passing the arg. Returns 0 if OK, -1
  --  if there was an error. If you register the pollitem more than once, each
  --  instance will invoke its corresponding handler. A pollitem with
  --  socket=NULL and fd=0 means 'poll on FD zero'.
   function zloop_poller
     (self : access czmq_library_h.zloop_t;
      item : access zmq_h.zmq_pollitem_t;
      handler : access function
        (arg1 : access czmq_library_h.zloop_t;
         arg2 : access zmq_h.zmq_pollitem_t;
         arg3 : System.Address) return int;
      arg : System.Address) return int  -- /homes/taft/_distrib/include/zloop.h:69
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_poller";

  --  Cancel a pollitem from the reactor, specified by socket or FD. If both
  --  are specified, uses only socket. If multiple poll items exist for same
  --  socket/FD, cancels ALL of them.
   procedure zloop_poller_end (self : access czmq_library_h.zloop_t; item : access zmq_h.zmq_pollitem_t)  -- /homes/taft/_distrib/include/zloop.h:75
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_poller_end";

  --  Configure a registered poller to ignore errors. If you do not set this,
  --  then poller that have errors are removed from the reactor silently.
   procedure zloop_poller_set_tolerant (self : access czmq_library_h.zloop_t; item : access zmq_h.zmq_pollitem_t)  -- /homes/taft/_distrib/include/zloop.h:80
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_poller_set_tolerant";

  --  Register a timer that expires after some delay and repeats some number of
  --  times. At each expiry, will call the handler, passing the arg. To run a
  --  timer forever, use 0 times. Returns a timer_id that is used to cancel the
  --  timer in the future. Returns -1 if there was an error.
   function zloop_timer
     (self : access czmq_library_h.zloop_t;
      c_delay : stddef_h.size_t;
      times : stddef_h.size_t;
      handler : access function
        (arg1 : access czmq_library_h.zloop_t;
         arg2 : int;
         arg3 : System.Address) return int;
      arg : System.Address) return int  -- /homes/taft/_distrib/include/zloop.h:87
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_timer";

  --  Cancel a specific timer identified by a specific timer_id (as returned by
  --  zloop_timer).
   function zloop_timer_end (self : access czmq_library_h.zloop_t; timer_id : int) return int  -- /homes/taft/_distrib/include/zloop.h:92
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_timer_end";

  --  Register a ticket timer. Ticket timers are very fast in the case where
  --  you use a lot of timers (thousands), and frequently remove and add them.
  --  The main use case is expiry timers for servers that handle many clients,
  --  and which reset the expiry timer for each message received from a client.
  --  Whereas normal timers perform poorly as the number of clients grows, the
  --  cost of ticket timers is constant, no matter the number of clients. You
  --  must set the ticket delay using zloop_set_ticket_delay before creating a
  --  ticket. Returns a handle to the timer that you should use in
  --  zloop_ticket_reset and zloop_ticket_delete.
   function zloop_ticket
     (self : access czmq_library_h.zloop_t;
      handler : access function
        (arg1 : access czmq_library_h.zloop_t;
         arg2 : int;
         arg3 : System.Address) return int;
      arg : System.Address) return System.Address  -- /homes/taft/_distrib/include/zloop.h:104
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_ticket";

  --  Reset a ticket timer, which moves it to the end of the ticket list and
  --  resets its execution time. This is a very fast operation.
   procedure zloop_ticket_reset (self : access czmq_library_h.zloop_t; handle : System.Address)  -- /homes/taft/_distrib/include/zloop.h:109
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_ticket_reset";

  --  Delete a ticket timer. We do not actually delete the ticket here, as
  --  other code may still refer to the ticket. We mark as deleted, and remove
  --  later and safely.
   procedure zloop_ticket_delete (self : access czmq_library_h.zloop_t; handle : System.Address)  -- /homes/taft/_distrib/include/zloop.h:115
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_ticket_delete";

  --  Set the ticket delay, which applies to all tickets. If you lower the
  --  delay and there are already tickets created, the results are undefined.
   procedure zloop_set_ticket_delay (self : access czmq_library_h.zloop_t; ticket_delay : stddef_h.size_t)  -- /homes/taft/_distrib/include/zloop.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_set_ticket_delay";

  --  Set hard limit on number of timers allowed. Setting more than a small
  --  number of timers (10-100) can have a dramatic impact on the performance
  --  of the reactor. For high-volume cases, use ticket timers. If the hard
  --  limit is reached, the reactor stops creating new timers and logs an
  --  error.
   procedure zloop_set_max_timers (self : access czmq_library_h.zloop_t; max_timers : stddef_h.size_t)  -- /homes/taft/_distrib/include/zloop.h:128
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_set_max_timers";

  --  Set verbose tracing of reactor on/off. The default verbose setting is
  --  off (false).
   procedure zloop_set_verbose (self : access czmq_library_h.zloop_t; verbose : Extensions.bool)  -- /homes/taft/_distrib/include/zloop.h:133
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_set_verbose";

  --  By default the reactor stops if the process receives a SIGINT or SIGTERM
  --  signal. This makes it impossible to shut-down message based architectures
  --  like zactors. This method lets you switch off break handling. The default
  --  nonstop setting is off (false).
   procedure zloop_set_nonstop (self : access czmq_library_h.zloop_t; nonstop : Extensions.bool)  -- /homes/taft/_distrib/include/zloop.h:140
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_set_nonstop";

  --  Start the reactor. Takes control of the thread and returns when the 0MQ
  --  context is terminated or the process is interrupted, or any event handler
  --  returns -1. Event handlers may register new sockets and timers, and
  --  cancel sockets. Returns 0 if interrupted, -1 if canceled by a handler.
   function zloop_start (self : access czmq_library_h.zloop_t) return int  -- /homes/taft/_distrib/include/zloop.h:147
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_start";

  --  Self test of this class.
   procedure zloop_test (verbose : Extensions.bool)  -- /homes/taft/_distrib/include/zloop.h:151
   with Import => True, 
        Convention => C, 
        External_Name => "zloop_test";

  --  @end
  --  Deprecated method aliases
end zloop_h;
