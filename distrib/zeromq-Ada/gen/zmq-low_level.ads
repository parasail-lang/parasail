-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                         Z M Q . L O W _ L E V E L                         --
--                                                                           --
--                                  S p e c                                  --
--                                                                           --
--            Copyright (C) 2020-2030, per.s.sandberg@bahnhof.se             --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------
--
--  The contents of this file is derived from zmq.h using the
--   -fdump-ada-spec switch for gcc.
pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package ZMQ.Low_Level is

   pragma Preelaborate;
   pragma Warnings (Off);

   package Defs is
      --  This package is here to give a namespace to constants, since identifiers in Ada are caseinsensetive.

      ZMQ_VERSION_MAJOR : constant := 4;  --  zmq.h:42
      ZMQ_VERSION_MINOR : constant := 3;  --  zmq.h:43
      ZMQ_VERSION_PATCH : constant := 2;  --  zmq.h:44
      --  arg-macro: function ZMQ_MAKE_VERSION (major, minor, patch)
      --    return (major) *10000 + (minor) *100 + (patch);
      --  unsupported macro: ZMQ_VERSION ZMQ_MAKE_VERSION (ZMQ_VERSION_MAJOR, ZMQ_VERSION_MINOR, ZMQ_VERSION_PATCH)

      ZMQ_DEFINED_STDINT : constant := 1;  --  zmq.h:97

      ZMQ_HAUSNUMERO : constant := 156_384_712;  --  zmq.h:134
      EFSM           : constant := ZMQ_HAUSNUMERO + 51;
      ENOCOMPATPROTO : constant := ZMQ_HAUSNUMERO + 52;
      ETERM          : constant := ZMQ_HAUSNUMERO + 53;
      EMTHREAD       : constant := ZMQ_HAUSNUMERO + 54;

      ZMQ_IO_THREADS                 : constant := 1;  --  zmq.h:215
      ZMQ_MAX_SOCKETS                : constant := 2;  --  zmq.h:216
      ZMQ_SOCKET_LIMIT               : constant := 3;  --  zmq.h:217
      ZMQ_THREAD_PRIORITY            : constant := 3;  --  zmq.h:218
      ZMQ_THREAD_SCHED_POLICY        : constant := 4;  --  zmq.h:219
      ZMQ_MAX_MSGSZ                  : constant := 5;  --  zmq.h:220
      ZMQ_MSG_T_SIZE                 : constant := 6;  --  zmq.h:221
      ZMQ_THREAD_AFFINITY_CPU_ADD    : constant := 7;  --  zmq.h:222
      ZMQ_THREAD_AFFINITY_CPU_REMOVE : constant := 8;  --  zmq.h:223
      ZMQ_THREAD_NAME_PREFIX         : constant := 9;  --  zmq.h:224

      ZMQ_IO_THREADS_DFLT          : constant := 1;  --  zmq.h:227
      ZMQ_MAX_SOCKETS_DFLT         : constant := 1_023;  --  zmq.h:228
      ZMQ_THREAD_PRIORITY_DFLT     : constant := -1;  --  zmq.h:229
      ZMQ_THREAD_SCHED_POLICY_DFLT : constant := -1;  --  zmq.h:230

      ZMQ_PAIR   : constant := 0;  --  zmq.h:291
      ZMQ_PUB    : constant := 1;  --  zmq.h:292
      ZMQ_SUB    : constant := 2;  --  zmq.h:293
      ZMQ_REQ    : constant := 3;  --  zmq.h:294
      ZMQ_REP    : constant := 4;  --  zmq.h:295
      ZMQ_DEALER : constant := 5;  --  zmq.h:296
      ZMQ_ROUTER : constant := 6;  --  zmq.h:297
      ZMQ_PULL   : constant := 7;  --  zmq.h:298
      ZMQ_PUSH   : constant := 8;  --  zmq.h:299
      ZMQ_XPUB   : constant := 9;  --  zmq.h:300
      ZMQ_XSUB   : constant := 10;  --  zmq.h:301
      ZMQ_STREAM : constant := 11;  --  zmq.h:302
      ZMQ_XREQ   : constant := ZMQ_DEALER;
      ZMQ_XREP   : constant := ZMQ_ROUTER;

      ZMQ_AFFINITY                          : constant := 4;  --  zmq.h:309
      ZMQ_ROUTING_ID                        : constant := 5;  --  zmq.h:310
      ZMQ_SUBSCRIBE                         : constant := 6;  --  zmq.h:311
      ZMQ_UNSUBSCRIBE                       : constant := 7;  --  zmq.h:312
      ZMQ_RATE                              : constant := 8;  --  zmq.h:313
      ZMQ_RECOVERY_IVL                      : constant := 9;  --  zmq.h:314
      ZMQ_SNDBUF                            : constant := 11;  --  zmq.h:315
      ZMQ_RCVBUF                            : constant := 12;  --  zmq.h:316
      ZMQ_RCVMORE                           : constant := 13;  --  zmq.h:317
      ZMQ_FD                                : constant := 14;  --  zmq.h:318
      ZMQ_EVENTS                            : constant := 15;  --  zmq.h:319
      ZMQ_TYPE                              : constant := 16;  --  zmq.h:320
      ZMQ_LINGER                            : constant := 17;  --  zmq.h:321
      ZMQ_RECONNECT_IVL                     : constant := 18;  --  zmq.h:322
      ZMQ_BACKLOG                           : constant := 19;  --  zmq.h:323
      ZMQ_RECONNECT_IVL_MAX                 : constant := 21;  --  zmq.h:324
      ZMQ_MAXMSGSIZE                        : constant := 22;  --  zmq.h:325
      ZMQ_SNDHWM                            : constant := 23;  --  zmq.h:326
      ZMQ_RCVHWM                            : constant := 24;  --  zmq.h:327
      ZMQ_MULTICAST_HOPS                    : constant := 25;  --  zmq.h:328
      ZMQ_RCVTIMEO                          : constant := 27;  --  zmq.h:329
      ZMQ_SNDTIMEO                          : constant := 28;  --  zmq.h:330
      ZMQ_LAST_ENDPOINT                     : constant := 32;  --  zmq.h:331
      ZMQ_ROUTER_MANDATORY                  : constant := 33;  --  zmq.h:332
      ZMQ_TCP_KEEPALIVE                     : constant := 34;  --  zmq.h:333
      ZMQ_TCP_KEEPALIVE_CNT                 : constant := 35;  --  zmq.h:334
      ZMQ_TCP_KEEPALIVE_IDLE                : constant := 36;  --  zmq.h:335
      ZMQ_TCP_KEEPALIVE_INTVL               : constant := 37;  --  zmq.h:336
      ZMQ_IMMEDIATE                         : constant := 39;  --  zmq.h:337
      ZMQ_XPUB_VERBOSE                      : constant := 40;  --  zmq.h:338
      ZMQ_ROUTER_RAW                        : constant := 41;  --  zmq.h:339
      ZMQ_IPV6                              : constant := 42;  --  zmq.h:340
      ZMQ_MECHANISM                         : constant := 43;  --  zmq.h:341
      ZMQ_PLAIN_SERVER                      : constant := 44;  --  zmq.h:342
      ZMQ_PLAIN_USERNAME                    : constant := 45;  --  zmq.h:343
      ZMQ_PLAIN_PASSWORD                    : constant := 46;  --  zmq.h:344
      ZMQ_CURVE_SERVER                      : constant := 47;  --  zmq.h:345
      ZMQ_CURVE_PUBLICKEY                   : constant := 48;  --  zmq.h:346
      ZMQ_CURVE_SECRETKEY                   : constant := 49;  --  zmq.h:347
      ZMQ_CURVE_SERVERKEY                   : constant := 50;  --  zmq.h:348
      ZMQ_PROBE_ROUTER                      : constant := 51;  --  zmq.h:349
      ZMQ_REQ_CORRELATE                     : constant := 52;  --  zmq.h:350
      ZMQ_REQ_RELAXED                       : constant := 53;  --  zmq.h:351
      ZMQ_CONFLATE                          : constant := 54;  --  zmq.h:352
      ZMQ_ZAP_DOMAIN                        : constant := 55;  --  zmq.h:353
      ZMQ_ROUTER_HANDOVER                   : constant := 56;  --  zmq.h:354
      ZMQ_TOS                               : constant := 57;  --  zmq.h:355
      ZMQ_CONNECT_ROUTING_ID                : constant := 61;  --  zmq.h:356
      ZMQ_GSSAPI_SERVER                     : constant := 62;  --  zmq.h:357
      ZMQ_GSSAPI_PRINCIPAL                  : constant := 63;  --  zmq.h:358
      ZMQ_GSSAPI_SERVICE_PRINCIPAL          : constant := 64;  --  zmq.h:359
      ZMQ_GSSAPI_PLAINTEXT                  : constant := 65;  --  zmq.h:360
      ZMQ_HANDSHAKE_IVL                     : constant := 66;  --  zmq.h:361
      ZMQ_SOCKS_PROXY                       : constant := 68;  --  zmq.h:362
      ZMQ_XPUB_NODROP                       : constant := 69;  --  zmq.h:363
      ZMQ_BLOCKY                            : constant := 70;  --  zmq.h:364
      ZMQ_XPUB_MANUAL                       : constant := 71;  --  zmq.h:365
      ZMQ_XPUB_WELCOME_MSG                  : constant := 72;  --  zmq.h:366
      ZMQ_STREAM_NOTIFY                     : constant := 73;  --  zmq.h:367
      ZMQ_INVERT_MATCHING                   : constant := 74;  --  zmq.h:368
      ZMQ_HEARTBEAT_IVL                     : constant := 75;  --  zmq.h:369
      ZMQ_HEARTBEAT_TTL                     : constant := 76;  --  zmq.h:370
      ZMQ_HEARTBEAT_TIMEOUT                 : constant := 77;  --  zmq.h:371
      ZMQ_XPUB_VERBOSER                     : constant := 78;  --  zmq.h:372
      ZMQ_CONNECT_TIMEOUT                   : constant := 79;  --  zmq.h:373
      ZMQ_TCP_MAXRT                         : constant := 80;  --  zmq.h:374
      ZMQ_THREAD_SAFE                       : constant := 81;  --  zmq.h:375
      ZMQ_MULTICAST_MAXTPDU                 : constant := 84;  --  zmq.h:376
      ZMQ_VMCI_BUFFER_SIZE                  : constant := 85;  --  zmq.h:377
      ZMQ_VMCI_BUFFER_MIN_SIZE              : constant := 86;  --  zmq.h:378
      ZMQ_VMCI_BUFFER_MAX_SIZE              : constant := 87;  --  zmq.h:379
      ZMQ_VMCI_CONNECT_TIMEOUT              : constant := 88;  --  zmq.h:380
      ZMQ_USE_FD                            : constant := 89;  --  zmq.h:381
      ZMQ_GSSAPI_PRINCIPAL_NAMETYPE         : constant := 90;  --  zmq.h:382
      ZMQ_GSSAPI_SERVICE_PRINCIPAL_NAMETYPE : constant := 91;  --  zmq.h:383
      ZMQ_BINDTODEVICE                      : constant := 92;  --  zmq.h:384

      ZMQ_MORE   : constant := 1;  --  zmq.h:387
      ZMQ_SHARED : constant := 3;  --  zmq.h:388

      ZMQ_DONTWAIT : constant := 1;  --  zmq.h:391
      ZMQ_SNDMORE  : constant := 2;  --  zmq.h:392

      ZMQ_NULL   : constant := 0;  --  zmq.h:395
      ZMQ_PLAIN  : constant := 1;  --  zmq.h:396
      ZMQ_CURVE  : constant := 2;  --  zmq.h:397
      ZMQ_GSSAPI : constant := 3;  --  zmq.h:398

      ZMQ_GROUP_MAX_LENGTH : constant := 15;  --  zmq.h:401
      ZMQ_IDENTITY         : constant := ZMQ_ROUTING_ID;
      ZMQ_CONNECT_RID      : constant := ZMQ_CONNECT_ROUTING_ID;

      ZMQ_TCP_ACCEPT_FILTER       : constant := 38;  --  zmq.h:406
      ZMQ_IPC_FILTER_PID          : constant := 58;  --  zmq.h:407
      ZMQ_IPC_FILTER_UID          : constant := 59;  --  zmq.h:408
      ZMQ_IPC_FILTER_GID          : constant := 60;  --  zmq.h:409
      ZMQ_IPV4ONLY                : constant := 31;  --  zmq.h:410
      ZMQ_DELAY_ATTACH_ON_CONNECT : constant := ZMQ_IMMEDIATE;
      ZMQ_NOBLOCK                 : constant := ZMQ_DONTWAIT;
      ZMQ_FAIL_UNROUTABLE         : constant := ZMQ_ROUTER_MANDATORY;
      ZMQ_ROUTER_BEHAVIOR         : constant := ZMQ_ROUTER_MANDATORY;

      ZMQ_SRCFD : constant := 2;  --  zmq.h:417

      ZMQ_GSSAPI_NT_HOSTBASED      : constant := 0;  --  zmq.h:424
      ZMQ_GSSAPI_NT_USER_NAME      : constant := 1;  --  zmq.h:425
      ZMQ_GSSAPI_NT_KRB5_PRINCIPAL : constant := 2;  --  zmq.h:426

      ZMQ_EVENT_CONNECTED       : constant := 16#0001#;  --  zmq.h:434
      ZMQ_EVENT_CONNECT_DELAYED : constant := 16#0002#;  --  zmq.h:435
      ZMQ_EVENT_CONNECT_RETRIED : constant := 16#0004#;  --  zmq.h:436
      ZMQ_EVENT_LISTENING       : constant := 16#0008#;  --  zmq.h:437
      ZMQ_EVENT_BIND_FAILED     : constant := 16#0010#;  --  zmq.h:438
      ZMQ_EVENT_ACCEPTED        : constant := 16#0020#;  --  zmq.h:439
      ZMQ_EVENT_ACCEPT_FAILED   : constant := 16#0040#;  --  zmq.h:440
      ZMQ_EVENT_CLOSED          : constant := 16#0080#;  --  zmq.h:441
      ZMQ_EVENT_CLOSE_FAILED    : constant := 16#0100#;  --  zmq.h:442
      ZMQ_EVENT_DISCONNECTED    : constant := 16#0200#;  --  zmq.h:443
      ZMQ_EVENT_MONITOR_STOPPED : constant := 16#0400#;  --  zmq.h:444
      ZMQ_EVENT_ALL             : constant := 16#FFFF#;  --  zmq.h:445

      ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL : constant := 16#0800#;  --  zmq.h:447

      ZMQ_EVENT_HANDSHAKE_SUCCEEDED : constant := 16#1000#;  --  zmq.h:450

      ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL : constant := 16#2000#;  --  zmq.h:453

      ZMQ_EVENT_HANDSHAKE_FAILED_AUTH                       : constant := 16#4000#;  --  zmq.h:456
      ZMQ_PROTOCOL_ERROR_ZMTP_UNSPECIFIED                   : constant := 16#1000_0000#;  --  zmq.h:457
      ZMQ_PROTOCOL_ERROR_ZMTP_UNEXPECTED_COMMAND            : constant := 16#1000_0001#;  --  zmq.h:458
      ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_SEQUENCE              : constant := 16#1000_0002#;  --  zmq.h:459
      ZMQ_PROTOCOL_ERROR_ZMTP_KEY_EXCHANGE                  : constant := 16#1000_0003#;  --  zmq.h:460
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_UNSPECIFIED : constant := 16#1000_0011#;  --  zmq.h:461
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_MESSAGE     : constant := 16#1000_0012#;  --  zmq.h:462
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_HELLO       : constant := 16#1000_0013#;  --  zmq.h:463
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_INITIATE    : constant := 16#1000_0014#;  --  zmq.h:464
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_ERROR       : constant := 16#1000_0015#;  --  zmq.h:465
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_READY       : constant := 16#1000_0016#;  --  zmq.h:466
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_WELCOME     : constant := 16#1000_0017#;  --  zmq.h:467
      ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_METADATA              : constant := 16#1000_0018#;  --  zmq.h:468

      ZMQ_PROTOCOL_ERROR_ZMTP_CRYPTOGRAPHIC      : constant := 16#1100_0001#;  --  zmq.h:470
      ZMQ_PROTOCOL_ERROR_ZMTP_MECHANISM_MISMATCH : constant := 16#1100_0002#;  --  zmq.h:471
      ZMQ_PROTOCOL_ERROR_ZAP_UNSPECIFIED         : constant := 16#2000_0000#;  --  zmq.h:472
      ZMQ_PROTOCOL_ERROR_ZAP_MALFORMED_REPLY     : constant := 16#2000_0001#;  --  zmq.h:473
      ZMQ_PROTOCOL_ERROR_ZAP_BAD_REQUEST_ID      : constant := 16#2000_0002#;  --  zmq.h:474
      ZMQ_PROTOCOL_ERROR_ZAP_BAD_VERSION         : constant := 16#2000_0003#;  --  zmq.h:475
      ZMQ_PROTOCOL_ERROR_ZAP_INVALID_STATUS_CODE : constant := 16#2000_0004#;  --  zmq.h:476
      ZMQ_PROTOCOL_ERROR_ZAP_INVALID_METADATA    : constant := 16#2000_0005#;  --  zmq.h:477

      ZMQ_POLLIN  : constant := 1;  --  zmq.h:500
      ZMQ_POLLOUT : constant := 2;  --  zmq.h:501
      ZMQ_POLLERR : constant := 4;  --  zmq.h:502
      ZMQ_POLLPRI : constant := 8;  --  zmq.h:503

      ZMQ_POLLITEMS_DFLT : constant := 16;  --  zmq.h:517

      ZMQ_HAS_CAPABILITIES : constant := 1;  --  zmq.h:535

      ZMQ_STREAMER  : constant := 1;  --  zmq.h:539
      ZMQ_FORWARDER : constant := 2;  --  zmq.h:540
      ZMQ_QUEUE     : constant := 3;  --  zmq.h:541

      --    Copyright (c) 2007-2016 Contributors as noted in the AUTHORS file
      --    This file is part of libzmq, the ZeroMQ core engine in C++.
      --    libzmq is free software; you can redistribute it and/or modify it under
      --    the terms of the GNU Lesser General Public License (LGPL) as published
      --    by the Free Software Foundation; either version 3 of the License, or
      --    (at your option) any later version.
      --    As a special exception, the Contributors give you permission to link
      --    this library with independent modules to produce an executable,
      --    regardless of the license terms of these independent modules, and to
      --    copy and distribute the resulting executable under terms of your choice,
      --    provided that you also meet, for each linked independent module, the
      --    terms and conditions of the license of that module. An independent
      --    module is a module which is not derived from or based on this library.
      --    If you modify this library, you must extend this exception to your
      --    version of the library.
      --    libzmq is distributed in the hope that it will be useful, but WITHOUT
      --    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
      --    FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
      --    License for more details.
      --    You should have received a copy of the GNU Lesser General Public License
      --    along with this program.  If not, see <http://www.gnu.org/licenses/>.
      --    *************************************************************************
      --    NOTE to contributors. This file comprises the principal public contract
      --    for ZeroMQ API users. Any change to this file supplied in a stable
      --    release SHOULD not break existing applications.
      --    In practice this means that the value of constants must not change, and
      --    that old values may not be reused for new constants.
      --    *************************************************************************
      --

      --  Version macros for compile-time API version detection Set target version to Windows Server 2008, Windows Vista or
      --  higher. Windows XP (0x0501) is supported but without client & server socket types. Require Windows XP or higher
      --  with MinGW for getaddrinfo(). Handle DSO symbol visibility Define integer types needed for event interface
      --  32-bit AIX's pollfd struct members are called reqevents and rtnevents so it defines compatibility macros for
      --  them. Need to include that header first to stop build failures since zmq_pollset_t defines them as events
      --  and revents. **************************************************************************** 0MQ errors.
      --  **************************************************************************** A number random enough not to collide
      --  with different errno ranges on different OSes. The assumption is that error_t is at least 32-bit type. On Windows
      --  platform some of the standard POSIX errnos are not defined. Native 0MQ error codes. This function retrieves the
      --  errno as it is known to 0MQ library. The goal of this function is to make the code 100% portable, including where
      --  0MQ compiled with certain CRT library (on Windows) is linked to an application that uses different CRT library.
   end Defs;

   function zmq_errno
      return int  -- zmq.h:202
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_errno";

      --  Resolves system errors and 0MQ errors to human-readable string.
   function zmq_strerror
     (errnum_u : int) return Interfaces.C.Strings
     .chars_ptr  -- zmq.h:205
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_strerror";

      --  Run-time API version detection
   procedure zmq_version
     (major_u : access int;
      minor_u : access int;
      patch_u : access int)  -- zmq.h:208
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_version";

      --  **************************************************************************** 0MQ infrastructure (a.k.a. context)
      --  initialisation & termination. **************************************************************************** Context
      --  options Default for new contexts
   function zmq_ctx_new return System
     .Address  -- zmq.h:232
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_ctx_new";

   function zmq_ctx_term
     (context_u : System.Address)
      return int  -- zmq.h:233
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_ctx_term";

   function zmq_ctx_shutdown
     (context_u : System.Address)
      return int  -- zmq.h:234
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_ctx_shutdown";

   function zmq_ctx_set
     (context_u : System.Address;
      option_u  : int;
      optval_u  : int)
      return int  -- zmq.h:235
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_ctx_set";

   function zmq_ctx_get
     (context_u : System.Address;
      option_u  : int)
      return int  -- zmq.h:236
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_ctx_get";

      --  Old (legacy) API
   function zmq_init
     (io_threads_u : int) return System
     .Address  -- zmq.h:239
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_init";

   function zmq_term
     (context_u : System.Address)
      return int  -- zmq.h:240
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_term";

   function zmq_ctx_destroy
     (context_u : System.Address)
      return int  -- zmq.h:241
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_ctx_destroy";

      --  **************************************************************************** 0MQ message definition.
      --  ****************************************************************************
      -- Some architectures, like sparc64 and some variants of aarch64, enforce pointer * alignment and raise sigbus on
      -- violations. Make sure applications allocate * zmq_msg_t on addresses aligned on a pointer-size boundary to avoid
      -- this issue.
      --

   type zmq_msg_t_array1158 is array (0 .. 63) of aliased unsigned_char;
   type zmq_msg_t is record
      u_u : aliased zmq_msg_t_array1158;  -- zmq.h:261
   end record with
      Convention => C_Pass_By_Copy;  -- zmq.h:252

      --  skipped function type zmq_free_fn

   function zmq_msg_init
     (msg_u : access zmq_msg_t)
      return int  -- zmq.h:269
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_init";

   function zmq_msg_init_size
     (msg_u  : access zmq_msg_t;
      size_u : size_t)
      return int  -- zmq.h:270
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_init_size";

   function zmq_msg_init_data
     (msg_u  : access zmq_msg_t; data_u : System.Address; size_u : size_t;
      ffn_u  : access procedure (arg1 : System.Address; arg2 : System.Address);
      hint_u : System.Address)
      return int  -- zmq.h:271
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_init_data";

   function zmq_msg_send
     (msg_u   : access zmq_msg_t;
      s_u     : System.Address;
      flags_u : int)
      return int  -- zmq.h:273
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_send";

   function zmq_msg_recv
     (msg_u   : access zmq_msg_t;
      s_u     : System.Address;
      flags_u : int)
      return int  -- zmq.h:274
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_recv";

   function zmq_msg_close
     (msg_u : access zmq_msg_t)
      return int  -- zmq.h:275
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_close";

   function zmq_msg_move
     (dest_u : access zmq_msg_t;
      src_u  : access zmq_msg_t)
      return int  -- zmq.h:276
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_move";

   function zmq_msg_copy
     (dest_u : access zmq_msg_t;
      src_u  : access zmq_msg_t)
      return int  -- zmq.h:277
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_copy";

   function zmq_msg_data
     (msg_u : access zmq_msg_t) return System
     .Address  -- zmq.h:278
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_data";

   function zmq_msg_size
     (msg_u : access constant zmq_msg_t)
      return size_t  -- zmq.h:279
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_size";

   function zmq_msg_more
     (msg_u : access constant zmq_msg_t)
      return int  -- zmq.h:280
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_more";

   function zmq_msg_get
     (msg_u      : access constant zmq_msg_t;
      property_u : int)
      return int  -- zmq.h:281
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_get";

   function zmq_msg_set
     (msg_u      : access zmq_msg_t;
      property_u : int;
      optval_u   : int)
      return int  -- zmq.h:282
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_set";

   function zmq_msg_gets
     (msg_u : access constant zmq_msg_t; property_u : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings
     .chars_ptr  -- zmq.h:283
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_msg_gets";

      --  **************************************************************************** 0MQ socket definition.
      --  **************************************************************************** Socket types. Deprecated aliases Socket
      --  options. Message options Send/recv options. Security mechanisms RADIO-DISH protocol Deprecated options and aliases
      --  Deprecated Message options **************************************************************************** GSSAPI
      --  definitions **************************************************************************** GSSAPI principal name
      --  types **************************************************************************** 0MQ socket events and monitoring
      --  **************************************************************************** Socket transport events (TCP, IPC and
      --  TIPC only) Unspecified system errors during handshake. Event value is an errno. Handshake complete successfully with
      --  successful authentication (if *
      -- * enabled). Event value is unused.

      --  Protocol errors between ZMTP peers or between server and ZAP handler. *
      -- * Event value is one of ZMQ_PROTOCOL_ERROR_*

      --  Failed authentication requests. Event value is the numeric ZAP status *
      -- * code, i.e. 300, 400 or 500.

      -- the following two may be due to erroneous configuration of a peer
   function zmq_socket
     (arg1 : System.Address; type_u : int) return System
     .Address  -- zmq.h:479
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_socket";

   function zmq_close
     (s_u : System.Address)
      return int  -- zmq.h:480
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_close";

   function zmq_setsockopt
     (s_u         : System.Address;
      option_u    : int;
      optval_u    : System.Address;
      optvallen_u : size_t)
      return int  -- zmq.h:482
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_setsockopt";

   function zmq_getsockopt
     (s_u         : System.Address;
      option_u    : int;
      optval_u    : System.Address;
      optvallen_u : access size_t)
      return int  -- zmq.h:484
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_getsockopt";

   function zmq_bind
     (s_u    : System.Address;
      addr_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:485
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_bind";

   function zmq_connect
     (s_u    : System.Address;
      addr_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:486
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_connect";

   function zmq_unbind
     (s_u    : System.Address;
      addr_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:487
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_unbind";

   function zmq_disconnect
     (s_u    : System.Address;
      addr_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:488
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_disconnect";

   function zmq_send
     (s_u     : System.Address;
      buf_u   : System.Address;
      len_u   : size_t;
      flags_u : int)
      return int  -- zmq.h:489
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_send";

   function zmq_send_const
     (s_u     : System.Address;
      buf_u   : System.Address;
      len_u   : size_t;
      flags_u : int)
      return int  -- zmq.h:491
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_send_const";

   function zmq_recv
     (s_u     : System.Address;
      buf_u   : System.Address;
      len_u   : size_t;
      flags_u : int)
      return int  -- zmq.h:492
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_recv";

   function zmq_socket_monitor
     (s_u      : System.Address;
      addr_u   : Interfaces.C.Strings.chars_ptr;
      events_u : int)
      return int  -- zmq.h:493
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_socket_monitor";

      --  **************************************************************************** Deprecated I/O multiplexing. Prefer using
      --  zmq_poller API ****************************************************************************
   type zmq_pollitem_t is record
      socket  : System.Address;  -- zmq.h:507
      fd      : aliased int;  -- zmq.h:511
      events  : aliased short;  -- zmq.h:513
      revents : aliased short;  -- zmq.h:514
   end record with
      Convention => C_Pass_By_Copy;  -- zmq.h:505

   function zmq_poll
     (items_u   : access zmq_pollitem_t;
      nitems_u  : int;
      timeout_u : long)
      return int  -- zmq.h:519
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_poll";

      --  **************************************************************************** Message proxying
      --  ****************************************************************************
   function zmq_proxy
     (frontend_u : System.Address;
      backend_u  : System.Address;
      capture_u  : System.Address)
      return int  -- zmq.h:525
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_proxy";

   function zmq_proxy_steerable
     (frontend_u : System.Address; backend_u : System.Address; capture_u : System.Address; control_u : System.Address)
      return int  -- zmq.h:526
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_proxy_steerable";

      --  **************************************************************************** Probe library capabilities
      --  ****************************************************************************
   function zmq_has
     (capability_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:536
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_has";

      --  Deprecated aliases
      --  Deprecated methods
   function zmq_device
     (type_u     : int;
      frontend_u : System.Address;
      backend_u  : System.Address)
      return int  -- zmq.h:544
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_device";

   function zmq_sendmsg
     (s_u     : System.Address;
      msg_u   : access zmq_msg_t;
      flags_u : int)
      return int  -- zmq.h:545
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_sendmsg";

   function zmq_recvmsg
     (s_u     : System.Address;
      msg_u   : access zmq_msg_t;
      flags_u : int)
      return int  -- zmq.h:546
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_recvmsg";

   type iovec is null record;   -- incomplete struct

   function zmq_sendiov
     (s_u     : System.Address;
      iov_u   : access iovec;
      count_u : size_t;
      flags_u : int)
      return int  -- zmq.h:549
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_sendiov";

   function zmq_recviov
     (s_u     : System.Address;
      iov_u   : access iovec;
      count_u : access size_t;
      flags_u : int)
      return int  -- zmq.h:551
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_recviov";

      --  **************************************************************************** Encryption functions
      --  **************************************************************************** Encode data with Z85 encoding.
      --  Returns encoded data
   function zmq_z85_encode
     (dest_u : Interfaces.C.Strings.chars_ptr; data_u : access Interfaces.Unsigned_8; size_u : size_t)
      return Interfaces.C.Strings
     .chars_ptr  -- zmq.h:559
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_z85_encode";

      --  Decode data with Z85 encoding. Returns decoded data
   function zmq_z85_decode
     (dest_u : access Interfaces.Unsigned_8; string_u : Interfaces.C.Strings
        .chars_ptr) return access Interfaces
     .Unsigned_8  -- zmq.h:562
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_z85_decode";

      --  Generate z85-encoded public and private keypair with tweetnacl/libsodium. Returns 0 on success.
   function zmq_curve_keypair
     (z85_public_key_u : Interfaces.C.Strings.chars_ptr;
      z85_secret_key_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:566
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_curve_keypair";

      --  Derive the z85-encoded public key from the z85-encoded secret key. Returns 0 on success.
   function zmq_curve_public
     (z85_public_key_u : Interfaces.C.Strings.chars_ptr;
      z85_secret_key_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:570
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_curve_public";

      --  **************************************************************************** Atomic utility methods
      --  ****************************************************************************
   function zmq_atomic_counter_new return System
     .Address  -- zmq.h:577
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_atomic_counter_new";

   procedure zmq_atomic_counter_set
     (counter_u : System.Address;
      value_u   : int)  -- zmq.h:578
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_atomic_counter_set";

   function zmq_atomic_counter_inc
     (counter_u : System.Address)
      return int  -- zmq.h:579
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_atomic_counter_inc";

   function zmq_atomic_counter_dec
     (counter_u : System.Address)
      return int  -- zmq.h:580
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_atomic_counter_dec";

   function zmq_atomic_counter_value
     (counter_u : System.Address)
      return int  -- zmq.h:581
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_atomic_counter_value";

   procedure zmq_atomic_counter_destroy
     (counter_p_u : System
        .Address)  -- zmq.h:582
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_atomic_counter_destroy";

      --  **************************************************************************** Scheduling timers
      --  ****************************************************************************
      --  skipped function type zmq_timer_fn

   function zmq_timers_new return System
     .Address  -- zmq.h:592
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_timers_new";

   function zmq_timers_destroy
     (timers_p : System.Address)
      return int  -- zmq.h:593
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_timers_destroy";

   function zmq_timers_add
     (timers : System.Address; interval : size_t; handler : access procedure (arg1 : int; arg2 : System.Address);
      arg    : System.Address)
      return int  -- zmq.h:595
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_timers_add";

   function zmq_timers_cancel
     (timers   : System.Address;
      timer_id : int)
      return int  -- zmq.h:596
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_timers_cancel";

   function zmq_timers_set_interval
     (timers   : System.Address;
      timer_id : int;
      interval : size_t)
      return int  -- zmq.h:598
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_timers_set_interval";

   function zmq_timers_reset
     (timers   : System.Address;
      timer_id : int)
      return int  -- zmq.h:599
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_timers_reset";

   function zmq_timers_timeout
     (timers : System.Address)
      return long  -- zmq.h:600
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_timers_timeout";

   function zmq_timers_execute
     (timers : System.Address)
      return int  -- zmq.h:601
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_timers_execute";

      --  **************************************************************************** These functions are not documented by
      --  man pages -- use at your own risk. If you need these to be part of the formal ZMQ API, then (a) write a man page, and
      --  (b) write a test case in tests. **************************************************************************** Helper
      --  functions are used by perf tests so that they don't have to care about minutiae of time-related functions on different
      --  OS platforms. Starts the stopwatch. Returns the handle to the watch.
   function zmq_stopwatch_start return System
     .Address  -- zmq.h:614
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_stopwatch_start";

      --  Returns the number of microseconds elapsed since the stopwatch was started, but does not stop or deallocate the
      --  stopwatch.
   function zmq_stopwatch_intermediate
     (watch_u : System.Address)
      return unsigned_long  -- zmq.h:618
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_stopwatch_intermediate";

      --  Stops the stopwatch. Returns the number of microseconds elapsed since the stopwatch was started, and deallocates that
      --  watch.
   function zmq_stopwatch_stop
     (watch_u : System.Address)
      return unsigned_long  -- zmq.h:622
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_stopwatch_stop";

      --  Sleeps for specified number of seconds.
   procedure zmq_sleep
     (seconds_u : int)  -- zmq.h:625
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_sleep";

      --  skipped function type zmq_thread_fn

      -- Start a thread. Returns a handle to the thread.
   function zmq_threadstart
     (func_u : access procedure (arg1 : System.Address); arg_u : System.Address) return System
     .Address  -- zmq.h:630
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_threadstart";

      -- Wait for thread to complete then free up resources.
   procedure zmq_threadclose
     (thread_u : System
        .Address)  -- zmq.h:633
   with
      Import        => True,
      Convention    => C,
      External_Name => "zmq_threadclose";

      --  **************************************************************************** These functions are
      --  DRAFT and disabled in stable releases, and subject to change at ANY time until declared stable.
      --  **************************************************************************** DRAFT Socket types. DRAFT Socket options.
      --  DRAFT Context options DRAFT Socket methods. DRAFT Msg methods. DRAFT Msg property names. Router notify options
      --  **************************************************************************** Poller polling on sockets,fd and
      --  thread-safe sockets **************************************************************************** DRAFT Socket
      --  monitoring events
end ZMQ.Low_Level;
