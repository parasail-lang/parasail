pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with stddef_h;
limited with x86_64_linux_gnu_bits_uio_h;
with stdint_h;

package zmq_h is

   ZMQ_VERSION_MAJOR : constant := 4;  --  /homes/taft/_distrib/include/zmq.h:42
   ZMQ_VERSION_MINOR : constant := 3;  --  /homes/taft/_distrib/include/zmq.h:43
   ZMQ_VERSION_PATCH : constant := 4;  --  /homes/taft/_distrib/include/zmq.h:44
   --  arg-macro: function ZMQ_MAKE_VERSION (major, minor, patch)
   --    return (major) *10000 + (minor) *100 + (patch);
   --  unsupported macro: ZMQ_VERSION ZMQ_MAKE_VERSION (ZMQ_VERSION_MAJOR, ZMQ_VERSION_MINOR, ZMQ_VERSION_PATCH)

   ZMQ_DEFINED_STDINT : constant := 1;  --  /homes/taft/_distrib/include/zmq.h:96

   ZMQ_HAUSNUMERO : constant := 156384712;  --  /homes/taft/_distrib/include/zmq.h:133
   --  unsupported macro: EFSM (ZMQ_HAUSNUMERO + 51)
   --  unsupported macro: ENOCOMPATPROTO (ZMQ_HAUSNUMERO + 52)
   --  unsupported macro: ETERM (ZMQ_HAUSNUMERO + 53)
   --  unsupported macro: EMTHREAD (ZMQ_HAUSNUMERO + 54)

   ZMQ_IO_THREADS : constant := 1;  --  /homes/taft/_distrib/include/zmq.h:214
   ZMQ_MAX_SOCKETS : constant := 2;  --  /homes/taft/_distrib/include/zmq.h:215
   ZMQ_SOCKET_LIMIT : constant := 3;  --  /homes/taft/_distrib/include/zmq.h:216
   ZMQ_THREAD_PRIORITY : constant := 3;  --  /homes/taft/_distrib/include/zmq.h:217
   ZMQ_THREAD_SCHED_POLICY : constant := 4;  --  /homes/taft/_distrib/include/zmq.h:218
   ZMQ_MAX_MSGSZ : constant := 5;  --  /homes/taft/_distrib/include/zmq.h:219
   ZMQ_MSG_T_SIZE : constant := 6;  --  /homes/taft/_distrib/include/zmq.h:220
   ZMQ_THREAD_AFFINITY_CPU_ADD : constant := 7;  --  /homes/taft/_distrib/include/zmq.h:221
   ZMQ_THREAD_AFFINITY_CPU_REMOVE : constant := 8;  --  /homes/taft/_distrib/include/zmq.h:222
   ZMQ_THREAD_NAME_PREFIX : constant := 9;  --  /homes/taft/_distrib/include/zmq.h:223

   ZMQ_IO_THREADS_DFLT : constant := 1;  --  /homes/taft/_distrib/include/zmq.h:226
   ZMQ_MAX_SOCKETS_DFLT : constant := 1023;  --  /homes/taft/_distrib/include/zmq.h:227
   ZMQ_THREAD_PRIORITY_DFLT : constant := -1;  --  /homes/taft/_distrib/include/zmq.h:228
   ZMQ_THREAD_SCHED_POLICY_DFLT : constant := -1;  --  /homes/taft/_distrib/include/zmq.h:229

   ZMQ_PAIR : constant := 0;  --  /homes/taft/_distrib/include/zmq.h:291
   ZMQ_PUB : constant := 1;  --  /homes/taft/_distrib/include/zmq.h:292
   ZMQ_SUB : constant := 2;  --  /homes/taft/_distrib/include/zmq.h:293
   ZMQ_REQ : constant := 3;  --  /homes/taft/_distrib/include/zmq.h:294
   ZMQ_REP : constant := 4;  --  /homes/taft/_distrib/include/zmq.h:295
   ZMQ_DEALER : constant := 5;  --  /homes/taft/_distrib/include/zmq.h:296
   ZMQ_ROUTER : constant := 6;  --  /homes/taft/_distrib/include/zmq.h:297
   ZMQ_PULL : constant := 7;  --  /homes/taft/_distrib/include/zmq.h:298
   ZMQ_PUSH : constant := 8;  --  /homes/taft/_distrib/include/zmq.h:299
   ZMQ_XPUB : constant := 9;  --  /homes/taft/_distrib/include/zmq.h:300
   ZMQ_XSUB : constant := 10;  --  /homes/taft/_distrib/include/zmq.h:301
   ZMQ_STREAM : constant := 11;  --  /homes/taft/_distrib/include/zmq.h:302
   --  unsupported macro: ZMQ_XREQ ZMQ_DEALER
   --  unsupported macro: ZMQ_XREP ZMQ_ROUTER

   ZMQ_AFFINITY : constant := 4;  --  /homes/taft/_distrib/include/zmq.h:309
   ZMQ_ROUTING_ID : constant := 5;  --  /homes/taft/_distrib/include/zmq.h:310
   ZMQ_SUBSCRIBE : constant := 6;  --  /homes/taft/_distrib/include/zmq.h:311
   ZMQ_UNSUBSCRIBE : constant := 7;  --  /homes/taft/_distrib/include/zmq.h:312
   ZMQ_RATE : constant := 8;  --  /homes/taft/_distrib/include/zmq.h:313
   ZMQ_RECOVERY_IVL : constant := 9;  --  /homes/taft/_distrib/include/zmq.h:314
   ZMQ_SNDBUF : constant := 11;  --  /homes/taft/_distrib/include/zmq.h:315
   ZMQ_RCVBUF : constant := 12;  --  /homes/taft/_distrib/include/zmq.h:316
   ZMQ_RCVMORE : constant := 13;  --  /homes/taft/_distrib/include/zmq.h:317
   ZMQ_FD : constant := 14;  --  /homes/taft/_distrib/include/zmq.h:318
   ZMQ_EVENTS : constant := 15;  --  /homes/taft/_distrib/include/zmq.h:319
   ZMQ_TYPE : constant := 16;  --  /homes/taft/_distrib/include/zmq.h:320
   ZMQ_LINGER : constant := 17;  --  /homes/taft/_distrib/include/zmq.h:321
   ZMQ_RECONNECT_IVL : constant := 18;  --  /homes/taft/_distrib/include/zmq.h:322
   ZMQ_BACKLOG : constant := 19;  --  /homes/taft/_distrib/include/zmq.h:323
   ZMQ_RECONNECT_IVL_MAX : constant := 21;  --  /homes/taft/_distrib/include/zmq.h:324
   ZMQ_MAXMSGSIZE : constant := 22;  --  /homes/taft/_distrib/include/zmq.h:325
   ZMQ_SNDHWM : constant := 23;  --  /homes/taft/_distrib/include/zmq.h:326
   ZMQ_RCVHWM : constant := 24;  --  /homes/taft/_distrib/include/zmq.h:327
   ZMQ_MULTICAST_HOPS : constant := 25;  --  /homes/taft/_distrib/include/zmq.h:328
   ZMQ_RCVTIMEO : constant := 27;  --  /homes/taft/_distrib/include/zmq.h:329
   ZMQ_SNDTIMEO : constant := 28;  --  /homes/taft/_distrib/include/zmq.h:330
   ZMQ_LAST_ENDPOINT : constant := 32;  --  /homes/taft/_distrib/include/zmq.h:331
   ZMQ_ROUTER_MANDATORY : constant := 33;  --  /homes/taft/_distrib/include/zmq.h:332
   ZMQ_TCP_KEEPALIVE : constant := 34;  --  /homes/taft/_distrib/include/zmq.h:333
   ZMQ_TCP_KEEPALIVE_CNT : constant := 35;  --  /homes/taft/_distrib/include/zmq.h:334
   ZMQ_TCP_KEEPALIVE_IDLE : constant := 36;  --  /homes/taft/_distrib/include/zmq.h:335
   ZMQ_TCP_KEEPALIVE_INTVL : constant := 37;  --  /homes/taft/_distrib/include/zmq.h:336
   ZMQ_IMMEDIATE : constant := 39;  --  /homes/taft/_distrib/include/zmq.h:337
   ZMQ_XPUB_VERBOSE : constant := 40;  --  /homes/taft/_distrib/include/zmq.h:338
   ZMQ_ROUTER_RAW : constant := 41;  --  /homes/taft/_distrib/include/zmq.h:339
   ZMQ_IPV6 : constant := 42;  --  /homes/taft/_distrib/include/zmq.h:340
   ZMQ_MECHANISM : constant := 43;  --  /homes/taft/_distrib/include/zmq.h:341
   ZMQ_PLAIN_SERVER : constant := 44;  --  /homes/taft/_distrib/include/zmq.h:342
   ZMQ_PLAIN_USERNAME : constant := 45;  --  /homes/taft/_distrib/include/zmq.h:343
   ZMQ_PLAIN_PASSWORD : constant := 46;  --  /homes/taft/_distrib/include/zmq.h:344
   ZMQ_CURVE_SERVER : constant := 47;  --  /homes/taft/_distrib/include/zmq.h:345
   ZMQ_CURVE_PUBLICKEY : constant := 48;  --  /homes/taft/_distrib/include/zmq.h:346
   ZMQ_CURVE_SECRETKEY : constant := 49;  --  /homes/taft/_distrib/include/zmq.h:347
   ZMQ_CURVE_SERVERKEY : constant := 50;  --  /homes/taft/_distrib/include/zmq.h:348
   ZMQ_PROBE_ROUTER : constant := 51;  --  /homes/taft/_distrib/include/zmq.h:349
   ZMQ_REQ_CORRELATE : constant := 52;  --  /homes/taft/_distrib/include/zmq.h:350
   ZMQ_REQ_RELAXED : constant := 53;  --  /homes/taft/_distrib/include/zmq.h:351
   ZMQ_CONFLATE : constant := 54;  --  /homes/taft/_distrib/include/zmq.h:352
   ZMQ_ZAP_DOMAIN : constant := 55;  --  /homes/taft/_distrib/include/zmq.h:353
   ZMQ_ROUTER_HANDOVER : constant := 56;  --  /homes/taft/_distrib/include/zmq.h:354
   ZMQ_TOS : constant := 57;  --  /homes/taft/_distrib/include/zmq.h:355
   ZMQ_CONNECT_ROUTING_ID : constant := 61;  --  /homes/taft/_distrib/include/zmq.h:356
   ZMQ_GSSAPI_SERVER : constant := 62;  --  /homes/taft/_distrib/include/zmq.h:357
   ZMQ_GSSAPI_PRINCIPAL : constant := 63;  --  /homes/taft/_distrib/include/zmq.h:358
   ZMQ_GSSAPI_SERVICE_PRINCIPAL : constant := 64;  --  /homes/taft/_distrib/include/zmq.h:359
   ZMQ_GSSAPI_PLAINTEXT : constant := 65;  --  /homes/taft/_distrib/include/zmq.h:360
   ZMQ_HANDSHAKE_IVL : constant := 66;  --  /homes/taft/_distrib/include/zmq.h:361
   ZMQ_SOCKS_PROXY : constant := 68;  --  /homes/taft/_distrib/include/zmq.h:362
   ZMQ_XPUB_NODROP : constant := 69;  --  /homes/taft/_distrib/include/zmq.h:363
   ZMQ_BLOCKY : constant := 70;  --  /homes/taft/_distrib/include/zmq.h:364
   ZMQ_XPUB_MANUAL : constant := 71;  --  /homes/taft/_distrib/include/zmq.h:365
   ZMQ_XPUB_WELCOME_MSG : constant := 72;  --  /homes/taft/_distrib/include/zmq.h:366
   ZMQ_STREAM_NOTIFY : constant := 73;  --  /homes/taft/_distrib/include/zmq.h:367
   ZMQ_INVERT_MATCHING : constant := 74;  --  /homes/taft/_distrib/include/zmq.h:368
   ZMQ_HEARTBEAT_IVL : constant := 75;  --  /homes/taft/_distrib/include/zmq.h:369
   ZMQ_HEARTBEAT_TTL : constant := 76;  --  /homes/taft/_distrib/include/zmq.h:370
   ZMQ_HEARTBEAT_TIMEOUT : constant := 77;  --  /homes/taft/_distrib/include/zmq.h:371
   ZMQ_XPUB_VERBOSER : constant := 78;  --  /homes/taft/_distrib/include/zmq.h:372
   ZMQ_CONNECT_TIMEOUT : constant := 79;  --  /homes/taft/_distrib/include/zmq.h:373
   ZMQ_TCP_MAXRT : constant := 80;  --  /homes/taft/_distrib/include/zmq.h:374
   ZMQ_THREAD_SAFE : constant := 81;  --  /homes/taft/_distrib/include/zmq.h:375
   ZMQ_MULTICAST_MAXTPDU : constant := 84;  --  /homes/taft/_distrib/include/zmq.h:376
   ZMQ_VMCI_BUFFER_SIZE : constant := 85;  --  /homes/taft/_distrib/include/zmq.h:377
   ZMQ_VMCI_BUFFER_MIN_SIZE : constant := 86;  --  /homes/taft/_distrib/include/zmq.h:378
   ZMQ_VMCI_BUFFER_MAX_SIZE : constant := 87;  --  /homes/taft/_distrib/include/zmq.h:379
   ZMQ_VMCI_CONNECT_TIMEOUT : constant := 88;  --  /homes/taft/_distrib/include/zmq.h:380
   ZMQ_USE_FD : constant := 89;  --  /homes/taft/_distrib/include/zmq.h:381
   ZMQ_GSSAPI_PRINCIPAL_NAMETYPE : constant := 90;  --  /homes/taft/_distrib/include/zmq.h:382
   ZMQ_GSSAPI_SERVICE_PRINCIPAL_NAMETYPE : constant := 91;  --  /homes/taft/_distrib/include/zmq.h:383
   ZMQ_BINDTODEVICE : constant := 92;  --  /homes/taft/_distrib/include/zmq.h:384

   ZMQ_MORE : constant := 1;  --  /homes/taft/_distrib/include/zmq.h:387
   ZMQ_SHARED : constant := 3;  --  /homes/taft/_distrib/include/zmq.h:388

   ZMQ_DONTWAIT : constant := 1;  --  /homes/taft/_distrib/include/zmq.h:391
   ZMQ_SNDMORE : constant := 2;  --  /homes/taft/_distrib/include/zmq.h:392

   ZMQ_NULL : constant := 0;  --  /homes/taft/_distrib/include/zmq.h:395
   ZMQ_PLAIN : constant := 1;  --  /homes/taft/_distrib/include/zmq.h:396
   ZMQ_CURVE : constant := 2;  --  /homes/taft/_distrib/include/zmq.h:397
   ZMQ_GSSAPI : constant := 3;  --  /homes/taft/_distrib/include/zmq.h:398

   ZMQ_GROUP_MAX_LENGTH : constant := 255;  --  /homes/taft/_distrib/include/zmq.h:401
   --  unsupported macro: ZMQ_IDENTITY ZMQ_ROUTING_ID
   --  unsupported macro: ZMQ_CONNECT_RID ZMQ_CONNECT_ROUTING_ID

   ZMQ_TCP_ACCEPT_FILTER : constant := 38;  --  /homes/taft/_distrib/include/zmq.h:406
   ZMQ_IPC_FILTER_PID : constant := 58;  --  /homes/taft/_distrib/include/zmq.h:407
   ZMQ_IPC_FILTER_UID : constant := 59;  --  /homes/taft/_distrib/include/zmq.h:408
   ZMQ_IPC_FILTER_GID : constant := 60;  --  /homes/taft/_distrib/include/zmq.h:409
   ZMQ_IPV4ONLY : constant := 31;  --  /homes/taft/_distrib/include/zmq.h:410
   --  unsupported macro: ZMQ_DELAY_ATTACH_ON_CONNECT ZMQ_IMMEDIATE
   --  unsupported macro: ZMQ_NOBLOCK ZMQ_DONTWAIT
   --  unsupported macro: ZMQ_FAIL_UNROUTABLE ZMQ_ROUTER_MANDATORY
   --  unsupported macro: ZMQ_ROUTER_BEHAVIOR ZMQ_ROUTER_MANDATORY

   ZMQ_SRCFD : constant := 2;  --  /homes/taft/_distrib/include/zmq.h:417

   ZMQ_GSSAPI_NT_HOSTBASED : constant := 0;  --  /homes/taft/_distrib/include/zmq.h:424
   ZMQ_GSSAPI_NT_USER_NAME : constant := 1;  --  /homes/taft/_distrib/include/zmq.h:425
   ZMQ_GSSAPI_NT_KRB5_PRINCIPAL : constant := 2;  --  /homes/taft/_distrib/include/zmq.h:426

   ZMQ_EVENT_CONNECTED : constant := 16#0001#;  --  /homes/taft/_distrib/include/zmq.h:434
   ZMQ_EVENT_CONNECT_DELAYED : constant := 16#0002#;  --  /homes/taft/_distrib/include/zmq.h:435
   ZMQ_EVENT_CONNECT_RETRIED : constant := 16#0004#;  --  /homes/taft/_distrib/include/zmq.h:436
   ZMQ_EVENT_LISTENING : constant := 16#0008#;  --  /homes/taft/_distrib/include/zmq.h:437
   ZMQ_EVENT_BIND_FAILED : constant := 16#0010#;  --  /homes/taft/_distrib/include/zmq.h:438
   ZMQ_EVENT_ACCEPTED : constant := 16#0020#;  --  /homes/taft/_distrib/include/zmq.h:439
   ZMQ_EVENT_ACCEPT_FAILED : constant := 16#0040#;  --  /homes/taft/_distrib/include/zmq.h:440
   ZMQ_EVENT_CLOSED : constant := 16#0080#;  --  /homes/taft/_distrib/include/zmq.h:441
   ZMQ_EVENT_CLOSE_FAILED : constant := 16#0100#;  --  /homes/taft/_distrib/include/zmq.h:442
   ZMQ_EVENT_DISCONNECTED : constant := 16#0200#;  --  /homes/taft/_distrib/include/zmq.h:443
   ZMQ_EVENT_MONITOR_STOPPED : constant := 16#0400#;  --  /homes/taft/_distrib/include/zmq.h:444
   ZMQ_EVENT_ALL : constant := 16#FFFF#;  --  /homes/taft/_distrib/include/zmq.h:445

   ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL : constant := 16#0800#;  --  /homes/taft/_distrib/include/zmq.h:447

   ZMQ_EVENT_HANDSHAKE_SUCCEEDED : constant := 16#1000#;  --  /homes/taft/_distrib/include/zmq.h:450

   ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL : constant := 16#2000#;  --  /homes/taft/_distrib/include/zmq.h:453

   ZMQ_EVENT_HANDSHAKE_FAILED_AUTH : constant := 16#4000#;  --  /homes/taft/_distrib/include/zmq.h:456
   ZMQ_PROTOCOL_ERROR_ZMTP_UNSPECIFIED : constant := 16#10000000#;  --  /homes/taft/_distrib/include/zmq.h:457
   ZMQ_PROTOCOL_ERROR_ZMTP_UNEXPECTED_COMMAND : constant := 16#10000001#;  --  /homes/taft/_distrib/include/zmq.h:458
   ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_SEQUENCE : constant := 16#10000002#;  --  /homes/taft/_distrib/include/zmq.h:459
   ZMQ_PROTOCOL_ERROR_ZMTP_KEY_EXCHANGE : constant := 16#10000003#;  --  /homes/taft/_distrib/include/zmq.h:460
   ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_UNSPECIFIED : constant := 16#10000011#;  --  /homes/taft/_distrib/include/zmq.h:461
   ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_MESSAGE : constant := 16#10000012#;  --  /homes/taft/_distrib/include/zmq.h:462
   ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_HELLO : constant := 16#10000013#;  --  /homes/taft/_distrib/include/zmq.h:463
   ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_INITIATE : constant := 16#10000014#;  --  /homes/taft/_distrib/include/zmq.h:464
   ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_ERROR : constant := 16#10000015#;  --  /homes/taft/_distrib/include/zmq.h:465
   ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_READY : constant := 16#10000016#;  --  /homes/taft/_distrib/include/zmq.h:466
   ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_WELCOME : constant := 16#10000017#;  --  /homes/taft/_distrib/include/zmq.h:467
   ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_METADATA : constant := 16#10000018#;  --  /homes/taft/_distrib/include/zmq.h:468

   ZMQ_PROTOCOL_ERROR_ZMTP_CRYPTOGRAPHIC : constant := 16#11000001#;  --  /homes/taft/_distrib/include/zmq.h:470
   ZMQ_PROTOCOL_ERROR_ZMTP_MECHANISM_MISMATCH : constant := 16#11000002#;  --  /homes/taft/_distrib/include/zmq.h:471
   ZMQ_PROTOCOL_ERROR_ZAP_UNSPECIFIED : constant := 16#20000000#;  --  /homes/taft/_distrib/include/zmq.h:472
   ZMQ_PROTOCOL_ERROR_ZAP_MALFORMED_REPLY : constant := 16#20000001#;  --  /homes/taft/_distrib/include/zmq.h:473
   ZMQ_PROTOCOL_ERROR_ZAP_BAD_REQUEST_ID : constant := 16#20000002#;  --  /homes/taft/_distrib/include/zmq.h:474
   ZMQ_PROTOCOL_ERROR_ZAP_BAD_VERSION : constant := 16#20000003#;  --  /homes/taft/_distrib/include/zmq.h:475
   ZMQ_PROTOCOL_ERROR_ZAP_INVALID_STATUS_CODE : constant := 16#20000004#;  --  /homes/taft/_distrib/include/zmq.h:476
   ZMQ_PROTOCOL_ERROR_ZAP_INVALID_METADATA : constant := 16#20000005#;  --  /homes/taft/_distrib/include/zmq.h:477
   ZMQ_PROTOCOL_ERROR_WS_UNSPECIFIED : constant := 16#30000000#;  --  /homes/taft/_distrib/include/zmq.h:478

   ZMQ_POLLIN : constant := 1;  --  /homes/taft/_distrib/include/zmq.h:515
   ZMQ_POLLOUT : constant := 2;  --  /homes/taft/_distrib/include/zmq.h:516
   ZMQ_POLLERR : constant := 4;  --  /homes/taft/_distrib/include/zmq.h:517
   ZMQ_POLLPRI : constant := 8;  --  /homes/taft/_distrib/include/zmq.h:518

   ZMQ_POLLITEMS_DFLT : constant := 16;  --  /homes/taft/_distrib/include/zmq.h:528

   ZMQ_HAS_CAPABILITIES : constant := 1;  --  /homes/taft/_distrib/include/zmq.h:546

   ZMQ_STREAMER : constant := 1;  --  /homes/taft/_distrib/include/zmq.h:550
   ZMQ_FORWARDER : constant := 2;  --  /homes/taft/_distrib/include/zmq.h:551
   ZMQ_QUEUE : constant := 3;  --  /homes/taft/_distrib/include/zmq.h:552

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

  --  Version macros for compile-time API version detection                      
  --  Set target version to Windows Server 2008, Windows Vista or higher.
  --  Windows XP (0x0501) is supported but without client & server socket types.
  --  Require Windows XP or higher with MinGW for getaddrinfo().
  --  Handle DSO symbol visibility                                              
  --  Define integer types needed for event interface                           
  --  32-bit AIX's pollfd struct members are called reqevents and rtnevents so it
  --  defines compatibility macros for them. Need to include that header first to
  --  stop build failures since zmq_pollset_t defines them as events and revents.
  --**************************************************************************** 
  --  0MQ errors.                                                                
  --**************************************************************************** 
  --  A number random enough not to collide with different errno ranges on       
  --  different OSes. The assumption is that error_t is at least 32-bit type.    
  --  On Windows platform some of the standard POSIX errnos are not defined.     
  --  Native 0MQ error codes.                                                    
  --  This function retrieves the errno as it is known to 0MQ library. The goal  
  --  of this function is to make the code 100% portable, including where 0MQ    
  --  compiled with certain CRT library (on Windows) is linked to an             
  --  application that uses different CRT library.                               
   function zmq_errno return int  -- /homes/taft/_distrib/include/zmq.h:201
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_errno";

  --  Resolves system errors and 0MQ errors to human-readable string.            
   function zmq_strerror (errnum_u : int) return Interfaces.C.Strings.chars_ptr  -- /homes/taft/_distrib/include/zmq.h:204
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_strerror";

  --  Run-time API version detection                                             
   procedure zmq_version
     (major_u : access int;
      minor_u : access int;
      patch_u : access int)  -- /homes/taft/_distrib/include/zmq.h:207
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_version";

  --**************************************************************************** 
  --  0MQ infrastructure (a.k.a. context) initialisation & termination.          
  --**************************************************************************** 
  --  Context options                                                            
  --  Default for new contexts                                                   
   function zmq_ctx_new return System.Address  -- /homes/taft/_distrib/include/zmq.h:231
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_ctx_new";

   function zmq_ctx_term (context_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:232
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_ctx_term";

   function zmq_ctx_shutdown (context_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:233
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_ctx_shutdown";

   function zmq_ctx_set
     (context_u : System.Address;
      option_u : int;
      optval_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:234
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_ctx_set";

   function zmq_ctx_get (context_u : System.Address; option_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:235
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_ctx_get";

  --  Old (legacy) API                                                           
   function zmq_init (io_threads_u : int) return System.Address  -- /homes/taft/_distrib/include/zmq.h:238
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_init";

   function zmq_term (context_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:239
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_term";

   function zmq_ctx_destroy (context_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:240
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_ctx_destroy";

  --**************************************************************************** 
  --  0MQ message definition.                                                    
  --**************************************************************************** 
  -- Some architectures, like sparc64 and some variants of aarch64, enforce pointer
  -- * alignment and raise sigbus on violations. Make sure applications allocate
  -- * zmq_msg_t on addresses aligned on a pointer-size boundary to avoid this issue.
  --  

   type zmq_msg_t_array1157 is array (0 .. 63) of aliased unsigned_char;
   type zmq_msg_t is record
      u_u : aliased zmq_msg_t_array1157;  -- /homes/taft/_distrib/include/zmq.h:261
   end record
   with Convention => C_Pass_By_Copy;  -- /homes/taft/_distrib/include/zmq.h:251

   --  skipped function type zmq_free_fn

   function zmq_msg_init (msg_u : access zmq_msg_t) return int  -- /homes/taft/_distrib/include/zmq.h:269
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_init";

   function zmq_msg_init_size (msg_u : access zmq_msg_t; size_u : stddef_h.size_t) return int  -- /homes/taft/_distrib/include/zmq.h:270
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_init_size";

   function zmq_msg_init_data
     (msg_u : access zmq_msg_t;
      data_u : System.Address;
      size_u : stddef_h.size_t;
      ffn_u : access procedure (arg1 : System.Address; arg2 : System.Address);
      hint_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:271
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_init_data";

   function zmq_msg_send
     (msg_u : access zmq_msg_t;
      s_u : System.Address;
      flags_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:273
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_send";

   function zmq_msg_recv
     (msg_u : access zmq_msg_t;
      s_u : System.Address;
      flags_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:274
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_recv";

   function zmq_msg_close (msg_u : access zmq_msg_t) return int  -- /homes/taft/_distrib/include/zmq.h:275
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_close";

   function zmq_msg_move (dest_u : access zmq_msg_t; src_u : access zmq_msg_t) return int  -- /homes/taft/_distrib/include/zmq.h:276
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_move";

   function zmq_msg_copy (dest_u : access zmq_msg_t; src_u : access zmq_msg_t) return int  -- /homes/taft/_distrib/include/zmq.h:277
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_copy";

   function zmq_msg_data (msg_u : access zmq_msg_t) return System.Address  -- /homes/taft/_distrib/include/zmq.h:278
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_data";

   function zmq_msg_size (msg_u : access constant zmq_msg_t) return stddef_h.size_t  -- /homes/taft/_distrib/include/zmq.h:279
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_size";

   function zmq_msg_more (msg_u : access constant zmq_msg_t) return int  -- /homes/taft/_distrib/include/zmq.h:280
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_more";

   function zmq_msg_get (msg_u : access constant zmq_msg_t; property_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:281
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_get";

   function zmq_msg_set
     (msg_u : access zmq_msg_t;
      property_u : int;
      optval_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:282
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_set";

   function zmq_msg_gets (msg_u : access constant zmq_msg_t; property_u : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /homes/taft/_distrib/include/zmq.h:283
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_msg_gets";

  --**************************************************************************** 
  --  0MQ socket definition.                                                     
  --**************************************************************************** 
  --  Socket types.                                                              
  --  Deprecated aliases                                                         
  --  Socket options.                                                            
  --  Message options                                                            
  --  Send/recv options.                                                         
  --  Security mechanisms                                                        
  --  RADIO-DISH protocol                                                        
  --  Deprecated options and aliases                                             
  --  Deprecated Message options                                                 
  --**************************************************************************** 
  --  GSSAPI definitions                                                         
  --**************************************************************************** 
  --  GSSAPI principal name types                                                
  --**************************************************************************** 
  --  0MQ socket events and monitoring                                           
  --**************************************************************************** 
  --  Socket transport events (TCP, IPC and TIPC only)                           
  --  Unspecified system errors during handshake. Event value is an errno.       
  --  Handshake complete successfully with successful authentication (if        *
  -- *  enabled). Event value is unused.                                           

  --  Protocol errors between ZMTP peers or between server and ZAP handler.     *
  -- *  Event value is one of ZMQ_PROTOCOL_ERROR_*                                 

  --  Failed authentication requests. Event value is the numeric ZAP status     *
  -- *  code, i.e. 300, 400 or 500.                                                

  -- the following two may be due to erroneous configuration of a peer
   function zmq_socket (arg1 : System.Address; type_u : int) return System.Address  -- /homes/taft/_distrib/include/zmq.h:480
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_socket";

   function zmq_close (s_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:481
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_close";

   function zmq_setsockopt
     (s_u : System.Address;
      option_u : int;
      optval_u : System.Address;
      optvallen_u : stddef_h.size_t) return int  -- /homes/taft/_distrib/include/zmq.h:483
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_setsockopt";

   function zmq_getsockopt
     (s_u : System.Address;
      option_u : int;
      optval_u : System.Address;
      optvallen_u : access stddef_h.size_t) return int  -- /homes/taft/_distrib/include/zmq.h:485
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_getsockopt";

   function zmq_bind (s_u : System.Address; addr_u : Interfaces.C.Strings.chars_ptr) return int  -- /homes/taft/_distrib/include/zmq.h:486
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_bind";

   function zmq_connect (s_u : System.Address; addr_u : Interfaces.C.Strings.chars_ptr) return int  -- /homes/taft/_distrib/include/zmq.h:487
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_connect";

   function zmq_unbind (s_u : System.Address; addr_u : Interfaces.C.Strings.chars_ptr) return int  -- /homes/taft/_distrib/include/zmq.h:488
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_unbind";

   function zmq_disconnect (s_u : System.Address; addr_u : Interfaces.C.Strings.chars_ptr) return int  -- /homes/taft/_distrib/include/zmq.h:489
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_disconnect";

   function zmq_send
     (s_u : System.Address;
      buf_u : System.Address;
      len_u : stddef_h.size_t;
      flags_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:490
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_send";

   function zmq_send_const
     (s_u : System.Address;
      buf_u : System.Address;
      len_u : stddef_h.size_t;
      flags_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:492
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_send_const";

   function zmq_recv
     (s_u : System.Address;
      buf_u : System.Address;
      len_u : stddef_h.size_t;
      flags_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:493
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_recv";

   function zmq_socket_monitor
     (s_u : System.Address;
      addr_u : Interfaces.C.Strings.chars_ptr;
      events_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:494
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_socket_monitor";

  --**************************************************************************** 
  --  Hide socket fd type; this was before zmq_poller_event_t typedef below      
  --**************************************************************************** 
  -- Windows uses a pointer-sized unsigned integer to store the socket fd.
   subtype zmq_fd_t is int;  -- /homes/taft/_distrib/include/zmq.h:508

  --**************************************************************************** 
  --  Deprecated I/O multiplexing. Prefer using zmq_poller API                   
  --**************************************************************************** 
   type zmq_pollitem_t is record
      socket : System.Address;  -- /homes/taft/_distrib/include/zmq.h:522
      fd : aliased zmq_fd_t;  -- /homes/taft/_distrib/include/zmq.h:523
      events : aliased short;  -- /homes/taft/_distrib/include/zmq.h:524
      revents : aliased short;  -- /homes/taft/_distrib/include/zmq.h:525
   end record
   with Convention => C_Pass_By_Copy;  -- /homes/taft/_distrib/include/zmq.h:520

   function zmq_poll
     (items_u : access zmq_pollitem_t;
      nitems_u : int;
      timeout_u : long) return int  -- /homes/taft/_distrib/include/zmq.h:530
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_poll";

  --**************************************************************************** 
  --  Message proxying                                                           
  --**************************************************************************** 
   function zmq_proxy
     (frontend_u : System.Address;
      backend_u : System.Address;
      capture_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:536
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_proxy";

   function zmq_proxy_steerable
     (frontend_u : System.Address;
      backend_u : System.Address;
      capture_u : System.Address;
      control_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:537
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_proxy_steerable";

  --**************************************************************************** 
  --  Probe library capabilities                                                 
  --**************************************************************************** 
   function zmq_has (capability_u : Interfaces.C.Strings.chars_ptr) return int  -- /homes/taft/_distrib/include/zmq.h:547
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_has";

  --  Deprecated aliases  
  --  Deprecated methods  
   function zmq_device
     (type_u : int;
      frontend_u : System.Address;
      backend_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:555
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_device";

   function zmq_sendmsg
     (s_u : System.Address;
      msg_u : access zmq_msg_t;
      flags_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:556
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_sendmsg";

   function zmq_recvmsg
     (s_u : System.Address;
      msg_u : access zmq_msg_t;
      flags_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:557
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_recvmsg";

   function zmq_sendiov
     (s_u : System.Address;
      iov_u : access x86_64_linux_gnu_bits_uio_h.iovec;
      count_u : stddef_h.size_t;
      flags_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:560
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_sendiov";

   function zmq_recviov
     (s_u : System.Address;
      iov_u : access x86_64_linux_gnu_bits_uio_h.iovec;
      count_u : access stddef_h.size_t;
      flags_u : int) return int  -- /homes/taft/_distrib/include/zmq.h:562
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_recviov";

  --**************************************************************************** 
  --  Encryption functions                                                       
  --**************************************************************************** 
  --  Encode data with Z85 encoding. Returns encoded data                        
   function zmq_z85_encode
     (dest_u : Interfaces.C.Strings.chars_ptr;
      data_u : access stdint_h.uint8_t;
      size_u : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /homes/taft/_distrib/include/zmq.h:570
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_z85_encode";

  --  Decode data with Z85 encoding. Returns decoded data                        
   function zmq_z85_decode (dest_u : access stdint_h.uint8_t; string_u : Interfaces.C.Strings.chars_ptr) return access stdint_h.uint8_t  -- /homes/taft/_distrib/include/zmq.h:573
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_z85_decode";

  --  Generate z85-encoded public and private keypair with tweetnacl/libsodium.  
  --  Returns 0 on success.                                                      
   function zmq_curve_keypair (z85_public_key_u : Interfaces.C.Strings.chars_ptr; z85_secret_key_u : Interfaces.C.Strings.chars_ptr) return int  -- /homes/taft/_distrib/include/zmq.h:577
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_curve_keypair";

  --  Derive the z85-encoded public key from the z85-encoded secret key.         
  --  Returns 0 on success.                                                      
   function zmq_curve_public (z85_public_key_u : Interfaces.C.Strings.chars_ptr; z85_secret_key_u : Interfaces.C.Strings.chars_ptr) return int  -- /homes/taft/_distrib/include/zmq.h:581
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_curve_public";

  --**************************************************************************** 
  --  Atomic utility methods                                                     
  --**************************************************************************** 
   function zmq_atomic_counter_new return System.Address  -- /homes/taft/_distrib/include/zmq.h:588
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_atomic_counter_new";

   procedure zmq_atomic_counter_set (counter_u : System.Address; value_u : int)  -- /homes/taft/_distrib/include/zmq.h:589
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_atomic_counter_set";

   function zmq_atomic_counter_inc (counter_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:590
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_atomic_counter_inc";

   function zmq_atomic_counter_dec (counter_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:591
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_atomic_counter_dec";

   function zmq_atomic_counter_value (counter_u : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:592
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_atomic_counter_value";

   procedure zmq_atomic_counter_destroy (counter_p_u : System.Address)  -- /homes/taft/_distrib/include/zmq.h:593
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_atomic_counter_destroy";

  --**************************************************************************** 
  --  Scheduling timers                                                          
  --**************************************************************************** 
   --  skipped function type zmq_timer_fn

   function zmq_timers_new return System.Address  -- /homes/taft/_distrib/include/zmq.h:603
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_timers_new";

   function zmq_timers_destroy (timers_p : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:604
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_timers_destroy";

   function zmq_timers_add
     (timers : System.Address;
      interval : stddef_h.size_t;
      handler : access procedure (arg1 : int; arg2 : System.Address);
      arg : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:606
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_timers_add";

   function zmq_timers_cancel (timers : System.Address; timer_id : int) return int  -- /homes/taft/_distrib/include/zmq.h:607
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_timers_cancel";

   function zmq_timers_set_interval
     (timers : System.Address;
      timer_id : int;
      interval : stddef_h.size_t) return int  -- /homes/taft/_distrib/include/zmq.h:609
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_timers_set_interval";

   function zmq_timers_reset (timers : System.Address; timer_id : int) return int  -- /homes/taft/_distrib/include/zmq.h:610
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_timers_reset";

   function zmq_timers_timeout (timers : System.Address) return long  -- /homes/taft/_distrib/include/zmq.h:611
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_timers_timeout";

   function zmq_timers_execute (timers : System.Address) return int  -- /homes/taft/_distrib/include/zmq.h:612
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_timers_execute";

  --**************************************************************************** 
  --  These functions are not documented by man pages -- use at your own risk.   
  --  If you need these to be part of the formal ZMQ API, then (a) write a man   
  --  page, and (b) write a test case in tests.                                  
  --**************************************************************************** 
  --  Helper functions are used by perf tests so that they don't have to care    
  --  about minutiae of time-related functions on different OS platforms.        
  --  Starts the stopwatch. Returns the handle to the watch.                     
   function zmq_stopwatch_start return System.Address  -- /homes/taft/_distrib/include/zmq.h:625
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_stopwatch_start";

  --  Returns the number of microseconds elapsed since the stopwatch was         
  --  started, but does not stop or deallocate the stopwatch.                    
   function zmq_stopwatch_intermediate (watch_u : System.Address) return unsigned_long  -- /homes/taft/_distrib/include/zmq.h:629
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_stopwatch_intermediate";

  --  Stops the stopwatch. Returns the number of microseconds elapsed since      
  --  the stopwatch was started, and deallocates that watch.                     
   function zmq_stopwatch_stop (watch_u : System.Address) return unsigned_long  -- /homes/taft/_distrib/include/zmq.h:633
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_stopwatch_stop";

  --  Sleeps for specified number of seconds.                                    
   procedure zmq_sleep (seconds_u : int)  -- /homes/taft/_distrib/include/zmq.h:636
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_sleep";

   --  skipped function type zmq_thread_fn

  -- Start a thread. Returns a handle to the thread.                             
   function zmq_threadstart (func_u : access procedure (arg1 : System.Address); arg_u : System.Address) return System.Address  -- /homes/taft/_distrib/include/zmq.h:641
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_threadstart";

  -- Wait for thread to complete then free up resources.                         
   procedure zmq_threadclose (thread_u : System.Address)  -- /homes/taft/_distrib/include/zmq.h:644
   with Import => True, 
        Convention => C, 
        External_Name => "zmq_threadclose";

  --**************************************************************************** 
  --  These functions are DRAFT and disabled in stable releases, and subject to  
  --  change at ANY time until declared stable.                                  
  --**************************************************************************** 
  --  DRAFT Socket types.                                                        
  --  DRAFT Socket options.                                                      
  --  DRAFT ZMQ_RECONNECT_STOP options                                           
  --  DRAFT Context options                                                      
  --  DRAFT Context methods.                                                     
  --  DRAFT Socket methods.                                                      
  --  DRAFT Msg methods.                                                         
  --  DRAFT Msg property names.                                                  
  --  Router notify options                                                      
  --**************************************************************************** 
  --  Poller polling on sockets,fd and thread-safe sockets                       
  --**************************************************************************** 
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

end zmq_h;
