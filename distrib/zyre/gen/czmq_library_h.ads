pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package czmq_library_h is

   CZMQ_VERSION_MAJOR : constant := 4;  --  /homes/taft/_distrib/include/czmq_library.h:29
   CZMQ_VERSION_MINOR : constant := 2;  --  /homes/taft/_distrib/include/czmq_library.h:30
   CZMQ_VERSION_PATCH : constant := 1;  --  /homes/taft/_distrib/include/czmq_library.h:31
   --  arg-macro: function CZMQ_MAKE_VERSION (major, minor, patch)
   --    return (major) * 10000 + (minor) * 100 + (patch);
   --  unsupported macro: CZMQ_VERSION CZMQ_MAKE_VERSION(CZMQ_VERSION_MAJOR, CZMQ_VERSION_MINOR, CZMQ_VERSION_PATCH)
   --  unsupported macro: CZMQ_PRIVATE __attribute__ ((visibility ("hidden")))
   --  unsupported macro: CZMQ_EXPORT __attribute__ ((visibility ("default")))

  --  =========================================================================
  --    czmq - generated layer of public API
  --    Copyright (c) the Contributors as noted in the AUTHORS file.
  --    This file is part of CZMQ, the high-level C binding for 0MQ:
  --    http://czmq.zeromq.org.
  --    This Source Code Form is subject to the terms of the Mozilla Public
  --    License, v. 2.0. If a copy of the MPL was not distributed with this
  --    file, You can obtain one at http://mozilla.org/MPL/2.0/.
  --################################################################################
  --#  THIS FILE IS 100% GENERATED BY ZPROJECT; DO NOT EDIT EXCEPT EXPERIMENTALLY  #
  --#  Read the zproject/README.md for information about making permanent changes. #
  --################################################################################
  --    =========================================================================
  -- 

  --  Set up environment for the application
  --  External dependencies
  --  CZMQ version macros for compile-time API detection
  --  Opaque class structures to allow forward references
  --  These classes are stable or legacy and built in all releases
   type u_zactor_t is null record;   -- incomplete struct

   subtype zactor_t is u_zactor_t;  -- /homes/taft/_distrib/include/czmq_library.h:68

   type u_zarmour_t is null record;   -- incomplete struct

   subtype zarmour_t is u_zarmour_t;  -- /homes/taft/_distrib/include/czmq_library.h:70

   type u_zcert_t is null record;   -- incomplete struct

   subtype zcert_t is u_zcert_t;  -- /homes/taft/_distrib/include/czmq_library.h:72

   type u_zcertstore_t is null record;   -- incomplete struct

   subtype zcertstore_t is u_zcertstore_t;  -- /homes/taft/_distrib/include/czmq_library.h:74

   type u_zchunk_t is null record;   -- incomplete struct

   subtype zchunk_t is u_zchunk_t;  -- /homes/taft/_distrib/include/czmq_library.h:76

   type u_zclock_t is null record;   -- incomplete struct

   subtype zclock_t is u_zclock_t;  -- /homes/taft/_distrib/include/czmq_library.h:78

   type u_zconfig_t is null record;   -- incomplete struct

   subtype zconfig_t is u_zconfig_t;  -- /homes/taft/_distrib/include/czmq_library.h:80

   type u_zdigest_t is null record;   -- incomplete struct

   subtype zdigest_t is u_zdigest_t;  -- /homes/taft/_distrib/include/czmq_library.h:82

   type u_zdir_t is null record;   -- incomplete struct

   subtype zdir_t is u_zdir_t;  -- /homes/taft/_distrib/include/czmq_library.h:84

   type u_zdir_patch_t is null record;   -- incomplete struct

   subtype zdir_patch_t is u_zdir_patch_t;  -- /homes/taft/_distrib/include/czmq_library.h:86

   type u_zfile_t is null record;   -- incomplete struct

   subtype zfile_t is u_zfile_t;  -- /homes/taft/_distrib/include/czmq_library.h:88

   type u_zframe_t is null record;   -- incomplete struct

   subtype zframe_t is u_zframe_t;  -- /homes/taft/_distrib/include/czmq_library.h:90

   type u_zhash_t is null record;   -- incomplete struct

   subtype zhash_t is u_zhash_t;  -- /homes/taft/_distrib/include/czmq_library.h:92

   type u_zhashx_t is null record;   -- incomplete struct

   subtype zhashx_t is u_zhashx_t;  -- /homes/taft/_distrib/include/czmq_library.h:94

   type u_ziflist_t is null record;   -- incomplete struct

   subtype ziflist_t is u_ziflist_t;  -- /homes/taft/_distrib/include/czmq_library.h:96

   type u_zlist_t is null record;   -- incomplete struct

   subtype zlist_t is u_zlist_t;  -- /homes/taft/_distrib/include/czmq_library.h:98

   type u_zlistx_t is null record;   -- incomplete struct

   subtype zlistx_t is u_zlistx_t;  -- /homes/taft/_distrib/include/czmq_library.h:100

   type u_zloop_t is null record;   -- incomplete struct

   subtype zloop_t is u_zloop_t;  -- /homes/taft/_distrib/include/czmq_library.h:102

   type u_zmsg_t is null record;   -- incomplete struct

   subtype zmsg_t is u_zmsg_t;  -- /homes/taft/_distrib/include/czmq_library.h:104

   type u_zpoller_t is null record;   -- incomplete struct

   subtype zpoller_t is u_zpoller_t;  -- /homes/taft/_distrib/include/czmq_library.h:106

   type u_zsock_t is null record;   -- incomplete struct

   subtype zsock_t is u_zsock_t;  -- /homes/taft/_distrib/include/czmq_library.h:108

   type u_zstr_t is null record;   -- incomplete struct

   subtype zstr_t is u_zstr_t;  -- /homes/taft/_distrib/include/czmq_library.h:110

   type u_zsys_t is null record;   -- incomplete struct

   subtype zsys_t is u_zsys_t;  -- /homes/taft/_distrib/include/czmq_library.h:112

   type u_zuuid_t is null record;   -- incomplete struct

   subtype zuuid_t is u_zuuid_t;  -- /homes/taft/_distrib/include/czmq_library.h:114

   type u_zauth_t is null record;   -- incomplete struct

   subtype zauth_t is u_zauth_t;  -- /homes/taft/_distrib/include/czmq_library.h:116

   type u_zbeacon_t is null record;   -- incomplete struct

   subtype zbeacon_t is u_zbeacon_t;  -- /homes/taft/_distrib/include/czmq_library.h:118

   type u_zgossip_t is null record;   -- incomplete struct

   subtype zgossip_t is u_zgossip_t;  -- /homes/taft/_distrib/include/czmq_library.h:120

   type u_zmonitor_t is null record;   -- incomplete struct

   subtype zmonitor_t is u_zmonitor_t;  -- /homes/taft/_distrib/include/czmq_library.h:122

   type u_zproxy_t is null record;   -- incomplete struct

   subtype zproxy_t is u_zproxy_t;  -- /homes/taft/_distrib/include/czmq_library.h:124

   type u_zrex_t is null record;   -- incomplete struct

   subtype zrex_t is u_zrex_t;  -- /homes/taft/_distrib/include/czmq_library.h:126

  --  Draft classes are by default not built in stable releases
  --  Public classes, each with its own header file
  --  Self test for private classes
  --################################################################################
  --#  THIS FILE IS 100% GENERATED BY ZPROJECT; DO NOT EDIT EXCEPT EXPERIMENTALLY  #
  --#  Read the zproject/README.md for information about making permanent changes. #
  --################################################################################
  -- 

end czmq_library_h;
