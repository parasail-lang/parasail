pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with netinet_in_h;
with stddef_h;
with Interfaces.C.Strings;
with System;

package czmq_prelude_h is

   --  unsupported macro: ipv4addr __inaddr_u.__addr
   --  unsupported macro: ipv6addr __inaddr_u.__addr6
   --  arg-macro: function streq (s1, s2)
   --    return notstrcmp ((s1), (s2));
   --  arg-macro: function strneq (s1, s2)
   --    return strcmp ((s1), (s2));
   --  arg-macro: procedure freen (x)
   --    do {free(x); x := NULL;} while(0)
   --  unsupported macro: ZSYS_RANDOF_FLT float
   --  unsupported macro: ZSYS_RANDOF_FUNC random
   ZSYS_RANDOF_FUNC_BITS : constant := 32;  --  /homes/taft/_distrib/include/czmq_prelude.h:475
   --  unsupported macro: ZSYS_RANDOF_MAX (UINT32_MAX>>6)
   --  arg-macro: function s_randof_factor ()
   --    return ZSYS_RANDOF_FLT)( (ZSYS_RANDOF_FLT)(ZSYS_RANDOF_FUNC() mod (ZSYS_RANDOF_MAX - 1)) / ( (ZSYS_RANDOF_FLT)(ZSYS_RANDOF_MAX) ) ;
   --  arg-macro: function randof (num)
   --    return int) ( (ZSYS_RANDOF_FLT)(num) * s_randof_factor() / ( 1.0 + s_randof_factor() / 100.0 ) ;
   --  unsupported macro: CZMQ_THREADLS __thread
   --  arg-macro: procedure zmalloc (size)
   --    safe_malloc((size), __FILE__, __LINE__)
   --  arg-macro: procedure CHECK_PRINTF (a)
   --    __attribute__((format (printf, a, a + 1)))
   --  unsupported macro: closesocket close

   INVALID_SOCKET : constant := -1;  --  /homes/taft/_distrib/include/czmq_prelude.h:672
   SOCKET_ERROR : constant := -1;  --  /homes/taft/_distrib/include/czmq_prelude.h:673

   O_BINARY : constant := 0;  --  /homes/taft/_distrib/include/czmq_prelude.h:675

   ZMQ_POLL_MSEC : constant := 1;  --  /homes/taft/_distrib/include/czmq_prelude.h:700

  --  =========================================================================
  --    czmq_prelude.h - CZMQ environment
  --    Copyright (c) the Contributors as noted in the AUTHORS file.
  --    This file is part of CZMQ, the high-level C binding for 0MQ:
  --    http://czmq.zeromq.org.
  --    This Source Code Form is subject to the terms of the Mozilla Public
  --    License, v. 2.0. If a copy of the MPL was not distributed with this
  --    file, You can obtain one at http://mozilla.org/MPL/2.0/.
  --    =========================================================================
  -- 

  --- Establish the compiler and computer system ------------------------------
  -- *  Defines zero or more of these symbols, for use in any non-portable
  -- *  code (for pre-defined values see e.g. build-system headers as well
  -- *  as output of GNU C preprocessor via `cpp -dM < /dev/null`):
  -- *
  -- *  __WINDOWS__         Microsoft C/C++ with Windows calls
  -- *  __MSDOS__           System is MS-DOS (set if __WINDOWS__ set)
  -- *  __VMS__             System is VAX/VMS or Alpha/OpenVMS
  -- *  __UNIX__            System is UNIX
  -- *  __OS2__             System is OS/2
  -- *
  -- *  __IS_32BIT__        OS/compiler is 32 bits
  -- *  __IS_64BIT__        OS/compiler is 64 bits
  -- *
  -- *  When __UNIX__ is defined, we also define exactly one of these:
  -- *
  -- *  __UTYPE_AUX         Apple AUX
  -- *  __UTYPE_BEOS        BeOS
  -- *  __UTYPE_BSDOS       BSD/OS
  -- *  __UTYPE_DECALPHA    Digital UNIX (Alpha)
  -- *  __UTYPE_IBMAIX      IBM RS/6000 AIX
  -- *  __UTYPE_FREEBSD     FreeBSD
  -- *  __UTYPE_HPUX        HP/UX
  -- *  __UTYPE_ANDROID     Android
  -- *  __UTYPE_LINUX       Linux
  -- *  __UTYPE_GNU         GNU/Hurd
  -- *  __UTYPE_MIPS        MIPS (BSD 4.3/System V mixture)
  -- *  __UTYPE_NETBSD      NetBSD
  -- *  __UTYPE_NEXT        NeXT
  -- *  __UTYPE_OPENBSD     OpenBSD
  -- *  __UTYPE_OSX         Apple Macintosh OS X
  -- *  __UTYPE_IOS         Apple iOS
  -- *  __UTYPE_QNX         QNX
  -- *  __UTYPE_IRIX        Silicon Graphics IRIX
  -- *  __UTYPE_SINIX       SINIX-N (Siemens-Nixdorf Unix)
  -- *  __UTYPE_SUNOS       SunOS
  -- *  __UTYPE_SUNSOLARIS  Sun Solaris
  -- *  __UTYPE_UNIXWARE    SCO UnixWare
  -- *                      ... these are the ones I know about so far.
  -- *  __UTYPE_GENERIC     Any other UNIX
  -- *
  -- *  When __VMS__ is defined, we may define one or more of these:
  -- *
  -- *  __VMS_XOPEN         Supports XOPEN functions
  --  

  --  Stop cheeky warnings about "deprecated" functions like fopen
  --  MSDOS               Microsoft C
  --  _MSC_VER            Microsoft C
  --  VMS                 VAX C (VAX/VMS)
  --  __VMS               Dec C (Alpha/OpenVMS)
  --  __vax__             gcc
  --  Try to define a __UTYPE_xxx symbol...
  --  unix                SunOS at least
  --  __unix__            gcc
  --  _POSIX_SOURCE is various UNIX systems, maybe also VAX/VMS
  -- Note: this rule and below should match legacy SunOS and Solaris
  -- on builds without the GNU toolchain; with one you get __UTYPE_GNU
  --- Always include ZeroMQ headers -------------------------------------------
  --- Standard ANSI include files ---------------------------------------------
  --- System-specific include files -------------------------------------------
  --  Let CZMQ build with libzmq/3.x 
  --  Must come before arpa/inet.h 
  --  Add missing defines for non-POSIX systems
  --- Check compiler data type sizes ------------------------------------------
  --- Data types --------------------------------------------------------------
  --  Single unsigned byte = 8 bits
   subtype byte is unsigned_char;  -- /homes/taft/_distrib/include/czmq_prelude.h:419

  --  Double byte = 16 bits
   subtype dbyte is unsigned_short;  -- /homes/taft/_distrib/include/czmq_prelude.h:420

  --  Quad byte = 32 bits
   subtype qbyte is unsigned;  -- /homes/taft/_distrib/include/czmq_prelude.h:421

  --  Internet socket address structure
   subtype inaddr_t is netinet_in_h.sockaddr_in;  -- /homes/taft/_distrib/include/czmq_prelude.h:422

  --  Internet 6 socket address structure
   subtype in6addr_t is netinet_in_h.sockaddr_in6;  -- /homes/taft/_distrib/include/czmq_prelude.h:423

  -- Common structure to hold inaddr_t and in6addr_t with length
  --  IPv4 address
  --  IPv6 address
   --  skipped anonymous struct anon_121

   type anon_122 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_addr : aliased inaddr_t;  -- /homes/taft/_distrib/include/czmq_prelude.h:428
         when others =>
            uu_addr6 : aliased in6addr_t;  -- /homes/taft/_distrib/include/czmq_prelude.h:429
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type inaddr_storage_t is record
      uu_inaddr_u : aliased anon_122;  -- /homes/taft/_distrib/include/czmq_prelude.h:430
      inaddrlen : aliased int;  -- /homes/taft/_distrib/include/czmq_prelude.h:433
   end record
   with Convention => C_Pass_By_Copy;  -- /homes/taft/_distrib/include/czmq_prelude.h:434

  --- Inevitable macros -------------------------------------------------------
  --  randof(num) : Provide random number from 0..(num-1)
  --  ASSUMES that "num" itself is at most an int (bit size no more than float
  --  on the host platform), and non-negative; may be a "function()" token.
  --  For practical reasons, "num" should be under 50M or so.
  --  The math libraries on different platforms and capabilities in HW are a
  --  nightmare. Seems we have to drown the code in casts to have reasonable
  --  results... Also note that the 32-bit float has a hard time representing
  --  values close to UINT32_MAX that we had before, so now limit to UINT16_MAX.
  --  Platforms where RAND_MAX is comparable to even signed INT32_MAX were
  --  rigged with problems here: even if the code used double-precision, the
  --  corner-case factors (divident close to INT32_MAX and close to divisor)
  --  were too close to 1.0, so the final product was "num" rather than "num-1".
  --  Finally note that on some platforms RAND_MAX can be smallish, like 32767,
  --  so we should use it if small enough.
  --  Precision for our calculations impacts the MAX values we can use below
  --  Still, say UINT64_MAX is overkill. But smaller MAXes can yield better
  --  distribution of values with e.g. double.
  -- Implementations vary... Note that many will get __UTYPE_GNU nowadays.
  --  Limits below were experimented for 32-bit floats on x86 with test_randof
  --  Due to discrete rounding, greater values caused collisions with the
  --  fraction s_randof_factor() defined below returning 1.0.
  -- Assume that random() is at least 32-bit as it is on most platforms
  -- Supplement the limited spectrum of ZSYS_RANDOF_MAX by stacking more random()s
  -- Note this can still be too little for very large "num" > ZSYS_RANDOF_MAX
  -- but we'd need a real randof() function to handle stacking in that case.
  -- Fuzziness added below (division by slightly more than a whole number) solves
  -- this wonderfully even for "num" ranges twice as big as the ZSYS_RANDOF_MAX.
  -- That's it about the randof() macro definition...
  -- Windows MSVS doesn't have stdbool
  --- A number of POSIX and C99 keywords and data types -----------------------
  --  CZMQ uses uint for array indices; equivalent to unsigned int, but more
  --  convenient in code. We define it in czmq_prelude.h on systems that do
  --  not define it by default.
  --  MSVC does not support C99's va_copy so we use a regular assignment
  --  This fixes header-order dependence problem with some Linux versions
  --- Non-portable declaration specifiers -------------------------------------
  --  For thread-local storage
  --  Replacement for malloc() which asserts if we run out of heap, and
  --  which zeroes the allocated block.
   function safe_malloc
     (size : stddef_h.size_t;
      file : Interfaces.C.Strings.chars_ptr;
      line : unsigned) return System.Address  -- /homes/taft/_distrib/include/czmq_prelude.h:637
   with Import => True, 
        Convention => C, 
        External_Name => "safe_malloc";

  --     printf ("%s:%u %08d\n", file, line, (int) size);
  --  Define _ZMALLOC_DEBUG if you need to trace memory leaks using e.g. mtrace,
  --  otherwise all allocations will claim to come from czmq_prelude.h. For best
  --  results, compile all classes so you see dangling object allocations.
  --  _ZMALLOC_PEDANTIC does the same thing, but its intention is to propagate
  --  out of memory condition back up the call stack.
  --  GCC supports validating format strings for functions that act like printf
  --  Lets us write code that compiles both on Windows and normal platforms
   subtype SOCKET is int;  -- /homes/taft/_distrib/include/czmq_prelude.h:670

  --- Include non-portable header files based on platform.h -------------------
  --  This would normally come from net/if.h
  --  32 on Linux, 256 on Windows, pick largest to avoid overflows
  --  ZMQ compatibility macros
  --  Older libzmq APIs may be missing some aspects of libzmq v3.0
end czmq_prelude_h;
