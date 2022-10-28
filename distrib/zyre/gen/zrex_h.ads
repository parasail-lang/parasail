pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
limited with czmq_library_h;
with System;
with Interfaces.C.Extensions;
with x86_64_linux_gnu_sys_types_h;

package zrex_h is

  --  =========================================================================
  --    zrex - work with regular expressions
  --    Copyright (c) the Contributors as noted in the AUTHORS file.
  --    This file is part of CZMQ, the high-level C binding for 0MQ:
  --    http://czmq.zeromq.org.
  --    This Source Code Form is subject to the terms of the Mozilla Public
  --    License, v. 2.0. If a copy of the MPL was not distributed with this
  --    file, You can obtain one at http://mozilla.org/MPL/2.0/.
  --    =========================================================================
  -- 

  --  @interface
  --  Constructor. Optionally, sets an expression against which we can match
  --  text and capture hits. If there is an error in the expression, reports
  --  zrex_valid() as false and provides the error in zrex_strerror(). If you
  --  set a pattern, you can call zrex_matches() to test it against text.
   function zrex_new (expression : Interfaces.C.Strings.chars_ptr) return access czmq_library_h.zrex_t  -- /homes/taft/_distrib/include/zrex.h:27
   with Import => True, 
        Convention => C, 
        External_Name => "zrex_new";

  --  Destructor
   procedure zrex_destroy (self_p : System.Address)  -- /homes/taft/_distrib/include/zrex.h:31
   with Import => True, 
        Convention => C, 
        External_Name => "zrex_destroy";

  --  Return true if the expression was valid and compiled without errors.
   function zrex_valid (self : access czmq_library_h.zrex_t) return Extensions.bool  -- /homes/taft/_distrib/include/zrex.h:35
   with Import => True, 
        Convention => C, 
        External_Name => "zrex_valid";

  --  Return the error message generated during compilation of the expression.
   function zrex_strerror (self : access czmq_library_h.zrex_t) return Interfaces.C.Strings.chars_ptr  -- /homes/taft/_distrib/include/zrex.h:39
   with Import => True, 
        Convention => C, 
        External_Name => "zrex_strerror";

  --  Returns true if the text matches the previously compiled expression.
  --  Use this method to compare one expression against many strings.
   function zrex_matches (self : access czmq_library_h.zrex_t; text : Interfaces.C.Strings.chars_ptr) return Extensions.bool  -- /homes/taft/_distrib/include/zrex.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "zrex_matches";

  --  Returns true if the text matches the supplied expression. Use this
  --  method to compare one string against several expressions.
   function zrex_eq
     (self : access czmq_library_h.zrex_t;
      text : Interfaces.C.Strings.chars_ptr;
      expression : Interfaces.C.Strings.chars_ptr) return Extensions.bool  -- /homes/taft/_distrib/include/zrex.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "zrex_eq";

  --  Returns number of hits from last zrex_matches or zrex_eq. If the text
  --  matched, returns 1 plus the number of capture groups. If the text did
  --  not match, returns zero. To retrieve individual capture groups, call
  --  zrex_hit ().
   function zrex_hits (self : access czmq_library_h.zrex_t) return int  -- /homes/taft/_distrib/include/zrex.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "zrex_hits";

  --  Returns the Nth capture group from the last expression match, where
  --  N is 0 to the value returned by zrex_hits(). Capture group 0 is the
  --  whole matching string. Sequence 1 is the first capture group, if any,
  --  and so on.
   function zrex_hit (self : access czmq_library_h.zrex_t; index : x86_64_linux_gnu_sys_types_h.uint) return Interfaces.C.Strings.chars_ptr  -- /homes/taft/_distrib/include/zrex.h:63
   with Import => True, 
        Convention => C, 
        External_Name => "zrex_hit";

  --  Fetches hits into string variables provided by caller; this makes for
  --  nicer code than accessing hits by index. Caller should not modify nor
  --  free the returned values. Returns number of strings returned. This
  --  method starts at hit 1, i.e. first capture group, as hit 0 is always
  --  the original matched string.
   function zrex_fetch (self : access czmq_library_h.zrex_t; string_p : System.Address  -- , ...
      ) return int  -- /homes/taft/_distrib/include/zrex.h:71
   with Import => True, 
        Convention => C, 
        External_Name => "zrex_fetch";

  --  Self test of this class
   procedure zrex_test (verbose : Extensions.bool)  -- /homes/taft/_distrib/include/zrex.h:75
   with Import => True, 
        Convention => C, 
        External_Name => "zrex_test";

  --  @end
end zrex_h;
