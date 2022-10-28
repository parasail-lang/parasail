pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;
with stddef_h;

package x86_64_linux_gnu_bits_sigstack_h is

   --  unsupported macro: SS_ONSTACK SS_ONSTACK
   --  unsupported macro: SS_DISABLE SS_DISABLE
   MINSIGSTKSZ : constant := 2048;  --  /usr/include/x86_64-linux-gnu/bits/sigstack.h:42

   SIGSTKSZ : constant := 8192;  --  /usr/include/x86_64-linux-gnu/bits/sigstack.h:45

  -- sigstack, sigaltstack definitions.
  --   Copyright (C) 1998-2014 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public
  --   License as published by the Free Software Foundation; either
  --   version 2.1 of the License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; if not, see
  --   <http://www.gnu.org/licenses/>.   

  -- Structure describing a signal stack (obsolete).   
  -- Signal stack pointer.   
   type sigstack is record
      ss_sp : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:27
      ss_onstack : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:28
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:25

  -- Nonzero if executing on this stack.   
  -- Possible values for `ss_flags.'.   
  -- Minimum stack size for a signal handler.   
  -- System default stack size.   
  -- Alternate, preferred interface.   
   type sigaltstack is record
      ss_sp : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:51
      ss_flags : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:52
      ss_size : aliased stddef_h.size_t;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:53
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:49

   subtype stack_t is sigaltstack;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:54

end x86_64_linux_gnu_bits_sigstack_h;
