pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package x86_64_linux_gnu_bits_sigset_h is

  -- __sig_atomic_t, __sigset_t, and related definitions.  Linux version.
  --   Copyright (C) 1991-2014 Free Software Foundation, Inc.
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

   subtype uu_sig_atomic_t is int;  -- /usr/include/x86_64-linux-gnu/bits/sigset.h:22

  -- A `sigset_t' has a bit for each signal.   
   --  skipped anonymous struct anon_14

   type uu_sigset_t_array1420 is array (0 .. 15) of aliased unsigned_long;
   type uu_sigset_t is record
      uu_val : aliased uu_sigset_t_array1420;  -- /usr/include/x86_64-linux-gnu/bits/sigset.h:29
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigset.h:30

  -- We only want to define these functions if <signal.h> was actually
  --   included; otherwise we were included just to define the types.  Since we
  --   are namespace-clean, it wouldn't hurt to define extra macros.  But
  --   trouble can be caused by functions being defined (e.g., any global
  --   register vars declared later will cause compilation errors).   

  -- Return a mask that includes the bit for SIG only.   
  -- Return the word index for SIG.   
  -- The POSIX does not specify for handling the whole signal set in one
  --   command.  This is often wanted and so we define three more functions
  --   here.   

  -- These functions needn't check for a bogus signal number -- error
  --   checking is done in the non __ versions.   

   --  skipped func __sigismember

   --  skipped func __sigaddset

   --  skipped func __sigdelset

end x86_64_linux_gnu_bits_sigset_h;
