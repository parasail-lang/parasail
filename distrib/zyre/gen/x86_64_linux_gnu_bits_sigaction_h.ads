pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with signal_h;
limited with x86_64_linux_gnu_bits_siginfo_h;
with System;
with x86_64_linux_gnu_bits_sigset_h;

package x86_64_linux_gnu_bits_sigaction_h is

   --  unsupported macro: sa_handler __sigaction_handler.sa_handler
   --  unsupported macro: sa_sigaction __sigaction_handler.sa_sigaction
   SA_NOCLDSTOP : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/sigaction.h:53
   SA_NOCLDWAIT : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/sigaction.h:54
   SA_SIGINFO : constant := 4;  --  /usr/include/x86_64-linux-gnu/bits/sigaction.h:55

   SA_ONSTACK : constant := 16#08000000#;  --  /usr/include/x86_64-linux-gnu/bits/sigaction.h:58

   SA_RESTART : constant := 16#10000000#;  --  /usr/include/x86_64-linux-gnu/bits/sigaction.h:61
   SA_NODEFER : constant := 16#40000000#;  --  /usr/include/x86_64-linux-gnu/bits/sigaction.h:62

   SA_RESETHAND : constant := 16#80000000#;  --  /usr/include/x86_64-linux-gnu/bits/sigaction.h:64

   SA_INTERRUPT : constant := 16#20000000#;  --  /usr/include/x86_64-linux-gnu/bits/sigaction.h:67
   --  unsupported macro: SA_NOMASK SA_NODEFER
   --  unsupported macro: SA_ONESHOT SA_RESETHAND
   --  unsupported macro: SA_STACK SA_ONSTACK

   SIG_BLOCK : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/sigaction.h:76
   SIG_UNBLOCK : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/sigaction.h:77
   SIG_SETMASK : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/sigaction.h:78

  -- The proper definitions for Linux's sigaction.
  --   Copyright (C) 1993-2014 Free Software Foundation, Inc.
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

  -- Structure describing the action to be taken when a signal arrives.   
  -- Signal handler.   
  -- Used if SA_SIGINFO is not set.   
  -- Used if SA_SIGINFO is set.   
   type anon_85 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            sa_handler : signal_h.uu_sighandler_t;  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:31
         when others =>
            sa_sigaction : access procedure
                 (arg1 : int;
                  arg2 : access x86_64_linux_gnu_bits_siginfo_h.siginfo_t;
                  arg3 : System.Address);  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:33
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type sigaction is record
      uu_sigaction_handler : aliased anon_85;  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:35
      sa_mask : aliased x86_64_linux_gnu_bits_sigset_h.uu_sigset_t;  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:43
      sa_flags : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:46
      sa_restorer : access procedure;  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:49
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:24

  -- Additional set of signals to be blocked.   
  -- Special flags.   
  -- Restore handler.   
  -- Bits in `sa_flags'.   
  -- Some aliases for the SA_ constants.   
  -- Values for the HOW argument to `sigprocmask'.   
end x86_64_linux_gnu_bits_sigaction_h;
