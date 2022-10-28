pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package x86_64_linux_gnu_bits_waitflags_h is

   WNOHANG : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/waitflags.h:25
   WUNTRACED : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/waitflags.h:26

   WSTOPPED : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/waitflags.h:29
   WEXITED : constant := 4;  --  /usr/include/x86_64-linux-gnu/bits/waitflags.h:30
   WCONTINUED : constant := 8;  --  /usr/include/x86_64-linux-gnu/bits/waitflags.h:31
   WNOWAIT : constant := 16#01000000#;  --  /usr/include/x86_64-linux-gnu/bits/waitflags.h:32

  -- Definitions of flag bits for `waitpid' et al.
  --   Copyright (C) 1992-2014 Free Software Foundation, Inc.
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

  -- Bits in the third argument to `waitpid'.   
  -- Bits in the fourth argument to `waitid'.   
  -- The following values are used by the `waitid' function.   
  -- The Linux kernel defines these bare, rather than an enum,
  --   which causes a conflict if the include order is reversed.  

  -- Wait for any child.   
  -- Wait for specified process.   
  -- Wait for members of process group.   
   type idtype_t is 
     (P_ALL,
      P_PID,
      P_PGID)
   with Convention => C;  -- /usr/include/x86_64-linux-gnu/bits/waitflags.h:55

end x86_64_linux_gnu_bits_waitflags_h;
