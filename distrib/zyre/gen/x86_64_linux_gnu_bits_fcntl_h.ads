pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;

package x86_64_linux_gnu_bits_fcntl_h is

   F_GETLK64 : constant := 5;  --  /usr/include/x86_64-linux-gnu/bits/fcntl.h:29
   F_SETLK64 : constant := 6;  --  /usr/include/x86_64-linux-gnu/bits/fcntl.h:30
   F_SETLKW64 : constant := 7;  --  /usr/include/x86_64-linux-gnu/bits/fcntl.h:31

  -- O_*, F_*, FD_* bit values for Linux/x86.
  --   Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

  -- Not necessary, we always have 64-bit offsets.   
  -- Type of lock: F_RDLCK, F_WRLCK, or F_UNLCK.	 
   type flock is record
      l_type : aliased short;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:37
      l_whence : aliased short;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:38
      l_start : aliased x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:40
      l_len : aliased x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:41
      l_pid : aliased x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:46
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:35

  -- Where `l_start' is relative to (like `lseek').   
  -- Offset where the lock begins.   
  -- Size of the locked area; zero means until EOF.   
  -- Offset where the lock begins.   
  -- Size of the locked area; zero means until EOF.   
  -- Process holding the lock.   
  -- Type of lock: F_RDLCK, F_WRLCK, or F_UNLCK.	 
   type flock64 is record
      l_type : aliased short;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:52
      l_whence : aliased short;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:53
      l_start : aliased x86_64_linux_gnu_bits_types_h.uu_off64_t;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:54
      l_len : aliased x86_64_linux_gnu_bits_types_h.uu_off64_t;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:55
      l_pid : aliased x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:56
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/fcntl.h:50

  -- Where `l_start' is relative to (like `lseek').   
  -- Offset where the lock begins.   
  -- Size of the locked area; zero means until EOF.   
  -- Process holding the lock.   
  -- Include generic Linux declarations.   
end x86_64_linux_gnu_bits_fcntl_h;
