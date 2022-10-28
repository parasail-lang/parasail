pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;

package x86_64_linux_gnu_bits_dirent_h is

   --  unsupported macro: d_fileno d_ino
  -- Copyright (C) 1996-2014 Free Software Foundation, Inc.
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

   subtype dirent_array4018 is Interfaces.C.char_array (0 .. 255);
   type dirent is record
      d_ino : aliased x86_64_linux_gnu_bits_types_h.uu_ino_t;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:25
      d_off : aliased x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:26
      d_reclen : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:31
      d_type : aliased unsigned_char;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:32
      d_name : aliased dirent_array4018;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:33
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:22

  -- We must not include limits.h!  
   subtype dirent64_array4018 is Interfaces.C.char_array (0 .. 255);
   type dirent64 is record
      d_ino : aliased x86_64_linux_gnu_bits_types_h.uu_ino64_t;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:39
      d_off : aliased x86_64_linux_gnu_bits_types_h.uu_off64_t;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:40
      d_reclen : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:41
      d_type : aliased unsigned_char;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:42
      d_name : aliased dirent64_array4018;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:43
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/dirent.h:37

  -- We must not include limits.h!  
  -- Inform libc code that these two types are effectively identical.   
end x86_64_linux_gnu_bits_dirent_h;
