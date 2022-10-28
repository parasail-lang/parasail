pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package x86_64_linux_gnu_sys_file_h is

   LOCK_SH : constant := 1;  --  /usr/include/x86_64-linux-gnu/sys/file.h:40
   LOCK_EX : constant := 2;  --  /usr/include/x86_64-linux-gnu/sys/file.h:41
   LOCK_UN : constant := 8;  --  /usr/include/x86_64-linux-gnu/sys/file.h:42

   LOCK_NB : constant := 4;  --  /usr/include/x86_64-linux-gnu/sys/file.h:46

  -- Copyright (C) 1991-2014 Free Software Foundation, Inc.
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

  -- Alternate names for values for the WHENCE argument to `lseek'.
  --   These are the same as SEEK_SET, SEEK_CUR, and SEEK_END, respectively.   

  -- Operations for the `flock' call.   
  -- Can be OR'd in to one of the above.   
  -- Apply or remove an advisory lock, according to OPERATION,
  --   on the file FD refers to.   

   function flock (uu_fd : int; uu_operation : int) return int  -- /usr/include/x86_64-linux-gnu/sys/file.h:51
   with Import => True, 
        Convention => C, 
        External_Name => "flock";

end x86_64_linux_gnu_sys_file_h;
