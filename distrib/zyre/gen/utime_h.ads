pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
with Interfaces.C.Strings;

package utime_h is

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

  -- *	POSIX Standard: 5.6.6 Set File Access and Modification Times  <utime.h>
  --  

  -- Structure describing file times.   
  -- Access time.   
   type utimbuf is record
      actime : aliased x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/utime.h:39
      modtime : aliased x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/utime.h:40
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/utime.h:37

  -- Modification time.   
  -- Set the access and modification times of FILE to those given in
  --   *FILE_TIMES.  If FILE_TIMES is NULL, set them to the current time.   

   function utime (uu_file : Interfaces.C.Strings.chars_ptr; uu_file_times : access constant utimbuf) return int  -- /usr/include/utime.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "utime";

end utime_h;
