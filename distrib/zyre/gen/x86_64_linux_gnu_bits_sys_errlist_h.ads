pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package x86_64_linux_gnu_bits_sys_errlist_h is

  -- Declare sys_errlist and sys_nerr, or don't.  Compatibility (do) version.
  --   Copyright (C) 2002-2014 Free Software Foundation, Inc.
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

  -- sys_errlist and sys_nerr are deprecated.  Use strerror instead.   
   sys_nerr : aliased int  -- /usr/include/x86_64-linux-gnu/bits/sys_errlist.h:26
   with Import => True, 
        Convention => C, 
        External_Name => "sys_nerr";

   sys_errlist : array (size_t) of Interfaces.C.Strings.chars_ptr  -- /usr/include/x86_64-linux-gnu/bits/sys_errlist.h:27
   with Import => True, 
        Convention => C, 
        External_Name => "sys_errlist";

end x86_64_linux_gnu_bits_sys_errlist_h;
