pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package x86_64_linux_gnu_bits_errno_h is

   --  unsupported macro: ENOTSUP EOPNOTSUPP
   --  unsupported macro: errno (*__errno_location ())
  -- Error constants.  Linux specific version.
  --   Copyright (C) 1996-2014 Free Software Foundation, Inc.
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

  -- Linux has no ENOTSUP error code.   
  -- Older Linux versions also had no ECANCELED error code.   
  -- Support for error codes to support robust mutexes was added later, too.   
  -- Function to get address of global `errno' variable.   
   --  skipped func __errno_location

  -- When using threads, errno is a per-thread value.   
  -- This is ugly but the kernel header is not clean enough.  We must
  --   define only the values EDOM, EILSEQ and ERANGE in case __need_Emath is
  --   defined.   

end x86_64_linux_gnu_bits_errno_h;
