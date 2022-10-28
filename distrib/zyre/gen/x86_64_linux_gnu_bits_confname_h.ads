pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package x86_64_linux_gnu_bits_confname_h is

  -- `sysconf', `pathconf', and `confstr' NAME values.  Generic version.
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

  -- Values for the NAME argument to `pathconf' and `fpathconf'.   
  -- Values for the argument to `sysconf'.   
  -- Values for the argument to `sysconf'
  --       corresponding to _POSIX2_* symbols.   

  -- Values according to POSIX 1003.1c (POSIX threads).   
  -- Leave room here, maybe we need a few more cache levels some day.   
  -- Values for the NAME argument to `confstr'.   
  -- The default search path.   
end x86_64_linux_gnu_bits_confname_h;
