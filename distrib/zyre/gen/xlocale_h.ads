pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package xlocale_h is

  -- Definition of locale datatype.
  --   Copyright (C) 1997-2014 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997.
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

  -- Structure for reentrant locale using functions.  This is an
  --   (almost) opaque type for the user level programs.  The file and
  --   this data structure is not standardized.  Don't rely on it.  It can
  --   go away without warning.   

  -- Note: LC_ALL is not a valid index into this array.   
  -- 13 = __LC_LAST.  
   type uu_locale_data;
   type uu_locale_struct_array1271 is array (0 .. 12) of access uu_locale_data;
   type uu_locale_struct_array1272 is array (0 .. 12) of Interfaces.C.Strings.chars_ptr;
   type uu_locale_struct is record
      uu_locales : uu_locale_struct_array1271;  -- /usr/include/xlocale.h:30
      uu_ctype_b : access unsigned_short;  -- /usr/include/xlocale.h:33
      uu_ctype_tolower : access int;  -- /usr/include/xlocale.h:34
      uu_ctype_toupper : access int;  -- /usr/include/xlocale.h:35
      uu_names : uu_locale_struct_array1272;  -- /usr/include/xlocale.h:38
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/xlocale.h:27

   type uu_locale_data is null record;   -- incomplete struct

  -- To increase the speed of this solution we add some special members.   
  -- Note: LC_ALL is not a valid index into this array.   
   type uu_locale_t is access all uu_locale_struct;  -- /usr/include/xlocale.h:39

  -- POSIX 2008 makes locale_t official.   
   subtype locale_t is uu_locale_t;  -- /usr/include/xlocale.h:42

end xlocale_h;
