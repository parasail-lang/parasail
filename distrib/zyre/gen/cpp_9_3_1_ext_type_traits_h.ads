pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package cpp_9_3_1_ext_type_traits_h is

  -- -*- C++ -*-
  -- Copyright (C) 2005-2019 Free Software Foundation, Inc.
  -- This file is part of the GNU ISO C++ Library.  This library is free
  -- software; you can redistribute it and/or modify it under the terms
  -- of the GNU General Public License as published by the Free Software
  -- Foundation; either version 3, or (at your option) any later
  -- version.
  -- This library is distributed in the hope that it will be useful, but
  -- WITHOUT ANY WARRANTY; without even the implied warranty of
  -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  -- General Public License for more details.
  -- Under Section 7 of GPL version 3, you are granted additional
  -- permissions described in the GCC Runtime Library Exception, version
  -- 3.1, as published by the Free Software Foundation.
  -- You should have received a copy of the GNU General Public License and
  -- a copy of the GCC Runtime Library Exception along with this program;
  -- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
  -- <http://www.gnu.org/licenses/>.
  --* @file ext/type_traits.h
  -- *  This file is a GNU extension to the Standard C++ Library.
  --  

  -- Define a nested type if some predicate holds.
  -- Conditional expression for types. If true, first, if false, second.
  -- Given an integral builtin type, return the corresponding unsigned type.
   package uu_add_unsigned_wchar_t is
      type uu_add_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_add_unsigned_wchar_t;

   package uu_add_unsigned_bool is
      type uu_add_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_add_unsigned_bool;

   package uu_add_unsigned_Long_Long_Integer is
      type uu_add_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_add_unsigned_Long_Long_Integer;

   package uu_add_unsigned_long is
      type uu_add_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_add_unsigned_long;

   package uu_add_unsigned_int is
      type uu_add_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_add_unsigned_int;

   package uu_add_unsigned_short is
      type uu_add_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_add_unsigned_short;

   package uu_add_unsigned_signed_char is
      type uu_add_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_add_unsigned_signed_char;

   package uu_add_unsigned_char is
      type uu_add_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_add_unsigned_char;



  -- Declare but don't define.
  -- Given an integral builtin type, return the corresponding signed type.
   package uu_remove_unsigned_wchar_t is
      type uu_remove_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_remove_unsigned_wchar_t;

   package uu_remove_unsigned_bool is
      type uu_remove_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_remove_unsigned_bool;

   package uu_remove_unsigned_unsigned_long_long is
      type uu_remove_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_remove_unsigned_unsigned_long_long;

   package uu_remove_unsigned_unsigned_long is
      type uu_remove_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_remove_unsigned_unsigned_long;

   package uu_remove_unsigned_unsigned is
      type uu_remove_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_remove_unsigned_unsigned;

   package uu_remove_unsigned_unsigned_short is
      type uu_remove_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_remove_unsigned_unsigned_short;

   package uu_remove_unsigned_unsigned_char is
      type uu_remove_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_remove_unsigned_unsigned_char;

   package uu_remove_unsigned_char is
      type uu_remove_unsigned is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_remove_unsigned_char;



  -- Declare but don't define.
  -- For use in string and vstring.
   --  skipped func __is_null_pointer

  -- For complex and cmath
   package uu_promote_float_0 is
      type uu_promote is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_promote_float_0;

   package uu_promote_double_0 is
      type uu_promote is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_promote_double_0;

   package uu_promote_long_double_0 is
      type uu_promote is limited record
         null;
      end record
      with Convention => C_Pass_By_Copy

   end;
   use uu_promote_long_double_0;



  -- No nested __type member for non-integer non-floating point types,
  -- allows this type to be used for SFINAE to constrain overloads in
  -- <cmath> and <complex> to only the intended types.
  -- namespace
  -- extern "C++"
end cpp_9_3_1_ext_type_traits_h;
