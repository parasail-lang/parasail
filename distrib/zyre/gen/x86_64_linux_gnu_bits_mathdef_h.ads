pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package x86_64_linux_gnu_bits_mathdef_h is

   FP_ILOGB0 : constant := (-2147483647 - 1);  --  /usr/include/x86_64-linux-gnu/bits/mathdef.h:42
   FP_ILOGBNAN : constant := (-2147483647 - 1);  --  /usr/include/x86_64-linux-gnu/bits/mathdef.h:43

  -- Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

  -- The x86-64 architecture computes values with the precission of the
  --   used type.  Similarly for -m32 -mfpmath=sse.   

  -- `float' expressions are evaluated as `float'.   
   subtype float_t is float;  -- /usr/include/x86_64-linux-gnu/bits/mathdef.h:28

  -- `double' expressions are evaluated
  --				   as `double'.   

   subtype double_t is double;  -- /usr/include/x86_64-linux-gnu/bits/mathdef.h:29

  -- The ix87 FPUs evaluate all values in the 80 bit floating-point format
  --   which is also available for the user as `long double'.  Therefore we
  --   define:   

  -- `float' expressions are evaluated as
  --				   `long double'.   

  -- `double' expressions are evaluated as
  --				   `long double'.   

  -- The values returned by `ilogb' for 0 and NaN respectively.   
  -- The GCC 4.6 compiler will define __FP_FAST_FMA{,F,L} if the fma{,f,l}
  --   builtins are supported.   

end x86_64_linux_gnu_bits_mathdef_h;
