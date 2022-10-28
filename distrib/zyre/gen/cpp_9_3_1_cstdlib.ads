pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with stdlib_h;

package cpp_9_3_1_cstdlib is

  -- -*- C++ -*- forwarding header.
  -- Copyright (C) 1997-2019 Free Software Foundation, Inc.
  -- This file is part of the GNU ISO C++ Library.  This library is free
  -- software; you can redistribute it and/or modify it under the
  -- terms of the GNU General Public License as published by the
  -- Free Software Foundation; either version 3, or (at your option)
  -- any later version.
  -- This library is distributed in the hope that it will be useful,
  -- but WITHOUT ANY WARRANTY; without even the implied warranty of
  -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  -- GNU General Public License for more details.
  -- Under Section 7 of GPL version 3, you are granted additional
  -- permissions described in the GCC Runtime Library Exception, version
  -- 3.1, as published by the Free Software Foundation.
  -- You should have received a copy of the GNU General Public License and
  -- a copy of the GCC Runtime Library Exception along with this program;
  -- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
  -- <http://www.gnu.org/licenses/>.
  --* @file include/cstdlib
  -- *  This is a Standard C++ Library file.  You should @c \#include this file
  -- *  in your programs, rather than any of the @a *.h implementation files.
  -- *
  -- *  This is the C++ version of the Standard C Library header @c stdlib.h,
  -- *  and its contents are (mostly) the same as that header, but are all
  -- *  contained in the namespace @c std (except for names which are defined
  -- *  as macros in C).
  --  

  -- ISO C++ 14882: 20.4.6  C library
  -- The C standard does not require a freestanding implementation to
  -- provide <stdlib.h>.  However, the C++ standard does still require
  -- <cstdlib> -- but only the functionality mentioned in
  -- [lib.support.start.term].
  -- namespace std
  -- Need to ensure this finds the C library's <stdlib.h> not a libstdc++
  -- wrapper that might already be installed later in the include search path.
  -- Get rid of those macros defined in <stdlib.h> in lieu of real functions.
   function div (uu_i : long; uu_j : long) return stdlib_h.ldiv_t  -- /nfs/nas/homes/taft/wave/x86_64-linux/gnat/install/include/c++/9.3.1/cstdlib:177
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZSt3divll";

  -- namespace
   function div (uu_n : Long_Long_Integer; uu_d : Long_Long_Integer) return stdlib_h.lldiv_t  -- /nfs/nas/homes/taft/wave/x86_64-linux/gnat/install/include/c++/9.3.1/cstdlib:213
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZN9__gnu_cxx3divExx";

  -- namespace __gnu_cxx
  -- namespace std
  -- extern "C++"
end cpp_9_3_1_cstdlib;
