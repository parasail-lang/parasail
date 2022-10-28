pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with stdint_h;
with Interfaces.C.Strings;
with System;

package inttypes_h is

   PRId8 : aliased constant String := "d" & ASCII.NUL;  --  /usr/include/inttypes.h:54
   PRId16 : aliased constant String := "d" & ASCII.NUL;  --  /usr/include/inttypes.h:55
   PRId32 : aliased constant String := "d" & ASCII.NUL;  --  /usr/include/inttypes.h:56
   --  unsupported macro: PRId64 __PRI64_PREFIX "d"

   PRIdLEAST8 : aliased constant String := "d" & ASCII.NUL;  --  /usr/include/inttypes.h:59
   PRIdLEAST16 : aliased constant String := "d" & ASCII.NUL;  --  /usr/include/inttypes.h:60
   PRIdLEAST32 : aliased constant String := "d" & ASCII.NUL;  --  /usr/include/inttypes.h:61
   --  unsupported macro: PRIdLEAST64 __PRI64_PREFIX "d"

   PRIdFAST8 : aliased constant String := "d" & ASCII.NUL;  --  /usr/include/inttypes.h:64
   --  unsupported macro: PRIdFAST16 __PRIPTR_PREFIX "d"
   --  unsupported macro: PRIdFAST32 __PRIPTR_PREFIX "d"
   --  unsupported macro: PRIdFAST64 __PRI64_PREFIX "d"

   PRIi8 : aliased constant String := "i" & ASCII.NUL;  --  /usr/include/inttypes.h:70
   PRIi16 : aliased constant String := "i" & ASCII.NUL;  --  /usr/include/inttypes.h:71
   PRIi32 : aliased constant String := "i" & ASCII.NUL;  --  /usr/include/inttypes.h:72
   --  unsupported macro: PRIi64 __PRI64_PREFIX "i"

   PRIiLEAST8 : aliased constant String := "i" & ASCII.NUL;  --  /usr/include/inttypes.h:75
   PRIiLEAST16 : aliased constant String := "i" & ASCII.NUL;  --  /usr/include/inttypes.h:76
   PRIiLEAST32 : aliased constant String := "i" & ASCII.NUL;  --  /usr/include/inttypes.h:77
   --  unsupported macro: PRIiLEAST64 __PRI64_PREFIX "i"

   PRIiFAST8 : aliased constant String := "i" & ASCII.NUL;  --  /usr/include/inttypes.h:80
   --  unsupported macro: PRIiFAST16 __PRIPTR_PREFIX "i"
   --  unsupported macro: PRIiFAST32 __PRIPTR_PREFIX "i"
   --  unsupported macro: PRIiFAST64 __PRI64_PREFIX "i"

   PRIo8 : aliased constant String := "o" & ASCII.NUL;  --  /usr/include/inttypes.h:86
   PRIo16 : aliased constant String := "o" & ASCII.NUL;  --  /usr/include/inttypes.h:87
   PRIo32 : aliased constant String := "o" & ASCII.NUL;  --  /usr/include/inttypes.h:88
   --  unsupported macro: PRIo64 __PRI64_PREFIX "o"

   PRIoLEAST8 : aliased constant String := "o" & ASCII.NUL;  --  /usr/include/inttypes.h:91
   PRIoLEAST16 : aliased constant String := "o" & ASCII.NUL;  --  /usr/include/inttypes.h:92
   PRIoLEAST32 : aliased constant String := "o" & ASCII.NUL;  --  /usr/include/inttypes.h:93
   --  unsupported macro: PRIoLEAST64 __PRI64_PREFIX "o"

   PRIoFAST8 : aliased constant String := "o" & ASCII.NUL;  --  /usr/include/inttypes.h:96
   --  unsupported macro: PRIoFAST16 __PRIPTR_PREFIX "o"
   --  unsupported macro: PRIoFAST32 __PRIPTR_PREFIX "o"
   --  unsupported macro: PRIoFAST64 __PRI64_PREFIX "o"

   PRIu8 : aliased constant String := "u" & ASCII.NUL;  --  /usr/include/inttypes.h:102
   PRIu16 : aliased constant String := "u" & ASCII.NUL;  --  /usr/include/inttypes.h:103
   PRIu32 : aliased constant String := "u" & ASCII.NUL;  --  /usr/include/inttypes.h:104
   --  unsupported macro: PRIu64 __PRI64_PREFIX "u"

   PRIuLEAST8 : aliased constant String := "u" & ASCII.NUL;  --  /usr/include/inttypes.h:107
   PRIuLEAST16 : aliased constant String := "u" & ASCII.NUL;  --  /usr/include/inttypes.h:108
   PRIuLEAST32 : aliased constant String := "u" & ASCII.NUL;  --  /usr/include/inttypes.h:109
   --  unsupported macro: PRIuLEAST64 __PRI64_PREFIX "u"

   PRIuFAST8 : aliased constant String := "u" & ASCII.NUL;  --  /usr/include/inttypes.h:112
   --  unsupported macro: PRIuFAST16 __PRIPTR_PREFIX "u"
   --  unsupported macro: PRIuFAST32 __PRIPTR_PREFIX "u"
   --  unsupported macro: PRIuFAST64 __PRI64_PREFIX "u"

   PRIx8 : aliased constant String := "x" & ASCII.NUL;  --  /usr/include/inttypes.h:118
   PRIx16 : aliased constant String := "x" & ASCII.NUL;  --  /usr/include/inttypes.h:119
   PRIx32 : aliased constant String := "x" & ASCII.NUL;  --  /usr/include/inttypes.h:120
   --  unsupported macro: PRIx64 __PRI64_PREFIX "x"

   PRIxLEAST8 : aliased constant String := "x" & ASCII.NUL;  --  /usr/include/inttypes.h:123
   PRIxLEAST16 : aliased constant String := "x" & ASCII.NUL;  --  /usr/include/inttypes.h:124
   PRIxLEAST32 : aliased constant String := "x" & ASCII.NUL;  --  /usr/include/inttypes.h:125
   --  unsupported macro: PRIxLEAST64 __PRI64_PREFIX "x"

   PRIxFAST8 : aliased constant String := "x" & ASCII.NUL;  --  /usr/include/inttypes.h:128
   --  unsupported macro: PRIxFAST16 __PRIPTR_PREFIX "x"
   --  unsupported macro: PRIxFAST32 __PRIPTR_PREFIX "x"
   --  unsupported macro: PRIxFAST64 __PRI64_PREFIX "x"

   PRIX8 : aliased constant String := "X" & ASCII.NUL;  --  /usr/include/inttypes.h:134
   PRIX16 : aliased constant String := "X" & ASCII.NUL;  --  /usr/include/inttypes.h:135
   PRIX32 : aliased constant String := "X" & ASCII.NUL;  --  /usr/include/inttypes.h:136
   --  unsupported macro: PRIX64 __PRI64_PREFIX "X"

   PRIXLEAST8 : aliased constant String := "X" & ASCII.NUL;  --  /usr/include/inttypes.h:139
   PRIXLEAST16 : aliased constant String := "X" & ASCII.NUL;  --  /usr/include/inttypes.h:140
   PRIXLEAST32 : aliased constant String := "X" & ASCII.NUL;  --  /usr/include/inttypes.h:141
   --  unsupported macro: PRIXLEAST64 __PRI64_PREFIX "X"

   PRIXFAST8 : aliased constant String := "X" & ASCII.NUL;  --  /usr/include/inttypes.h:144
   --  unsupported macro: PRIXFAST16 __PRIPTR_PREFIX "X"
   --  unsupported macro: PRIXFAST32 __PRIPTR_PREFIX "X"
   --  unsupported macro: PRIXFAST64 __PRI64_PREFIX "X"
   --  unsupported macro: PRIdMAX __PRI64_PREFIX "d"
   --  unsupported macro: PRIiMAX __PRI64_PREFIX "i"
   --  unsupported macro: PRIoMAX __PRI64_PREFIX "o"
   --  unsupported macro: PRIuMAX __PRI64_PREFIX "u"
   --  unsupported macro: PRIxMAX __PRI64_PREFIX "x"
   --  unsupported macro: PRIXMAX __PRI64_PREFIX "X"
   --  unsupported macro: PRIdPTR __PRIPTR_PREFIX "d"
   --  unsupported macro: PRIiPTR __PRIPTR_PREFIX "i"
   --  unsupported macro: PRIoPTR __PRIPTR_PREFIX "o"
   --  unsupported macro: PRIuPTR __PRIPTR_PREFIX "u"
   --  unsupported macro: PRIxPTR __PRIPTR_PREFIX "x"
   --  unsupported macro: PRIXPTR __PRIPTR_PREFIX "X"

   SCNd8 : aliased constant String := "hhd" & ASCII.NUL;  --  /usr/include/inttypes.h:171
   SCNd16 : aliased constant String := "hd" & ASCII.NUL;  --  /usr/include/inttypes.h:172
   SCNd32 : aliased constant String := "d" & ASCII.NUL;  --  /usr/include/inttypes.h:173
   --  unsupported macro: SCNd64 __PRI64_PREFIX "d"

   SCNdLEAST8 : aliased constant String := "hhd" & ASCII.NUL;  --  /usr/include/inttypes.h:176
   SCNdLEAST16 : aliased constant String := "hd" & ASCII.NUL;  --  /usr/include/inttypes.h:177
   SCNdLEAST32 : aliased constant String := "d" & ASCII.NUL;  --  /usr/include/inttypes.h:178
   --  unsupported macro: SCNdLEAST64 __PRI64_PREFIX "d"

   SCNdFAST8 : aliased constant String := "hhd" & ASCII.NUL;  --  /usr/include/inttypes.h:181
   --  unsupported macro: SCNdFAST16 __PRIPTR_PREFIX "d"
   --  unsupported macro: SCNdFAST32 __PRIPTR_PREFIX "d"
   --  unsupported macro: SCNdFAST64 __PRI64_PREFIX "d"

   SCNi8 : aliased constant String := "hhi" & ASCII.NUL;  --  /usr/include/inttypes.h:187
   SCNi16 : aliased constant String := "hi" & ASCII.NUL;  --  /usr/include/inttypes.h:188
   SCNi32 : aliased constant String := "i" & ASCII.NUL;  --  /usr/include/inttypes.h:189
   --  unsupported macro: SCNi64 __PRI64_PREFIX "i"

   SCNiLEAST8 : aliased constant String := "hhi" & ASCII.NUL;  --  /usr/include/inttypes.h:192
   SCNiLEAST16 : aliased constant String := "hi" & ASCII.NUL;  --  /usr/include/inttypes.h:193
   SCNiLEAST32 : aliased constant String := "i" & ASCII.NUL;  --  /usr/include/inttypes.h:194
   --  unsupported macro: SCNiLEAST64 __PRI64_PREFIX "i"

   SCNiFAST8 : aliased constant String := "hhi" & ASCII.NUL;  --  /usr/include/inttypes.h:197
   --  unsupported macro: SCNiFAST16 __PRIPTR_PREFIX "i"
   --  unsupported macro: SCNiFAST32 __PRIPTR_PREFIX "i"
   --  unsupported macro: SCNiFAST64 __PRI64_PREFIX "i"

   SCNu8 : aliased constant String := "hhu" & ASCII.NUL;  --  /usr/include/inttypes.h:203
   SCNu16 : aliased constant String := "hu" & ASCII.NUL;  --  /usr/include/inttypes.h:204
   SCNu32 : aliased constant String := "u" & ASCII.NUL;  --  /usr/include/inttypes.h:205
   --  unsupported macro: SCNu64 __PRI64_PREFIX "u"

   SCNuLEAST8 : aliased constant String := "hhu" & ASCII.NUL;  --  /usr/include/inttypes.h:208
   SCNuLEAST16 : aliased constant String := "hu" & ASCII.NUL;  --  /usr/include/inttypes.h:209
   SCNuLEAST32 : aliased constant String := "u" & ASCII.NUL;  --  /usr/include/inttypes.h:210
   --  unsupported macro: SCNuLEAST64 __PRI64_PREFIX "u"

   SCNuFAST8 : aliased constant String := "hhu" & ASCII.NUL;  --  /usr/include/inttypes.h:213
   --  unsupported macro: SCNuFAST16 __PRIPTR_PREFIX "u"
   --  unsupported macro: SCNuFAST32 __PRIPTR_PREFIX "u"
   --  unsupported macro: SCNuFAST64 __PRI64_PREFIX "u"

   SCNo8 : aliased constant String := "hho" & ASCII.NUL;  --  /usr/include/inttypes.h:219
   SCNo16 : aliased constant String := "ho" & ASCII.NUL;  --  /usr/include/inttypes.h:220
   SCNo32 : aliased constant String := "o" & ASCII.NUL;  --  /usr/include/inttypes.h:221
   --  unsupported macro: SCNo64 __PRI64_PREFIX "o"

   SCNoLEAST8 : aliased constant String := "hho" & ASCII.NUL;  --  /usr/include/inttypes.h:224
   SCNoLEAST16 : aliased constant String := "ho" & ASCII.NUL;  --  /usr/include/inttypes.h:225
   SCNoLEAST32 : aliased constant String := "o" & ASCII.NUL;  --  /usr/include/inttypes.h:226
   --  unsupported macro: SCNoLEAST64 __PRI64_PREFIX "o"

   SCNoFAST8 : aliased constant String := "hho" & ASCII.NUL;  --  /usr/include/inttypes.h:229
   --  unsupported macro: SCNoFAST16 __PRIPTR_PREFIX "o"
   --  unsupported macro: SCNoFAST32 __PRIPTR_PREFIX "o"
   --  unsupported macro: SCNoFAST64 __PRI64_PREFIX "o"

   SCNx8 : aliased constant String := "hhx" & ASCII.NUL;  --  /usr/include/inttypes.h:235
   SCNx16 : aliased constant String := "hx" & ASCII.NUL;  --  /usr/include/inttypes.h:236
   SCNx32 : aliased constant String := "x" & ASCII.NUL;  --  /usr/include/inttypes.h:237
   --  unsupported macro: SCNx64 __PRI64_PREFIX "x"

   SCNxLEAST8 : aliased constant String := "hhx" & ASCII.NUL;  --  /usr/include/inttypes.h:240
   SCNxLEAST16 : aliased constant String := "hx" & ASCII.NUL;  --  /usr/include/inttypes.h:241
   SCNxLEAST32 : aliased constant String := "x" & ASCII.NUL;  --  /usr/include/inttypes.h:242
   --  unsupported macro: SCNxLEAST64 __PRI64_PREFIX "x"

   SCNxFAST8 : aliased constant String := "hhx" & ASCII.NUL;  --  /usr/include/inttypes.h:245
   --  unsupported macro: SCNxFAST16 __PRIPTR_PREFIX "x"
   --  unsupported macro: SCNxFAST32 __PRIPTR_PREFIX "x"
   --  unsupported macro: SCNxFAST64 __PRI64_PREFIX "x"
   --  unsupported macro: SCNdMAX __PRI64_PREFIX "d"
   --  unsupported macro: SCNiMAX __PRI64_PREFIX "i"
   --  unsupported macro: SCNoMAX __PRI64_PREFIX "o"
   --  unsupported macro: SCNuMAX __PRI64_PREFIX "u"
   --  unsupported macro: SCNxMAX __PRI64_PREFIX "x"
   --  unsupported macro: SCNdPTR __PRIPTR_PREFIX "d"
   --  unsupported macro: SCNiPTR __PRIPTR_PREFIX "i"
   --  unsupported macro: SCNoPTR __PRIPTR_PREFIX "o"
   --  unsupported macro: SCNuPTR __PRIPTR_PREFIX "u"
   --  unsupported macro: SCNxPTR __PRIPTR_PREFIX "x"

  -- Copyright (C) 1997-2014 Free Software Foundation, Inc.
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

  -- *	ISO C99: 7.8 Format conversion of integer types	<inttypes.h>
  --  

  -- Get the type definitions.   
  -- Get a definition for wchar_t.  But we must not define wchar_t itself.   
  -- Macros for printing format specifiers.   
  -- Decimal notation.   
  -- Octal notation.   
  -- Unsigned integers.   
  -- lowercase hexadecimal notation.   
  -- UPPERCASE hexadecimal notation.   
  -- Macros for printing `intmax_t' and `uintmax_t'.   
  -- Macros for printing `intptr_t' and `uintptr_t'.   
  -- Macros for scanning format specifiers.   
  -- Signed decimal notation.   
  -- Signed decimal notation.   
  -- Unsigned decimal notation.   
  -- Octal notation.   
  -- Hexadecimal notation.   
  -- Macros for scanning `intmax_t' and `uintmax_t'.   
  -- Macros for scaning `intptr_t' and `uintptr_t'.   
  -- We have to define the `uintmax_t' type using `ldiv_t'.   
  -- Quotient.   
   --  skipped anonymous struct anon_114

   type imaxdiv_t is record
      quot : aliased long;  -- /usr/include/inttypes.h:273
      c_rem : aliased long;  -- /usr/include/inttypes.h:274
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/inttypes.h:275

  -- Remainder.   
  -- We have to define the `uintmax_t' type using `lldiv_t'.   
  -- Quotient.   
  -- Remainder.   
  -- Compute absolute value of N.   
   function imaxabs (uu_n : stdint_h.intmax_t) return stdint_h.intmax_t  -- /usr/include/inttypes.h:290
   with Import => True, 
        Convention => C, 
        External_Name => "imaxabs";

  -- Return the `imaxdiv_t' representation of the value of NUMER over DENOM.  
   function imaxdiv (uu_numer : stdint_h.intmax_t; uu_denom : stdint_h.intmax_t) return imaxdiv_t  -- /usr/include/inttypes.h:293
   with Import => True, 
        Convention => C, 
        External_Name => "imaxdiv";

  -- Like `strtol' but convert to `intmax_t'.   
   function strtoimax
     (uu_nptr : Interfaces.C.Strings.chars_ptr;
      uu_endptr : System.Address;
      uu_base : int) return stdint_h.intmax_t  -- /usr/include/inttypes.h:297
   with Import => True, 
        Convention => C, 
        External_Name => "strtoimax";

  -- Like `strtoul' but convert to `uintmax_t'.   
   function strtoumax
     (uu_nptr : Interfaces.C.Strings.chars_ptr;
      uu_endptr : System.Address;
      uu_base : int) return stdint_h.uintmax_t  -- /usr/include/inttypes.h:301
   with Import => True, 
        Convention => C, 
        External_Name => "strtoumax";

  -- Like `wcstol' but convert to `intmax_t'.   
   function wcstoimax
     (uu_nptr : access wchar_t;
      uu_endptr : System.Address;
      uu_base : int) return stdint_h.intmax_t  -- /usr/include/inttypes.h:305
   with Import => True, 
        Convention => C, 
        External_Name => "wcstoimax";

  -- Like `wcstoul' but convert to `uintmax_t'.   
   function wcstoumax
     (uu_nptr : access wchar_t;
      uu_endptr : System.Address;
      uu_base : int) return stdint_h.uintmax_t  -- /usr/include/inttypes.h:310
   with Import => True, 
        Convention => C, 
        External_Name => "wcstoumax";

  -- Like `strtol' but convert to `intmax_t'.   
  -- Like `strtoul' but convert to `uintmax_t'.   
  -- Like `wcstol' but convert to `intmax_t'.   
  -- Like `wcstoul' but convert to `uintmax_t'.   
  -- Like `strtol' but convert to `intmax_t'.   
  -- Like `strtoul' but convert to `uintmax_t'.   
  -- Like `wcstol' but convert to `intmax_t'.   
  -- Like `wcstoul' but convert to `uintmax_t'.   
end inttypes_h;
