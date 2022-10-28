pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with xlocale_h;

package ctype_h is

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

  -- *	ISO C99 Standard 7.4: Character handling	<ctype.h>
  --  

  -- These are all the characteristics of characters.
  --   If there get to be more than 16 distinct characteristics,
  --   many things must be changed that use `unsigned short int's.
  --   The characteristics are stored always in network byte order (big
  --   endian).  We define the bit value interpretations here dependent on the
  --   machine's byte order.   

  -- UPPERCASE.   
  -- lowercase.   
  -- Alphabetic.   
  -- Numeric.   
  -- Hexadecimal numeric.   
  -- Whitespace.   
  -- Printing.   
  -- Graphical.   
  -- Blank (usually SPC and TAB).   
  -- Control character.   
  -- Punctuation.   
  -- Alphanumeric.   
  -- These are defined in ctype-info.c.
  --   The declarations here must match those in localeinfo.h.
  --   In the thread-specific locale model (see `uselocale' in <locale.h>)
  --   we cannot use global variables for these as was done in the past.
  --   Instead, the following accessor functions return the address of
  --   each variable, which is local to the current thread if multithreaded.
  --   These point into arrays of 384, so they can be indexed by any `unsigned
  --   char' value [0,255]; by EOF (-1); or by any `signed char' value
  --   [-128,-1).  ISO C requires that the ctype functions work for `unsigned
  --   char' values and for EOF; we also support negative `signed char' values
  --   for broken old programs.  The case conversion arrays are of `int's
  --   rather than `unsigned char's because tolower (EOF) must be EOF, which
  --   doesn't fit into an `unsigned char'.  But today more important is that
  --   the arrays are also used for multi-byte character sets.   

   --  skipped func __ctype_b_loc

   --  skipped func __ctype_tolower_loc

   --  skipped func __ctype_toupper_loc

  -- The following names are all functions:
  --     int isCHARACTERISTIC(int c);
  --   which return nonzero iff C has CHARACTERISTIC.
  --   For the meaning of the characteristic names, see the `enum' above.   

   function isalnum (arg1 : int) return int  -- /usr/include/ctype.h:110
   with Import => True, 
        Convention => C, 
        External_Name => "isalnum";

   function isalpha (arg1 : int) return int  -- /usr/include/ctype.h:111
   with Import => True, 
        Convention => C, 
        External_Name => "isalpha";

   function iscntrl (arg1 : int) return int  -- /usr/include/ctype.h:112
   with Import => True, 
        Convention => C, 
        External_Name => "iscntrl";

   function isdigit (arg1 : int) return int  -- /usr/include/ctype.h:113
   with Import => True, 
        Convention => C, 
        External_Name => "isdigit";

   function islower (arg1 : int) return int  -- /usr/include/ctype.h:114
   with Import => True, 
        Convention => C, 
        External_Name => "islower";

   function isgraph (arg1 : int) return int  -- /usr/include/ctype.h:115
   with Import => True, 
        Convention => C, 
        External_Name => "isgraph";

   function isprint (arg1 : int) return int  -- /usr/include/ctype.h:116
   with Import => True, 
        Convention => C, 
        External_Name => "isprint";

   function ispunct (arg1 : int) return int  -- /usr/include/ctype.h:117
   with Import => True, 
        Convention => C, 
        External_Name => "ispunct";

   function isspace (arg1 : int) return int  -- /usr/include/ctype.h:118
   with Import => True, 
        Convention => C, 
        External_Name => "isspace";

   function isupper (arg1 : int) return int  -- /usr/include/ctype.h:119
   with Import => True, 
        Convention => C, 
        External_Name => "isupper";

   function isxdigit (arg1 : int) return int  -- /usr/include/ctype.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "isxdigit";

  -- Return the lowercase version of C.   
   function tolower (uu_c : int) return int  -- /usr/include/ctype.h:124
   with Import => True, 
        Convention => C, 
        External_Name => "tolower";

  -- Return the uppercase version of C.   
   function toupper (uu_c : int) return int  -- /usr/include/ctype.h:127
   with Import => True, 
        Convention => C, 
        External_Name => "toupper";

  -- ISO C99 introduced one new function.   
   function isblank (arg1 : int) return int  -- /usr/include/ctype.h:136
   with Import => True, 
        Convention => C, 
        External_Name => "isblank";

  -- Test C for a set of character classes according to MASK.   
   function isctype (uu_c : int; uu_mask : int) return int  -- /usr/include/ctype.h:143
   with Import => True, 
        Convention => C, 
        External_Name => "isctype";

  -- Return nonzero iff C is in the ASCII set
  --   (i.e., is no more than 7 bits wide).   

   function isascii (uu_c : int) return int  -- /usr/include/ctype.h:150
   with Import => True, 
        Convention => C, 
        External_Name => "isascii";

  -- Return the part of C that is in the ASCII set
  --   (i.e., the low-order 7 bits of C).   

   function toascii (uu_c : int) return int  -- /usr/include/ctype.h:154
   with Import => True, 
        Convention => C, 
        External_Name => "toascii";

  -- These are the same as `toupper' and `tolower' except that they do not
  --   check the argument for being in the range of a `char'.   

   --  skipped func _toupper

   --  skipped func _tolower

  -- This code is needed for the optimized mapping functions.   
  -- The concept of one static locale per category is not very well
  --   thought out.  Many applications will need to process its data using
  --   information from several different locales.  Another application is
  --   the implementation of the internationalization handling in the
  --   upcoming ISO C++ standard library.  To support this another set of
  --   the functions using locale data exist which have an additional
  --   argument.
  --   Attention: all these functions are *not* standardized in any form.
  --   This is a proof-of-concept implementation.   

  -- Structure for reentrant locale using functions.  This is an
  --   (almost) opaque type for the user level programs.   

  -- These definitions are similar to the ones above but all functions
  --   take as an argument a handle for the locale which shall be used.   

  -- The following names are all functions:
  --     int isCHARACTERISTIC(int c, locale_t *locale);
  --   which return nonzero iff C has CHARACTERISTIC.
  --   For the meaning of the characteristic names, see the `enum' above.   

   function isalnum_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:271
   with Import => True, 
        Convention => C, 
        External_Name => "isalnum_l";

   function isalpha_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:272
   with Import => True, 
        Convention => C, 
        External_Name => "isalpha_l";

   function iscntrl_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:273
   with Import => True, 
        Convention => C, 
        External_Name => "iscntrl_l";

   function isdigit_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:274
   with Import => True, 
        Convention => C, 
        External_Name => "isdigit_l";

   function islower_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:275
   with Import => True, 
        Convention => C, 
        External_Name => "islower_l";

   function isgraph_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:276
   with Import => True, 
        Convention => C, 
        External_Name => "isgraph_l";

   function isprint_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:277
   with Import => True, 
        Convention => C, 
        External_Name => "isprint_l";

   function ispunct_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:278
   with Import => True, 
        Convention => C, 
        External_Name => "ispunct_l";

   function isspace_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:279
   with Import => True, 
        Convention => C, 
        External_Name => "isspace_l";

   function isupper_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:280
   with Import => True, 
        Convention => C, 
        External_Name => "isupper_l";

   function isxdigit_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:281
   with Import => True, 
        Convention => C, 
        External_Name => "isxdigit_l";

   function isblank_l (arg1 : int; arg2 : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:283
   with Import => True, 
        Convention => C, 
        External_Name => "isblank_l";

  -- Return the lowercase version of C in locale L.   
   --  skipped func __tolower_l

   function tolower_l (uu_c : int; uu_l : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:288
   with Import => True, 
        Convention => C, 
        External_Name => "tolower_l";

  -- Return the uppercase version of C.   
   --  skipped func __toupper_l

   function toupper_l (uu_c : int; uu_l : xlocale_h.uu_locale_t) return int  -- /usr/include/ctype.h:292
   with Import => True, 
        Convention => C, 
        External_Name => "toupper_l";

end ctype_h;
