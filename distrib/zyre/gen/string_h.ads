pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;
with stddef_h;
with Interfaces.C.Strings;
with xlocale_h;

package string_h is

   --  arg-macro: function strdupa (s)
   --    return __extension__ ({ const char *__old := (s); size_t __len := strlen (__old) + 1; char *__new := (char *) __builtin_alloca (__len); (char *) memcpy (__new, __old, __len); });
   --  arg-macro: function strndupa (s, n)
   --    return __extension__ ({ const char *__old := (s); size_t __len := strnlen (__old, (n)); char *__new := (char *) __builtin_alloca (__len + 1); __new(__len) := Character'Val (0); (char *) memcpy (__new, __old, __len); });
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

  -- *	ISO C99 Standard: 7.21 String handling	<string.h>
  --  

  -- Get size_t and NULL from <stddef.h>.   
  -- Provide correct C++ prototypes, and indicate this to the caller.  This
  --   requires a compatible C++ standard library.  As a heuristic, we provide
  --   these when the compiler indicates full conformance with C++98 or later,
  --   and for older GCC versions that are known to provide a compatible
  --   libstdc++.   

  -- Copy N bytes of SRC to DEST.   
   function memcpy
     (uu_dest : System.Address;
      uu_src : System.Address;
      uu_n : stddef_h.size_t) return System.Address  -- /usr/include/string.h:46
   with Import => True, 
        Convention => C, 
        External_Name => "memcpy";

  -- Copy N bytes of SRC to DEST, guaranteeing
  --   correct behavior for overlapping strings.   

   function memmove
     (uu_dest : System.Address;
      uu_src : System.Address;
      uu_n : stddef_h.size_t) return System.Address  -- /usr/include/string.h:50
   with Import => True, 
        Convention => C, 
        External_Name => "memmove";

  -- Copy no more than N bytes of SRC to DEST, stopping when C is found.
  --   Return the position in DEST one byte past where C was copied,
  --   or NULL if C was not found in the first N bytes of SRC.   

   function memccpy
     (uu_dest : System.Address;
      uu_src : System.Address;
      uu_c : int;
      uu_n : stddef_h.size_t) return System.Address  -- /usr/include/string.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "memccpy";

  -- Set N bytes of S to C.   
   function memset
     (uu_s : System.Address;
      uu_c : int;
      uu_n : stddef_h.size_t) return System.Address  -- /usr/include/string.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "memset";

  -- Compare N bytes of S1 and S2.   
   function memcmp
     (uu_s1 : System.Address;
      uu_s2 : System.Address;
      uu_n : stddef_h.size_t) return int  -- /usr/include/string.h:69
   with Import => True, 
        Convention => C, 
        External_Name => "memcmp";

  -- Search N bytes of S for C.   
   function memchr
     (uu_s : System.Address;
      uu_c : int;
      uu_n : stddef_h.size_t) return System.Address  -- /usr/include/string.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "memchr";

  -- Search in S for C.  This is similar to `memchr' but there is no
  --   length limit.   

   function rawmemchr (uu_s : System.Address; uu_c : int) return System.Address  -- /usr/include/string.h:105
   with Import => True, 
        Convention => C, 
        External_Name => "rawmemchr";

  -- Search N bytes of S for the final occurrence of C.   
   function memrchr
     (uu_s : System.Address;
      uu_c : int;
      uu_n : stddef_h.size_t) return System.Address  -- /usr/include/string.h:116
   with Import => True, 
        Convention => C, 
        External_Name => "memrchr";

  -- Copy SRC to DEST.   
   function strcpy (uu_dest : Interfaces.C.Strings.chars_ptr; uu_src : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:129
   with Import => True, 
        Convention => C, 
        External_Name => "strcpy";

  -- Copy no more than N characters of SRC to DEST.   
   function strncpy
     (uu_dest : Interfaces.C.Strings.chars_ptr;
      uu_src : Interfaces.C.Strings.chars_ptr;
      uu_n : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:132
   with Import => True, 
        Convention => C, 
        External_Name => "strncpy";

  -- Append SRC onto DEST.   
   function strcat (uu_dest : Interfaces.C.Strings.chars_ptr; uu_src : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:137
   with Import => True, 
        Convention => C, 
        External_Name => "strcat";

  -- Append no more than N characters from SRC onto DEST.   
   function strncat
     (uu_dest : Interfaces.C.Strings.chars_ptr;
      uu_src : Interfaces.C.Strings.chars_ptr;
      uu_n : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:140
   with Import => True, 
        Convention => C, 
        External_Name => "strncat";

  -- Compare S1 and S2.   
   function strcmp (uu_s1 : Interfaces.C.Strings.chars_ptr; uu_s2 : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/string.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "strcmp";

  -- Compare N characters of S1 and S2.   
   function strncmp
     (uu_s1 : Interfaces.C.Strings.chars_ptr;
      uu_s2 : Interfaces.C.Strings.chars_ptr;
      uu_n : stddef_h.size_t) return int  -- /usr/include/string.h:147
   with Import => True, 
        Convention => C, 
        External_Name => "strncmp";

  -- Compare the collated forms of S1 and S2.   
   function strcoll (uu_s1 : Interfaces.C.Strings.chars_ptr; uu_s2 : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/string.h:151
   with Import => True, 
        Convention => C, 
        External_Name => "strcoll";

  -- Put a transformation of SRC into no more than N bytes of DEST.   
   function strxfrm
     (uu_dest : Interfaces.C.Strings.chars_ptr;
      uu_src : Interfaces.C.Strings.chars_ptr;
      uu_n : stddef_h.size_t) return stddef_h.size_t  -- /usr/include/string.h:154
   with Import => True, 
        Convention => C, 
        External_Name => "strxfrm";

  -- The following functions are equivalent to the both above but they
  --   take the locale they use for the collation as an extra argument.
  --   This is not standardsized but something like will come.   

  -- Compare the collated forms of S1 and S2 using rules from L.   
   function strcoll_l
     (uu_s1 : Interfaces.C.Strings.chars_ptr;
      uu_s2 : Interfaces.C.Strings.chars_ptr;
      uu_l : xlocale_h.uu_locale_t) return int  -- /usr/include/string.h:166
   with Import => True, 
        Convention => C, 
        External_Name => "strcoll_l";

  -- Put a transformation of SRC into no more than N bytes of DEST.   
   function strxfrm_l
     (uu_dest : Interfaces.C.Strings.chars_ptr;
      uu_src : Interfaces.C.Strings.chars_ptr;
      uu_n : stddef_h.size_t;
      uu_l : xlocale_h.uu_locale_t) return stddef_h.size_t  -- /usr/include/string.h:169
   with Import => True, 
        Convention => C, 
        External_Name => "strxfrm_l";

  -- Duplicate S, returning an identical malloc'd string.   
   function strdup (uu_s : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:176
   with Import => True, 
        Convention => C, 
        External_Name => "strdup";

  -- Return a malloc'd copy of at most N bytes of STRING.  The
  --   resultant string is terminated even if no null terminator
  --   appears before STRING[N].   

   function strndup (uu_string : Interfaces.C.Strings.chars_ptr; uu_n : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:184
   with Import => True, 
        Convention => C, 
        External_Name => "strndup";

  -- Duplicate S, returning an identical alloca'd string.   
  -- Return an alloca'd copy of at most N bytes of string.   
  -- Find the first occurrence of C in S.   
   function strchr (uu_s : Interfaces.C.Strings.chars_ptr; uu_c : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:216
   with Import => True, 
        Convention => C, 
        External_Name => "strchr";

  -- Find the last occurrence of C in S.   
   function strrchr (uu_s : Interfaces.C.Strings.chars_ptr; uu_c : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:243
   with Import => True, 
        Convention => C, 
        External_Name => "strrchr";

  -- This function is similar to `strchr'.  But it returns a pointer to
  --   the closing NUL byte in case C is not found in S.   

   function strchrnul (uu_s : Interfaces.C.Strings.chars_ptr; uu_c : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:272
   with Import => True, 
        Convention => C, 
        External_Name => "strchrnul";

  -- Return the length of the initial segment of S which
  --   consists entirely of characters not in REJECT.   

   function strcspn (uu_s : Interfaces.C.Strings.chars_ptr; uu_reject : Interfaces.C.Strings.chars_ptr) return stddef_h.size_t  -- /usr/include/string.h:285
   with Import => True, 
        Convention => C, 
        External_Name => "strcspn";

  -- Return the length of the initial segment of S which
  --   consists entirely of characters in ACCEPT.   

   function strspn (uu_s : Interfaces.C.Strings.chars_ptr; uu_accept : Interfaces.C.Strings.chars_ptr) return stddef_h.size_t  -- /usr/include/string.h:289
   with Import => True, 
        Convention => C, 
        External_Name => "strspn";

  -- Find the first occurrence in S of any character in ACCEPT.   
   function strpbrk (uu_s : Interfaces.C.Strings.chars_ptr; uu_accept : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:295
   with Import => True, 
        Convention => C, 
        External_Name => "strpbrk";

  -- Find the first occurrence of NEEDLE in HAYSTACK.   
   function strstr (uu_haystack : Interfaces.C.Strings.chars_ptr; uu_needle : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:322
   with Import => True, 
        Convention => C, 
        External_Name => "strstr";

  -- Divide S into tokens separated by characters in DELIM.   
   function strtok (uu_s : Interfaces.C.Strings.chars_ptr; uu_delim : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:348
   with Import => True, 
        Convention => C, 
        External_Name => "strtok";

  -- Divide S into tokens separated by characters in DELIM.  Information
  --   passed between calls are stored in SAVE_PTR.   

   --  skipped func __strtok_r

   function strtok_r
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_delim : Interfaces.C.Strings.chars_ptr;
      uu_save_ptr : System.Address) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:359
   with Import => True, 
        Convention => C, 
        External_Name => "strtok_r";

  -- Similar to `strstr' but this function ignores the case of both strings.   
   function strcasestr (uu_haystack : Interfaces.C.Strings.chars_ptr; uu_needle : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:367
   with Import => True, 
        Convention => C, 
        External_Name => "strcasestr";

  -- Find the first occurrence of NEEDLE in HAYSTACK.
  --   NEEDLE is NEEDLELEN bytes long;
  --   HAYSTACK is HAYSTACKLEN bytes long.   

   function memmem
     (uu_haystack : System.Address;
      uu_haystacklen : stddef_h.size_t;
      uu_needle : System.Address;
      uu_needlelen : stddef_h.size_t) return System.Address  -- /usr/include/string.h:382
   with Import => True, 
        Convention => C, 
        External_Name => "memmem";

  -- Copy N bytes of SRC to DEST, return pointer to bytes after the
  --   last written byte.   

   --  skipped func __mempcpy

   function mempcpy
     (uu_dest : System.Address;
      uu_src : System.Address;
      uu_n : stddef_h.size_t) return System.Address  -- /usr/include/string.h:391
   with Import => True, 
        Convention => C, 
        External_Name => "mempcpy";

  -- Return the length of S.   
   function strlen (uu_s : Interfaces.C.Strings.chars_ptr) return stddef_h.size_t  -- /usr/include/string.h:399
   with Import => True, 
        Convention => C, 
        External_Name => "strlen";

  -- Find the length of STRING, but scan at most MAXLEN characters.
  --   If no '\0' terminator is found in that many characters, return MAXLEN.   

   function strnlen (uu_string : Interfaces.C.Strings.chars_ptr; uu_maxlen : stddef_h.size_t) return stddef_h.size_t  -- /usr/include/string.h:406
   with Import => True, 
        Convention => C, 
        External_Name => "strnlen";

  -- Return a string describing the meaning of the `errno' code in ERRNUM.   
   function strerror (uu_errnum : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:413
   with Import => True, 
        Convention => C, 
        External_Name => "strerror";

  -- Reentrant version of `strerror'.
  --   There are 2 flavors of `strerror_r', GNU which returns the string
  --   and may or may not use the supplied temporary buffer and POSIX one
  --   which fills the string into the buffer.
  --   To use the POSIX version, -D_XOPEN_SOURCE=600 or -D_POSIX_C_SOURCE=200112L
  --   without -D_GNU_SOURCE is needed, otherwise the GNU version is
  --   preferred.   

  -- Fill BUF with a string describing the meaning of the `errno' code in
  --   ERRNUM.   

  -- If a temporary buffer is required, at most BUFLEN bytes of BUF will be
  --   used.   

   function strerror_r
     (uu_errnum : int;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:438
   with Import => True, 
        Convention => C, 
        External_Name => "strerror_r";

  -- Translate error number to string according to the locale L.   
   function strerror_l (uu_errnum : int; uu_l : xlocale_h.uu_locale_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:445
   with Import => True, 
        Convention => C, 
        External_Name => "strerror_l";

  -- We define this function always since `bzero' is sometimes needed when
  --   the namespace rules does not allow this.   

   --  skipped func __bzero

  -- Copy N bytes of SRC to DEST (like memmove, but args reversed).   
   procedure bcopy
     (uu_src : System.Address;
      uu_dest : System.Address;
      uu_n : stddef_h.size_t)  -- /usr/include/string.h:455
   with Import => True, 
        Convention => C, 
        External_Name => "bcopy";

  -- Set N bytes of S to 0.   
   procedure bzero (uu_s : System.Address; uu_n : stddef_h.size_t)  -- /usr/include/string.h:459
   with Import => True, 
        Convention => C, 
        External_Name => "bzero";

  -- Compare N bytes of S1 and S2 (same as memcmp).   
   function bcmp
     (uu_s1 : System.Address;
      uu_s2 : System.Address;
      uu_n : stddef_h.size_t) return int  -- /usr/include/string.h:462
   with Import => True, 
        Convention => C, 
        External_Name => "bcmp";

  -- Find the first occurrence of C in S (same as strchr).   
   function index (uu_s : Interfaces.C.Strings.chars_ptr; uu_c : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:469
   with Import => True, 
        Convention => C, 
        External_Name => "index";

  -- Find the last occurrence of C in S (same as strrchr).   
   function rindex (uu_s : Interfaces.C.Strings.chars_ptr; uu_c : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:497
   with Import => True, 
        Convention => C, 
        External_Name => "rindex";

  -- Return the position of the first bit set in I, or 0 if none are set.
  --   The least-significant bit is position 1, the most-significant 32.   

   function ffs (uu_i : int) return int  -- /usr/include/string.h:523
   with Import => True, 
        Convention => C, 
        External_Name => "ffs";

  -- The following two functions are non-standard but necessary for non-32 bit
  --   platforms.   

   function ffsl (uu_l : long) return int  -- /usr/include/string.h:528
   with Import => True, 
        Convention => C, 
        External_Name => "ffsl";

   function ffsll (uu_ll : Long_Long_Integer) return int  -- /usr/include/string.h:529
   with Import => True, 
        Convention => C, 
        External_Name => "ffsll";

  -- Compare S1 and S2, ignoring case.   
   function strcasecmp (uu_s1 : Interfaces.C.Strings.chars_ptr; uu_s2 : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/string.h:534
   with Import => True, 
        Convention => C, 
        External_Name => "strcasecmp";

  -- Compare no more than N chars of S1 and S2, ignoring case.   
   function strncasecmp
     (uu_s1 : Interfaces.C.Strings.chars_ptr;
      uu_s2 : Interfaces.C.Strings.chars_ptr;
      uu_n : stddef_h.size_t) return int  -- /usr/include/string.h:538
   with Import => True, 
        Convention => C, 
        External_Name => "strncasecmp";

  -- Again versions of a few functions which use the given locale instead
  --   of the global one.   

   function strcasecmp_l
     (uu_s1 : Interfaces.C.Strings.chars_ptr;
      uu_s2 : Interfaces.C.Strings.chars_ptr;
      uu_loc : xlocale_h.uu_locale_t) return int  -- /usr/include/string.h:545
   with Import => True, 
        Convention => C, 
        External_Name => "strcasecmp_l";

   function strncasecmp_l
     (uu_s1 : Interfaces.C.Strings.chars_ptr;
      uu_s2 : Interfaces.C.Strings.chars_ptr;
      uu_n : stddef_h.size_t;
      uu_loc : xlocale_h.uu_locale_t) return int  -- /usr/include/string.h:549
   with Import => True, 
        Convention => C, 
        External_Name => "strncasecmp_l";

  -- Return the next DELIM-delimited token from *STRINGP,
  --   terminating it with a '\0', and update *STRINGP to point past it.   

   function strsep (uu_stringp : System.Address; uu_delim : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:557
   with Import => True, 
        Convention => C, 
        External_Name => "strsep";

  -- Return a string describing the meaning of the signal number in SIG.   
   function strsignal (uu_sig : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:564
   with Import => True, 
        Convention => C, 
        External_Name => "strsignal";

  -- Copy SRC to DEST, returning the address of the terminating '\0' in DEST.   
   --  skipped func __stpcpy

   function stpcpy (uu_dest : Interfaces.C.Strings.chars_ptr; uu_src : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:569
   with Import => True, 
        Convention => C, 
        External_Name => "stpcpy";

  -- Copy no more than N characters of SRC to DEST, returning the address of
  --   the last character written into DEST.   

   --  skipped func __stpncpy

   function stpncpy
     (uu_dest : Interfaces.C.Strings.chars_ptr;
      uu_src : Interfaces.C.Strings.chars_ptr;
      uu_n : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:577
   with Import => True, 
        Convention => C, 
        External_Name => "stpncpy";

  -- Compare S1 and S2 as strings holding name & indices/version numbers.   
   function strverscmp (uu_s1 : Interfaces.C.Strings.chars_ptr; uu_s2 : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/string.h:584
   with Import => True, 
        Convention => C, 
        External_Name => "strverscmp";

  -- Sautee STRING briskly.   
   function strfry (uu_string : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:588
   with Import => True, 
        Convention => C, 
        External_Name => "strfry";

  -- Frobnicate N bytes of S.   
   function memfrob (uu_s : System.Address; uu_n : stddef_h.size_t) return System.Address  -- /usr/include/string.h:591
   with Import => True, 
        Convention => C, 
        External_Name => "memfrob";

  -- Return the file name within directory of FILENAME.  We don't
  --   declare the function if the `basename' macro is available (defined
  --   in <libgen.h>) which makes the XPG version of this function
  --   available.   

   function basename (uu_filename : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/string.h:599
   with Import => True, 
        Convention => C, 
        External_Name => "basename";

  -- When using GNU CC we provide some optimized versions of selected
  --   functions from this header.  There are two kinds of optimizations:
  --   - machine-dependent optimizations, most probably using inline
  --     assembler code; these might be quite expensive since the code
  --     size can increase significantly.
  --     These optimizations are not used unless the symbol
  --	__USE_STRING_INLINES
  --     is defined before including this header.
  --   - machine-independent optimizations which do not increase the
  --     code size significantly and which optimize mainly situations
  --     where one or more arguments are compile-time constants.
  --     These optimizations are used always when the compiler is
  --     taught to optimize.
  --   One can inhibit all optimizations by defining __NO_STRING_INLINES.   

  -- Get the machine-dependent optimizations (if any).   
  -- These are generic optimizations which do not add too much inline code.   
  -- Functions with security checks.   
end string_h;
