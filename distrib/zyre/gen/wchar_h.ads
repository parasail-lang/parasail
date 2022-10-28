pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package wchar_h is

  -- Copyright (C) 1995-2014 Free Software Foundation, Inc.
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

  -- *      ISO C99 Standard: 7.24
  -- *	Extended multibyte and wide character utilities	<wchar.h>
  --  

  -- Get FILE definition.   
  -- Get va_list definition.   
  -- Get size_t, wchar_t, wint_t and NULL from <stddef.h>.   
  -- We try to get wint_t from <stddef.h>, but not all GCC versions define it
  --   there.  So define it ourselves if it remains undefined.   

  -- Integral type unchanged by default argument promotions that can
  --   hold any value corresponding to members of the extended character
  --   set, as well as at least one value that does not correspond to any
  --   member of the extended character set.   

  -- Work around problems with the <stddef.h> file which doesn't put
  --   wint_t in the std namespace.   

  -- Tell the caller that we provide correct C++ prototypes.   
  -- Conversion state information.   
   --  skipped anonymous struct anon_2

   subtype uu_mbstate_t_array897 is Interfaces.C.char_array (0 .. 3);
   type anon_3 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_wch : aliased unsigned;  -- /usr/include/wchar.h:88
         when others =>
            uu_wchb : aliased uu_mbstate_t_array897;  -- /usr/include/wchar.h:92
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type uu_mbstate_t is record
      uu_count : aliased int;  -- /usr/include/wchar.h:84
      uu_value : aliased anon_3;  -- /usr/include/wchar.h:93
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/wchar.h:94

  -- Value so far.   
  -- The rest of the file is only used if used if __need_mbstate_t is not
  --   defined.   

  -- Public type.   
  -- These constants might also be defined in <inttypes.h>.   
  -- For XPG4 compliance we have to define the stuff from <wctype.h> here
  --   as well.   

  -- This incomplete type is defined in <time.h> but needed here because
  --   of `wcsftime'.   

  -- XXX We have to clean this up at some point.  Since tm is in the std
  --   namespace but wcsftime is in __c99 the type wouldn't be found
  --   without inserting it in the global namespace.   

  -- Copy SRC to DEST.   
  -- Copy no more than N wide-characters of SRC to DEST.   
  -- Append SRC onto DEST.   
  -- Append no more than N wide-characters of SRC onto DEST.   
  -- Compare S1 and S2.   
  -- Compare N wide-characters of S1 and S2.   
  -- Compare S1 and S2, ignoring case.   
  -- Compare no more than N chars of S1 and S2, ignoring case.   
  -- Similar to the two functions above but take the information from
  --   the provided locale and not the global locale.   

  -- Compare S1 and S2, both interpreted as appropriate to the
  --   LC_COLLATE category of the current locale.   

  -- Transform S2 into array pointed to by S1 such that if wcscmp is
  --   applied to two transformed strings the result is the as applying
  --   `wcscoll' to the original strings.   

  -- Similar to the two functions above but take the information from
  --   the provided locale and not the global locale.   

  -- Compare S1 and S2, both interpreted as appropriate to the
  --   LC_COLLATE category of the given locale.   

  -- Transform S2 into array pointed to by S1 such that if wcscmp is
  --   applied to two transformed strings the result is the as applying
  --   `wcscoll' to the original strings.   

  -- Duplicate S, returning an identical malloc'd string.   
  -- Find the first occurrence of WC in WCS.   
  -- Find the last occurrence of WC in WCS.   
  -- This function is similar to `wcschr'.  But it returns a pointer to
  --   the closing NUL wide character in case C is not found in S.   

  -- Return the length of the initial segmet of WCS which
  --   consists entirely of wide characters not in REJECT.   

  -- Return the length of the initial segmet of WCS which
  --   consists entirely of wide characters in  ACCEPT.   

  -- Find the first occurrence in WCS of any character in ACCEPT.   
  -- Find the first occurrence of NEEDLE in HAYSTACK.   
  -- Divide WCS into tokens separated by characters in DELIM.   
  -- Return the number of wide characters in S.   
  -- Another name for `wcsstr' from XPG4.   
  -- Return the number of wide characters in S, but at most MAXLEN.   
  -- Search N wide characters of S for C.   
  -- Compare N wide characters of S1 and S2.   
  -- Copy N wide characters of SRC to DEST.   
  -- Copy N wide characters of SRC to DEST, guaranteeing
  --   correct behavior for overlapping strings.   

  -- Set N wide characters of S to C.   
  -- Copy N wide characters of SRC to DEST and return pointer to following
  --   wide character.   

  -- Determine whether C constitutes a valid (one-byte) multibyte
  --   character.   

  -- Determine whether C corresponds to a member of the extended
  --   character set whose multibyte representation is a single byte.   

  -- Determine whether PS points to an object representing the initial
  --   state.   

  -- Write wide character representation of multibyte character pointed
  --   to by S to PWC.   

  -- Write multibyte representation of wide character WC to S.   
  -- Return number of bytes in multibyte character pointed to by S.   
  -- Define inline function as optimization.   
  -- We can use the BTOWC and WCTOB optimizations since we know that all
  --   locales must use ASCII encoding for the values in the ASCII range
  --   and because the wchar_t encoding is always ISO 10646.   

  -- Write wide character representation of multibyte character string
  --   SRC to DST.   

  -- Write multibyte character representation of wide character string
  --   SRC to DST.   

  -- Write wide character representation of at most NMC bytes of the
  --   multibyte character string SRC to DST.   

  -- Write multibyte character representation of at most NWC characters
  --   from the wide character string SRC to DST.   

  -- The following functions are extensions found in X/Open CAE.   
  -- Determine number of column positions required for C.   
  -- Determine number of column positions required for first N wide
  --   characters (or fewer if S ends before this) in S.   

  -- Convert initial portion of the wide string NPTR to `double'
  --   representation.   

  -- Likewise for `float' and `long double' sizes of floating-point numbers.   
  -- Convert initial portion of wide string NPTR to `long int'
  --   representation.   

  -- Convert initial portion of wide string NPTR to `unsigned long int'
  --   representation.   

  -- Convert initial portion of wide string NPTR to `long long int'
  --   representation.   

  -- Convert initial portion of wide string NPTR to `unsigned long long int'
  --   representation.   

  -- Convert initial portion of wide string NPTR to `long long int'
  --   representation.   

  -- Convert initial portion of wide string NPTR to `unsigned long long int'
  --   representation.   

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

  -- Special versions of the functions above which take the locale to
  --   use as an additional parameter.   

  -- Copy SRC to DEST, returning the address of the terminating L'\0' in
  --   DEST.   

  -- Copy no more than N characters of SRC to DEST, returning the address of
  --   the last character written into DEST.   

  -- Wide character I/O functions.   
  -- Like OPEN_MEMSTREAM, but the stream is wide oriented and produces
  --   a wide character string.   

  -- Select orientation for stream.   
  -- Write formatted output to STREAM.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- __attribute__ ((__format__ (__wprintf__, 2, 3)))  
  -- Write formatted output to stdout.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- __attribute__ ((__format__ (__wprintf__, 1, 2)))  
  -- Write formatted output of at most N characters to S.   
  -- __attribute__ ((__format__ (__wprintf__, 3, 4)))  
  -- Write formatted output to S from argument list ARG.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- __attribute__ ((__format__ (__wprintf__, 2, 0)))  
  -- Write formatted output to stdout from argument list ARG.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- __attribute__ ((__format__ (__wprintf__, 1, 0)))  
  -- Write formatted output of at most N character to S from argument
  --   list ARG.   

  -- __attribute__ ((__format__ (__wprintf__, 3, 0)))  
  -- Read formatted input from STREAM.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- __attribute__ ((__format__ (__wscanf__, 2, 3)))  
  -- Read formatted input from stdin.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- __attribute__ ((__format__ (__wscanf__, 1, 2)))  
  -- Read formatted input from S.   
  -- __attribute__ ((__format__ (__wscanf__, 2, 3)))  
  -- For strict ISO C99 or POSIX compliance disallow %as, %aS and %a[
  --   GNU extension which conflicts with valid %a followed by letter
  --   s, S or [.   

  -- __attribute__ ((__format__ (__wscanf__, 2, 3)))  
  -- __attribute__ ((__format__ (__wscanf__, 1, 2)))  
  -- __attribute__ ((__format__ (__wscanf__, 2, 3)))  
  -- Read formatted input from S into argument list ARG.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- __attribute__ ((__format__ (__wscanf__, 2, 0)))  
  -- Read formatted input from stdin into argument list ARG.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- __attribute__ ((__format__ (__wscanf__, 1, 0)))  
  -- Read formatted input from S into argument list ARG.   
  -- __attribute__ ((__format__ (__wscanf__, 2, 0)))  
  -- __attribute__ ((__format__ (__wscanf__, 2, 0)))  
  -- __attribute__ ((__format__ (__wscanf__, 1, 0)))  
  -- __attribute__ ((__format__ (__wscanf__, 2, 0)))  
  -- Read a character from STREAM.
  --   These functions are possible cancellation points and therefore not
  --   marked with __THROW.   

  -- Read a character from stdin.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- Write a character to STREAM.
  --   These functions are possible cancellation points and therefore not
  --   marked with __THROW.   

  -- Write a character to stdout.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- Get a newline-terminated wide character string of finite length
  --   from STREAM.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- Write a string to STREAM.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- Push a character back onto the input buffer of STREAM.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

  -- These are defined to be equivalent to the `char' functions defined
  --   in POSIX.1:1996.
  --   These functions are not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation they are cancellation points and
  --   therefore not marked with __THROW.   

  -- This is the wide character version of a GNU extension.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

  -- Faster version when locking is not necessary.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

  -- These are defined to be equivalent to the `char' functions defined
  --   in POSIX.1:1996.
  --   These functions are not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation they are cancellation points and
  --   therefore not marked with __THROW.   

  -- This function does the same as `fgetws' but does not lock the stream.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

  -- This function does the same as `fputws' but does not lock the stream.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

  -- Format TP into S according to FORMAT.
  --   Write no more than MAXSIZE wide characters and return the number
  --   of wide characters written, or 0 if it would exceed MAXSIZE.   

  -- Similar to `wcsftime' but takes the information from
  --   the provided locale and not the global locale.   

  -- The X/Open standard demands that most of the functions defined in
  --   the <wctype.h> header must also appear here.  This is probably
  --   because some X/Open members wrote their implementation before the
  --   ISO C standard was published and introduced the better solution.
  --   We have to provide these definitions for compliance reasons but we
  --   do this nonsense only if really necessary.   

  -- Define some macros helping to catch buffer overflows.   
  -- Undefine all __need_* constants in case we are included to get those
  --   constants but the whole file was already read.   

end wchar_h;
