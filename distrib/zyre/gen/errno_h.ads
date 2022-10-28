pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package errno_h is

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

  -- *	ISO C99 Standard: 7.5 Errors	<errno.h>
  --  

  -- The includer defined __need_Emath if he wants only the definitions
  --   of EDOM and ERANGE, and not everything else.   

  -- Get the error number constants from the system-specific file.
  --   This file will test __need_Emath and _ERRNO_H.   

  -- Declare the `errno' variable, unless it's defined as a macro by
  --   bits/errno.h.  This is the case in GNU, where it is a per-thread
  --   variable.  This redeclaration using the macro still works, but it
  --   will be a function declaration without a prototype and may trigger
  --   a -Wstrict-prototypes warning.   

  -- The full and simple forms of the name with which the program was
  --   invoked.  These variables are set up automatically at startup based on
  --   the value of ARGV[0] (this works only if you use GNU ld).   

   program_invocation_name : Interfaces.C.Strings.chars_ptr  -- /usr/include/errno.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "program_invocation_name";

   program_invocation_short_name : Interfaces.C.Strings.chars_ptr  -- /usr/include/errno.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "program_invocation_short_name";

  -- The Hurd <bits/errno.h> defines `error_t' as an enumerated type so
  --   that printing `error_t' values in the debugger shows the names.  We
  --   might need this definition sometimes even if this file was included
  --   before.   

   subtype error_t is int;  -- /usr/include/errno.h:68

end errno_h;
