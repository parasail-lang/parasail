pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package assert_h is

   --  arg-macro: function assert (expr)
   --    return (expr) ? __ASSERT_VOID_CAST (0) : __assert_fail (__STRING(expr), __FILE__, __LINE__, __ASSERT_FUNCTION);
   --  arg-macro: function assert_perror (errnum)
   --    return not(errnum) ? __ASSERT_VOID_CAST (0) : __assert_perror_fail ((errnum), __FILE__, __LINE__, __ASSERT_FUNCTION);
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

  -- *	ISO C99 Standard: 7.2 Diagnostics	<assert.h>
  --  

  -- void assert (int expression);
  --   If NDEBUG is defined, do nothing.
  --   If not, and EXPRESSION is zero, print an error message and abort.   

  -- void assert_perror (int errnum);
  --   If NDEBUG is defined, do nothing.  If not, and ERRNUM is not zero, print an
  --   error message with the error text for ERRNUM and abort.
  --   (This is a GNU extension.)  

  -- This prints an "Assertion failed" message and aborts.   
   --  skipped func __assert_fail

  -- Likewise, but prints the error text for ERRNUM.   
   --  skipped func __assert_perror_fail

  -- The following is not at all used here but needed for standard
  --   compliance.   

   --  skipped func __assert

  -- Version 2.4 and later of GCC define a magical variable `__PRETTY_FUNCTION__'
  --   which contains the name of the function currently being defined.
  --   This is broken in G++ before version 2.6.
  --   C9x has a similar variable called __func__, but prefer the GCC one since
  --   it demangles C++ function names.   

  -- Static assertion.  Requires support in the compiler.   
end assert_h;
