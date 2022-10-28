pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;

package stdarg_h is

   --  arg-macro: procedure va_start (v, l)
   --    __builtin_va_start(v,l)
   --  arg-macro: procedure va_end (v)
   --    __builtin_va_end(v)
   --  arg-macro: procedure va_arg (v, l)
   --    __builtin_va_arg(v,l)
   --  arg-macro: procedure va_copy (d, s)
   --    __builtin_va_copy(d,s)
  -- Copyright (C) 1989-2019 Free Software Foundation, Inc.
  --This file is part of GCC.
  --GCC is free software; you can redistribute it and/or modify
  --it under the terms of the GNU General Public License as published by
  --the Free Software Foundation; either version 3, or (at your option)
  --any later version.
  --GCC is distributed in the hope that it will be useful,
  --but WITHOUT ANY WARRANTY; without even the implied warranty of
  --MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  --GNU General Public License for more details.
  --Under Section 7 of GPL version 3, you are granted additional
  --permissions described in the GCC Runtime Library Exception, version
  --3.1, as published by the Free Software Foundation.
  --You should have received a copy of the GNU General Public License and
  --a copy of the GCC Runtime Library Exception along with this program;
  --see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
  --<http://www.gnu.org/licenses/>.   

  -- * ISO C Standard:  7.15  Variable arguments  <stdarg.h>
  --  

  -- Define __gnuc_va_list.   
   subtype uu_gnuc_va_list is System.Address;  -- /nfs/nas/homes/taft/wave/x86_64-linux/gnat/install/lib/gcc/x86_64-pc-linux-gnu/9.3.1/include/stdarg.h:40

  -- Define the standard macros for the user,
  --   if this invocation was from the user program.   

  -- Define va_list, if desired, from __gnuc_va_list.  
  -- We deliberately do not define va_list when called from
  --   stdio.h, because ANSI C says that stdio.h is not supposed to define
  --   va_list.  stdio.h needs to have access to that data type, 
  --   but must not use that name.  It should use the name __gnuc_va_list,
  --   which is safe because it is reserved for the implementation.   

  -- SVR4.2 uses _VA_LIST for an internal alias for va_list,
  --   so we must avoid testing it and setting it here.
  --   SVR4 uses _VA_LIST as a flag in stdarg.h, but we should
  --   have no conflict with that.   

  -- The macro _VA_LIST_ is the same thing used by this file in Ultrix.
  --   But on BSD NET2 we must not test or define or undef it.
  --   (Note that the comments in NET 2's ansi.h
  --   are incorrect for _VA_LIST_--see stdio.h!)   

  -- The macro _VA_LIST_DEFINED is used in Windows NT 3.5   
  -- The macro _VA_LIST is used in SCO Unix 3.2.   
  -- The macro _VA_LIST_T_H is used in the Bull dpx2   
  -- The macro __va_list__ is used by BeOS.   
end stdarg_h;
