pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_setjmp_h;
with x86_64_linux_gnu_bits_sigset_h;

package setjmp_h is

   --  arg-macro: procedure setjmp (env)
   --    _setjmp (env)
   --  arg-macro: procedure sigsetjmp (env, savemask)
   --    __sigsetjmp (env, savemask)
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

  -- *	ISO C99 Standard: 7.13 Nonlocal jumps	<setjmp.h>
  --  

  -- Get `__jmp_buf'.   
  -- Get `__sigset_t'.   
  -- Calling environment, plus possibly a saved signal mask.   
  -- NOTE: The machine-dependent definitions of `__sigsetjmp'
  --       assume that a `jmp_buf' begins with a `__jmp_buf' and that
  --       `__mask_was_saved' follows it.  Do not move these members
  --       or add others before it.   

  -- Calling environment.   
   type uu_jmp_buf_tag is record
      uu_jmpbuf : aliased x86_64_linux_gnu_bits_setjmp_h.uu_jmp_buf;  -- /usr/include/setjmp.h:40
      uu_mask_was_saved : aliased int;  -- /usr/include/setjmp.h:41
      uu_saved_mask : aliased x86_64_linux_gnu_bits_sigset_h.uu_sigset_t;  -- /usr/include/setjmp.h:42
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/setjmp.h:34

  -- Saved the signal mask?   
  -- Saved signal mask.   
   type jmp_buf is array (0 .. 0) of aliased uu_jmp_buf_tag;  -- /usr/include/setjmp.h:48

  -- Store the calling environment in ENV, also saving the signal mask.
  --   Return 0.   

   function setjmp (uu_env : access uu_jmp_buf_tag) return int  -- /usr/include/setjmp.h:52
   with Import => True, 
        Convention => C, 
        External_Name => "setjmp";

  -- Store the calling environment in ENV, also saving the
  --   signal mask if SAVEMASK is nonzero.  Return 0.
  --   This is the internal name for `sigsetjmp'.   

  -- Store the calling environment in ENV, not saving the signal mask.
  --   Return 0.   

   --  skipped func _setjmp

  -- Do not save the signal mask.  This is equivalent to the `_setjmp'
  --   BSD function.   

  -- Jump to the environment saved in ENV, making the
  --   `setjmp' call there return VAL, or 1 if VAL is 0.   

   procedure longjmp (uu_env : access uu_jmp_buf_tag; uu_val : int)  -- /usr/include/setjmp.h:74
   with Import => True, 
        Convention => C, 
        External_Name => "longjmp";

  -- Same.  Usually `_longjmp' is used with `_setjmp', which does not save
  --   the signal mask.  But it is how ENV was saved that determines whether
  --   `longjmp' restores the mask; `_longjmp' is just an alias.   

   --  skipped func _longjmp

  -- Use the same type for `jmp_buf' and `sigjmp_buf'.
  --   The `__mask_was_saved' flag determines whether
  --   or not `longjmp' will restore the signal mask.   

   type sigjmp_buf is array (0 .. 0) of aliased uu_jmp_buf_tag;  -- /usr/include/setjmp.h:92

  -- Store the calling environment in ENV, also saving the
  --   signal mask if SAVEMASK is nonzero.  Return 0.   

  -- Jump to the environment saved in ENV, making the
  --   sigsetjmp call there return VAL, or 1 if VAL is 0.
  --   Restore the signal mask if that sigsetjmp call saved it.
  --   This is just an alias `longjmp'.   

   procedure siglongjmp (uu_env : access uu_jmp_buf_tag; uu_val : int)  -- /usr/include/setjmp.h:102
   with Import => True, 
        Convention => C, 
        External_Name => "siglongjmp";

  -- Define helper functions to catch unsafe code.   
end setjmp_h;
