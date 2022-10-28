pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
with Interfaces.C.Extensions;
with x86_64_linux_gnu_bits_sigstack_h;
with x86_64_linux_gnu_bits_sigset_h;

package x86_64_linux_gnu_sys_ucontext_h is

   NGREG : constant := 23;  --  /usr/include/x86_64-linux-gnu/sys/ucontext.h:34
   --  unsupported macro: REG_R8 REG_R8
   --  unsupported macro: REG_R9 REG_R9
   --  unsupported macro: REG_R10 REG_R10
   --  unsupported macro: REG_R11 REG_R11
   --  unsupported macro: REG_R12 REG_R12
   --  unsupported macro: REG_R13 REG_R13
   --  unsupported macro: REG_R14 REG_R14
   --  unsupported macro: REG_R15 REG_R15
   --  unsupported macro: REG_RDI REG_RDI
   --  unsupported macro: REG_RSI REG_RSI
   --  unsupported macro: REG_RBP REG_RBP
   --  unsupported macro: REG_RBX REG_RBX
   --  unsupported macro: REG_RDX REG_RDX
   --  unsupported macro: REG_RAX REG_RAX
   --  unsupported macro: REG_RCX REG_RCX
   --  unsupported macro: REG_RSP REG_RSP
   --  unsupported macro: REG_RIP REG_RIP
   --  unsupported macro: REG_EFL REG_EFL
   --  unsupported macro: REG_CSGSFS REG_CSGSFS
   --  unsupported macro: REG_ERR REG_ERR
   --  unsupported macro: REG_TRAPNO REG_TRAPNO
   --  unsupported macro: REG_OLDMASK REG_OLDMASK
   --  unsupported macro: REG_CR2 REG_CR2

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

  -- We need the signal context definitions even if they are not used
  --   included in <signal.h>.   

  -- Type for general register.   
   subtype greg_t is Long_Long_Integer;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:31

  -- Number of general registers.   
  -- Container for all general registers.   
   type gregset_t is array (0 .. 22) of aliased greg_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:37

  -- Number of each register in the `gregset_t' array.   
  -- Actually short cs, gs, fs, __pad0.   
   type u_libc_fpxreg_array2912 is array (0 .. 3) of aliased unsigned_short;
   type u_libc_fpxreg_array1536 is array (0 .. 2) of aliased unsigned_short;
   type u_libc_fpxreg is record
      significand : aliased u_libc_fpxreg_array2912;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:94
      exponent : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:95
      padding : aliased u_libc_fpxreg_array1536;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:96
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:92

   type u_libc_xmmreg_array2918 is array (0 .. 3) of aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;
   type u_libc_xmmreg is record
      element : aliased u_libc_xmmreg_array2918;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:101
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:99

  -- 64-bit FXSAVE format.   
   type u_libc_fpstate_array2969 is array (0 .. 7) of aliased u_libc_fpxreg;
   type u_libc_fpstate_array2970 is array (0 .. 15) of aliased u_libc_xmmreg;
   type u_libc_fpstate_array2925 is array (0 .. 23) of aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;
   type u_libc_fpstate is record
      cwd : aliased x86_64_linux_gnu_bits_types_h.uu_uint16_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:107
      swd : aliased x86_64_linux_gnu_bits_types_h.uu_uint16_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:108
      ftw : aliased x86_64_linux_gnu_bits_types_h.uu_uint16_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:109
      fop : aliased x86_64_linux_gnu_bits_types_h.uu_uint16_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:110
      rip : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:111
      rdp : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:112
      mxcsr : aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:113
      mxcr_mask : aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:114
      u_st : aliased u_libc_fpstate_array2969;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:115
      u_xmm : aliased u_libc_fpstate_array2970;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:116
      padding : aliased u_libc_fpstate_array2925;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:117
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:104

  -- Structure to describe FPU registers.   
   type fpregset_t is access all u_libc_fpstate;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:121

  -- Context to describe whole processor state.   
   --  skipped anonymous struct anon_89

   type mcontext_t_array2975 is array (0 .. 7) of aliased Extensions.unsigned_long_long;
   type mcontext_t is record
      gregs : aliased gregset_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:126
      fpregs : fpregset_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:128
      uu_reserved1 : aliased mcontext_t_array2975;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:129
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:130

  -- Note that fpregs is a pointer.   
  -- Userlevel context.   
   type ucontext;
   type ucontext is record
      uc_flags : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:135
      uc_link : access ucontext;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:136
      uc_stack : aliased x86_64_linux_gnu_bits_sigstack_h.stack_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:137
      uc_mcontext : aliased mcontext_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:138
      uc_sigmask : aliased x86_64_linux_gnu_bits_sigset_h.uu_sigset_t;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:139
      uu_fpregs_mem : aliased u_libc_fpstate;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:140
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:133

   subtype ucontext_t is ucontext;  -- /usr/include/x86_64-linux-gnu/sys/ucontext.h:141

  -- Type for general register.   
  -- Number of general registers.   
  -- Container for all general registers.   
  -- Number of each register is the `gregset_t' array.   
  -- Definitions taken from the kernel headers.   
  -- Structure to describe FPU registers.   
  -- Context to describe whole processor state.   
  -- Due to Linux's history we have to use a pointer here.  The SysV/i386
  --       ABI requires a struct with the values.   

  -- Userlevel context.   
end x86_64_linux_gnu_sys_ucontext_h;
