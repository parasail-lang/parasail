pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;

package x86_64_linux_gnu_bits_sigcontext_h is

   FP_XSTATE_MAGIC1 : constant := 16#46505853#;  --  /usr/include/x86_64-linux-gnu/bits/sigcontext.h:25
   FP_XSTATE_MAGIC2 : constant := 16#46505845#;  --  /usr/include/x86_64-linux-gnu/bits/sigcontext.h:26
   --  unsupported macro: FP_XSTATE_MAGIC2_SIZE sizeof(FP_XSTATE_MAGIC2)

  -- Copyright (C) 2002-2014 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public
  --   License as published by the Free Software Foundation; either
  --   version 2.1 of the License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; if not, see
  --   <http://www.gnu.org/licenses/>.   

   type u_fpx_sw_bytes_array2909 is array (0 .. 6) of aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;
   type u_fpx_sw_bytes is record
      magic1 : aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:31
      extended_size : aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:32
      xstate_bv : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:33
      xstate_size : aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:34
      padding : aliased u_fpx_sw_bytes_array2909;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:35
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:29

   type u_fpreg_array2912 is array (0 .. 3) of aliased unsigned_short;
   type u_fpreg is record
      significand : aliased u_fpreg_array2912;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:40
      exponent : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:41
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:38

   type u_fpxreg_array2912 is array (0 .. 3) of aliased unsigned_short;
   type u_fpxreg_array1536 is array (0 .. 2) of aliased unsigned_short;
   type u_fpxreg is record
      significand : aliased u_fpxreg_array2912;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:46
      exponent : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:47
      padding : aliased u_fpxreg_array1536;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:48
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:44

   type u_xmmreg_array2918 is array (0 .. 3) of aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;
   type u_xmmreg is record
      element : aliased u_xmmreg_array2918;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:53
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:51

  -- Regular FPU environment.   
  -- FXSR FPU environment.   
  -- Kernel headers before 2.1.1 define a struct sigcontext_struct, but
  --   we need sigcontext.  Some packages have come to rely on
  --   sigcontext_struct being defined on 32-bit x86, so define this for
  --   their benefit.   

  -- FPU environment matching the 64-bit FXSAVE layout.   
   type u_fpstate_array2921 is array (0 .. 7) of aliased u_fpxreg;
   type u_fpstate_array2922 is array (0 .. 15) of aliased u_xmmreg;
   type u_fpstate_array2925 is array (0 .. 23) of aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;
   type u_fpstate is record
      cwd : aliased x86_64_linux_gnu_bits_types_h.uu_uint16_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:124
      swd : aliased x86_64_linux_gnu_bits_types_h.uu_uint16_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:125
      ftw : aliased x86_64_linux_gnu_bits_types_h.uu_uint16_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:126
      fop : aliased x86_64_linux_gnu_bits_types_h.uu_uint16_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:127
      rip : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:128
      rdp : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:129
      mxcsr : aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:130
      mxcr_mask : aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:131
      u_st : aliased u_fpstate_array2921;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:132
      u_xmm : aliased u_fpstate_array2922;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:133
      padding : aliased u_fpstate_array2925;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:134
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:121

   type anon_86 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            fpstate : access u_fpstate;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:167
         when others =>
            uu_fpstate_word : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:168
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type sigcontext_array2932 is array (0 .. 7) of aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;
   type sigcontext is record
      r8 : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:139
      r9 : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:140
      r10 : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:141
      r11 : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:142
      r12 : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:143
      r13 : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:144
      r14 : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:145
      r15 : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:146
      rdi : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:147
      rsi : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:148
      rbp : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:149
      rbx : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:150
      rdx : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:151
      rax : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:152
      rcx : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:153
      rsp : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:154
      rip : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:155
      eflags : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:156
      cs : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:157
      gs : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:158
      fs : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:159
      uu_pad0 : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:160
      err : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:161
      trapno : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:162
      oldmask : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:163
      cr2 : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:164
      field_27 : aliased anon_86;
      uu_reserved1 : aliased sigcontext_array2932;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:170
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:137

   type u_xsave_hdr_array2936 is array (0 .. 1) of aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;
   type u_xsave_hdr_array2939 is array (0 .. 4) of aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;
   type u_xsave_hdr is record
      xstate_bv : aliased x86_64_linux_gnu_bits_types_h.uu_uint64_t;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:177
      reserved1 : aliased u_xsave_hdr_array2936;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:178
      reserved2 : aliased u_xsave_hdr_array2939;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:179
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:175

   type u_ymmh_state_array2943 is array (0 .. 63) of aliased x86_64_linux_gnu_bits_types_h.uu_uint32_t;
   type u_ymmh_state is record
      ymmh_space : aliased u_ymmh_state_array2943;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:184
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:182

   type u_xstate is record
      fpstate : aliased u_fpstate;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:189
      xstate_hdr : aliased u_xsave_hdr;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:190
      ymmh : aliased u_ymmh_state;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:191
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:187

end x86_64_linux_gnu_bits_sigcontext_h;
