pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;
with x86_64_linux_gnu_bits_types_h;
limited with x86_64_linux_gnu_bits_pthreadtypes_h;

package x86_64_linux_gnu_bits_siginfo_h is

   --  unsupported macro: si_pid _sifields._kill.si_pid
   --  unsupported macro: si_uid _sifields._kill.si_uid
   --  unsupported macro: si_timerid _sifields._timer.si_tid
   --  unsupported macro: si_overrun _sifields._timer.si_overrun
   --  unsupported macro: si_status _sifields._sigchld.si_status
   --  unsupported macro: si_utime _sifields._sigchld.si_utime
   --  unsupported macro: si_stime _sifields._sigchld.si_stime
   --  unsupported macro: si_value _sifields._rt.si_sigval
   --  unsupported macro: si_int _sifields._rt.si_sigval.sival_int
   --  unsupported macro: si_ptr _sifields._rt.si_sigval.sival_ptr
   --  unsupported macro: si_addr _sifields._sigfault.si_addr
   --  unsupported macro: si_addr_lsb _sifields._sigfault.si_addr_lsb
   --  unsupported macro: si_band _sifields._sigpoll.si_band
   --  unsupported macro: si_fd _sifields._sigpoll.si_fd
   --  unsupported macro: si_call_addr _sifields._sigsys._call_addr
   --  unsupported macro: si_syscall _sifields._sigsys._syscall
   --  unsupported macro: si_arch _sifields._sigsys._arch
   --  unsupported macro: SI_ASYNCNL SI_ASYNCNL
   --  unsupported macro: SI_TKILL SI_TKILL
   --  unsupported macro: SI_SIGIO SI_SIGIO
   --  unsupported macro: SI_ASYNCIO SI_ASYNCIO
   --  unsupported macro: SI_MESGQ SI_MESGQ
   --  unsupported macro: SI_TIMER SI_TIMER
   --  unsupported macro: SI_QUEUE SI_QUEUE
   --  unsupported macro: SI_USER SI_USER
   --  unsupported macro: SI_KERNEL SI_KERNEL
   --  unsupported macro: ILL_ILLOPC ILL_ILLOPC
   --  unsupported macro: ILL_ILLOPN ILL_ILLOPN
   --  unsupported macro: ILL_ILLADR ILL_ILLADR
   --  unsupported macro: ILL_ILLTRP ILL_ILLTRP
   --  unsupported macro: ILL_PRVOPC ILL_PRVOPC
   --  unsupported macro: ILL_PRVREG ILL_PRVREG
   --  unsupported macro: ILL_COPROC ILL_COPROC
   --  unsupported macro: ILL_BADSTK ILL_BADSTK
   --  unsupported macro: FPE_INTDIV FPE_INTDIV
   --  unsupported macro: FPE_INTOVF FPE_INTOVF
   --  unsupported macro: FPE_FLTDIV FPE_FLTDIV
   --  unsupported macro: FPE_FLTOVF FPE_FLTOVF
   --  unsupported macro: FPE_FLTUND FPE_FLTUND
   --  unsupported macro: FPE_FLTRES FPE_FLTRES
   --  unsupported macro: FPE_FLTINV FPE_FLTINV
   --  unsupported macro: FPE_FLTSUB FPE_FLTSUB
   --  unsupported macro: SEGV_MAPERR SEGV_MAPERR
   --  unsupported macro: SEGV_ACCERR SEGV_ACCERR
   --  unsupported macro: BUS_ADRALN BUS_ADRALN
   --  unsupported macro: BUS_ADRERR BUS_ADRERR
   --  unsupported macro: BUS_OBJERR BUS_OBJERR
   --  unsupported macro: BUS_MCEERR_AR BUS_MCEERR_AR
   --  unsupported macro: BUS_MCEERR_AO BUS_MCEERR_AO
   --  unsupported macro: TRAP_BRKPT TRAP_BRKPT
   --  unsupported macro: TRAP_TRACE TRAP_TRACE
   --  unsupported macro: CLD_EXITED CLD_EXITED
   --  unsupported macro: CLD_KILLED CLD_KILLED
   --  unsupported macro: CLD_DUMPED CLD_DUMPED
   --  unsupported macro: CLD_TRAPPED CLD_TRAPPED
   --  unsupported macro: CLD_STOPPED CLD_STOPPED
   --  unsupported macro: CLD_CONTINUED CLD_CONTINUED
   --  unsupported macro: POLL_IN POLL_IN
   --  unsupported macro: POLL_OUT POLL_OUT
   --  unsupported macro: POLL_MSG POLL_MSG
   --  unsupported macro: POLL_ERR POLL_ERR
   --  unsupported macro: POLL_PRI POLL_PRI
   --  unsupported macro: POLL_HUP POLL_HUP
   --  unsupported macro: sigev_notify_function _sigev_un._sigev_thread._function
   --  unsupported macro: sigev_notify_attributes _sigev_un._sigev_thread._attribute
   --  unsupported macro: SIGEV_SIGNAL SIGEV_SIGNAL
   --  unsupported macro: SIGEV_NONE SIGEV_NONE
   --  unsupported macro: SIGEV_THREAD SIGEV_THREAD
   --  unsupported macro: SIGEV_THREAD_ID SIGEV_THREAD_ID
  -- siginfo_t, sigevent and constants.  Linux x86-64 version.
  --   Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

  -- Type for data associated with a signal.   
   type sigval (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            sival_int : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:34
         when others =>
            sival_ptr : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:35
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:32

   subtype sigval_t is sigval;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:36

  -- si_utime and si_stime must be 4 byte aligned for x32 to match the
  --   kernel.  We align siginfo_t to 8 bytes so that si_utime and si_stime
  --   are actually aligned to 8 bytes since their offsets are multiple of
  --   8 bytes.   

   subtype uu_sigchld_clock_t is x86_64_linux_gnu_bits_types_h.uu_clock_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:58

  -- Signal number.   
   --  skipped anonymous struct anon_65

   type anon_67 is record
      si_pid : aliased x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:76
      si_uid : aliased x86_64_linux_gnu_bits_types_h.uu_uid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:77
   end record
   with Convention => C_Pass_By_Copy;
   type anon_68 is record
      si_tid : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:83
      si_overrun : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:84
      si_sigval : aliased sigval_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:85
   end record
   with Convention => C_Pass_By_Copy;
   type anon_69 is record
      si_pid : aliased x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:91
      si_uid : aliased x86_64_linux_gnu_bits_types_h.uu_uid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:92
      si_sigval : aliased sigval_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:93
   end record
   with Convention => C_Pass_By_Copy;
   type anon_70 is record
      si_pid : aliased x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:99
      si_uid : aliased x86_64_linux_gnu_bits_types_h.uu_uid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:100
      si_status : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:101
      si_utime : aliased uu_sigchld_clock_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:102
      si_stime : aliased uu_sigchld_clock_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:103
   end record
   with Convention => C_Pass_By_Copy;
   type anon_71 is record
      si_addr : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:109
      si_addr_lsb : aliased short;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:110
   end record
   with Convention => C_Pass_By_Copy;
   type anon_72 is record
      si_band : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:116
      si_fd : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:117
   end record
   with Convention => C_Pass_By_Copy;
   type anon_73 is record
      u_call_addr : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:123
      u_syscall : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:124
      u_arch : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:125
   end record
   with Convention => C_Pass_By_Copy;
   type siginfo_t_array2772 is array (0 .. 27) of aliased int;
   type anon_66 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            u_pad : aliased siginfo_t_array2772;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:71
         when 1 =>
            u_kill : aliased anon_67;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:78
         when 2 =>
            u_timer : aliased anon_68;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:86
         when 3 =>
            u_rt : aliased anon_69;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:94
         when 4 =>
            u_sigchld : aliased anon_70;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:104
         when 5 =>
            u_sigfault : aliased anon_71;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:111
         when 6 =>
            u_sigpoll : aliased anon_72;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:118
         when others =>
            u_sigsys : aliased anon_73;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:126
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type siginfo_t is record
      si_signo : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:64
      si_errno : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:65
      si_code : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:67
      u_sifields : aliased anon_66;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:127
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:128

  -- If non-zero, an errno value associated with
  --				   this signal, as defined in <errno.h>.   

  -- Signal code.   
  -- kill().   
  -- Sending process ID.   
  -- Real user ID of sending process.   
  -- POSIX.1b timers.   
  -- Timer ID.   
  -- Overrun count.   
  -- Signal value.   
  -- POSIX.1b signals.   
  -- Sending process ID.   
  -- Real user ID of sending process.   
  -- Signal value.   
  -- SIGCHLD.   
  -- Which child.   
  -- Real user ID of sending process.   
  -- Exit value or signal.   
  -- SIGILL, SIGFPE, SIGSEGV, SIGBUS.   
  -- Faulting insn/memory ref.   
  -- Valid LSB of the reported address.   
  -- SIGPOLL.   
  -- Band event for SIGPOLL.   
  -- SIGSYS.   
  -- Calling user insn.   
  -- Triggering system call number.   
  -- AUDIT_ARCH_* of syscall.   
  -- X/Open requires some more fields with fixed names.   
  -- Values for `si_code'.  Positive values are reserved for kernel-generated
  --   signals.   

  -- Sent by asynch name lookup completion.   
  -- Sent by tkill.   
  -- Sent by queued SIGIO.  
  -- Sent by AIO completion.   
  -- Sent by real time mesq state change.   
  -- Sent by timer expiration.   
  -- Sent by sigqueue.   
  -- Sent by kill, sigsend.   
  -- Send by kernel.   
  -- `si_code' values for SIGILL signal.   
  -- Illegal opcode.   
  -- Illegal operand.   
  -- Illegal addressing mode.   
  -- Illegal trap.  
  -- Privileged opcode.   
  -- Privileged register.   
  -- Coprocessor error.   
  -- Internal stack error.   
  -- `si_code' values for SIGFPE signal.   
  -- Integer divide by zero.   
  -- Integer overflow.   
  -- Floating point divide by zero.   
  -- Floating point overflow.   
  -- Floating point underflow.   
  -- Floating point inexact result.   
  -- Floating point invalid operation.   
  -- Subscript out of range.   
  -- `si_code' values for SIGSEGV signal.   
  -- Address not mapped to object.   
  -- Invalid permissions for mapped object.   
  -- `si_code' values for SIGBUS signal.   
  -- Invalid address alignment.   
  -- Non-existant physical address.   
  -- Object specific hardware error.   
  -- Hardware memory error: action required.   
  -- Hardware memory error: action optional.   
  -- `si_code' values for SIGTRAP signal.   
  -- Process breakpoint.   
  -- Process trace trap.   
  -- `si_code' values for SIGCHLD signal.   
  -- Child has exited.   
  -- Child was killed.   
  -- Child terminated abnormally.   
  -- Traced child has trapped.   
  -- Child has stopped.   
  -- Stopped child has continued.   
  -- `si_code' values for SIGPOLL signal.   
  -- Data input available.   
  -- Output buffers available.   
  -- Input message available.    
  -- I/O error.   
  -- High priority input available.   
  -- Device disconnected.   
  -- Structure to transport application-defined values with signals.   
  -- Forward declaration.   
   type anon_83 is record
      u_function : access procedure (arg1 : sigval_t);  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:323
      u_attribute : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:324
   end record
   with Convention => C_Pass_By_Copy;
   type sigevent_array2807 is array (0 .. 11) of aliased int;
   type anon_82 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            u_pad : aliased sigevent_array2807;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:315
         when 1 =>
            u_tid : aliased x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:319
         when others =>
            u_sigev_thread : aliased anon_83;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:325
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type sigevent is record
      sigev_value : aliased sigval_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:309
      sigev_signo : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:310
      sigev_notify : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:311
      u_sigev_un : aliased anon_82;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:326
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:307

  -- When SIGEV_SIGNAL and SIGEV_THREAD_ID set, LWP ID of the
  --	   thread to receive the signal.   

  -- Function to start.   
  -- Thread attributes.   
   subtype sigevent_t is sigevent;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:327

  -- POSIX names to access some of the members.   
  -- `sigev_notify' values.   
  -- Notify via signal.   
  -- Other notification: meaningless.   
  -- Deliver via thread creation.   
  -- Send signal to specific thread.   
end x86_64_linux_gnu_bits_siginfo_h;
