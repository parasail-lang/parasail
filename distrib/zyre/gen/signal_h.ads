pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_sigset_h;
with x86_64_linux_gnu_bits_types_h;
with Interfaces.C.Strings;
with x86_64_linux_gnu_bits_siginfo_h;
limited with x86_64_linux_gnu_sys_select_h;
limited with x86_64_linux_gnu_bits_sigaction_h;
limited with time_h;
limited with x86_64_linux_gnu_bits_sigcontext_h;
limited with x86_64_linux_gnu_bits_sigstack_h;

package signal_h is

   --  arg-macro: procedure sigmask (sig)
   --    __sigmask(sig)
   --  unsupported macro: NSIG _NSIG
   --  unsupported macro: sv_onstack sv_flags
   SV_ONSTACK : constant := (2 ** 0);  --  /usr/include/signal.h:317
   SV_INTERRUPT : constant := (2 ** 1);  --  /usr/include/signal.h:318
   SV_RESETHAND : constant := (2 ** 2);  --  /usr/include/signal.h:319

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

  -- *	ISO C99 Standard: 7.14 Signal handling <signal.h>
  --  

  -- __sigset_t, __sig_atomic_t.   
  -- An integral type that can be modified atomically, without the
  --   possibility of a signal arriving in the middle of the operation.   

   subtype sig_atomic_t is x86_64_linux_gnu_bits_sigset_h.uu_sig_atomic_t;  -- /usr/include/signal.h:40

  -- We need `struct timespec' later on.   
  -- Get the `siginfo_t' type plus the needed symbols.   
  -- Type of a signal handler.   
   type uu_sighandler_t is access procedure (arg1 : int)
   with Convention => C;  -- /usr/include/signal.h:85

  -- The X/Open definition of `signal' specifies the SVID semantic.  Use
  --   the additional function `sysv_signal' when X/Open compatibility is
  --   requested.   

   --  skipped func __sysv_signal

   function sysv_signal (uu_sig : int; uu_handler : uu_sighandler_t) return uu_sighandler_t  -- /usr/include/signal.h:93
   with Import => True, 
        Convention => C, 
        External_Name => "sysv_signal";

  -- Set the handler for the signal SIG to HANDLER, returning the old
  --   handler, or SIG_ERR on error.
  --   By default `signal' has the BSD semantic.   

   function signal (uu_sig : int; uu_handler : uu_sighandler_t) return uu_sighandler_t  -- /usr/include/signal.h:102
   with Import => True, 
        Convention => C, 
        External_Name => "signal";

  -- Make sure the used `signal' implementation is the SVID version.  
  -- The X/Open definition of `signal' conflicts with the BSD version.
  --   So they defined another function `bsd_signal'.   

   function bsd_signal (uu_sig : int; uu_handler : uu_sighandler_t) return uu_sighandler_t  -- /usr/include/signal.h:119
   with Import => True, 
        Convention => C, 
        External_Name => "bsd_signal";

  -- Send signal SIG to process number PID.  If PID is zero,
  --   send SIG to all processes in the current process's process group.
  --   If PID is < -1, send SIG to all processes in process group - PID.   

   function kill (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t; uu_sig : int) return int  -- /usr/include/signal.h:127
   with Import => True, 
        Convention => C, 
        External_Name => "kill";

  -- Send SIG to all processes in process group PGRP.
  --   If PGRP is zero, send SIG to all processes in
  --   the current process's process group.   

   function killpg (uu_pgrp : x86_64_linux_gnu_bits_types_h.uu_pid_t; uu_sig : int) return int  -- /usr/include/signal.h:134
   with Import => True, 
        Convention => C, 
        External_Name => "killpg";

  -- Raise signal SIG, i.e., send SIG to yourself.   
   function c_raise (uu_sig : int) return int  -- /usr/include/signal.h:139
   with Import => True, 
        Convention => C, 
        External_Name => "raise";

  -- SVID names for the same things.   
   function ssignal (uu_sig : int; uu_handler : uu_sighandler_t) return uu_sighandler_t  -- /usr/include/signal.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "ssignal";

   function gsignal (uu_sig : int) return int  -- /usr/include/signal.h:146
   with Import => True, 
        Convention => C, 
        External_Name => "gsignal";

  -- Print a message describing the meaning of the given signal number.   
   procedure psignal (uu_sig : int; uu_s : Interfaces.C.Strings.chars_ptr)  -- /usr/include/signal.h:151
   with Import => True, 
        Convention => C, 
        External_Name => "psignal";

  -- Print a message describing the meaning of the given signal information.   
   procedure psiginfo (uu_pinfo : access constant x86_64_linux_gnu_bits_siginfo_h.siginfo_t; uu_s : Interfaces.C.Strings.chars_ptr)  -- /usr/include/signal.h:156
   with Import => True, 
        Convention => C, 
        External_Name => "psiginfo";

  -- The `sigpause' function in X/Open defines the argument as the
  --   signal number.  This requires redirecting to another function
  --   because the default version in glibc uses an old BSD interface.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   --  skipped func __sigpause

   function sigpause (uu_sig : int) return int  -- /usr/include/signal.h:171
   with Import => True, 
        Convention => C, 
        External_Name => "__xpg_sigpause";

  -- Remove a signal from the signal mask and suspend the process.   
  -- None of the following functions should be used anymore.  They are here
  --   only for compatibility.  A single word (`int') is not guaranteed to be
  --   enough to hold a complete signal mask and therefore these functions
  --   simply do not work in many situations.  Use `sigprocmask' instead.   

  -- Compute mask for signal SIG.   
  -- Block signals in MASK, returning the old mask.   
   function sigblock (uu_mask : int) return int  -- /usr/include/signal.h:189
   with Import => True, 
        Convention => C, 
        External_Name => "sigblock";

  -- Set the mask of blocked signals to MASK, returning the old mask.   
   function sigsetmask (uu_mask : int) return int  -- /usr/include/signal.h:192
   with Import => True, 
        Convention => C, 
        External_Name => "sigsetmask";

  -- Return currently selected signal mask.   
   function siggetmask return int  -- /usr/include/signal.h:195
   with Import => True, 
        Convention => C, 
        External_Name => "siggetmask";

   subtype sighandler_t is uu_sighandler_t;  -- /usr/include/signal.h:204

  -- 4.4 BSD uses the name `sig_t' for this.   
   subtype sig_t is uu_sighandler_t;  -- /usr/include/signal.h:209

  -- Clear all signals from SET.   
   function sigemptyset (uu_set : access x86_64_linux_gnu_sys_select_h.sigset_t) return int  -- /usr/include/signal.h:215
   with Import => True, 
        Convention => C, 
        External_Name => "sigemptyset";

  -- Set all signals in SET.   
   function sigfillset (uu_set : access x86_64_linux_gnu_sys_select_h.sigset_t) return int  -- /usr/include/signal.h:218
   with Import => True, 
        Convention => C, 
        External_Name => "sigfillset";

  -- Add SIGNO to SET.   
   function sigaddset (uu_set : access x86_64_linux_gnu_sys_select_h.sigset_t; uu_signo : int) return int  -- /usr/include/signal.h:221
   with Import => True, 
        Convention => C, 
        External_Name => "sigaddset";

  -- Remove SIGNO from SET.   
   function sigdelset (uu_set : access x86_64_linux_gnu_sys_select_h.sigset_t; uu_signo : int) return int  -- /usr/include/signal.h:224
   with Import => True, 
        Convention => C, 
        External_Name => "sigdelset";

  -- Return 1 if SIGNO is in SET, 0 if not.   
   function sigismember (uu_set : access constant x86_64_linux_gnu_sys_select_h.sigset_t; uu_signo : int) return int  -- /usr/include/signal.h:227
   with Import => True, 
        Convention => C, 
        External_Name => "sigismember";

  -- Return non-empty value is SET is not empty.   
   function sigisemptyset (uu_set : access constant x86_64_linux_gnu_sys_select_h.sigset_t) return int  -- /usr/include/signal.h:232
   with Import => True, 
        Convention => C, 
        External_Name => "sigisemptyset";

  -- Build new signal set by combining the two inputs set using logical AND.   
   function sigandset
     (uu_set : access x86_64_linux_gnu_sys_select_h.sigset_t;
      uu_left : access constant x86_64_linux_gnu_sys_select_h.sigset_t;
      uu_right : access constant x86_64_linux_gnu_sys_select_h.sigset_t) return int  -- /usr/include/signal.h:235
   with Import => True, 
        Convention => C, 
        External_Name => "sigandset";

  -- Build new signal set by combining the two inputs set using logical OR.   
   function sigorset
     (uu_set : access x86_64_linux_gnu_sys_select_h.sigset_t;
      uu_left : access constant x86_64_linux_gnu_sys_select_h.sigset_t;
      uu_right : access constant x86_64_linux_gnu_sys_select_h.sigset_t) return int  -- /usr/include/signal.h:239
   with Import => True, 
        Convention => C, 
        External_Name => "sigorset";

  -- Get the system-specific definitions of `struct sigaction'
  --   and the `SA_*' and `SIG_*'. constants.   

  -- Get and/or change the set of blocked signals.   
   function sigprocmask
     (uu_how : int;
      uu_set : access constant x86_64_linux_gnu_sys_select_h.sigset_t;
      uu_oset : access x86_64_linux_gnu_sys_select_h.sigset_t) return int  -- /usr/include/signal.h:248
   with Import => True, 
        Convention => C, 
        External_Name => "sigprocmask";

  -- Change the set of blocked signals to SET,
  --   wait until a signal arrives, and restore the set of blocked signals.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function sigsuspend (uu_set : access constant x86_64_linux_gnu_sys_select_h.sigset_t) return int  -- /usr/include/signal.h:256
   with Import => True, 
        Convention => C, 
        External_Name => "sigsuspend";

  -- Get and/or set the action for signal SIG.   
   function sigaction
     (uu_sig : int;
      uu_act : access constant x86_64_linux_gnu_bits_sigaction_h.sigaction;
      uu_oact : access x86_64_linux_gnu_bits_sigaction_h.sigaction) return int  -- /usr/include/signal.h:259
   with Import => True, 
        Convention => C, 
        External_Name => "sigaction";

  -- Put in SET all signals that are blocked and waiting to be delivered.   
   function sigpending (uu_set : access x86_64_linux_gnu_sys_select_h.sigset_t) return int  -- /usr/include/signal.h:263
   with Import => True, 
        Convention => C, 
        External_Name => "sigpending";

  -- Select any of pending signals from SET or wait for any to arrive.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function sigwait (uu_set : access constant x86_64_linux_gnu_sys_select_h.sigset_t; uu_sig : access int) return int  -- /usr/include/signal.h:270
   with Import => True, 
        Convention => C, 
        External_Name => "sigwait";

  -- Select any of pending signals from SET and place information in INFO.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function sigwaitinfo (uu_set : access constant x86_64_linux_gnu_sys_select_h.sigset_t; uu_info : access x86_64_linux_gnu_bits_siginfo_h.siginfo_t) return int  -- /usr/include/signal.h:278
   with Import => True, 
        Convention => C, 
        External_Name => "sigwaitinfo";

  -- Select any of pending signals from SET and place information in INFO.
  --   Wait the time specified by TIMEOUT if no signal is pending.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function sigtimedwait
     (uu_set : access constant x86_64_linux_gnu_sys_select_h.sigset_t;
      uu_info : access x86_64_linux_gnu_bits_siginfo_h.siginfo_t;
      uu_timeout : access constant time_h.timespec) return int  -- /usr/include/signal.h:286
   with Import => True, 
        Convention => C, 
        External_Name => "sigtimedwait";

  -- Send signal SIG to the process PID.  Associate data in VAL with the
  --   signal.   

   function sigqueue
     (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t;
      uu_sig : int;
      uu_val : x86_64_linux_gnu_bits_siginfo_h.sigval) return int  -- /usr/include/signal.h:293
   with Import => True, 
        Convention => C, 
        External_Name => "sigqueue";

  -- Names of the signals.  This variable exists only for compatibility.
  --   Use `strsignal' instead (see <string.h>).   

   sys_siglist : array (0 .. 64) of Interfaces.C.Strings.chars_ptr  -- /usr/include/signal.h:304
   with Import => True, 
        Convention => C, 
        External_Name => "sys_siglist";

  -- Structure passed to `sigvec'.   
  -- Signal handler.   
   type sigvec is record
      sv_handler : uu_sighandler_t;  -- /usr/include/signal.h:309
      sv_mask : aliased int;  -- /usr/include/signal.h:310
      sv_flags : aliased int;  -- /usr/include/signal.h:312
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/signal.h:307

  -- Mask of signals to be blocked.   
  -- Flags (see below).   
  -- Bits in `sv_flags'.   
  -- If VEC is non-NULL, set the handler for SIG to the `sv_handler' member
  --   of VEC.  The signals in `sv_mask' will be blocked while the handler runs.
  --   If the SV_RESETHAND bit is set in `sv_flags', the handler for SIG will be
  --   reset to SIG_DFL before `sv_handler' is entered.  If OVEC is non-NULL,
  --   it is filled in with the old information for SIG.   

   function sigvec
     (uu_sig : int;
      uu_vec : access constant sigvec;
      uu_ovec : access sigvec) return int  -- /usr/include/signal.h:327
   with Import => True, 
        Convention => C, 
        External_Name => "sigvec";

  -- Get machine-dependent `struct sigcontext' and signal subcodes.   
  -- Restore the state saved in SCP.   
   function sigreturn (uu_scp : access x86_64_linux_gnu_bits_sigcontext_h.sigcontext) return int  -- /usr/include/signal.h:335
   with Import => True, 
        Convention => C, 
        External_Name => "sigreturn";

  -- If INTERRUPT is nonzero, make signal SIG interrupt system calls
  --   (causing them to fail with EINTR); if INTERRUPT is zero, make system
  --   calls be restarted after signal SIG.   

   function siginterrupt (uu_sig : int; uu_interrupt : int) return int  -- /usr/include/signal.h:347
   with Import => True, 
        Convention => C, 
        External_Name => "siginterrupt";

  -- This will define `ucontext_t' and `mcontext_t'.   
  -- Run signals handlers on the stack specified by SS (if not NULL).
  --   If OSS is not NULL, it is filled in with the old signal stack status.
  --   This interface is obsolete and on many platform not implemented.   

   function sigstack (uu_ss : access x86_64_linux_gnu_bits_sigstack_h.sigstack; uu_oss : access x86_64_linux_gnu_bits_sigstack_h.sigstack) return int  -- /usr/include/signal.h:358
   with Import => True, 
        Convention => C, 
        External_Name => "sigstack";

  -- Alternate signal handler stack interface.
  --   This interface should always be preferred over `sigstack'.   

   function sigaltstack (uu_ss : access constant x86_64_linux_gnu_bits_sigstack_h.sigaltstack; uu_oss : access x86_64_linux_gnu_bits_sigstack_h.sigaltstack) return int  -- /usr/include/signal.h:363
   with Import => True, 
        Convention => C, 
        External_Name => "sigaltstack";

  -- Simplified interface for signal management.   
  -- Add SIG to the calling process' signal mask.   
   function sighold (uu_sig : int) return int  -- /usr/include/signal.h:372
   with Import => True, 
        Convention => C, 
        External_Name => "sighold";

  -- Remove SIG from the calling process' signal mask.   
   function sigrelse (uu_sig : int) return int  -- /usr/include/signal.h:375
   with Import => True, 
        Convention => C, 
        External_Name => "sigrelse";

  -- Set the disposition of SIG to SIG_IGN.   
   function sigignore (uu_sig : int) return int  -- /usr/include/signal.h:378
   with Import => True, 
        Convention => C, 
        External_Name => "sigignore";

  -- Set the disposition of SIG.   
   function sigset (uu_sig : int; uu_disp : uu_sighandler_t) return uu_sighandler_t  -- /usr/include/signal.h:381
   with Import => True, 
        Convention => C, 
        External_Name => "sigset";

  -- Some of the functions for handling signals in threaded programs must
  --   be defined here.   

  -- The following functions are used internally in the C library and in
  --   other code which need deep insights.   

  -- Return number of available real-time signal with highest priority.   
   --  skipped func __libc_current_sigrtmin

  -- Return number of available real-time signal with lowest priority.   
   --  skipped func __libc_current_sigrtmax

end signal_h;
