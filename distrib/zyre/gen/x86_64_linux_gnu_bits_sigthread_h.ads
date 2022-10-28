pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
limited with x86_64_linux_gnu_bits_sigset_h;
with x86_64_linux_gnu_bits_pthreadtypes_h;
with x86_64_linux_gnu_bits_siginfo_h;

package x86_64_linux_gnu_bits_sigthread_h is

  -- Signal handling function for threaded programs.
  --   Copyright (C) 1998-2014 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public License as
  --   published by the Free Software Foundation; either version 2.1 of the
  --   License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; see the file COPYING.LIB.  If
  --   not, see <http://www.gnu.org/licenses/>.   

  -- Functions for handling signals.  
  -- Modify the signal mask for the calling thread.  The arguments have
  --   the same meaning as for sigprocmask(2).  

   function pthread_sigmask
     (uu_how : int;
      uu_newmask : access constant x86_64_linux_gnu_bits_sigset_h.uu_sigset_t;
      uu_oldmask : access x86_64_linux_gnu_bits_sigset_h.uu_sigset_t) return int  -- /usr/include/x86_64-linux-gnu/bits/sigthread.h:30
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_sigmask";

  -- Send signal SIGNO to the given thread.  
   function pthread_kill (uu_threadid : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t; uu_signo : int) return int  -- /usr/include/x86_64-linux-gnu/bits/sigthread.h:35
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_kill";

  -- Queue signal and data to a thread.   
   function pthread_sigqueue
     (uu_threadid : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t;
      uu_signo : int;
      uu_value : x86_64_linux_gnu_bits_siginfo_h.sigval) return int  -- /usr/include/x86_64-linux-gnu/bits/sigthread.h:39
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_sigqueue";

end x86_64_linux_gnu_bits_sigthread_h;
