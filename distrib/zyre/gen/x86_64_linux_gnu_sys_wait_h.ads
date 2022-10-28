pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;
with x86_64_linux_gnu_bits_types_h;
with x86_64_linux_gnu_bits_waitflags_h;
limited with x86_64_linux_gnu_bits_siginfo_h;

package x86_64_linux_gnu_sys_wait_h is

   --  unsupported macro: WCOREFLAG __WCOREFLAG
   --  arg-macro: procedure WCOREDUMP (status)
   --    __WCOREDUMP (__WAIT_INT (status))
   --  arg-macro: procedure W_EXITCODE (ret, sig)
   --    __W_EXITCODE (ret, sig)
   --  arg-macro: procedure W_STOPCODE (sig)
   --    __W_STOPCODE (sig)
   WAIT_ANY : constant := (-1);  --  /usr/include/x86_64-linux-gnu/sys/wait.h:106
   WAIT_MYPGRP : constant := 0;  --  /usr/include/x86_64-linux-gnu/sys/wait.h:107

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

  -- *	POSIX Standard: 3.2.1 Wait for Process Termination	<sys/wait.h>
  --  

  -- These macros could also be defined in <stdlib.h>.   
  -- This will define the `W*' macros for the flag
  --   bits to `waitpid', `wait3', and `wait4'.   

  -- Lots of hair to allow traditional BSD use of `union wait'
  --   as well as POSIX.1 use of `int' for the status word.   

  -- This is the type of the argument to `wait'.  The funky union
  --   causes redeclarations with either `int *' or `union wait *' to be
  --   allowed without complaint.  __WAIT_STATUS_DEFN is the type used in
  --   the actual function definitions.   

  -- This works in GCC 2.6.1 and later.   
  -- This will define all the `__W*' macros.   
  -- Wait for a child to die.  When one does, put its status in *STAT_LOC
  --   and return its process ID.  For errors, return (pid_t) -1.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function wait (uu_stat_loc : System.Address) return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/x86_64-linux-gnu/sys/wait.h:102
   with Import => True, 
        Convention => C, 
        External_Name => "wait";

  -- Special values for the PID argument to `waitpid' and `wait4'.   
  -- Wait for a child matching PID to die.
  --   If PID is greater than 0, match any process whose process ID is PID.
  --   If PID is (pid_t) -1, match any process.
  --   If PID is (pid_t) 0, match any process with the
  --   same process group as the current process.
  --   If PID is less than -1, match any process whose
  --   process group is the absolute value of PID.
  --   If the WNOHANG bit is set in OPTIONS, and that child
  --   is not already dead, return (pid_t) 0.  If successful,
  --   return PID and store the dead child's status in STAT_LOC.
  --   Return (pid_t) -1 for errors.  If the WUNTRACED bit is
  --   set in OPTIONS, return status for stopped children; otherwise don't.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function waitpid
     (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t;
      uu_stat_loc : access int;
      uu_options : int) return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/x86_64-linux-gnu/sys/wait.h:125
   with Import => True, 
        Convention => C, 
        External_Name => "waitpid";

  -- Wait for a childing matching IDTYPE and ID to change the status and
  --   place appropriate information in *INFOP.
  --   If IDTYPE is P_PID, match any process whose process ID is ID.
  --   If IDTYPE is P_PGID, match any process whose process group is ID.
  --   If IDTYPE is P_ALL, match any process.
  --   If the WNOHANG bit is set in OPTIONS, and that child
  --   is not already dead, clear *INFOP and return 0.  If successful, store
  --   exit code and status in *INFOP.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function waitid
     (uu_idtype : x86_64_linux_gnu_bits_waitflags_h.idtype_t;
      uu_id : x86_64_linux_gnu_bits_types_h.uu_id_t;
      uu_infop : access x86_64_linux_gnu_bits_siginfo_h.siginfo_t;
      uu_options : int) return int  -- /usr/include/x86_64-linux-gnu/sys/wait.h:148
   with Import => True, 
        Convention => C, 
        External_Name => "waitid";

  -- This being here makes the prototypes valid whether or not
  --   we have already included <sys/resource.h> to define `struct rusage'.   

   type rusage is null record;   -- incomplete struct

  -- Wait for a child to exit.  When one does, put its status in *STAT_LOC and
  --   return its process ID.  For errors return (pid_t) -1.  If USAGE is not
  --   nil, store information about the child's resource usage there.  If the
  --   WUNTRACED bit is set in OPTIONS, return status for stopped children;
  --   otherwise don't.   

   function wait3
     (uu_stat_loc : System.Address;
      uu_options : int;
      uu_usage : access rusage) return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/x86_64-linux-gnu/sys/wait.h:162
   with Import => True, 
        Convention => C, 
        External_Name => "wait3";

  -- PID is like waitpid.  Other args are like wait3.   
   function wait4
     (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t;
      uu_stat_loc : System.Address;
      uu_options : int;
      uu_usage : access rusage) return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/x86_64-linux-gnu/sys/wait.h:168
   with Import => True, 
        Convention => C, 
        External_Name => "wait4";

end x86_64_linux_gnu_sys_wait_h;
