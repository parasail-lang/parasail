pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;

package x86_64_linux_gnu_bits_sched_h is

   SCHED_OTHER : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:28
   SCHED_FIFO : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:29
   SCHED_RR : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:30

   SCHED_BATCH : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:32
   SCHED_IDLE : constant := 5;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:33

   SCHED_RESET_ON_FORK : constant := 16#40000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:35

   CSIGNAL : constant := 16#000000ff#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:40
   CLONE_VM : constant := 16#00000100#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:41
   CLONE_FS : constant := 16#00000200#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:42
   CLONE_FILES : constant := 16#00000400#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:43
   CLONE_SIGHAND : constant := 16#00000800#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:44
   CLONE_PTRACE : constant := 16#00002000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:45
   CLONE_VFORK : constant := 16#00004000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:46

   CLONE_PARENT : constant := 16#00008000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:48

   CLONE_THREAD : constant := 16#00010000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:50
   CLONE_NEWNS : constant := 16#00020000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:51
   CLONE_SYSVSEM : constant := 16#00040000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:52
   CLONE_SETTLS : constant := 16#00080000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:53
   CLONE_PARENT_SETTID : constant := 16#00100000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:54

   CLONE_CHILD_CLEARTID : constant := 16#00200000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:56

   CLONE_DETACHED : constant := 16#00400000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:58
   CLONE_UNTRACED : constant := 16#00800000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:59

   CLONE_CHILD_SETTID : constant := 16#01000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:61

   CLONE_NEWUTS : constant := 16#04000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:63
   CLONE_NEWIPC : constant := 16#08000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:64
   CLONE_NEWUSER : constant := 16#10000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:65
   CLONE_NEWPID : constant := 16#20000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:66
   CLONE_NEWNET : constant := 16#40000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:67
   CLONE_IO : constant := 16#80000000#;  --  /usr/include/x86_64-linux-gnu/bits/sched.h:68

  -- Definitions of constants and data structure for POSIX 1003.1b-1993
  --   scheduling interface.
  --   Copyright (C) 1996-2014 Free Software Foundation, Inc.
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

  -- Scheduling algorithms.   
  -- Cloning flags.   
  -- The official definition.   
   type sched_param is record
      uu_sched_priority : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:74
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:72

  -- Clone current process.   
   function clone
     (uu_fn : access function (arg1 : System.Address) return int;
      uu_child_stack : System.Address;
      uu_flags : int;
      uu_arg : System.Address  -- , ...
      ) return int  -- /usr/include/x86_64-linux-gnu/bits/sched.h:81
   with Import => True, 
        Convention => C, 
        External_Name => "clone";

  -- Unshare the specified resources.   
   function unshare (uu_flags : int) return int  -- /usr/include/x86_64-linux-gnu/bits/sched.h:85
   with Import => True, 
        Convention => C, 
        External_Name => "unshare";

  -- Get index of currently used CPU.   
   function sched_getcpu return int  -- /usr/include/x86_64-linux-gnu/bits/sched.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "sched_getcpu";

  -- Switch process to namespace of type NSTYPE indicated by FD.   
   function setns (uu_fd : int; uu_nstype : int) return int  -- /usr/include/x86_64-linux-gnu/bits/sched.h:91
   with Import => True, 
        Convention => C, 
        External_Name => "setns";

  -- Data structure to describe a process' schedulability.   
   type uu_sched_param is record
      uu_sched_priority : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:105
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:103

  -- Size definition for CPU sets.   
  -- Type for array elements in 'cpu_set_t'.   
   subtype uu_cpu_mask is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:118

  -- Basic access functions.   
  -- Data structure to describe CPU mask.   
   --  skipped anonymous struct anon_100

   type cpu_set_t_array3623 is array (0 .. 15) of aliased uu_cpu_mask;
   type cpu_set_t is record
      uu_bits : aliased cpu_set_t_array3623;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:127
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/sched.h:128

  -- Access functions for CPU masks.   
   --  skipped func __sched_cpucount

   --  skipped func __sched_cpualloc

   --  skipped func __sched_cpufree

end x86_64_linux_gnu_bits_sched_h;
