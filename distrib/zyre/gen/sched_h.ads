pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
limited with x86_64_linux_gnu_bits_sched_h;
limited with time_h;
with stddef_h;

package sched_h is

   --  unsupported macro: sched_priority __sched_priority
   --  unsupported macro: CPU_SETSIZE __CPU_SETSIZE
   --  arg-macro: procedure CPU_SET (cpu, cpusetp)
   --    __CPU_SET_S (cpu, sizeof (cpu_set_t), cpusetp)
   --  arg-macro: procedure CPU_CLR (cpu, cpusetp)
   --    __CPU_CLR_S (cpu, sizeof (cpu_set_t), cpusetp)
   --  arg-macro: procedure CPU_ISSET (cpu, cpusetp)
   --    __CPU_ISSET_S (cpu, sizeof (cpu_set_t), cpusetp)
   --  arg-macro: procedure CPU_ZERO (cpusetp)
   --    __CPU_ZERO_S (sizeof (cpu_set_t), cpusetp)
   --  arg-macro: procedure CPU_COUNT (cpusetp)
   --    __CPU_COUNT_S (sizeof (cpu_set_t), cpusetp)
   --  arg-macro: procedure CPU_SET_S (cpu, setsize, cpusetp)
   --    __CPU_SET_S (cpu, setsize, cpusetp)
   --  arg-macro: procedure CPU_CLR_S (cpu, setsize, cpusetp)
   --    __CPU_CLR_S (cpu, setsize, cpusetp)
   --  arg-macro: procedure CPU_ISSET_S (cpu, setsize, cpusetp)
   --    __CPU_ISSET_S (cpu, setsize, cpusetp)
   --  arg-macro: procedure CPU_ZERO_S (setsize, cpusetp)
   --    __CPU_ZERO_S (setsize, cpusetp)
   --  arg-macro: procedure CPU_COUNT_S (setsize, cpusetp)
   --    __CPU_COUNT_S (setsize, cpusetp)
   --  arg-macro: procedure CPU_EQUAL (cpusetp1, cpusetp2)
   --    __CPU_EQUAL_S (sizeof (cpu_set_t), cpusetp1, cpusetp2)
   --  arg-macro: procedure CPU_EQUAL_S (setsize, cpusetp1, cpusetp2)
   --    __CPU_EQUAL_S (setsize, cpusetp1, cpusetp2)
   --  arg-macro: procedure CPU_AND (destset, srcset1, srcset2)
   --    __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, and)
   --  arg-macro: procedure CPU_OR (destset, srcset1, srcset2)
   --    __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, or)
   --  arg-macro: procedure CPU_XOR (destset, srcset1, srcset2)
   --    __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, xor)
   --  arg-macro: procedure CPU_AND_S (setsize, destset, srcset1, srcset2)
   --    __CPU_OP_S (setsize, destset, srcset1, srcset2, and)
   --  arg-macro: procedure CPU_OR_S (setsize, destset, srcset1, srcset2)
   --    __CPU_OP_S (setsize, destset, srcset1, srcset2, or)
   --  arg-macro: procedure CPU_XOR_S (setsize, destset, srcset1, srcset2)
   --    __CPU_OP_S (setsize, destset, srcset1, srcset2, xor)
   --  arg-macro: procedure CPU_ALLOC_SIZE (count)
   --    __CPU_ALLOC_SIZE (count)
   --  arg-macro: procedure CPU_ALLOC (count)
   --    __CPU_ALLOC (count)
   --  arg-macro: procedure CPU_FREE (cpuset)
   --    __CPU_FREE (cpuset)
  -- Definitions for POSIX 1003.1b-1993 (aka POSIX.4) scheduling interface.
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

  -- Get type definitions.   
  -- Get system specific constant and data structure definitions.   
  -- Define the real names for the elements of `struct sched_param'.   
  -- Set scheduling parameters for a process.   
   function sched_setparam (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t; uu_param : access constant x86_64_linux_gnu_bits_sched_h.sched_param) return int  -- /usr/include/sched.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "sched_setparam";

  -- Retrieve scheduling parameters for a particular process.   
   function sched_getparam (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t; uu_param : access x86_64_linux_gnu_bits_sched_h.sched_param) return int  -- /usr/include/sched.h:53
   with Import => True, 
        Convention => C, 
        External_Name => "sched_getparam";

  -- Set scheduling algorithm and/or parameters for a process.   
   function sched_setscheduler
     (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t;
      uu_policy : int;
      uu_param : access constant x86_64_linux_gnu_bits_sched_h.sched_param) return int  -- /usr/include/sched.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "sched_setscheduler";

  -- Retrieve scheduling algorithm for a particular purpose.   
   function sched_getscheduler (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t) return int  -- /usr/include/sched.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "sched_getscheduler";

  -- Yield the processor.   
   function sched_yield return int  -- /usr/include/sched.h:63
   with Import => True, 
        Convention => C, 
        External_Name => "sched_yield";

  -- Get maximum priority value for a scheduler.   
   function sched_get_priority_max (uu_algorithm : int) return int  -- /usr/include/sched.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "sched_get_priority_max";

  -- Get minimum priority value for a scheduler.   
   function sched_get_priority_min (uu_algorithm : int) return int  -- /usr/include/sched.h:69
   with Import => True, 
        Convention => C, 
        External_Name => "sched_get_priority_min";

  -- Get the SCHED_RR interval for the named process.   
   function sched_rr_get_interval (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t; uu_t : access time_h.timespec) return int  -- /usr/include/sched.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "sched_rr_get_interval";

  -- Access macros for `cpu_set'.   
  -- Set the CPU affinity for a task  
   function sched_setaffinity
     (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t;
      uu_cpusetsize : stddef_h.size_t;
      uu_cpuset : access constant x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int  -- /usr/include/sched.h:116
   with Import => True, 
        Convention => C, 
        External_Name => "sched_setaffinity";

  -- Get the CPU affinity for a task  
   function sched_getaffinity
     (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t;
      uu_cpusetsize : stddef_h.size_t;
      uu_cpuset : access x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int  -- /usr/include/sched.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "sched_getaffinity";

end sched_h;
