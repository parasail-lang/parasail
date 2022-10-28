pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
limited with x86_64_linux_gnu_bits_timex_h;

package x86_64_linux_gnu_bits_time_h is

   CLOCKS_PER_SEC : constant := 1000000;  --  /usr/include/x86_64-linux-gnu/bits/time.h:48

   CLOCK_REALTIME : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/time.h:61

   CLOCK_MONOTONIC : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/time.h:63

   CLOCK_PROCESS_CPUTIME_ID : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/time.h:65

   CLOCK_THREAD_CPUTIME_ID : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/time.h:67

   CLOCK_MONOTONIC_RAW : constant := 4;  --  /usr/include/x86_64-linux-gnu/bits/time.h:69

   CLOCK_REALTIME_COARSE : constant := 5;  --  /usr/include/x86_64-linux-gnu/bits/time.h:71

   CLOCK_MONOTONIC_COARSE : constant := 6;  --  /usr/include/x86_64-linux-gnu/bits/time.h:73

   CLOCK_BOOTTIME : constant := 7;  --  /usr/include/x86_64-linux-gnu/bits/time.h:75

   CLOCK_REALTIME_ALARM : constant := 8;  --  /usr/include/x86_64-linux-gnu/bits/time.h:77

   CLOCK_BOOTTIME_ALARM : constant := 9;  --  /usr/include/x86_64-linux-gnu/bits/time.h:79

   TIMER_ABSTIME : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/time.h:82

  -- System-dependent timing definitions.  Linux version.
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

  -- * Never include this file directly; use <time.h> instead.
  --  

  -- A time value that is accurate to the nearest
  --   microsecond but also has a range of years.   

  -- Seconds.   
   type timeval is record
      tv_sec : aliased x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/x86_64-linux-gnu/bits/time.h:32
      tv_usec : aliased x86_64_linux_gnu_bits_types_h.uu_suseconds_t;  -- /usr/include/x86_64-linux-gnu/bits/time.h:33
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/time.h:30

  -- Microseconds.   
  -- ISO/IEC 9899:1990 7.12.1: <time.h>
  --   The macro `CLOCKS_PER_SEC' is the number per second of the value
  --   returned by the `clock' function.  

  -- CAE XSH, Issue 4, Version 2: <time.h>
  --   The value of CLOCKS_PER_SEC is required to be 1 million on all
  --   XSI-conformant systems.  

  -- Even though CLOCKS_PER_SEC has such a strange value CLK_TCK
  --   presents the real value for clock ticks per second for the system.   

  -- Identifier for system-wide realtime clock.   
  -- Monotonic system-wide clock.   
  -- High-resolution timer from the CPU.   
  -- Thread-specific CPU-time clock.   
  -- Monotonic system-wide clock, not adjusted for frequency scaling.   
  -- Identifier for system-wide realtime clock, updated only on ticks.   
  -- Monotonic system-wide clock, updated only on ticks.   
  -- Monotonic system-wide clock that includes time spent in suspension.   
  -- Like CLOCK_REALTIME but also wakes suspended system.   
  -- Like CLOCK_BOOTTIME but also wakes suspended system.   
  -- Flag to indicate time is absolute.   
  -- Tune a POSIX clock.   
   function clock_adjtime (uu_clock_id : x86_64_linux_gnu_bits_types_h.uu_clockid_t; uu_utx : access x86_64_linux_gnu_bits_timex_h.timex) return int  -- /usr/include/x86_64-linux-gnu/bits/time.h:91
   with Import => True, 
        Convention => C, 
        External_Name => "clock_adjtime";

end x86_64_linux_gnu_bits_time_h;
