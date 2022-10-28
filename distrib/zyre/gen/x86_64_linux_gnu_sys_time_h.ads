pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_time_h;
with Interfaces.C.Strings;

package x86_64_linux_gnu_sys_time_h is

   --  arg-macro: procedure TIMEVAL_TO_TIMESPEC (tv, ts)
   --    { (ts).tv_sec := (tv).tv_sec; (ts).tv_nsec := (tv).tv_usec * 1000; }
   --  arg-macro: procedure TIMESPEC_TO_TIMEVAL (tv, ts)
   --    { (tv).tv_sec := (ts).tv_sec; (tv).tv_usec := (ts).tv_nsec / 1000; }
   --  unsupported macro: ITIMER_REAL ITIMER_REAL
   --  unsupported macro: ITIMER_VIRTUAL ITIMER_VIRTUAL
   --  unsupported macro: ITIMER_PROF ITIMER_PROF
   --  arg-macro: function timerisset (tvp)
   --    return (tvp).tv_sec  or else  (tvp).tv_usec;
   --  arg-macro: function timerclear (tvp)
   --    return (tvp).tv_sec := (tvp).tv_usec := 0;
   --  arg-macro: function timercmp (a, b, CMP)
   --    return ((a).tv_sec = (b).tv_sec) ? ((a).tv_usec CMP (b).tv_usec) : ((a).tv_sec CMP (b).tv_sec);
   --  arg-macro: procedure timeradd (a, b, result)
   --    do { (result).tv_sec := (a).tv_sec + (b).tv_sec; (result).tv_usec := (a).tv_usec + (b).tv_usec; if ((result).tv_usec >= 1000000) { ++(result).tv_sec; (result).tv_usec -= 1000000; } } while (0)
   --  arg-macro: procedure timersub (a, b, result)
   --    do { (result).tv_sec := (a).tv_sec - (b).tv_sec; (result).tv_usec := (a).tv_usec - (b).tv_usec; if ((result).tv_usec < 0) { --(result).tv_sec; (result).tv_usec += 1000000; } } while (0)
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

  -- Macros for converting between `struct timeval' and `struct timespec'.   
  -- Structure crudely representing a timezone.
  --   This is obsolete and should never be used.   

  -- Minutes west of GMT.   
   type timezone is record
      tz_minuteswest : aliased int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:57
      tz_dsttime : aliased int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:58
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/sys/time.h:55

  -- Nonzero if DST is ever in effect.   
   type uu_timezone_ptr_t is access all timezone;  -- /usr/include/x86_64-linux-gnu/sys/time.h:61

  -- Get the current time of day and timezone information,
  --   putting it into *TV and *TZ.  If TZ is NULL, *TZ is not filled.
  --   Returns 0 on success, -1 on errors.
  --   NOTE: This form of timezone information is obsolete.
  --   Use the functions and variables declared in <time.h> instead.   

   function gettimeofday (uu_tv : access x86_64_linux_gnu_bits_time_h.timeval; uu_tz : uu_timezone_ptr_t) return int  -- /usr/include/x86_64-linux-gnu/sys/time.h:71
   with Import => True, 
        Convention => C, 
        External_Name => "gettimeofday";

  -- Set the current time of day and timezone information.
  --   This call is restricted to the super-user.   

   function settimeofday (uu_tv : access constant x86_64_linux_gnu_bits_time_h.timeval; uu_tz : access constant timezone) return int  -- /usr/include/x86_64-linux-gnu/sys/time.h:77
   with Import => True, 
        Convention => C, 
        External_Name => "settimeofday";

  -- Adjust the current time of day by the amount in DELTA.
  --   If OLDDELTA is not NULL, it is filled in with the amount
  --   of time adjustment remaining to be done from the last `adjtime' call.
  --   This call is restricted to the super-user.   

   function adjtime (uu_delta : access constant x86_64_linux_gnu_bits_time_h.timeval; uu_olddelta : access x86_64_linux_gnu_bits_time_h.timeval) return int  -- /usr/include/x86_64-linux-gnu/sys/time.h:85
   with Import => True, 
        Convention => C, 
        External_Name => "adjtime";

  -- Values for the first argument to `getitimer' and `setitimer'.   
   type uu_itimer_which is 
     (ITIMER_REAL,
      ITIMER_VIRTUAL,
      ITIMER_PROF)
   with Convention => C;  -- /usr/include/x86_64-linux-gnu/sys/time.h:91

  -- Timers run in real time.   
  -- Timers run only when the process is executing.   
  -- Timers run when the process is executing and when
  --       the system is executing on behalf of the process.   

  -- Type of the second argument to `getitimer' and
  --   the second and third arguments `setitimer'.   

  -- Value to put into `it_value' when the timer expires.   
   type itimerval is record
      it_interval : aliased x86_64_linux_gnu_bits_time_h.timeval;  -- /usr/include/x86_64-linux-gnu/sys/time.h:110
      it_value : aliased x86_64_linux_gnu_bits_time_h.timeval;  -- /usr/include/x86_64-linux-gnu/sys/time.h:112
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/sys/time.h:107

  -- Time to the next timer expiration.   
  -- Use the nicer parameter type only in GNU mode and not for C++ since the
  --   strict C++ rules prevent the automatic promotion.   

   subtype uu_itimer_which_t is int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:120

  -- Set *VALUE to the current setting of timer WHICH.
  --   Return 0 on success, -1 on errors.   

   function getitimer (uu_which : uu_itimer_which_t; uu_value : access itimerval) return int  -- /usr/include/x86_64-linux-gnu/sys/time.h:125
   with Import => True, 
        Convention => C, 
        External_Name => "getitimer";

  -- Set the timer WHICH to *NEW.  If OLD is not NULL,
  --   set *OLD to the old value of timer WHICH.
  --   Returns 0 on success, -1 on errors.   

   function setitimer
     (uu_which : uu_itimer_which_t;
      uu_new : access constant itimerval;
      uu_old : access itimerval) return int  -- /usr/include/x86_64-linux-gnu/sys/time.h:131
   with Import => True, 
        Convention => C, 
        External_Name => "setitimer";

  -- Change the access time of FILE to TVP[0] and the modification time of
  --   FILE to TVP[1].  If TVP is a null pointer, use the current time instead.
  --   Returns 0 on success, -1 on errors.   

   function utimes (uu_file : Interfaces.C.Strings.chars_ptr; uu_tvp : access constant x86_64_linux_gnu_bits_time_h.timeval) return int  -- /usr/include/x86_64-linux-gnu/sys/time.h:138
   with Import => True, 
        Convention => C, 
        External_Name => "utimes";

  -- Same as `utimes', but does not follow symbolic links.   
   function lutimes (uu_file : Interfaces.C.Strings.chars_ptr; uu_tvp : access constant x86_64_linux_gnu_bits_time_h.timeval) return int  -- /usr/include/x86_64-linux-gnu/sys/time.h:143
   with Import => True, 
        Convention => C, 
        External_Name => "lutimes";

  -- Same as `utimes', but takes an open file descriptor instead of a name.   
   function futimes (uu_fd : int; uu_tvp : access constant x86_64_linux_gnu_bits_time_h.timeval) return int  -- /usr/include/x86_64-linux-gnu/sys/time.h:147
   with Import => True, 
        Convention => C, 
        External_Name => "futimes";

  -- Change the access time of FILE relative to FD to TVP[0] and the
  --   modification time of FILE to TVP[1].  If TVP is a null pointer, use
  --   the current time instead.  Returns 0 on success, -1 on errors.   

   function futimesat
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_tvp : access constant x86_64_linux_gnu_bits_time_h.timeval) return int  -- /usr/include/x86_64-linux-gnu/sys/time.h:154
   with Import => True, 
        Convention => C, 
        External_Name => "futimesat";

  -- Convenience macros for operations on timevals.
  --   NOTE: `timercmp' does not work for >= or <=.   

end x86_64_linux_gnu_sys_time_h;
