pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
with Interfaces.C.Strings;
with stddef_h;
with xlocale_h;
with x86_64_linux_gnu_sys_types_h;
limited with x86_64_linux_gnu_bits_siginfo_h;
with System;

package time_h is

   TIME_UTC : constant := 1;  --  /usr/include/time.h:182

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

  -- *	ISO C99 Standard: 7.23 Date and time	<time.h>
  --  

  -- Get size_t and NULL from <stddef.h>.   
  -- This defines CLOCKS_PER_SEC, which is the number of processor clock
  --   ticks per second.   

  -- This is the obsolete POSIX.1-1988 name for the same constant.   
  -- Returned by `clock'.   
   subtype clock_t is x86_64_linux_gnu_bits_types_h.uu_clock_t;  -- /usr/include/time.h:59

  -- Returned by `time'.   
   subtype time_t is x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/time.h:75

  -- Clock ID used in clock and timer functions.   
   subtype clockid_t is x86_64_linux_gnu_bits_types_h.uu_clockid_t;  -- /usr/include/time.h:91

  -- Timer ID returned by `timer_create'.   
   subtype timer_t is x86_64_linux_gnu_bits_types_h.uu_timer_t;  -- /usr/include/time.h:103

  -- This defines __time_t for us.   
  -- POSIX.1b structure for a time value.  This is like a `struct timeval' but
  --   has nanoseconds instead of microseconds.   

  -- Seconds.   
   type timespec is record
      tv_sec : aliased x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/time.h:122
      tv_nsec : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/time.h:123
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/time.h:120

  -- Nanoseconds.   
  -- Used by other time functions.   
  -- Seconds.	[0-60] (1 leap second)  
   type tm is record
      tm_sec : aliased int;  -- /usr/include/time.h:135
      tm_min : aliased int;  -- /usr/include/time.h:136
      tm_hour : aliased int;  -- /usr/include/time.h:137
      tm_mday : aliased int;  -- /usr/include/time.h:138
      tm_mon : aliased int;  -- /usr/include/time.h:139
      tm_year : aliased int;  -- /usr/include/time.h:140
      tm_wday : aliased int;  -- /usr/include/time.h:141
      tm_yday : aliased int;  -- /usr/include/time.h:142
      tm_isdst : aliased int;  -- /usr/include/time.h:143
      tm_gmtoff : aliased long;  -- /usr/include/time.h:146
      tm_zone : Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:147
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/time.h:133

  -- Minutes.	[0-59]  
  -- Hours.	[0-23]  
  -- Day.		[1-31]  
  -- Month.	[0-11]  
  -- Year	- 1900.   
  -- Day of week.	[0-6]  
  -- Days in year.[0-365]	 
  -- DST.		[-1/0/1] 
  -- Seconds east of UTC.   
  -- Timezone abbreviation.   
  -- Seconds east of UTC.   
  -- Timezone abbreviation.   
  -- POSIX.1b structure for timer start values and intervals.   
   type itimerspec is record
      it_interval : aliased timespec;  -- /usr/include/time.h:163
      it_value : aliased timespec;  -- /usr/include/time.h:164
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/time.h:161

  -- We can use a simple forward declaration.   
  -- Time base values for timespec_get.   
  -- Time used by the program so far (user time + system time).
  --   The result / CLOCKS_PER_SECOND is program time in seconds.   

   function clock return clock_t  -- /usr/include/time.h:189
   with Import => True, 
        Convention => C, 
        External_Name => "clock";

  -- Return the current time and put it in *TIMER if TIMER is not NULL.   
   function time (uu_timer : access time_t) return time_t  -- /usr/include/time.h:192
   with Import => True, 
        Convention => C, 
        External_Name => "time";

  -- Return the difference between TIME1 and TIME0.   
   function difftime (uu_time1 : time_t; uu_time0 : time_t) return double  -- /usr/include/time.h:195
   with Import => True, 
        Convention => C, 
        External_Name => "difftime";

  -- Return the `time_t' representation of TP and normalize TP.   
   function mktime (uu_tp : access tm) return time_t  -- /usr/include/time.h:199
   with Import => True, 
        Convention => C, 
        External_Name => "mktime";

  -- Format TP into S according to FORMAT.
  --   Write no more than MAXSIZE characters and return the number
  --   of characters written, or 0 if it would exceed MAXSIZE.   

   function strftime
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_maxsize : stddef_h.size_t;
      uu_format : Interfaces.C.Strings.chars_ptr;
      uu_tp : access constant tm) return stddef_h.size_t  -- /usr/include/time.h:205
   with Import => True, 
        Convention => C, 
        External_Name => "strftime";

  -- Parse S according to FORMAT and store binary time information in TP.
  --   The return value is a pointer to the first unparsed character in S.   

   function strptime
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_fmt : Interfaces.C.Strings.chars_ptr;
      uu_tp : access tm) return Interfaces.C.Strings.chars_ptr  -- /usr/include/time.h:213
   with Import => True, 
        Convention => C, 
        External_Name => "strptime";

  -- Similar to the two functions above but take the information from
  --   the provided locale and not the global locale.   

   function strftime_l
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_maxsize : stddef_h.size_t;
      uu_format : Interfaces.C.Strings.chars_ptr;
      uu_tp : access constant tm;
      uu_loc : xlocale_h.uu_locale_t) return stddef_h.size_t  -- /usr/include/time.h:223
   with Import => True, 
        Convention => C, 
        External_Name => "strftime_l";

   function strptime_l
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_fmt : Interfaces.C.Strings.chars_ptr;
      uu_tp : access tm;
      uu_loc : xlocale_h.uu_locale_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/time.h:230
   with Import => True, 
        Convention => C, 
        External_Name => "strptime_l";

  -- Return the `struct tm' representation of *TIMER
  --   in Universal Coordinated Time (aka Greenwich Mean Time).   

   function gmtime (uu_timer : access time_t) return access tm  -- /usr/include/time.h:239
   with Import => True, 
        Convention => C, 
        External_Name => "gmtime";

  -- Return the `struct tm' representation
  --   of *TIMER in the local timezone.   

   function localtime (uu_timer : access time_t) return access tm  -- /usr/include/time.h:243
   with Import => True, 
        Convention => C, 
        External_Name => "localtime";

  -- Return the `struct tm' representation of *TIMER in UTC,
  --   using *TP to store the result.   

   function gmtime_r (uu_timer : access time_t; uu_tp : access tm) return access tm  -- /usr/include/time.h:249
   with Import => True, 
        Convention => C, 
        External_Name => "gmtime_r";

  -- Return the `struct tm' representation of *TIMER in local time,
  --   using *TP to store the result.   

   function localtime_r (uu_timer : access time_t; uu_tp : access tm) return access tm  -- /usr/include/time.h:254
   with Import => True, 
        Convention => C, 
        External_Name => "localtime_r";

  -- Return a string of the form "Day Mon dd hh:mm:ss yyyy\n"
  --   that is the representation of TP in this format.   

   function asctime (uu_tp : access constant tm) return Interfaces.C.Strings.chars_ptr  -- /usr/include/time.h:261
   with Import => True, 
        Convention => C, 
        External_Name => "asctime";

  -- Equivalent to `asctime (localtime (timer))'.   
   function ctime (uu_timer : access time_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/time.h:264
   with Import => True, 
        Convention => C, 
        External_Name => "ctime";

  -- Reentrant versions of the above functions.   
  -- Return in BUF a string of the form "Day Mon dd hh:mm:ss yyyy\n"
  --   that is the representation of TP in this format.   

   function asctime_r (uu_tp : access constant tm; uu_buf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/time.h:272
   with Import => True, 
        Convention => C, 
        External_Name => "asctime_r";

  -- Equivalent to `asctime_r (localtime_r (timer, *TMP*), buf)'.   
   function ctime_r (uu_timer : access time_t; uu_buf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/time.h:276
   with Import => True, 
        Convention => C, 
        External_Name => "ctime_r";

  -- Defined in localtime.c.   
  -- Current timezone names.   
  -- If daylight-saving time is ever in use.   
  -- Seconds west of UTC.   
  -- Same as above.   
   tzname : array (0 .. 1) of Interfaces.C.Strings.chars_ptr  -- /usr/include/time.h:289
   with Import => True, 
        Convention => C, 
        External_Name => "tzname";

  -- Set time conversion information from the TZ environment variable.
  --   If TZ is not defined, a locale-dependent default is used.   

   procedure tzset  -- /usr/include/time.h:293
   with Import => True, 
        Convention => C, 
        External_Name => "tzset";

   daylight : aliased int  -- /usr/include/time.h:297
   with Import => True, 
        Convention => C, 
        External_Name => "daylight";

   timezone : aliased long  -- /usr/include/time.h:298
   with Import => True, 
        Convention => C, 
        External_Name => "timezone";

  -- Set the system time to *WHEN.
  --   This call is restricted to the superuser.   

   function stime (uu_when : access time_t) return int  -- /usr/include/time.h:304
   with Import => True, 
        Convention => C, 
        External_Name => "stime";

  -- Nonzero if YEAR is a leap year (every 4 years,
  --   except every 100th isn't, and every 400th is).   

  -- Miscellaneous functions many Unices inherited from the public domain
  --   localtime package.  These are included only for compatibility.   

  -- Like `mktime', but for TP represents Universal Time, not local time.   
   function timegm (uu_tp : access tm) return time_t  -- /usr/include/time.h:319
   with Import => True, 
        Convention => C, 
        External_Name => "timegm";

  -- Another name for `mktime'.   
   function timelocal (uu_tp : access tm) return time_t  -- /usr/include/time.h:322
   with Import => True, 
        Convention => C, 
        External_Name => "timelocal";

  -- Return the number of days in YEAR.   
   function dysize (uu_year : int) return int  -- /usr/include/time.h:325
   with Import => True, 
        Convention => C, 
        External_Name => "dysize";

  -- Pause execution for a number of nanoseconds.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function nanosleep (uu_requested_time : access constant timespec; uu_remaining : access timespec) return int  -- /usr/include/time.h:334
   with Import => True, 
        Convention => C, 
        External_Name => "nanosleep";

  -- Get resolution of clock CLOCK_ID.   
   function clock_getres (uu_clock_id : clockid_t; uu_res : access timespec) return int  -- /usr/include/time.h:339
   with Import => True, 
        Convention => C, 
        External_Name => "clock_getres";

  -- Get current value of clock CLOCK_ID and store it in TP.   
   function clock_gettime (uu_clock_id : clockid_t; uu_tp : access timespec) return int  -- /usr/include/time.h:342
   with Import => True, 
        Convention => C, 
        External_Name => "clock_gettime";

  -- Set clock CLOCK_ID to value TP.   
   function clock_settime (uu_clock_id : clockid_t; uu_tp : access constant timespec) return int  -- /usr/include/time.h:345
   with Import => True, 
        Convention => C, 
        External_Name => "clock_settime";

  -- High-resolution sleep with the specified clock.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function clock_nanosleep
     (uu_clock_id : clockid_t;
      uu_flags : int;
      uu_req : access constant timespec;
      uu_rem : access timespec) return int  -- /usr/include/time.h:353
   with Import => True, 
        Convention => C, 
        External_Name => "clock_nanosleep";

  -- Return clock ID for CPU-time clock.   
   function clock_getcpuclockid (uu_pid : x86_64_linux_gnu_sys_types_h.pid_t; uu_clock_id : access clockid_t) return int  -- /usr/include/time.h:358
   with Import => True, 
        Convention => C, 
        External_Name => "clock_getcpuclockid";

  -- Create new per-process timer using CLOCK_ID.   
   function timer_create
     (uu_clock_id : clockid_t;
      uu_evp : access x86_64_linux_gnu_bits_siginfo_h.sigevent;
      uu_timerid : System.Address) return int  -- /usr/include/time.h:363
   with Import => True, 
        Convention => C, 
        External_Name => "timer_create";

  -- Delete timer TIMERID.   
   function timer_delete (uu_timerid : timer_t) return int  -- /usr/include/time.h:368
   with Import => True, 
        Convention => C, 
        External_Name => "timer_delete";

  -- Set timer TIMERID to VALUE, returning old value in OVALUE.   
   function timer_settime
     (uu_timerid : timer_t;
      uu_flags : int;
      uu_value : access constant itimerspec;
      uu_ovalue : access itimerspec) return int  -- /usr/include/time.h:371
   with Import => True, 
        Convention => C, 
        External_Name => "timer_settime";

  -- Get current value of timer TIMERID and store it in VALUE.   
   function timer_gettime (uu_timerid : timer_t; uu_value : access itimerspec) return int  -- /usr/include/time.h:376
   with Import => True, 
        Convention => C, 
        External_Name => "timer_gettime";

  -- Get expiration overrun for timer TIMERID.   
   function timer_getoverrun (uu_timerid : timer_t) return int  -- /usr/include/time.h:380
   with Import => True, 
        Convention => C, 
        External_Name => "timer_getoverrun";

  -- Set TS to calendar time based in time base BASE.   
   function timespec_get (uu_ts : access timespec; uu_base : int) return int  -- /usr/include/time.h:386
   with Import => True, 
        Convention => C, 
        External_Name => "timespec_get";

  -- Set to one of the following values to indicate an error.
  --     1  the DATEMSK environment variable is null or undefined,
  --     2  the template file cannot be opened for reading,
  --     3  failed to get file status information,
  --     4  the template file is not a regular file,
  --     5  an error is encountered while reading the template file,
  --     6  memory allication failed (not enough memory available),
  --     7  there is no line in the template that matches the input,
  --     8  invalid input specification Example: February 31 or a time is
  --	specified that can not be represented in a time_t (representing
  --	the time in seconds since 00:00:00 UTC, January 1, 1970)  

   getdate_err : aliased int  -- /usr/include/time.h:403
   with Import => True, 
        Convention => C, 
        External_Name => "getdate_err";

  -- Parse the given string as a date specification and return a value
  --   representing the value.  The templates from the file identified by
  --   the environment variable DATEMSK are used.  In case of an error
  --   `getdate_err' is set.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function getdate (uu_string : Interfaces.C.Strings.chars_ptr) return access tm  -- /usr/include/time.h:412
   with Import => True, 
        Convention => C, 
        External_Name => "getdate";

  -- Since `getdate' is not reentrant because of the use of `getdate_err'
  --   and the static buffer to return the result in, we provide a thread-safe
  --   variant.  The functionality is the same.  The result is returned in
  --   the buffer pointed to by RESBUFP and in case of an error the return
  --   value is != 0 with the same values as given above for `getdate_err'.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function getdate_r (uu_string : Interfaces.C.Strings.chars_ptr; uu_resbufp : access tm) return int  -- /usr/include/time.h:426
   with Import => True, 
        Convention => C, 
        External_Name => "getdate_r";

end time_h;
