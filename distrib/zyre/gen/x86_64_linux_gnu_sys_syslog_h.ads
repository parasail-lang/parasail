pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package x86_64_linux_gnu_sys_syslog_h is

   LOG_EMERG : constant := 0;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:51
   LOG_ALERT : constant := 1;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:52
   LOG_CRIT : constant := 2;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:53
   LOG_ERR : constant := 3;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:54
   LOG_WARNING : constant := 4;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:55
   LOG_NOTICE : constant := 5;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:56
   LOG_INFO : constant := 6;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:57
   LOG_DEBUG : constant := 7;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:58

   LOG_PRIMASK : constant := 16#07#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:60
   --  arg-macro: function LOG_PRI (p)
   --    return (p) and LOG_PRIMASK;
   --  arg-macro: function LOG_MAKEPRI (fac, pri)
   --    return (fac) or (pri);
   --  unsupported macro: LOG_KERN (0<<3)

   LOG_USER : constant := (2**3);  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:94
   --  unsupported macro: LOG_MAIL (2<<3)
   --  unsupported macro: LOG_DAEMON (3<<3)
   --  unsupported macro: LOG_AUTH (4<<3)
   --  unsupported macro: LOG_SYSLOG (5<<3)
   --  unsupported macro: LOG_LPR (6<<3)
   --  unsupported macro: LOG_NEWS (7<<3)
   --  unsupported macro: LOG_UUCP (8<<3)
   --  unsupported macro: LOG_CRON (9<<3)
   --  unsupported macro: LOG_AUTHPRIV (10<<3)
   --  unsupported macro: LOG_FTP (11<<3)
   --  unsupported macro: LOG_LOCAL0 (16<<3)
   --  unsupported macro: LOG_LOCAL1 (17<<3)
   --  unsupported macro: LOG_LOCAL2 (18<<3)
   --  unsupported macro: LOG_LOCAL3 (19<<3)
   --  unsupported macro: LOG_LOCAL4 (20<<3)
   --  unsupported macro: LOG_LOCAL5 (21<<3)
   --  unsupported macro: LOG_LOCAL6 (22<<3)
   --  unsupported macro: LOG_LOCAL7 (23<<3)

   LOG_NFACILITIES : constant := 24;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:116
   LOG_FACMASK : constant := 16#03f8#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:117
   --  arg-macro: function LOG_FAC (p)
   --    return ((p) and LOG_FACMASK) >> 3;
   --  arg-macro: function LOG_MASK (pri)
   --    return 2 ** (pri);
   --  arg-macro: function LOG_UPTO (pri)
   --    return (2 ** ((pri)+1)) - 1;

   LOG_PID : constant := 16#01#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:162
   LOG_CONS : constant := 16#02#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:163
   LOG_ODELAY : constant := 16#04#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:164
   LOG_NDELAY : constant := 16#08#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:165
   LOG_NOWAIT : constant := 16#10#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:166
   LOG_PERROR : constant := 16#20#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:167

  -- * Copyright (c) 1982, 1986, 1988, 1993
  -- *	The Regents of the University of California.  All rights reserved.
  -- *
  -- * Redistribution and use in source and binary forms, with or without
  -- * modification, are permitted provided that the following conditions
  -- * are met:
  -- * 1. Redistributions of source code must retain the above copyright
  -- *    notice, this list of conditions and the following disclaimer.
  -- * 2. Redistributions in binary form must reproduce the above copyright
  -- *    notice, this list of conditions and the following disclaimer in the
  -- *    documentation and/or other materials provided with the distribution.
  -- * 4. Neither the name of the University nor the names of its contributors
  -- *    may be used to endorse or promote products derived from this software
  -- *    without specific prior written permission.
  -- *
  -- * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  -- * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  -- * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  -- * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  -- * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  -- * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  -- * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  -- * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  -- * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  -- * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  -- * SUCH DAMAGE.
  -- *
  -- *	@(#)syslog.h	8.1 (Berkeley) 6/2/93
  --  

  -- This file defines _PATH_LOG.   
  -- * priorities/facilities are encoded into a single 32-bit quantity, where the
  -- * bottom 3 bits are the priority (0-7) and the top 28 bits are the facility
  -- * (0-big number).  Both the priorities and the facilities map roughly
  -- * one-to-one to strings in the syslogd(8) source code.  This mapping is
  -- * included in this file.
  -- *
  -- * priorities (these are ordered)
  --  

  -- extract priority  
  -- mark "facility"  
  -- DEPRECATED  
  -- INTERNAL  
  -- DEPRECATED  
  -- DEPRECATED  
  -- facility codes  
  -- other codes through 15 reserved for system use  
  -- facility of pri  
  -- INTERNAL  
  -- DEPRECATED  
  -- * arguments to setlogmask.
  --  

  -- * Option flags for openlog.
  -- *
  -- * LOG_ODELAY no longer does anything.
  -- * LOG_NDELAY is the inverse of what it used to be.
  --  

  -- Close descriptor used to write to system logger.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   procedure closelog  -- /usr/include/x86_64-linux-gnu/sys/syslog.h:175
   with Import => True, 
        Convention => C, 
        External_Name => "closelog";

  -- Open connection to system logger.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   procedure openlog
     (uu_ident : Interfaces.C.Strings.chars_ptr;
      uu_option : int;
      uu_facility : int)  -- /usr/include/x86_64-linux-gnu/sys/syslog.h:181
   with Import => True, 
        Convention => C, 
        External_Name => "openlog";

  -- Set the log mask level.   
   function setlogmask (uu_mask : int) return int  -- /usr/include/x86_64-linux-gnu/sys/syslog.h:184
   with Import => True, 
        Convention => C, 
        External_Name => "setlogmask";

  -- Generate a log message using FMT string and option arguments.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   procedure syslog (uu_pri : int; uu_fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      )  -- /usr/include/x86_64-linux-gnu/sys/syslog.h:190
   with Import => True, 
        Convention => C, 
        External_Name => "syslog";

  -- Generate a log message using FMT and using arguments pointed to by AP.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   procedure vsyslog
     (uu_pri : int;
      uu_fmt : Interfaces.C.Strings.chars_ptr;
      uu_ap : access System.Address)  -- /usr/include/x86_64-linux-gnu/sys/syslog.h:200
   with Import => True, 
        Convention => C, 
        External_Name => "vsyslog";

  -- Define some macros helping to catch buffer overflows.   
end x86_64_linux_gnu_sys_syslog_h;
