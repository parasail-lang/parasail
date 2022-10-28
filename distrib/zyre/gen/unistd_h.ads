pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with x86_64_linux_gnu_bits_types_h;
with System;
with stddef_h;
with stdio_h;
with stdint_h;

package unistd_h is

   STDIN_FILENO : constant := 0;  --  /usr/include/unistd.h:210
   STDOUT_FILENO : constant := 1;  --  /usr/include/unistd.h:211
   STDERR_FILENO : constant := 2;  --  /usr/include/unistd.h:212

   R_OK : constant := 4;  --  /usr/include/unistd.h:281
   W_OK : constant := 2;  --  /usr/include/unistd.h:282
   X_OK : constant := 1;  --  /usr/include/unistd.h:283
   F_OK : constant := 0;  --  /usr/include/unistd.h:284
   --  unsupported macro: L_SET SEEK_SET
   --  unsupported macro: L_INCR SEEK_CUR
   --  unsupported macro: L_XTND SEEK_END
   --  arg-macro: function TEMP_FAILURE_RETRY (expression)
   --    return __extension__ ({ long int __result; do __result := (long int) (expression); while (__result = -1  and then  errno = EINTR); __result; });

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

  -- *	POSIX Standard: 2.10 Symbolic Constants		<unistd.h>
  --  

  -- These may be used to determine what facilities are present at compile time.
  --   Their values can be obtained at run time from `sysconf'.   

  -- POSIX Standard approved as ISO/IEC 9945-1 as of September 2008.   
  -- POSIX Standard approved as ISO/IEC 9945-1 as of December 2001.   
  -- POSIX Standard approved as ISO/IEC 9945-1 as of June 1995.   
  -- POSIX Standard approved as ISO/IEC 9945-1 as of September 1993.   
  -- POSIX Standard approved as ISO/IEC 9945-1 as of September 1990.   
  -- These are not #ifdef __USE_POSIX2 because they are
  --   in the theoretically application-owned namespace.   

  -- The utilities on GNU systems also correspond to this version.   
  -- The utilities on GNU systems also correspond to this version.   
  -- The utilities on GNU systems also correspond to this version.   
  -- The utilities on GNU systems also correspond to this version.   
  -- The utilities on GNU systems also correspond to this version.   
  -- If defined, the implementation supports the
  --   C Language Bindings Option.   

  -- If defined, the implementation supports the
  --   C Language Development Utilities Option.   

  -- If defined, the implementation supports the
  --   Software Development Utilities Option.   

  -- If defined, the implementation supports the
  --   creation of locales with the localedef utility.   

  -- X/Open version number to which the library conforms.  It is selectable.   
  -- Commands and utilities from XPG4 are available.   
  -- We are compatible with the old published standards as well.   
  -- The X/Open Unix extensions are available.   
  -- Encryption is present.   
  -- The enhanced internationalization capabilities according to XPG4.2
  --   are present.   

  -- The legacy interfaces are also available.   
  -- Get values of POSIX options:
  --   If these symbols are defined, the corresponding features are
  --   always available.  If not, they may be available sometimes.
  --   The current values can be obtained with `sysconf'.
  --   _POSIX_JOB_CONTROL		Job control is supported.
  --   _POSIX_SAVED_IDS		Processes have a saved set-user-ID
  --				and a saved set-group-ID.
  --   _POSIX_REALTIME_SIGNALS	Real-time, queued signals are supported.
  --   _POSIX_PRIORITY_SCHEDULING	Priority scheduling is supported.
  --   _POSIX_TIMERS		POSIX.4 clocks and timers are supported.
  --   _POSIX_ASYNCHRONOUS_IO	Asynchronous I/O is supported.
  --   _POSIX_PRIORITIZED_IO	Prioritized asynchronous I/O is supported.
  --   _POSIX_SYNCHRONIZED_IO	Synchronizing file data is supported.
  --   _POSIX_FSYNC			The fsync function is present.
  --   _POSIX_MAPPED_FILES		Mapping of files to memory is supported.
  --   _POSIX_MEMLOCK		Locking of all memory is supported.
  --   _POSIX_MEMLOCK_RANGE		Locking of ranges of memory is supported.
  --   _POSIX_MEMORY_PROTECTION	Setting of memory protections is supported.
  --   _POSIX_MESSAGE_PASSING	POSIX.4 message queues are supported.
  --   _POSIX_SEMAPHORES		POSIX.4 counting semaphores are supported.
  --   _POSIX_SHARED_MEMORY_OBJECTS	POSIX.4 shared memory objects are supported.
  --   _POSIX_THREADS		POSIX.1c pthreads are supported.
  --   _POSIX_THREAD_ATTR_STACKADDR	Thread stack address attribute option supported.
  --   _POSIX_THREAD_ATTR_STACKSIZE	Thread stack size attribute option supported.
  --   _POSIX_THREAD_SAFE_FUNCTIONS	Thread-safe functions are supported.
  --   _POSIX_THREAD_PRIORITY_SCHEDULING
  --				POSIX.1c thread execution scheduling supported.
  --   _POSIX_THREAD_PRIO_INHERIT	Thread priority inheritance option supported.
  --   _POSIX_THREAD_PRIO_PROTECT	Thread priority protection option supported.
  --   _POSIX_THREAD_PROCESS_SHARED	Process-shared synchronization supported.
  --   _POSIX_PII			Protocol-independent interfaces are supported.
  --   _POSIX_PII_XTI		XTI protocol-indep. interfaces are supported.
  --   _POSIX_PII_SOCKET		Socket protocol-indep. interfaces are supported.
  --   _POSIX_PII_INTERNET		Internet family of protocols supported.
  --   _POSIX_PII_INTERNET_STREAM	Connection-mode Internet protocol supported.
  --   _POSIX_PII_INTERNET_DGRAM	Connectionless Internet protocol supported.
  --   _POSIX_PII_OSI		ISO/OSI family of protocols supported.
  --   _POSIX_PII_OSI_COTS		Connection-mode ISO/OSI service supported.
  --   _POSIX_PII_OSI_CLTS		Connectionless ISO/OSI service supported.
  --   _POSIX_POLL			Implementation supports `poll' function.
  --   _POSIX_SELECT		Implementation supports `select' and `pselect'.
  --   _XOPEN_REALTIME		X/Open realtime support is available.
  --   _XOPEN_REALTIME_THREADS	X/Open realtime thread support is available.
  --   _XOPEN_SHM			Shared memory interface according to XPG4.2.
  --   _XBS5_ILP32_OFF32		Implementation provides environment with 32-bit
  --				int, long, pointer, and off_t types.
  --   _XBS5_ILP32_OFFBIG		Implementation provides environment with 32-bit
  --				int, long, and pointer and off_t with at least
  --				64 bits.
  --   _XBS5_LP64_OFF64		Implementation provides environment with 32-bit
  --				int, and 64-bit long, pointer, and off_t types.
  --   _XBS5_LPBIG_OFFBIG		Implementation provides environment with at
  --				least 32 bits int and long, pointer, and off_t
  --				with at least 64 bits.
  --   If any of these symbols is defined as -1, the corresponding option is not
  --   true for any file.  If any is defined as other than -1, the corresponding
  --   option is true for all files.  If a symbol is not defined at all, the value
  --   for a specific file can be obtained from `pathconf' and `fpathconf'.
  --   _POSIX_CHOWN_RESTRICTED	Only the super user can use `chown' to change
  --				the owner of a file.  `chown' can only be used
  --				to change the group ID of a file to a group of
  --				which the calling process is a member.
  --   _POSIX_NO_TRUNC		Pathname components longer than
  --				NAME_MAX generate an error.
  --   _POSIX_VDISABLE		If defined, if the value of an element of the
  --				`c_cc' member of `struct termios' is
  --				_POSIX_VDISABLE, no character will have the
  --				effect associated with that element.
  --   _POSIX_SYNC_IO		Synchronous I/O may be performed.
  --   _POSIX_ASYNC_IO		Asynchronous I/O may be performed.
  --   _POSIX_PRIO_IO		Prioritized Asynchronous I/O may be performed.
  --   Support for the Large File Support interface is not generally available.
  --   If it is available the following constants are defined to one.
  --   _LFS64_LARGEFILE		Low-level I/O supports large files.
  --   _LFS64_STDIO			Standard I/O supports large files.
  --    

  -- Get the environment definitions from Unix98.   
  -- Standard file descriptors.   
  -- All functions that are not declared anywhere else.   
  -- The Single Unix specification says that some more types are
  --   available here.   

  -- Values for the second argument to access.
  --   These may be OR'd together.   

  -- Test for access to NAME using the real UID and real GID.   
   function c_access (uu_name : Interfaces.C.Strings.chars_ptr; uu_type : int) return int  -- /usr/include/unistd.h:287
   with Import => True, 
        Convention => C, 
        External_Name => "access";

  -- Test for access to NAME using the effective UID and GID
  --   (as normal file operations use).   

   function euidaccess (uu_name : Interfaces.C.Strings.chars_ptr; uu_type : int) return int  -- /usr/include/unistd.h:292
   with Import => True, 
        Convention => C, 
        External_Name => "euidaccess";

  -- An alias for `euidaccess', used by some other systems.   
   function eaccess (uu_name : Interfaces.C.Strings.chars_ptr; uu_type : int) return int  -- /usr/include/unistd.h:296
   with Import => True, 
        Convention => C, 
        External_Name => "eaccess";

  -- Test for access to FILE relative to the directory FD is open on.
  --   If AT_EACCESS is set in FLAG, then use effective IDs like `eaccess',
  --   otherwise use real IDs like `access'.   

   function faccessat
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_type : int;
      uu_flag : int) return int  -- /usr/include/unistd.h:304
   with Import => True, 
        Convention => C, 
        External_Name => "faccessat";

  -- Values for the WHENCE argument to lseek.   
  -- Old BSD names for the same constants; just for compatibility.   
  -- Move FD's file position to OFFSET bytes from the
  --   beginning of the file (if WHENCE is SEEK_SET),
  --   the current position (if WHENCE is SEEK_CUR),
  --   or the end of the file (if WHENCE is SEEK_END).
  --   Return the new file position.   

   function lseek
     (uu_fd : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off_t;
      uu_whence : int) return x86_64_linux_gnu_bits_types_h.uu_off_t  -- /usr/include/unistd.h:334
   with Import => True, 
        Convention => C, 
        External_Name => "lseek";

   function lseek64
     (uu_fd : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off64_t;
      uu_whence : int) return x86_64_linux_gnu_bits_types_h.uu_off64_t  -- /usr/include/unistd.h:345
   with Import => True, 
        Convention => C, 
        External_Name => "lseek64";

  -- Close the file descriptor FD.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function close (uu_fd : int) return int  -- /usr/include/unistd.h:353
   with Import => True, 
        Convention => C, 
        External_Name => "close";

  -- Read NBYTES into BUF from FD.  Return the
  --   number read, -1 for errors or 0 for EOF.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function read
     (uu_fd : int;
      uu_buf : System.Address;
      uu_nbytes : stddef_h.size_t) return stdio_h.ssize_t  -- /usr/include/unistd.h:360
   with Import => True, 
        Convention => C, 
        External_Name => "read";

  -- Write N bytes of BUF to FD.  Return the number written, or -1.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function write
     (uu_fd : int;
      uu_buf : System.Address;
      uu_n : stddef_h.size_t) return stdio_h.ssize_t  -- /usr/include/unistd.h:366
   with Import => True, 
        Convention => C, 
        External_Name => "write";

  -- Read NBYTES into BUF from FD at the given position OFFSET without
  --   changing the file pointer.  Return the number read, -1 for errors
  --   or 0 for EOF.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function pread
     (uu_fd : int;
      uu_buf : System.Address;
      uu_nbytes : stddef_h.size_t;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off_t) return stdio_h.ssize_t  -- /usr/include/unistd.h:376
   with Import => True, 
        Convention => C, 
        External_Name => "pread";

  -- Write N bytes of BUF to FD at the given position OFFSET without
  --   changing the file pointer.  Return the number written, or -1.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function pwrite
     (uu_fd : int;
      uu_buf : System.Address;
      uu_n : stddef_h.size_t;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off_t) return stdio_h.ssize_t  -- /usr/include/unistd.h:384
   with Import => True, 
        Convention => C, 
        External_Name => "pwrite";

  -- Read NBYTES into BUF from FD at the given position OFFSET without
  --   changing the file pointer.  Return the number read, -1 for errors
  --   or 0 for EOF.   

   function pread64
     (uu_fd : int;
      uu_buf : System.Address;
      uu_nbytes : stddef_h.size_t;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off64_t) return stdio_h.ssize_t  -- /usr/include/unistd.h:404
   with Import => True, 
        Convention => C, 
        External_Name => "pread64";

  -- Write N bytes of BUF to FD at the given position OFFSET without
  --   changing the file pointer.  Return the number written, or -1.   

   function pwrite64
     (uu_fd : int;
      uu_buf : System.Address;
      uu_n : stddef_h.size_t;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off64_t) return stdio_h.ssize_t  -- /usr/include/unistd.h:408
   with Import => True, 
        Convention => C, 
        External_Name => "pwrite64";

  -- Create a one-way communication channel (pipe).
  --   If successful, two file descriptors are stored in PIPEDES;
  --   bytes written on PIPEDES[1] can be read from PIPEDES[0].
  --   Returns 0 if successful, -1 if not.   

   function pipe (uu_pipedes : access int) return int  -- /usr/include/unistd.h:417
   with Import => True, 
        Convention => C, 
        External_Name => "pipe";

  -- Same as pipe but apply flags passed in FLAGS to the new file
  --   descriptors.   

   function pipe2 (uu_pipedes : access int; uu_flags : int) return int  -- /usr/include/unistd.h:422
   with Import => True, 
        Convention => C, 
        External_Name => "pipe2";

  -- Schedule an alarm.  In SECONDS seconds, the process will get a SIGALRM.
  --   If SECONDS is zero, any currently scheduled alarm will be cancelled.
  --   The function returns the number of seconds remaining until the last
  --   alarm scheduled would have signaled, or zero if there wasn't one.
  --   There is no return value to indicate an error, but you can set `errno'
  --   to 0 and check its value after calling `alarm', and this might tell you.
  --   The signal may come late due to processor scheduling.   

   function alarm (uu_seconds : unsigned) return unsigned  -- /usr/include/unistd.h:432
   with Import => True, 
        Convention => C, 
        External_Name => "alarm";

  -- Make the process sleep for SECONDS seconds, or until a signal arrives
  --   and is not ignored.  The function returns the number of seconds less
  --   than SECONDS which it actually slept (thus zero if it slept the full time).
  --   If a signal handler does a `longjmp' or modifies the handling of the
  --   SIGALRM signal while inside `sleep' call, the handling of the SIGALRM
  --   signal afterwards is undefined.  There is no return value to indicate
  --   error, but if `sleep' returns SECONDS, it probably didn't work.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function sleep (uu_seconds : unsigned) return unsigned  -- /usr/include/unistd.h:444
   with Import => True, 
        Convention => C, 
        External_Name => "sleep";

  -- Set an alarm to go off (generating a SIGALRM signal) in VALUE
  --   microseconds.  If INTERVAL is nonzero, when the alarm goes off, the
  --   timer is reset to go off every INTERVAL microseconds thereafter.
  --   Returns the number of microseconds remaining before the alarm.   

   function ualarm (uu_value : x86_64_linux_gnu_bits_types_h.uu_useconds_t; uu_interval : x86_64_linux_gnu_bits_types_h.uu_useconds_t) return x86_64_linux_gnu_bits_types_h.uu_useconds_t  -- /usr/include/unistd.h:452
   with Import => True, 
        Convention => C, 
        External_Name => "ualarm";

  -- Sleep USECONDS microseconds, or until a signal arrives that is not blocked
  --   or ignored.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function usleep (uu_useconds : x86_64_linux_gnu_bits_types_h.uu_useconds_t) return int  -- /usr/include/unistd.h:460
   with Import => True, 
        Convention => C, 
        External_Name => "usleep";

  -- Suspend the process until a signal arrives.
  --   This always returns -1 and sets `errno' to EINTR.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function pause return int  -- /usr/include/unistd.h:469
   with Import => True, 
        Convention => C, 
        External_Name => "pause";

  -- Change the owner and group of FILE.   
   function chown
     (uu_file : Interfaces.C.Strings.chars_ptr;
      uu_owner : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_group : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/unistd.h:473
   with Import => True, 
        Convention => C, 
        External_Name => "chown";

  -- Change the owner and group of the file that FD is open on.   
   function fchown
     (uu_fd : int;
      uu_owner : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_group : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/unistd.h:478
   with Import => True, 
        Convention => C, 
        External_Name => "fchown";

  -- Change owner and group of FILE, if it is a symbolic
  --   link the ownership of the symbolic link is changed.   

   function lchown
     (uu_file : Interfaces.C.Strings.chars_ptr;
      uu_owner : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_group : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/unistd.h:483
   with Import => True, 
        Convention => C, 
        External_Name => "lchown";

  -- Change the owner and group of FILE relative to the directory FD is open
  --   on.   

   function fchownat
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_owner : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_group : x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_flag : int) return int  -- /usr/include/unistd.h:491
   with Import => True, 
        Convention => C, 
        External_Name => "fchownat";

  -- Change the process's working directory to PATH.   
   function chdir (uu_path : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/unistd.h:497
   with Import => True, 
        Convention => C, 
        External_Name => "chdir";

  -- Change the process's working directory to the one FD is open on.   
   function fchdir (uu_fd : int) return int  -- /usr/include/unistd.h:501
   with Import => True, 
        Convention => C, 
        External_Name => "fchdir";

  -- Get the pathname of the current working directory,
  --   and put it in SIZE bytes of BUF.  Returns NULL if the
  --   directory couldn't be determined or SIZE was too small.
  --   If successful, returns BUF.  In GNU, if BUF is NULL,
  --   an array is allocated with `malloc'; the array is SIZE
  --   bytes long, unless SIZE == 0, in which case it is as
  --   big as necessary.   

   function getcwd (uu_buf : Interfaces.C.Strings.chars_ptr; uu_size : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/unistd.h:511
   with Import => True, 
        Convention => C, 
        External_Name => "getcwd";

  -- Return a malloc'd string containing the current directory name.
  --   If the environment variable `PWD' is set, and its value is correct,
  --   that value is used.   

   function get_current_dir_name return Interfaces.C.Strings.chars_ptr  -- /usr/include/unistd.h:517
   with Import => True, 
        Convention => C, 
        External_Name => "get_current_dir_name";

  -- Put the absolute pathname of the current working directory in BUF.
  --   If successful, return BUF.  If not, put an error message in
  --   BUF and return NULL.  BUF should be at least PATH_MAX bytes long.   

   function getwd (uu_buf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/unistd.h:525
   with Import => True, 
        Convention => C, 
        External_Name => "getwd";

  -- Duplicate FD, returning a new file descriptor on the same file.   
   function dup (uu_fd : int) return int  -- /usr/include/unistd.h:531
   with Import => True, 
        Convention => C, 
        External_Name => "dup";

  -- Duplicate FD to FD2, closing FD2 and making it open on the same file.   
   function dup2 (uu_fd : int; uu_fd2 : int) return int  -- /usr/include/unistd.h:534
   with Import => True, 
        Convention => C, 
        External_Name => "dup2";

  -- Duplicate FD to FD2, closing FD2 and making it open on the same
  --   file while setting flags according to FLAGS.   

   function dup3
     (uu_fd : int;
      uu_fd2 : int;
      uu_flags : int) return int  -- /usr/include/unistd.h:539
   with Import => True, 
        Convention => C, 
        External_Name => "dup3";

  -- NULL-terminated array of "NAME=VALUE" environment variables.   
   environ : System.Address  -- /usr/include/unistd.h:545
   with Import => True, 
        Convention => C, 
        External_Name => "environ";

  -- Replace the current process, executing PATH with arguments ARGV and
  --   environment ENVP.  ARGV and ENVP are terminated by NULL pointers.   

   function execve
     (uu_path : Interfaces.C.Strings.chars_ptr;
      uu_argv : System.Address;
      uu_envp : System.Address) return int  -- /usr/include/unistd.h:551
   with Import => True, 
        Convention => C, 
        External_Name => "execve";

  -- Execute the file FD refers to, overlaying the running program image.
  --   ARGV and ENVP are passed to the new program, as for `execve'.   

   function fexecve
     (uu_fd : int;
      uu_argv : System.Address;
      uu_envp : System.Address) return int  -- /usr/include/unistd.h:557
   with Import => True, 
        Convention => C, 
        External_Name => "fexecve";

  -- Execute PATH with arguments ARGV and environment from `environ'.   
   function execv (uu_path : Interfaces.C.Strings.chars_ptr; uu_argv : System.Address) return int  -- /usr/include/unistd.h:563
   with Import => True, 
        Convention => C, 
        External_Name => "execv";

  -- Execute PATH with all arguments after PATH until a NULL pointer,
  --   and the argument after that for environment.   

   function execle (uu_path : Interfaces.C.Strings.chars_ptr; uu_arg : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/include/unistd.h:568
   with Import => True, 
        Convention => C, 
        External_Name => "execle";

  -- Execute PATH with all arguments after PATH until
  --   a NULL pointer and environment from `environ'.   

   function execl (uu_path : Interfaces.C.Strings.chars_ptr; uu_arg : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/include/unistd.h:573
   with Import => True, 
        Convention => C, 
        External_Name => "execl";

  -- Execute FILE, searching in the `PATH' environment variable if it contains
  --   no slashes, with arguments ARGV and environment from `environ'.   

   function execvp (uu_file : Interfaces.C.Strings.chars_ptr; uu_argv : System.Address) return int  -- /usr/include/unistd.h:578
   with Import => True, 
        Convention => C, 
        External_Name => "execvp";

  -- Execute FILE, searching in the `PATH' environment variable if
  --   it contains no slashes, with all arguments after FILE until a
  --   NULL pointer and environment from `environ'.   

   function execlp (uu_file : Interfaces.C.Strings.chars_ptr; uu_arg : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/include/unistd.h:584
   with Import => True, 
        Convention => C, 
        External_Name => "execlp";

  -- Execute FILE, searching in the `PATH' environment variable if it contains
  --   no slashes, with arguments ARGV and environment from `environ'.   

   function execvpe
     (uu_file : Interfaces.C.Strings.chars_ptr;
      uu_argv : System.Address;
      uu_envp : System.Address) return int  -- /usr/include/unistd.h:590
   with Import => True, 
        Convention => C, 
        External_Name => "execvpe";

  -- Add INC to priority of the current process.   
   function nice (uu_inc : int) return int  -- /usr/include/unistd.h:598
   with Import => True, 
        Convention => C, 
        External_Name => "nice";

  -- Terminate program execution with the low-order 8 bits of STATUS.   
   --  skipped func _exit

  -- Get the `_PC_*' symbols for the NAME argument to `pathconf' and `fpathconf';
  --   the `_SC_*' symbols for the NAME argument to `sysconf';
  --   and the `_CS_*' symbols for the NAME argument to `confstr'.   

  -- Get file-specific configuration information about PATH.   
   function pathconf (uu_path : Interfaces.C.Strings.chars_ptr; uu_name : int) return long  -- /usr/include/unistd.h:612
   with Import => True, 
        Convention => C, 
        External_Name => "pathconf";

  -- Get file-specific configuration about descriptor FD.   
   function fpathconf (uu_fd : int; uu_name : int) return long  -- /usr/include/unistd.h:616
   with Import => True, 
        Convention => C, 
        External_Name => "fpathconf";

  -- Get the value of the system variable NAME.   
   function sysconf (uu_name : int) return long  -- /usr/include/unistd.h:619
   with Import => True, 
        Convention => C, 
        External_Name => "sysconf";

  -- Get the value of the string-valued system variable NAME.   
   function confstr
     (uu_name : int;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_len : stddef_h.size_t) return stddef_h.size_t  -- /usr/include/unistd.h:623
   with Import => True, 
        Convention => C, 
        External_Name => "confstr";

  -- Get the process ID of the calling process.   
   function getpid return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/unistd.h:628
   with Import => True, 
        Convention => C, 
        External_Name => "getpid";

  -- Get the process ID of the calling process's parent.   
   function getppid return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/unistd.h:631
   with Import => True, 
        Convention => C, 
        External_Name => "getppid";

  -- Get the process group ID of the calling process.   
   function getpgrp return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/unistd.h:634
   with Import => True, 
        Convention => C, 
        External_Name => "getpgrp";

  -- Get the process group ID of process PID.   
   --  skipped func __getpgid

   function getpgid (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t) return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/unistd.h:639
   with Import => True, 
        Convention => C, 
        External_Name => "getpgid";

  -- Set the process group ID of the process matching PID to PGID.
  --   If PID is zero, the current process's process group ID is set.
  --   If PGID is zero, the process ID of the process is used.   

   function setpgid (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t; uu_pgid : x86_64_linux_gnu_bits_types_h.uu_pid_t) return int  -- /usr/include/unistd.h:646
   with Import => True, 
        Convention => C, 
        External_Name => "setpgid";

  -- Both System V and BSD have `setpgrp' functions, but with different
  --   calling conventions.  The BSD function is the same as POSIX.1 `setpgid'
  --   (above).  The System V function takes no arguments and puts the calling
  --   process in its on group like `setpgid (0, 0)'.
  --   New programs should always use `setpgid' instead.
  --   GNU provides the POSIX.1 function.   

  -- Set the process group ID of the calling process to its own PID.
  --   This is exactly the same as `setpgid (0, 0)'.   

   function setpgrp return int  -- /usr/include/unistd.h:660
   with Import => True, 
        Convention => C, 
        External_Name => "setpgrp";

  -- Create a new session with the calling process as its leader.
  --   The process group IDs of the session and the calling process
  --   are set to the process ID of the calling process, which is returned.   

   function setsid return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/unistd.h:667
   with Import => True, 
        Convention => C, 
        External_Name => "setsid";

  -- Return the session ID of the given process.   
   function getsid (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t) return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/unistd.h:671
   with Import => True, 
        Convention => C, 
        External_Name => "getsid";

  -- Get the real user ID of the calling process.   
   function getuid return x86_64_linux_gnu_bits_types_h.uu_uid_t  -- /usr/include/unistd.h:675
   with Import => True, 
        Convention => C, 
        External_Name => "getuid";

  -- Get the effective user ID of the calling process.   
   function geteuid return x86_64_linux_gnu_bits_types_h.uu_uid_t  -- /usr/include/unistd.h:678
   with Import => True, 
        Convention => C, 
        External_Name => "geteuid";

  -- Get the real group ID of the calling process.   
   function getgid return x86_64_linux_gnu_bits_types_h.uu_gid_t  -- /usr/include/unistd.h:681
   with Import => True, 
        Convention => C, 
        External_Name => "getgid";

  -- Get the effective group ID of the calling process.   
   function getegid return x86_64_linux_gnu_bits_types_h.uu_gid_t  -- /usr/include/unistd.h:684
   with Import => True, 
        Convention => C, 
        External_Name => "getegid";

  -- If SIZE is zero, return the number of supplementary groups
  --   the calling process is in.  Otherwise, fill in the group IDs
  --   of its supplementary groups in LIST and return the number written.   

   function getgroups (uu_size : int; uu_list : access x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/unistd.h:689
   with Import => True, 
        Convention => C, 
        External_Name => "getgroups";

  -- Return nonzero iff the calling process is in group GID.   
   function group_member (uu_gid : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/unistd.h:693
   with Import => True, 
        Convention => C, 
        External_Name => "group_member";

  -- Set the user ID of the calling process to UID.
  --   If the calling process is the super-user, set the real
  --   and effective user IDs, and the saved set-user-ID to UID;
  --   if not, the effective user ID is set to UID.   

   function setuid (uu_uid : x86_64_linux_gnu_bits_types_h.uu_uid_t) return int  -- /usr/include/unistd.h:700
   with Import => True, 
        Convention => C, 
        External_Name => "setuid";

  -- Set the real user ID of the calling process to RUID,
  --   and the effective user ID of the calling process to EUID.   

   function setreuid (uu_ruid : x86_64_linux_gnu_bits_types_h.uu_uid_t; uu_euid : x86_64_linux_gnu_bits_types_h.uu_uid_t) return int  -- /usr/include/unistd.h:705
   with Import => True, 
        Convention => C, 
        External_Name => "setreuid";

  -- Set the effective user ID of the calling process to UID.   
   function seteuid (uu_uid : x86_64_linux_gnu_bits_types_h.uu_uid_t) return int  -- /usr/include/unistd.h:710
   with Import => True, 
        Convention => C, 
        External_Name => "seteuid";

  -- Set the group ID of the calling process to GID.
  --   If the calling process is the super-user, set the real
  --   and effective group IDs, and the saved set-group-ID to GID;
  --   if not, the effective group ID is set to GID.   

   function setgid (uu_gid : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/unistd.h:717
   with Import => True, 
        Convention => C, 
        External_Name => "setgid";

  -- Set the real group ID of the calling process to RGID,
  --   and the effective group ID of the calling process to EGID.   

   function setregid (uu_rgid : x86_64_linux_gnu_bits_types_h.uu_gid_t; uu_egid : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/unistd.h:722
   with Import => True, 
        Convention => C, 
        External_Name => "setregid";

  -- Set the effective group ID of the calling process to GID.   
   function setegid (uu_gid : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/unistd.h:727
   with Import => True, 
        Convention => C, 
        External_Name => "setegid";

  -- Fetch the real user ID, effective user ID, and saved-set user ID,
  --   of the calling process.   

   function getresuid
     (uu_ruid : access x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_euid : access x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_suid : access x86_64_linux_gnu_bits_types_h.uu_uid_t) return int  -- /usr/include/unistd.h:733
   with Import => True, 
        Convention => C, 
        External_Name => "getresuid";

  -- Fetch the real group ID, effective group ID, and saved-set group ID,
  --   of the calling process.   

   function getresgid
     (uu_rgid : access x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_egid : access x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_sgid : access x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/unistd.h:738
   with Import => True, 
        Convention => C, 
        External_Name => "getresgid";

  -- Set the real user ID, effective user ID, and saved-set user ID,
  --   of the calling process to RUID, EUID, and SUID, respectively.   

   function setresuid
     (uu_ruid : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_euid : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_suid : x86_64_linux_gnu_bits_types_h.uu_uid_t) return int  -- /usr/include/unistd.h:743
   with Import => True, 
        Convention => C, 
        External_Name => "setresuid";

  -- Set the real group ID, effective group ID, and saved-set group ID,
  --   of the calling process to RGID, EGID, and SGID, respectively.   

   function setresgid
     (uu_rgid : x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_egid : x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_sgid : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/unistd.h:748
   with Import => True, 
        Convention => C, 
        External_Name => "setresgid";

  -- Clone the calling process, creating an exact copy.
  --   Return -1 for errors, 0 to the new process,
  --   and the process ID of the new process to the old process.   

   function fork return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/unistd.h:756
   with Import => True, 
        Convention => C, 
        External_Name => "fork";

  -- Clone the calling process, but without copying the whole address space.
  --   The calling process is suspended until the new process exits or is
  --   replaced by a call to `execve'.  Return -1 for errors, 0 to the new process,
  --   and the process ID of the new process to the old process.   

   function vfork return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/unistd.h:764
   with Import => True, 
        Convention => C, 
        External_Name => "vfork";

  -- Return the pathname of the terminal FD is open on, or NULL on errors.
  --   The returned storage is good only until the next call to this function.   

   function ttyname (uu_fd : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/unistd.h:770
   with Import => True, 
        Convention => C, 
        External_Name => "ttyname";

  -- Store at most BUFLEN characters of the pathname of the terminal FD is
  --   open on in BUF.  Return 0 on success, otherwise an error number.   

   function ttyname_r
     (uu_fd : int;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t) return int  -- /usr/include/unistd.h:774
   with Import => True, 
        Convention => C, 
        External_Name => "ttyname_r";

  -- Return 1 if FD is a valid descriptor associated
  --   with a terminal, zero if not.   

   function isatty (uu_fd : int) return int  -- /usr/include/unistd.h:779
   with Import => True, 
        Convention => C, 
        External_Name => "isatty";

  -- Return the index into the active-logins file (utmp) for
  --   the controlling terminal.   

   function ttyslot return int  -- /usr/include/unistd.h:785
   with Import => True, 
        Convention => C, 
        External_Name => "ttyslot";

  -- Make a link to FROM named TO.   
   function link (uu_from : Interfaces.C.Strings.chars_ptr; uu_to : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/unistd.h:790
   with Import => True, 
        Convention => C, 
        External_Name => "link";

  -- Like link but relative paths in TO and FROM are interpreted relative
  --   to FROMFD and TOFD respectively.   

   function linkat
     (uu_fromfd : int;
      uu_from : Interfaces.C.Strings.chars_ptr;
      uu_tofd : int;
      uu_to : Interfaces.C.Strings.chars_ptr;
      uu_flags : int) return int  -- /usr/include/unistd.h:796
   with Import => True, 
        Convention => C, 
        External_Name => "linkat";

  -- Make a symbolic link to FROM named TO.   
   function symlink (uu_from : Interfaces.C.Strings.chars_ptr; uu_to : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/unistd.h:803
   with Import => True, 
        Convention => C, 
        External_Name => "symlink";

  -- Read the contents of the symbolic link PATH into no more than
  --   LEN bytes of BUF.  The contents are not null-terminated.
  --   Returns the number of characters read, or -1 for errors.   

   function readlink
     (uu_path : Interfaces.C.Strings.chars_ptr;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_len : stddef_h.size_t) return stdio_h.ssize_t  -- /usr/include/unistd.h:809
   with Import => True, 
        Convention => C, 
        External_Name => "readlink";

  -- Like symlink but a relative path in TO is interpreted relative to TOFD.   
   function symlinkat
     (uu_from : Interfaces.C.Strings.chars_ptr;
      uu_tofd : int;
      uu_to : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/unistd.h:816
   with Import => True, 
        Convention => C, 
        External_Name => "symlinkat";

  -- Like readlink but a relative PATH is interpreted relative to FD.   
   function readlinkat
     (uu_fd : int;
      uu_path : Interfaces.C.Strings.chars_ptr;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_len : stddef_h.size_t) return stdio_h.ssize_t  -- /usr/include/unistd.h:820
   with Import => True, 
        Convention => C, 
        External_Name => "readlinkat";

  -- Remove the link NAME.   
   function unlink (uu_name : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/unistd.h:826
   with Import => True, 
        Convention => C, 
        External_Name => "unlink";

  -- Remove the link NAME relative to FD.   
   function unlinkat
     (uu_fd : int;
      uu_name : Interfaces.C.Strings.chars_ptr;
      uu_flag : int) return int  -- /usr/include/unistd.h:830
   with Import => True, 
        Convention => C, 
        External_Name => "unlinkat";

  -- Remove the directory PATH.   
   function rmdir (uu_path : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/unistd.h:835
   with Import => True, 
        Convention => C, 
        External_Name => "rmdir";

  -- Return the foreground process group ID of FD.   
   function tcgetpgrp (uu_fd : int) return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- /usr/include/unistd.h:839
   with Import => True, 
        Convention => C, 
        External_Name => "tcgetpgrp";

  -- Set the foreground process group ID of FD set PGRP_ID.   
   function tcsetpgrp (uu_fd : int; uu_pgrp_id : x86_64_linux_gnu_bits_types_h.uu_pid_t) return int  -- /usr/include/unistd.h:842
   with Import => True, 
        Convention => C, 
        External_Name => "tcsetpgrp";

  -- Return the login name of the user.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function getlogin return Interfaces.C.Strings.chars_ptr  -- /usr/include/unistd.h:849
   with Import => True, 
        Convention => C, 
        External_Name => "getlogin";

  -- Return at most NAME_LEN characters of the login name of the user in NAME.
  --   If it cannot be determined or some other error occurred, return the error
  --   code.  Otherwise return 0.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function getlogin_r (uu_name : Interfaces.C.Strings.chars_ptr; uu_name_len : stddef_h.size_t) return int  -- /usr/include/unistd.h:857
   with Import => True, 
        Convention => C, 
        External_Name => "getlogin_r";

  -- Set the login name returned by `getlogin'.   
   function setlogin (uu_name : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/unistd.h:862
   with Import => True, 
        Convention => C, 
        External_Name => "setlogin";

  -- Get definitions and prototypes for functions to process the
  --   arguments in ARGV (ARGC of them, minus the program name) for
  --   options given in OPTS.   

  -- Put the name of the current host in no more than LEN bytes of NAME.
  --   The result is null-terminated if LEN is large enough for the full
  --   name and the terminator.   

   function gethostname (uu_name : Interfaces.C.Strings.chars_ptr; uu_len : stddef_h.size_t) return int  -- /usr/include/unistd.h:879
   with Import => True, 
        Convention => C, 
        External_Name => "gethostname";

  -- Set the name of the current host to NAME, which is LEN bytes long.
  --   This call is restricted to the super-user.   

   function sethostname (uu_name : Interfaces.C.Strings.chars_ptr; uu_len : stddef_h.size_t) return int  -- /usr/include/unistd.h:886
   with Import => True, 
        Convention => C, 
        External_Name => "sethostname";

  -- Set the current machine's Internet number to ID.
  --   This call is restricted to the super-user.   

   function sethostid (uu_id : long) return int  -- /usr/include/unistd.h:891
   with Import => True, 
        Convention => C, 
        External_Name => "sethostid";

  -- Get and set the NIS (aka YP) domain name, if any.
  --   Called just like `gethostname' and `sethostname'.
  --   The NIS domain name is usually the empty string when not using NIS.   

   function getdomainname (uu_name : Interfaces.C.Strings.chars_ptr; uu_len : stddef_h.size_t) return int  -- /usr/include/unistd.h:897
   with Import => True, 
        Convention => C, 
        External_Name => "getdomainname";

   function setdomainname (uu_name : Interfaces.C.Strings.chars_ptr; uu_len : stddef_h.size_t) return int  -- /usr/include/unistd.h:899
   with Import => True, 
        Convention => C, 
        External_Name => "setdomainname";

  -- Revoke access permissions to all processes currently communicating
  --   with the control terminal, and then send a SIGHUP signal to the process
  --   group of the control terminal.   

   function vhangup return int  -- /usr/include/unistd.h:906
   with Import => True, 
        Convention => C, 
        External_Name => "vhangup";

  -- Revoke the access of all descriptors currently open on FILE.   
   function revoke (uu_file : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/unistd.h:909
   with Import => True, 
        Convention => C, 
        External_Name => "revoke";

  -- Enable statistical profiling, writing samples of the PC into at most
  --   SIZE bytes of SAMPLE_BUFFER; every processor clock tick while profiling
  --   is enabled, the system examines the user PC and increments
  --   SAMPLE_BUFFER[((PC - OFFSET) / 2) * SCALE / 65536].  If SCALE is zero,
  --   disable profiling.  Returns zero on success, -1 on error.   

   function profil
     (uu_sample_buffer : access unsigned_short;
      uu_size : stddef_h.size_t;
      uu_offset : stddef_h.size_t;
      uu_scale : unsigned) return int  -- /usr/include/unistd.h:917
   with Import => True, 
        Convention => C, 
        External_Name => "profil";

  -- Turn accounting on if NAME is an existing file.  The system will then write
  --   a record for each process as it terminates, to this file.  If NAME is NULL,
  --   turn accounting off.  This call is restricted to the super-user.   

   function acct (uu_name : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/unistd.h:925
   with Import => True, 
        Convention => C, 
        External_Name => "acct";

  -- Successive calls return the shells listed in `/etc/shells'.   
   function getusershell return Interfaces.C.Strings.chars_ptr  -- /usr/include/unistd.h:929
   with Import => True, 
        Convention => C, 
        External_Name => "getusershell";

  -- Discard cached info.   
   procedure endusershell  -- /usr/include/unistd.h:930
   with Import => True, 
        Convention => C, 
        External_Name => "endusershell";

  -- Rewind and re-read the file.   
   procedure setusershell  -- /usr/include/unistd.h:931
   with Import => True, 
        Convention => C, 
        External_Name => "setusershell";

  -- Put the program in the background, and dissociate from the controlling
  --   terminal.  If NOCHDIR is zero, do `chdir ("/")'.  If NOCLOSE is zero,
  --   redirects stdin, stdout, and stderr to /dev/null.   

   function daemon (uu_nochdir : int; uu_noclose : int) return int  -- /usr/include/unistd.h:937
   with Import => True, 
        Convention => C, 
        External_Name => "daemon";

  -- Make PATH be the root directory (the starting point for absolute paths).
  --   This call is restricted to the super-user.   

   function chroot (uu_path : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/unistd.h:944
   with Import => True, 
        Convention => C, 
        External_Name => "chroot";

  -- Prompt with PROMPT and read a string from the terminal without echoing.
  --   Uses /dev/tty if possible; otherwise stderr and stdin.   

   function getpass (uu_prompt : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/unistd.h:948
   with Import => True, 
        Convention => C, 
        External_Name => "getpass";

  -- Make all changes done to FD actually appear on disk.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function fsync (uu_fd : int) return int  -- /usr/include/unistd.h:956
   with Import => True, 
        Convention => C, 
        External_Name => "fsync";

  -- Make all changes done to all files on the file system associated
  --   with FD actually appear on disk.   

   function syncfs (uu_fd : int) return int  -- /usr/include/unistd.h:962
   with Import => True, 
        Convention => C, 
        External_Name => "syncfs";

  -- Return identifier for the current host.   
   function gethostid return long  -- /usr/include/unistd.h:969
   with Import => True, 
        Convention => C, 
        External_Name => "gethostid";

  -- Make all changes done to all files actually appear on disk.   
   procedure sync  -- /usr/include/unistd.h:972
   with Import => True, 
        Convention => C, 
        External_Name => "sync";

  -- Return the number of bytes in a page.  This is the system's page size,
  --   which is not necessarily the same as the hardware page size.   

   function getpagesize return int  -- /usr/include/unistd.h:978
   with Import => True, 
        Convention => C, 
        External_Name => "getpagesize";

  -- Return the maximum number of file descriptors
  --   the current process could possibly have.   

   function getdtablesize return int  -- /usr/include/unistd.h:983
   with Import => True, 
        Convention => C, 
        External_Name => "getdtablesize";

  -- Truncate FILE to LENGTH bytes.   
   function truncate (uu_file : Interfaces.C.Strings.chars_ptr; uu_length : x86_64_linux_gnu_bits_types_h.uu_off_t) return int  -- /usr/include/unistd.h:993
   with Import => True, 
        Convention => C, 
        External_Name => "truncate";

   function truncate64 (uu_file : Interfaces.C.Strings.chars_ptr; uu_length : x86_64_linux_gnu_bits_types_h.uu_off64_t) return int  -- /usr/include/unistd.h:1005
   with Import => True, 
        Convention => C, 
        External_Name => "truncate64";

  -- Truncate the file FD is open on to LENGTH bytes.   
   function ftruncate (uu_fd : int; uu_length : x86_64_linux_gnu_bits_types_h.uu_off_t) return int  -- /usr/include/unistd.h:1016
   with Import => True, 
        Convention => C, 
        External_Name => "ftruncate";

   function ftruncate64 (uu_fd : int; uu_length : x86_64_linux_gnu_bits_types_h.uu_off64_t) return int  -- /usr/include/unistd.h:1026
   with Import => True, 
        Convention => C, 
        External_Name => "ftruncate64";

  -- Set the end of accessible data space (aka "the break") to ADDR.
  --   Returns zero on success and -1 for errors (with errno set).   

   function brk (uu_addr : System.Address) return int  -- /usr/include/unistd.h:1037
   with Import => True, 
        Convention => C, 
        External_Name => "brk";

  -- Increase or decrease the end of accessible data space by DELTA bytes.
  --   If successful, returns the address the previous end of data space
  --   (i.e. the beginning of the new space, if DELTA > 0);
  --   returns (void *) -1 for errors (with errno set).   

   function sbrk (uu_delta : stdint_h.intptr_t) return System.Address  -- /usr/include/unistd.h:1043
   with Import => True, 
        Convention => C, 
        External_Name => "sbrk";

  -- Invoke `system call' number SYSNO, passing it the remaining arguments.
  --   This is completely system-dependent, and not often useful.
  --   In Unix, `syscall' sets `errno' for all errors and most calls return -1
  --   for errors; in many systems you cannot pass arguments or get return
  --   values for all system calls (`pipe', `fork', and `getppid' typically
  --   among them).
  --   In Mach, all system calls take normal arguments and always return an
  --   error code (zero for success).   

   function syscall (uu_sysno : long  -- , ...
      ) return long  -- /usr/include/unistd.h:1058
   with Import => True, 
        Convention => C, 
        External_Name => "syscall";

  -- NOTE: These declarations also appear in <fcntl.h>; be sure to keep both
  --   files consistent.  Some systems have them there and some here, and some
  --   software depends on the macros being defined without including both.   

  -- `lockf' is a simpler interface to the locking facilities of `fcntl'.
  --   LEN is always relative to the current file position.
  --   The CMD argument is one of the following.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

  -- Evaluate EXPRESSION, and repeat as long as it returns -1 with `errno'
  --   set to EINTR.   

  -- Synchronize at least the data part of a file with the underlying
  --   media.   

   function fdatasync (uu_fildes : int) return int  -- /usr/include/unistd.h:1112
   with Import => True, 
        Convention => C, 
        External_Name => "fdatasync";

  -- XPG4.2 specifies that prototypes for the encryption functions must
  --   be defined here.   

  -- Encrypt at most 8 characters from KEY using salt to perturb DES.   
   function crypt (uu_key : Interfaces.C.Strings.chars_ptr; uu_salt : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/unistd.h:1120
   with Import => True, 
        Convention => C, 
        External_Name => "crypt";

  -- Encrypt data in BLOCK in place if EDFLAG is zero; otherwise decrypt
  --   block in place.   

   procedure encrypt (uu_glibc_block : Interfaces.C.Strings.chars_ptr; uu_edflag : int)  -- /usr/include/unistd.h:1125
   with Import => True, 
        Convention => C, 
        External_Name => "encrypt";

  -- Swab pairs bytes in the first N bytes of the area pointed to by
  --   FROM and copy the result to TO.  The value of TO must not be in the
  --   range [FROM - N + 1, FROM - 1].  If N is odd the first byte in FROM
  --   is without partner.   

   procedure swab
     (uu_from : System.Address;
      uu_to : System.Address;
      uu_n : stdio_h.ssize_t)  -- /usr/include/unistd.h:1133
   with Import => True, 
        Convention => C, 
        External_Name => "swab";

  -- The Single Unix specification demands this prototype to be here.
  --   It is also found in <stdio.h>.   

  -- Return the name of the controlling terminal.   
  -- Define some macros helping to catch buffer overflows.   
end unistd_h;
