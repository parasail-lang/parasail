pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
limited with x86_64_linux_gnu_bits_stat_h;
with x86_64_linux_gnu_bits_types_h;
limited with time_h;

package x86_64_linux_gnu_sys_stat_h is

   --  unsupported macro: S_IFMT __S_IFMT
   --  unsupported macro: S_IFDIR __S_IFDIR
   --  unsupported macro: S_IFCHR __S_IFCHR
   --  unsupported macro: S_IFBLK __S_IFBLK
   --  unsupported macro: S_IFREG __S_IFREG
   --  unsupported macro: S_IFIFO __S_IFIFO
   --  unsupported macro: S_IFLNK __S_IFLNK
   --  unsupported macro: S_IFSOCK __S_IFSOCK
   --  arg-macro: procedure S_ISDIR (mode)
   --    __S_ISTYPE((mode), __S_IFDIR)
   --  arg-macro: procedure S_ISCHR (mode)
   --    __S_ISTYPE((mode), __S_IFCHR)
   --  arg-macro: procedure S_ISBLK (mode)
   --    __S_ISTYPE((mode), __S_IFBLK)
   --  arg-macro: procedure S_ISREG (mode)
   --    __S_ISTYPE((mode), __S_IFREG)
   --  arg-macro: procedure S_ISFIFO (mode)
   --    __S_ISTYPE((mode), __S_IFIFO)
   --  arg-macro: procedure S_ISLNK (mode)
   --    __S_ISTYPE((mode), __S_IFLNK)
   --  arg-macro: procedure S_ISSOCK (mode)
   --    __S_ISTYPE((mode), __S_IFSOCK)
   --  arg-macro: procedure S_TYPEISMQ (buf)
   --    __S_TYPEISMQ(buf)
   --  arg-macro: procedure S_TYPEISSEM (buf)
   --    __S_TYPEISSEM(buf)
   --  arg-macro: procedure S_TYPEISSHM (buf)
   --    __S_TYPEISSHM(buf)
   --  unsupported macro: S_ISUID __S_ISUID
   --  unsupported macro: S_ISGID __S_ISGID
   --  unsupported macro: S_ISVTX __S_ISVTX
   --  unsupported macro: S_IRUSR __S_IREAD
   --  unsupported macro: S_IWUSR __S_IWRITE
   --  unsupported macro: S_IXUSR __S_IEXEC
   --  unsupported macro: S_IRWXU (__S_IREAD|__S_IWRITE|__S_IEXEC)
   --  unsupported macro: S_IREAD S_IRUSR
   --  unsupported macro: S_IWRITE S_IWUSR
   --  unsupported macro: S_IEXEC S_IXUSR
   --  unsupported macro: S_IRGRP (S_IRUSR >> 3)
   --  unsupported macro: S_IWGRP (S_IWUSR >> 3)
   --  unsupported macro: S_IXGRP (S_IXUSR >> 3)
   --  unsupported macro: S_IRWXG (S_IRWXU >> 3)
   --  unsupported macro: S_IROTH (S_IRGRP >> 3)
   --  unsupported macro: S_IWOTH (S_IWGRP >> 3)
   --  unsupported macro: S_IXOTH (S_IXGRP >> 3)
   --  unsupported macro: S_IRWXO (S_IRWXG >> 3)
   --  unsupported macro: ACCESSPERMS (S_IRWXU|S_IRWXG|S_IRWXO)
   --  unsupported macro: ALLPERMS (S_ISUID|S_ISGID|S_ISVTX|S_IRWXU|S_IRWXG|S_IRWXO)
   --  unsupported macro: DEFFILEMODE (S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)
   S_BLKSIZE : constant := 512;  --  /usr/include/x86_64-linux-gnu/sys/stat.h:203

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

  -- *	POSIX Standard: 5.6 File Characteristics	<sys/stat.h>
  --  

  -- For __mode_t and __dev_t.   
  -- For time_t resp. timespec.   
  -- The Single Unix specification says that some more types are
  --   available here.   

  -- Test macros for file types.	 
  -- These are from POSIX.1b.  If the objects are not implemented using separate
  --   distinct file types, the macros always will evaluate to zero.  Unlike the
  --   other S_* macros the following three take a pointer to a `struct stat'
  --   object as the argument.   

  -- Protection bits.   
  -- Save swapped text after use (sticky bit).  This is pretty well obsolete.   
  -- Read, write, and execute by owner.   
  -- Read, write, and execute by group.   
  -- Read, write, and execute by others.   
  -- Macros for common mode bit masks.   
  -- Get file attributes for FILE and put them in BUF.   
   function stat (uu_file : Interfaces.C.Strings.chars_ptr; uu_buf : access x86_64_linux_gnu_bits_stat_h.stat) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:209
   with Import => True, 
        Convention => C, 
        External_Name => "stat";

  -- Get file attributes for the file, device, pipe, or socket
  --   that file descriptor FD is open on and put them in BUF.   

   function fstat (uu_fd : int; uu_buf : access x86_64_linux_gnu_bits_stat_h.stat) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:214
   with Import => True, 
        Convention => C, 
        External_Name => "fstat";

   function stat64 (uu_file : Interfaces.C.Strings.chars_ptr; uu_buf : access x86_64_linux_gnu_bits_stat_h.stat64) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:228
   with Import => True, 
        Convention => C, 
        External_Name => "stat64";

   function fstat64 (uu_fd : int; uu_buf : access x86_64_linux_gnu_bits_stat_h.stat64) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:230
   with Import => True, 
        Convention => C, 
        External_Name => "fstat64";

  -- Similar to stat, get the attributes for FILE and put them in BUF.
  --   Relative path names are interpreted relative to FD unless FD is
  --   AT_FDCWD.   

   function fstatat
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_buf : access x86_64_linux_gnu_bits_stat_h.stat;
      uu_flag : int) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:238
   with Import => True, 
        Convention => C, 
        External_Name => "fstatat";

   function fstatat64
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_buf : access x86_64_linux_gnu_bits_stat_h.stat64;
      uu_flag : int) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:253
   with Import => True, 
        Convention => C, 
        External_Name => "fstatat64";

  -- Get file attributes about FILE and put them in BUF.
  --   If FILE is a symbolic link, do not follow it.   

   function lstat (uu_file : Interfaces.C.Strings.chars_ptr; uu_buf : access x86_64_linux_gnu_bits_stat_h.stat) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:263
   with Import => True, 
        Convention => C, 
        External_Name => "lstat";

   function lstat64 (uu_file : Interfaces.C.Strings.chars_ptr; uu_buf : access x86_64_linux_gnu_bits_stat_h.stat64) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:276
   with Import => True, 
        Convention => C, 
        External_Name => "lstat64";

  -- Set file access permissions for FILE to MODE.
  --   If FILE is a symbolic link, this affects its target instead.   

   function chmod (uu_file : Interfaces.C.Strings.chars_ptr; uu_mode : x86_64_linux_gnu_bits_types_h.uu_mode_t) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:284
   with Import => True, 
        Convention => C, 
        External_Name => "chmod";

  -- Set file access permissions for FILE to MODE.
  --   If FILE is a symbolic link, this affects the link itself
  --   rather than its target.   

   function lchmod (uu_file : Interfaces.C.Strings.chars_ptr; uu_mode : x86_64_linux_gnu_bits_types_h.uu_mode_t) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:291
   with Import => True, 
        Convention => C, 
        External_Name => "lchmod";

  -- Set file access permissions of the file FD is open on to MODE.   
   function fchmod (uu_fd : int; uu_mode : x86_64_linux_gnu_bits_types_h.uu_mode_t) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:297
   with Import => True, 
        Convention => C, 
        External_Name => "fchmod";

  -- Set file access permissions of FILE relative to
  --   the directory FD is open on.   

   function fchmodat
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_mode : x86_64_linux_gnu_bits_types_h.uu_mode_t;
      uu_flag : int) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:303
   with Import => True, 
        Convention => C, 
        External_Name => "fchmodat";

  -- Set the file creation mask of the current process to MASK,
  --   and return the old creation mask.   

   function umask (uu_mask : x86_64_linux_gnu_bits_types_h.uu_mode_t) return x86_64_linux_gnu_bits_types_h.uu_mode_t  -- /usr/include/x86_64-linux-gnu/sys/stat.h:312
   with Import => True, 
        Convention => C, 
        External_Name => "umask";

  -- Get the current `umask' value without changing it.
  --   This function is only available under the GNU Hurd.   

   function getumask return x86_64_linux_gnu_bits_types_h.uu_mode_t  -- /usr/include/x86_64-linux-gnu/sys/stat.h:317
   with Import => True, 
        Convention => C, 
        External_Name => "getumask";

  -- Create a new directory named PATH, with permission bits MODE.   
   function mkdir (uu_path : Interfaces.C.Strings.chars_ptr; uu_mode : x86_64_linux_gnu_bits_types_h.uu_mode_t) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:321
   with Import => True, 
        Convention => C, 
        External_Name => "mkdir";

  -- Like mkdir, create a new directory with permission bits MODE.  But
  --   interpret relative PATH names relative to the directory associated
  --   with FD.   

   function mkdirat
     (uu_fd : int;
      uu_path : Interfaces.C.Strings.chars_ptr;
      uu_mode : x86_64_linux_gnu_bits_types_h.uu_mode_t) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:328
   with Import => True, 
        Convention => C, 
        External_Name => "mkdirat";

  -- Create a device file named PATH, with permission and special bits MODE
  --   and device number DEV (which can be constructed from major and minor
  --   device numbers with the `makedev' macro above).   

   function mknod
     (uu_path : Interfaces.C.Strings.chars_ptr;
      uu_mode : x86_64_linux_gnu_bits_types_h.uu_mode_t;
      uu_dev : x86_64_linux_gnu_bits_types_h.uu_dev_t) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:336
   with Import => True, 
        Convention => C, 
        External_Name => "mknod";

  -- Like mknod, create a new device file with permission bits MODE and
  --   device number DEV.  But interpret relative PATH names relative to
  --   the directory associated with FD.   

   function mknodat
     (uu_fd : int;
      uu_path : Interfaces.C.Strings.chars_ptr;
      uu_mode : x86_64_linux_gnu_bits_types_h.uu_mode_t;
      uu_dev : x86_64_linux_gnu_bits_types_h.uu_dev_t) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:343
   with Import => True, 
        Convention => C, 
        External_Name => "mknodat";

  -- Create a new FIFO named PATH, with permission bits MODE.   
   function mkfifo (uu_path : Interfaces.C.Strings.chars_ptr; uu_mode : x86_64_linux_gnu_bits_types_h.uu_mode_t) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:350
   with Import => True, 
        Convention => C, 
        External_Name => "mkfifo";

  -- Like mkfifo, create a new FIFO with permission bits MODE.  But
  --   interpret relative PATH names relative to the directory associated
  --   with FD.   

   function mkfifoat
     (uu_fd : int;
      uu_path : Interfaces.C.Strings.chars_ptr;
      uu_mode : x86_64_linux_gnu_bits_types_h.uu_mode_t) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:357
   with Import => True, 
        Convention => C, 
        External_Name => "mkfifoat";

  -- Set file access and modification times relative to directory file
  --   descriptor.   

   function utimensat
     (uu_fd : int;
      uu_path : Interfaces.C.Strings.chars_ptr;
      uu_times : access constant time_h.timespec;
      uu_flags : int) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:364
   with Import => True, 
        Convention => C, 
        External_Name => "utimensat";

  -- Set file access and modification times of the file associated with FD.   
   function futimens (uu_fd : int; uu_times : access constant time_h.timespec) return int  -- /usr/include/x86_64-linux-gnu/sys/stat.h:372
   with Import => True, 
        Convention => C, 
        External_Name => "futimens";

  -- To allow the `struct stat' structure and the file type `mode_t'
  --   bits to vary without changing shared library major version number,
  --   the `stat' family of functions and `mknod' are in fact inline
  --   wrappers around calls to `xstat', `fxstat', `lxstat', and `xmknod',
  --   which all take a leading version-number argument designating the
  --   data structure and bits used.  <bits/stat.h> defines _STAT_VER with
  --   the version number corresponding to `struct stat' as defined in
  --   that file; and _MKNOD_VER with the version number corresponding to
  --   the S_IF* macros defined therein.  It is arranged that when not
  --   inlined these function are always statically linked; that way a
  --   dynamically-linked executable always encodes the version number
  --   corresponding to the data structures it uses, so the `x' functions
  --   in the shared library can adapt without needing to recompile all
  --   callers.   

  -- Wrappers for stat and mknod system calls.   
   --  skipped func __fxstat

   --  skipped func __xstat

   --  skipped func __lxstat

   --  skipped func __fxstatat

   --  skipped func __fxstat64

   --  skipped func __xstat64

   --  skipped func __lxstat64

   --  skipped func __fxstatat64

   --  skipped func __xmknod

   --  skipped func __xmknodat

  -- Inlined versions of the real stat and mknod functions.   
end x86_64_linux_gnu_sys_stat_h;
