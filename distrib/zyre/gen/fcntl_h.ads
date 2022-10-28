pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with x86_64_linux_gnu_sys_types_h;
with stdio_h;

package fcntl_h is

   SEEK_SET : constant := 0;  --  /usr/include/fcntl.h:136
   SEEK_CUR : constant := 1;  --  /usr/include/fcntl.h:137
   SEEK_END : constant := 2;  --  /usr/include/fcntl.h:138

   F_ULOCK : constant := 0;  --  /usr/include/fcntl.h:225
   F_LOCK : constant := 1;  --  /usr/include/fcntl.h:226
   F_TLOCK : constant := 2;  --  /usr/include/fcntl.h:227
   F_TEST : constant := 3;  --  /usr/include/fcntl.h:228

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

  -- *	POSIX Standard: 6.5 File Control Operations	<fcntl.h>
  --  

  -- This must be early so <bits/fcntl.h> can define types winningly.   
  -- Get __mode_t, __dev_t and __off_t  . 
  -- Get the definitions of O_*, F_*, FD_*: all the
  --   numbers and flag bits for `open', `fcntl', et al.   

  -- Detect if open needs mode as a third argument (or for openat as a fourth
  --   argument).   

  -- POSIX.1-2001 specifies that these types are defined by <fcntl.h>.
  --   Earlier POSIX standards permitted any type ending in `_t' to be defined
  --   by any POSIX header, so we don't conditionalize the definitions here.   

  -- For XPG all symbols from <sys/stat.h> should also be available.   
  -- Protection bits.   
  -- Save swapped text after use (sticky bit).  This is pretty well obsolete.   
  -- Read, write, and execute by owner.   
  -- Read, write, and execute by group.   
  -- Read, write, and execute by others.   
  -- Values for the second argument to access.
  --   These may be OR'd together.   

  -- XPG wants the following symbols.   <stdio.h> has the same definitions.   
  -- Do the file control operation described by CMD on FD.
  --   The remaining arguments are interpreted depending on CMD.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function fcntl (uu_fd : int; uu_cmd : int  -- , ...
      ) return int  -- /usr/include/fcntl.h:146
   with Import => True, 
        Convention => C, 
        External_Name => "fcntl";

  -- Open FILE and return a new file descriptor for it, or -1 on error.
  --   OFLAG determines the type of access used.  If O_CREAT or O_TMPFILE is set
  --   in OFLAG, the third argument is taken as a `mode_t', the mode of the
  --   created file.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function open (uu_file : Interfaces.C.Strings.chars_ptr; uu_oflag : int  -- , ...
      ) return int  -- /usr/include/fcntl.h:156
   with Import => True, 
        Convention => C, 
        External_Name => "open";

   function open64 (uu_file : Interfaces.C.Strings.chars_ptr; uu_oflag : int  -- , ...
      ) return int  -- /usr/include/fcntl.h:166
   with Import => True, 
        Convention => C, 
        External_Name => "open64";

  -- Similar to `open' but a relative path name is interpreted relative to
  --   the directory for which FD is a descriptor.
  --   NOTE: some other `openat' implementation support additional functionality
  --   through this interface, especially using the O_XATTR flag.  This is not
  --   yet supported here.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function openat
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_oflag : int  -- , ...
      ) return int  -- /usr/include/fcntl.h:180
   with Import => True, 
        Convention => C, 
        External_Name => "openat";

   function openat64
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_oflag : int  -- , ...
      ) return int  -- /usr/include/fcntl.h:191
   with Import => True, 
        Convention => C, 
        External_Name => "openat64";

  -- Create and open FILE, with mode MODE.  This takes an `int' MODE
  --   argument because that is what `mode_t' will be widened to.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function creat (uu_file : Interfaces.C.Strings.chars_ptr; uu_mode : x86_64_linux_gnu_sys_types_h.mode_t) return int  -- /usr/include/fcntl.h:202
   with Import => True, 
        Convention => C, 
        External_Name => "creat";

   function creat64 (uu_file : Interfaces.C.Strings.chars_ptr; uu_mode : x86_64_linux_gnu_sys_types_h.mode_t) return int  -- /usr/include/fcntl.h:212
   with Import => True, 
        Convention => C, 
        External_Name => "creat64";

  -- NOTE: These declarations also appear in <unistd.h>; be sure to keep both
  --   files consistent.  Some systems have them there and some here, and some
  --   software depends on the macros being defined without including both.   

  -- `lockf' is a simpler interface to the locking facilities of `fcntl'.
  --   LEN is always relative to the current file position.
  --   The CMD argument is one of the following.   

   function lockf
     (uu_fd : int;
      uu_cmd : int;
      uu_len : stdio_h.off_t) return int  -- /usr/include/fcntl.h:231
   with Import => True, 
        Convention => C, 
        External_Name => "lockf";

   function lockf64
     (uu_fd : int;
      uu_cmd : int;
      uu_len : stdio_h.off64_t) return int  -- /usr/include/fcntl.h:240
   with Import => True, 
        Convention => C, 
        External_Name => "lockf64";

  -- Advice the system about the expected behaviour of the application with
  --   respect to the file associated with FD.   

   function posix_fadvise
     (uu_fd : int;
      uu_offset : stdio_h.off_t;
      uu_len : stdio_h.off_t;
      uu_advise : int) return int  -- /usr/include/fcntl.h:248
   with Import => True, 
        Convention => C, 
        External_Name => "posix_fadvise";

   function posix_fadvise64
     (uu_fd : int;
      uu_offset : stdio_h.off64_t;
      uu_len : stdio_h.off64_t;
      uu_advise : int) return int  -- /usr/include/fcntl.h:260
   with Import => True, 
        Convention => C, 
        External_Name => "posix_fadvise64";

  -- Reserve storage for the data of the file associated with FD.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function posix_fallocate
     (uu_fd : int;
      uu_offset : stdio_h.off_t;
      uu_len : stdio_h.off_t) return int  -- /usr/include/fcntl.h:270
   with Import => True, 
        Convention => C, 
        External_Name => "posix_fallocate";

   function posix_fallocate64
     (uu_fd : int;
      uu_offset : stdio_h.off64_t;
      uu_len : stdio_h.off64_t) return int  -- /usr/include/fcntl.h:281
   with Import => True, 
        Convention => C, 
        External_Name => "posix_fallocate64";

  -- Define some inlines helping to catch common problems.   
end fcntl_h;
