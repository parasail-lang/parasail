pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
with stddef_h;
with stdio_h;
limited with x86_64_linux_gnu_bits_uio_h;
with Interfaces.C.Strings;

package x86_64_linux_gnu_bits_fcntl_linux_h is

   O_ACCMODE : constant := 8#003#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:42
   O_RDONLY : constant := 8#0#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:43
   O_WRONLY : constant := 8#1#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:44
   O_RDWR : constant := 8#2#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:45

   O_CREAT : constant := 8#100#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:47

   O_EXCL : constant := 8#200#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:50

   O_NOCTTY : constant := 8#400#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:53

   O_TRUNC : constant := 8#1000#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:56

   O_APPEND : constant := 8#2000#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:59

   O_NONBLOCK : constant := 8#4000#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:62
   --  unsupported macro: O_NDELAY O_NONBLOCK

   O_SYNC : constant := 8#4010000#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:68
   --  unsupported macro: O_FSYNC O_SYNC

   O_ASYNC : constant := 8#20000#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:72

   F_GETLK : constant := 5;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:105
   F_SETLK : constant := 6;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:106
   F_SETLKW : constant := 7;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:107
   --  unsupported macro: O_LARGEFILE __O_LARGEFILE
   --  unsupported macro: O_DIRECTORY __O_DIRECTORY
   --  unsupported macro: O_NOFOLLOW __O_NOFOLLOW
   --  unsupported macro: O_CLOEXEC __O_CLOEXEC
   --  unsupported macro: O_DIRECT __O_DIRECT
   --  unsupported macro: O_NOATIME __O_NOATIME
   --  unsupported macro: O_PATH __O_PATH
   --  unsupported macro: O_TMPFILE __O_TMPFILE
   --  unsupported macro: O_DSYNC __O_DSYNC
   --  unsupported macro: O_RSYNC O_SYNC

   F_DUPFD : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:150
   F_GETFD : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:151
   F_SETFD : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:152
   F_GETFL : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:153
   F_SETFL : constant := 4;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:154
   --  unsupported macro: F_SETOWN __F_SETOWN
   --  unsupported macro: F_GETOWN __F_GETOWN
   --  unsupported macro: F_SETSIG __F_SETSIG
   --  unsupported macro: F_GETSIG __F_GETSIG
   --  unsupported macro: F_SETOWN_EX __F_SETOWN_EX
   --  unsupported macro: F_GETOWN_EX __F_GETOWN_EX

   F_SETLEASE : constant := 1024;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:183
   F_GETLEASE : constant := 1025;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:184
   F_NOTIFY : constant := 1026;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:185
   F_SETPIPE_SZ : constant := 1031;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:186
   F_GETPIPE_SZ : constant := 1032;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:187

   F_DUPFD_CLOEXEC : constant := 1030;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:190

   FD_CLOEXEC : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:195

   F_RDLCK : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:199
   F_WRLCK : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:200
   F_UNLCK : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:201

   F_EXLCK : constant := 4;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:207
   F_SHLCK : constant := 8;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:208

   LOCK_MAND : constant := 32;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:221
   LOCK_READ : constant := 64;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:222
   LOCK_WRITE : constant := 128;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:223
   LOCK_RW : constant := 192;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:224

   DN_ACCESS : constant := 16#00000001#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:229
   DN_MODIFY : constant := 16#00000002#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:230
   DN_CREATE : constant := 16#00000004#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:231
   DN_DELETE : constant := 16#00000008#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:232
   DN_RENAME : constant := 16#00000010#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:233
   DN_ATTRIB : constant := 16#00000020#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:234
   DN_MULTISHOT : constant := 16#80000000#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:235
   --  unsupported macro: FAPPEND O_APPEND
   --  unsupported macro: FFSYNC O_FSYNC
   --  unsupported macro: FASYNC O_ASYNC
   --  unsupported macro: FNONBLOCK O_NONBLOCK
   --  unsupported macro: FNDELAY O_NDELAY

   POSIX_FADV_NORMAL : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:273
   POSIX_FADV_RANDOM : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:274
   POSIX_FADV_SEQUENTIAL : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:275
   POSIX_FADV_WILLNEED : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:276
   --  unsupported macro: POSIX_FADV_DONTNEED __POSIX_FADV_DONTNEED
   --  unsupported macro: POSIX_FADV_NOREUSE __POSIX_FADV_NOREUSE

   SYNC_FILE_RANGE_WAIT_BEFORE : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:284

   SYNC_FILE_RANGE_WRITE : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:287

   SYNC_FILE_RANGE_WAIT_AFTER : constant := 4;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:290

   SPLICE_F_MOVE : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:295
   SPLICE_F_NONBLOCK : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:296

   SPLICE_F_MORE : constant := 4;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:299
   SPLICE_F_GIFT : constant := 8;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:300

   FALLOC_FL_KEEP_SIZE : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:304

   FALLOC_FL_PUNCH_HOLE : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:307

   MAX_HANDLE_SZ : constant := 128;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:320

   AT_FDCWD : constant := -100;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:325

   AT_SYMLINK_NOFOLLOW : constant := 16#100#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:328
   AT_REMOVEDIR : constant := 16#200#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:329

   AT_SYMLINK_FOLLOW : constant := 16#400#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:331

   AT_NO_AUTOMOUNT : constant := 16#800#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:333

   AT_EMPTY_PATH : constant := 16#1000#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:335

   AT_EACCESS : constant := 16#200#;  --  /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:337

  -- O_*, F_*, FD_* bit values for Linux.
  --   Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

  -- This file contains shared definitions between Linux architectures
  --   and is included by <bits/fcntl.h> to declare them.  The various
  --   #ifndef cases allow the architecture specific file to define those
  --   values with different values.
  --   A minimal <bits/fcntl.h> contains just:
  --   struct flock {...}
  --   #ifdef __USE_LARGEFILE64
  --   struct flock64 {...}
  --   #endif
  --   #include <bits/fcntl-linux.h>
  -- 

  -- open/fcntl.   
  -- For now, Linux has no separate synchronicitiy options for read
  --   operations.  We define O_RSYNC therefore as the same as O_SYNC
  --   since this is a superset.   

  -- Values for the second argument to `fcntl'.   
  -- For F_[GET|SET]FD.   
  -- For posix fcntl() and `l_type' field of a `struct flock' for lockf().   
  -- For old implementation of BSD flock.   
  -- Operations for BSD flock, also used by the kernel implementation.   
  -- Types of directory notifications that may be requested with F_NOTIFY.   
  -- Owner types.   
   subtype uu_pid_type is unsigned;
   F_OWNER_TID : constant unsigned := 0;
   F_OWNER_PID : constant unsigned := 1;
   F_OWNER_PGRP : constant unsigned := 2;
   F_OWNER_GID : constant unsigned := 2;  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:241

  -- Kernel thread.   
  -- Process.   
  -- Process group.   
  -- Alternative, obsolete name.   
  -- Structure to use with F_GETOWN_EX and F_SETOWN_EX.   
  -- Owner type of ID.   
   type f_owner_ex is record
      c_type : aliased uu_pid_type;  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:252
      pid : aliased x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:253
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:250

  -- ID of owner.   
  -- Define some more compatibility macros to be backward compatible with
  --   BSD systems which did not managed to hide these kernel macros.   

  -- Advise to `posix_fadvise'.   
  -- Flags for SYNC_FILE_RANGE.   
  -- Flags for SPLICE and VMSPLICE.   
  -- Flags for fallocate.   
  -- File handle structure.   
   type file_handle_array3029 is array (0 .. -1) of aliased unsigned_char;
   type file_handle is record
      handle_bytes : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:313
      handle_type : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:314
      f_handle : aliased file_handle_array3029;  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:316
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:311

  -- File identifier.   
  -- Maximum handle size (for now).   
  -- Values for `*at' functions.   
  -- Provide kernel hint to read ahead.   
   function readahead
     (uu_fd : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off64_t;
      uu_count : stddef_h.size_t) return stdio_h.ssize_t  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:346
   with Import => True, 
        Convention => C, 
        External_Name => "readahead";

  -- Selective file content synch'ing.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function sync_file_range
     (uu_fd : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off64_t;
      uu_count : x86_64_linux_gnu_bits_types_h.uu_off64_t;
      uu_flags : unsigned) return int  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:354
   with Import => True, 
        Convention => C, 
        External_Name => "sync_file_range";

  -- Splice address range into a pipe.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function vmsplice
     (uu_fdout : int;
      uu_iov : access constant x86_64_linux_gnu_bits_uio_h.iovec;
      uu_count : stddef_h.size_t;
      uu_flags : unsigned) return stdio_h.ssize_t  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:362
   with Import => True, 
        Convention => C, 
        External_Name => "vmsplice";

  -- Splice two files together.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function splice
     (uu_fdin : int;
      uu_offin : access x86_64_linux_gnu_bits_types_h.uu_off64_t;
      uu_fdout : int;
      uu_offout : access x86_64_linux_gnu_bits_types_h.uu_off64_t;
      uu_len : stddef_h.size_t;
      uu_flags : unsigned) return stdio_h.ssize_t  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:369
   with Import => True, 
        Convention => C, 
        External_Name => "splice";

  -- In-kernel implementation of tee for pipe buffers.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function tee
     (uu_fdin : int;
      uu_fdout : int;
      uu_len : stddef_h.size_t;
      uu_flags : unsigned) return stdio_h.ssize_t  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:377
   with Import => True, 
        Convention => C, 
        External_Name => "tee";

  -- Reserve storage for the data of the file associated with FD.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function fallocate
     (uu_fd : int;
      uu_mode : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off_t;
      uu_len : x86_64_linux_gnu_bits_types_h.uu_off_t) return int  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:385
   with Import => True, 
        Convention => C, 
        External_Name => "fallocate";

   function fallocate64
     (uu_fd : int;
      uu_mode : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off64_t;
      uu_len : x86_64_linux_gnu_bits_types_h.uu_off64_t) return int  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:396
   with Import => True, 
        Convention => C, 
        External_Name => "fallocate64";

  -- Map file name to file handle.   
   function name_to_handle_at
     (uu_dfd : int;
      uu_name : Interfaces.C.Strings.chars_ptr;
      uu_handle : access file_handle;
      uu_mnt_id : access int;
      uu_flags : int) return int  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:402
   with Import => True, 
        Convention => C, 
        External_Name => "name_to_handle_at";

  -- Open file using the file handle.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function open_by_handle_at
     (uu_mountdirfd : int;
      uu_handle : access file_handle;
      uu_flags : int) return int  -- /usr/include/x86_64-linux-gnu/bits/fcntl-linux.h:410
   with Import => True, 
        Convention => C, 
        External_Name => "open_by_handle_at";

end x86_64_linux_gnu_bits_fcntl_linux_h;
