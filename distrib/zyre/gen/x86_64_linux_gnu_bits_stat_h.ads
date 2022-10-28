pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
with time_h;

package x86_64_linux_gnu_bits_stat_h is

   --  unsupported macro: st_atime st_atim.tv_sec
   --  unsupported macro: st_mtime st_mtim.tv_sec
   --  unsupported macro: st_ctime st_ctim.tv_sec
   UTIME_NOW : constant := ((2 ** 30) - 1);  --  /usr/include/x86_64-linux-gnu/bits/stat.h:206
   UTIME_OMIT : constant := ((2 ** 30) - 2);  --  /usr/include/x86_64-linux-gnu/bits/stat.h:207

  -- Copyright (C) 1999-2014 Free Software Foundation, Inc.
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

  -- Versions of the `struct stat' data structure.   
  -- i386 versions of the `xmknod' interface.   
  -- x86-64 versions of the `xmknod' interface.   
  -- Device.   
   type stat_array3053 is array (0 .. 2) of aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;
   type stat is record
      st_dev : aliased x86_64_linux_gnu_bits_types_h.uu_dev_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:48
      st_ino : aliased x86_64_linux_gnu_bits_types_h.uu_ino_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:53
      st_nlink : aliased x86_64_linux_gnu_bits_types_h.uu_nlink_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:61
      st_mode : aliased x86_64_linux_gnu_bits_types_h.uu_mode_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:62
      st_uid : aliased x86_64_linux_gnu_bits_types_h.uu_uid_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:64
      st_gid : aliased x86_64_linux_gnu_bits_types_h.uu_gid_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:65
      uu_pad0 : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:67
      st_rdev : aliased x86_64_linux_gnu_bits_types_h.uu_dev_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:69
      st_size : aliased x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:74
      st_blksize : aliased x86_64_linux_gnu_bits_types_h.uu_blksize_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:78
      st_blocks : aliased x86_64_linux_gnu_bits_types_h.uu_blkcnt_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:80
      st_atim : aliased time_h.timespec;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:91
      st_mtim : aliased time_h.timespec;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:92
      st_ctim : aliased time_h.timespec;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:93
      uu_glibc_reserved : aliased stat_array3053;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:106
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:46

  -- File serial number.	 
  -- 32bit file serial number.	 
  -- File mode.   
  -- Link count.   
  -- Link count.   
  -- File mode.   
  -- User ID of the file's owner.	 
  -- Group ID of the file's group. 
  -- Device number, if device.   
  -- Size of file, in bytes.   
  -- Size of file, in bytes.   
  -- Optimal block size for I/O.   
  -- Number 512-byte blocks allocated.  
  -- Number 512-byte blocks allocated.  
  -- Nanosecond resolution timestamps are stored in a format
  --       equivalent to 'struct timespec'.  This is the type used
  --       whenever possible but the Unix namespace rules do not allow the
  --       identifier 'timespec' to appear in the <sys/stat.h> header.
  --       Therefore we have to handle the use of this header in strictly
  --       standard-compliant sources special.   

  -- Time of last access.   
  -- Time of last modification.   
  -- Time of last status change.   
  -- Time of last access.   
  -- Nscecs of last access.   
  -- Time of last modification.   
  -- Nsecs of last modification.   
  -- Time of last status change.   
  -- Nsecs of last status change.   
  -- File serial number.	 
  -- Note stat64 has the same shape as stat for x86-64.   
  -- Device.   
   type stat64_array3053 is array (0 .. 2) of aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;
   type stat64 is record
      st_dev : aliased x86_64_linux_gnu_bits_types_h.uu_dev_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:121
      st_ino : aliased x86_64_linux_gnu_bits_types_h.uu_ino64_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:123
      st_nlink : aliased x86_64_linux_gnu_bits_types_h.uu_nlink_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:124
      st_mode : aliased x86_64_linux_gnu_bits_types_h.uu_mode_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:125
      st_uid : aliased x86_64_linux_gnu_bits_types_h.uu_uid_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:132
      st_gid : aliased x86_64_linux_gnu_bits_types_h.uu_gid_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:133
      uu_pad0 : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:135
      st_rdev : aliased x86_64_linux_gnu_bits_types_h.uu_dev_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:136
      st_size : aliased x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:137
      st_blksize : aliased x86_64_linux_gnu_bits_types_h.uu_blksize_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:143
      st_blocks : aliased x86_64_linux_gnu_bits_types_h.uu_blkcnt64_t;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:144
      st_atim : aliased time_h.timespec;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:152
      st_mtim : aliased time_h.timespec;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:153
      st_ctim : aliased time_h.timespec;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:154
      uu_glibc_reserved : aliased stat64_array3053;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:164
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/stat.h:119

  -- File serial number.   
  -- Link count.   
  -- File mode.   
  -- 32bit file serial number.	 
  -- File mode.   
  -- Link count.   
  -- User ID of the file's owner.	 
  -- Group ID of the file's group. 
  -- Device number, if device.   
  -- Size of file, in bytes.   
  -- Device number, if device.   
  -- Size of file, in bytes.   
  -- Optimal block size for I/O.   
  -- Nr. 512-byte blocks allocated.   
  -- Nanosecond resolution timestamps are stored in a format
  --       equivalent to 'struct timespec'.  This is the type used
  --       whenever possible but the Unix namespace rules do not allow the
  --       identifier 'timespec' to appear in the <sys/stat.h> header.
  --       Therefore we have to handle the use of this header in strictly
  --       standard-compliant sources special.   

  -- Time of last access.   
  -- Time of last modification.   
  -- Time of last status change.   
  -- Time of last access.   
  -- Nscecs of last access.   
  -- Time of last modification.   
  -- Nsecs of last modification.   
  -- Time of last status change.   
  -- Nsecs of last status change.   
  -- File serial number.		 
  -- Tell code we have these members.   
  -- Nanosecond resolution time values are supported.   
  -- Encoding of the file mode.   
  -- File types.   
  -- POSIX.1b objects.  Note that these macros always evaluate to zero.  But
  --   they do it by enforcing the correct use of the macros.   

  -- Protection bits.   
end x86_64_linux_gnu_bits_stat_h;
