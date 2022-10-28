pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
limited with x86_64_linux_gnu_bits_uio_h;
with stdio_h;
with x86_64_linux_gnu_bits_types_h;

package x86_64_linux_gnu_sys_uio_h is

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

  -- This file defines `struct iovec'.   
  -- Read data from file descriptor FD, and put the result in the
  --   buffers described by IOVEC, which is a vector of COUNT 'struct iovec's.
  --   The buffers are filled in the order specified.
  --   Operates just like 'read' (see <unistd.h>) except that data are
  --   put in IOVEC instead of a contiguous buffer.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function readv
     (uu_fd : int;
      uu_iovec : access constant x86_64_linux_gnu_bits_uio_h.iovec;
      uu_count : int) return stdio_h.ssize_t  -- /usr/include/x86_64-linux-gnu/sys/uio.h:39
   with Import => True, 
        Convention => C, 
        External_Name => "readv";

  -- Write data pointed by the buffers described by IOVEC, which
  --   is a vector of COUNT 'struct iovec's, to file descriptor FD.
  --   The data is written in the order specified.
  --   Operates just like 'write' (see <unistd.h>) except that the data
  --   are taken from IOVEC instead of a contiguous buffer.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function writev
     (uu_fd : int;
      uu_iovec : access constant x86_64_linux_gnu_bits_uio_h.iovec;
      uu_count : int) return stdio_h.ssize_t  -- /usr/include/x86_64-linux-gnu/sys/uio.h:50
   with Import => True, 
        Convention => C, 
        External_Name => "writev";

  -- Read data from file descriptor FD at the given position OFFSET
  --   without change the file pointer, and put the result in the buffers
  --   described by IOVEC, which is a vector of COUNT 'struct iovec's.
  --   The buffers are filled in the order specified.  Operates just like
  --   'pread' (see <unistd.h>) except that data are put in IOVEC instead
  --   of a contiguous buffer.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function preadv
     (uu_fd : int;
      uu_iovec : access constant x86_64_linux_gnu_bits_uio_h.iovec;
      uu_count : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off_t) return stdio_h.ssize_t  -- /usr/include/x86_64-linux-gnu/sys/uio.h:65
   with Import => True, 
        Convention => C, 
        External_Name => "preadv";

  -- Write data pointed by the buffers described by IOVEC, which is a
  --   vector of COUNT 'struct iovec's, to file descriptor FD at the given
  --   position OFFSET without change the file pointer.  The data is
  --   written in the order specified.  Operates just like 'pwrite' (see
  --   <unistd.h>) except that the data are taken from IOVEC instead of a
  --   contiguous buffer.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function pwritev
     (uu_fd : int;
      uu_iovec : access constant x86_64_linux_gnu_bits_uio_h.iovec;
      uu_count : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off_t) return stdio_h.ssize_t  -- /usr/include/x86_64-linux-gnu/sys/uio.h:77
   with Import => True, 
        Convention => C, 
        External_Name => "pwritev";

  -- Read data from file descriptor FD at the given position OFFSET
  --   without change the file pointer, and put the result in the buffers
  --   described by IOVEC, which is a vector of COUNT 'struct iovec's.
  --   The buffers are filled in the order specified.  Operates just like
  --   'pread' (see <unistd.h>) except that data are put in IOVEC instead
  --   of a contiguous buffer.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function preadv64
     (uu_fd : int;
      uu_iovec : access constant x86_64_linux_gnu_bits_uio_h.iovec;
      uu_count : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off64_t) return stdio_h.ssize_t  -- /usr/include/x86_64-linux-gnu/sys/uio.h:103
   with Import => True, 
        Convention => C, 
        External_Name => "preadv64";

  -- Write data pointed by the buffers described by IOVEC, which is a
  --   vector of COUNT 'struct iovec's, to file descriptor FD at the given
  --   position OFFSET without change the file pointer.  The data is
  --   written in the order specified.  Operates just like 'pwrite' (see
  --   <unistd.h>) except that the data are taken from IOVEC instead of a
  --   contiguous buffer.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function pwritev64
     (uu_fd : int;
      uu_iovec : access constant x86_64_linux_gnu_bits_uio_h.iovec;
      uu_count : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off64_t) return stdio_h.ssize_t  -- /usr/include/x86_64-linux-gnu/sys/uio.h:115
   with Import => True, 
        Convention => C, 
        External_Name => "pwritev64";

end x86_64_linux_gnu_sys_uio_h;
