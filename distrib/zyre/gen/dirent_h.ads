pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
limited with x86_64_linux_gnu_bits_dirent_h;
with System;
with stddef_h;
with x86_64_linux_gnu_bits_types_h;

package dirent_h is

   --  unsupported macro: DT_UNKNOWN DT_UNKNOWN
   --  unsupported macro: DT_FIFO DT_FIFO
   --  unsupported macro: DT_CHR DT_CHR
   --  unsupported macro: DT_DIR DT_DIR
   --  unsupported macro: DT_BLK DT_BLK
   --  unsupported macro: DT_REG DT_REG
   --  unsupported macro: DT_LNK DT_LNK
   --  unsupported macro: DT_SOCK DT_SOCK
   --  unsupported macro: DT_WHT DT_WHT
   --  arg-macro: function IFTODT (mode)
   --    return ((mode) and 8#170000#) >> 12;
   --  arg-macro: function DTTOIF (dirtype)
   --    return (dirtype) << 12;
   --  unsupported macro: MAXNAMLEN NAME_MAX
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

  -- *	POSIX Standard: 5.1.2 Directory Operations	<dirent.h>
  --  

  -- This file defines `struct dirent'.
  --   It defines the macro `_DIRENT_HAVE_D_NAMLEN' iff there is a `d_namlen'
  --   member that gives the length of `d_name'.
  --   It defines the macro `_DIRENT_HAVE_D_RECLEN' iff there is a `d_reclen'
  --   member that gives the size of the entire directory entry.
  --   It defines the macro `_DIRENT_HAVE_D_OFF' iff there is a `d_off'
  --   member that gives the file offset of the next directory entry.
  --   It defines the macro `_DIRENT_HAVE_D_TYPE' iff there is a `d_type'
  --   member that gives the type of the file.
  --  

  -- These macros extract size information from a `struct dirent *'.
  --   They may evaluate their argument multiple times, so it must not
  --   have side effects.  Each of these may involve a relatively costly
  --   call to `strlen' on some systems, so these values should be cached.
  --   _D_EXACT_NAMLEN (DP)	returns the length of DP->d_name, not including
  --   its terminating null character.
  --   _D_ALLOC_NAMLEN (DP)	returns a size at least (_D_EXACT_NAMLEN (DP) + 1);
  --   that is, the allocation size needed to hold the DP->d_name string.
  --   Use this macro when you don't need the exact length, just an upper bound.
  --   This macro is less likely to require calling `strlen' than _D_EXACT_NAMLEN.
  --    

  -- File types for `d_type'.   
  -- Convert between stat structure types and directory types.   
  -- This is the data type of directory stream objects.
  --   The actual structure is opaque to users.   

   type uu_dirstream is null record;   -- incomplete struct

   subtype DIR is uu_dirstream;  -- /usr/include/dirent.h:127

  -- Open a directory stream on NAME.
  --   Return a DIR stream on the directory, or NULL if it could not be opened.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function opendir (uu_name : Interfaces.C.Strings.chars_ptr) return access DIR  -- /usr/include/dirent.h:134
   with Import => True, 
        Convention => C, 
        External_Name => "opendir";

  -- Same as opendir, but open the stream on the file descriptor FD.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function fdopendir (uu_fd : int) return access DIR  -- /usr/include/dirent.h:141
   with Import => True, 
        Convention => C, 
        External_Name => "fdopendir";

  -- Close the directory stream DIRP.
  --   Return 0 if successful, -1 if not.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function closedir (uu_dirp : access DIR) return int  -- /usr/include/dirent.h:149
   with Import => True, 
        Convention => C, 
        External_Name => "closedir";

  -- Read a directory entry from DIRP.  Return a pointer to a `struct
  --   dirent' describing the entry, or NULL for EOF or error.  The
  --   storage returned may be overwritten by a later readdir call on the
  --   same DIR stream.
  --   If the Large File Support API is selected we have to use the
  --   appropriate interface.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function readdir (uu_dirp : access DIR) return access x86_64_linux_gnu_bits_dirent_h.dirent  -- /usr/include/dirent.h:162
   with Import => True, 
        Convention => C, 
        External_Name => "readdir";

   function readdir64 (uu_dirp : access DIR) return access x86_64_linux_gnu_bits_dirent_h.dirent64  -- /usr/include/dirent.h:173
   with Import => True, 
        Convention => C, 
        External_Name => "readdir64";

  -- Reentrant version of `readdir'.  Return in RESULT a pointer to the
  --   next entry.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function readdir_r
     (uu_dirp : access DIR;
      uu_entry : access x86_64_linux_gnu_bits_dirent_h.dirent;
      uu_result : System.Address) return int  -- /usr/include/dirent.h:183
   with Import => True, 
        Convention => C, 
        External_Name => "readdir_r";

   function readdir64_r
     (uu_dirp : access DIR;
      uu_entry : access x86_64_linux_gnu_bits_dirent_h.dirent64;
      uu_result : System.Address) return int  -- /usr/include/dirent.h:200
   with Import => True, 
        Convention => C, 
        External_Name => "readdir64_r";

  -- Rewind DIRP to the beginning of the directory.   
   procedure rewinddir (uu_dirp : access DIR)  -- /usr/include/dirent.h:208
   with Import => True, 
        Convention => C, 
        External_Name => "rewinddir";

  -- Seek to position POS on DIRP.   
   procedure seekdir (uu_dirp : access DIR; uu_pos : long)  -- /usr/include/dirent.h:214
   with Import => True, 
        Convention => C, 
        External_Name => "seekdir";

  -- Return the current position of DIRP.   
   function telldir (uu_dirp : access DIR) return long  -- /usr/include/dirent.h:217
   with Import => True, 
        Convention => C, 
        External_Name => "telldir";

  -- Return the file descriptor used by DIRP.   
   function dirfd (uu_dirp : access DIR) return int  -- /usr/include/dirent.h:223
   with Import => True, 
        Convention => C, 
        External_Name => "dirfd";

  -- Get the definitions of the POSIX.1 limits.   
  -- `MAXNAMLEN' is the BSD name for what POSIX calls `NAME_MAX'.   
  -- Scan the directory DIR, calling SELECTOR on each directory entry.
  --   Entries for which SELECT returns nonzero are individually malloc'd,
  --   sorted using qsort with CMP, and collected in a malloc'd array in
  --   *NAMELIST.  Returns the number of entries selected, or -1 on error.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function scandir
     (uu_dir : Interfaces.C.Strings.chars_ptr;
      uu_namelist : System.Address;
      uu_selector : access function (arg1 : access constant x86_64_linux_gnu_bits_dirent_h.dirent) return int;
      uu_cmp : access function (arg1 : System.Address; arg2 : System.Address) return int) return int  -- /usr/include/dirent.h:254
   with Import => True, 
        Convention => C, 
        External_Name => "scandir";

  -- This function is like `scandir' but it uses the 64bit dirent structure.
  --   Please note that the CMP function must now work with struct dirent64 **.   

   function scandir64
     (uu_dir : Interfaces.C.Strings.chars_ptr;
      uu_namelist : System.Address;
      uu_selector : access function (arg1 : access constant x86_64_linux_gnu_bits_dirent_h.dirent64) return int;
      uu_cmp : access function (arg1 : System.Address; arg2 : System.Address) return int) return int  -- /usr/include/dirent.h:277
   with Import => True, 
        Convention => C, 
        External_Name => "scandir64";

  -- Similar to `scandir' but a relative DIR name is interpreted relative
  --   to the directory for which DFD is a descriptor.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function scandirat
     (uu_dfd : int;
      uu_dir : Interfaces.C.Strings.chars_ptr;
      uu_namelist : System.Address;
      uu_selector : access function (arg1 : access constant x86_64_linux_gnu_bits_dirent_h.dirent) return int;
      uu_cmp : access function (arg1 : System.Address; arg2 : System.Address) return int) return int  -- /usr/include/dirent.h:292
   with Import => True, 
        Convention => C, 
        External_Name => "scandirat";

  -- This function is like `scandir' but it uses the 64bit dirent structure.
  --   Please note that the CMP function must now work with struct dirent64 **.   

   function scandirat64
     (uu_dfd : int;
      uu_dir : Interfaces.C.Strings.chars_ptr;
      uu_namelist : System.Address;
      uu_selector : access function (arg1 : access constant x86_64_linux_gnu_bits_dirent_h.dirent64) return int;
      uu_cmp : access function (arg1 : System.Address; arg2 : System.Address) return int) return int  -- /usr/include/dirent.h:314
   with Import => True, 
        Convention => C, 
        External_Name => "scandirat64";

  -- Function to compare two `struct dirent's alphabetically.   
   function alphasort (uu_e1 : System.Address; uu_e2 : System.Address) return int  -- /usr/include/dirent.h:324
   with Import => True, 
        Convention => C, 
        External_Name => "alphasort";

   function alphasort64 (uu_e1 : System.Address; uu_e2 : System.Address) return int  -- /usr/include/dirent.h:339
   with Import => True, 
        Convention => C, 
        External_Name => "alphasort64";

  -- Read directory entries from FD into BUF, reading at most NBYTES.
  --   Reading starts at offset *BASEP, and *BASEP is updated with the new
  --   position after reading.  Returns the number of bytes read; zero when at
  --   end of directory; or -1 for errors.   

   function getdirentries
     (uu_fd : int;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_nbytes : stddef_h.size_t;
      uu_basep : access x86_64_linux_gnu_bits_types_h.uu_off_t) return x86_64_linux_gnu_bits_types_h.uu_ssize_t  -- /usr/include/dirent.h:352
   with Import => True, 
        Convention => C, 
        External_Name => "getdirentries";

   function getdirentries64
     (uu_fd : int;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_nbytes : stddef_h.size_t;
      uu_basep : access x86_64_linux_gnu_bits_types_h.uu_off64_t) return x86_64_linux_gnu_bits_types_h.uu_ssize_t  -- /usr/include/dirent.h:369
   with Import => True, 
        Convention => C, 
        External_Name => "getdirentries64";

  -- Function to compare two `struct dirent's by name & version.   
   function versionsort (uu_e1 : System.Address; uu_e2 : System.Address) return int  -- /usr/include/dirent.h:379
   with Import => True, 
        Convention => C, 
        External_Name => "versionsort";

   function versionsort64 (uu_e1 : System.Address; uu_e2 : System.Address) return int  -- /usr/include/dirent.h:395
   with Import => True, 
        Convention => C, 
        External_Name => "versionsort64";

end dirent_h;
