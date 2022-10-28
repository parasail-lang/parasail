pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with x86_64_linux_gnu_bits_types_h;
limited with stdio_h;
with stddef_h;
with System;

package pwd_h is

   NSS_BUFLEN_PASSWD : constant := 1024;  --  /usr/include/pwd.h:123

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

  -- *	POSIX Standard: 9.2.2 User Database Access	<pwd.h>
  --  

  -- The Single Unix specification says that some more types are
  --   available here.   

  -- The passwd structure.   
  -- Username.   
   type passwd is record
      pw_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pwd.h:51
      pw_passwd : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pwd.h:52
      pw_uid : aliased x86_64_linux_gnu_bits_types_h.uu_uid_t;  -- /usr/include/pwd.h:53
      pw_gid : aliased x86_64_linux_gnu_bits_types_h.uu_gid_t;  -- /usr/include/pwd.h:54
      pw_gecos : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pwd.h:55
      pw_dir : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pwd.h:56
      pw_shell : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pwd.h:57
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pwd.h:49

  -- Password.   
  -- User ID.   
  -- Group ID.   
  -- Real name.   
  -- Home directory.   
  -- Shell program.   
  -- Rewind the password-file stream.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   procedure setpwent  -- /usr/include/pwd.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "setpwent";

  -- Close the password-file stream.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   procedure endpwent  -- /usr/include/pwd.h:78
   with Import => True, 
        Convention => C, 
        External_Name => "endpwent";

  -- Read an entry from the password-file stream, opening it if necessary.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function getpwent return access passwd  -- /usr/include/pwd.h:84
   with Import => True, 
        Convention => C, 
        External_Name => "getpwent";

  -- Read an entry from STREAM.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function fgetpwent (uu_stream : access stdio_h.FILE) return access passwd  -- /usr/include/pwd.h:94
   with Import => True, 
        Convention => C, 
        External_Name => "fgetpwent";

  -- Write the given entry onto the given stream.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function putpwent (uu_p : access constant passwd; uu_f : access stdio_h.FILE) return int  -- /usr/include/pwd.h:102
   with Import => True, 
        Convention => C, 
        External_Name => "putpwent";

  -- Search for an entry with a matching user ID.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function getpwuid (uu_uid : x86_64_linux_gnu_bits_types_h.uu_uid_t) return access passwd  -- /usr/include/pwd.h:110
   with Import => True, 
        Convention => C, 
        External_Name => "getpwuid";

  -- Search for an entry with a matching username.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function getpwnam (uu_name : Interfaces.C.Strings.chars_ptr) return access passwd  -- /usr/include/pwd.h:116
   with Import => True, 
        Convention => C, 
        External_Name => "getpwnam";

  -- Reasonable value for the buffer sized used in the reentrant
  --   functions below.  But better use `sysconf'.   

  -- Reentrant versions of some of the functions above.
  --   PLEASE NOTE: the `getpwent_r' function is not (yet) standardized.
  --   The interface may change in later versions of this library.  But
  --   the interface is designed following the principals used for the
  --   other reentrant functions so the chances are good this is what the
  --   POSIX people would choose.   

  -- This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function getpwent_r
     (uu_resultbuf : access passwd;
      uu_buffer : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t;
      uu_result : System.Address) return int  -- /usr/include/pwd.h:139
   with Import => True, 
        Convention => C, 
        External_Name => "getpwent_r";

   function getpwuid_r
     (uu_uid : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_resultbuf : access passwd;
      uu_buffer : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t;
      uu_result : System.Address) return int  -- /usr/include/pwd.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "getpwuid_r";

   function getpwnam_r
     (uu_name : Interfaces.C.Strings.chars_ptr;
      uu_resultbuf : access passwd;
      uu_buffer : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t;
      uu_result : System.Address) return int  -- /usr/include/pwd.h:149
   with Import => True, 
        Convention => C, 
        External_Name => "getpwnam_r";

  -- Read an entry from STREAM.  This function is not standardized and
  --   probably never will.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function fgetpwent_r
     (uu_stream : access stdio_h.FILE;
      uu_resultbuf : access passwd;
      uu_buffer : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t;
      uu_result : System.Address) return int  -- /usr/include/pwd.h:163
   with Import => True, 
        Convention => C, 
        External_Name => "fgetpwent_r";

  -- Re-construct the password-file line for the given uid
  --   in the given buffer.  This knows the format that the caller
  --   will expect, but this need not be the format of the password file.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function getpw (uu_uid : x86_64_linux_gnu_bits_types_h.uu_uid_t; uu_buffer : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/pwd.h:180
   with Import => True, 
        Convention => C, 
        External_Name => "getpw";

end pwd_h;
