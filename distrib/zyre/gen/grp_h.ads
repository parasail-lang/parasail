pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with x86_64_linux_gnu_bits_types_h;
with System;
limited with stdio_h;
with stddef_h;

package grp_h is

   NSS_BUFLEN_GROUP : constant := 1024;  --  /usr/include/grp.h:118

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

  -- *	POSIX Standard: 9.2.1 Group Database Access	<grp.h>
  --  

  -- For the Single Unix specification we must define this type here.   
  -- The group structure.	  
  -- Group name.	 
   type group is record
      gr_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/grp.h:44
      gr_passwd : Interfaces.C.Strings.chars_ptr;  -- /usr/include/grp.h:45
      gr_gid : aliased x86_64_linux_gnu_bits_types_h.uu_gid_t;  -- /usr/include/grp.h:46
      gr_mem : System.Address;  -- /usr/include/grp.h:47
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/grp.h:42

  -- Password.	 
  -- Group ID.	 
  -- Member list.	 
  -- Rewind the group-file stream.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   procedure setgrent  -- /usr/include/grp.h:62
   with Import => True, 
        Convention => C, 
        External_Name => "setgrent";

  -- Close the group-file stream.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   procedure endgrent  -- /usr/include/grp.h:71
   with Import => True, 
        Convention => C, 
        External_Name => "endgrent";

  -- Read an entry from the group-file stream, opening it if necessary.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function getgrent return access group  -- /usr/include/grp.h:77
   with Import => True, 
        Convention => C, 
        External_Name => "getgrent";

  -- Read a group entry from STREAM.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function fgetgrent (uu_stream : access stdio_h.FILE) return access group  -- /usr/include/grp.h:87
   with Import => True, 
        Convention => C, 
        External_Name => "fgetgrent";

  -- Write the given entry onto the given stream.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function putgrent (uu_p : access constant group; uu_f : access stdio_h.FILE) return int  -- /usr/include/grp.h:97
   with Import => True, 
        Convention => C, 
        External_Name => "putgrent";

  -- Search for an entry with a matching group ID.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function getgrgid (uu_gid : x86_64_linux_gnu_bits_types_h.uu_gid_t) return access group  -- /usr/include/grp.h:105
   with Import => True, 
        Convention => C, 
        External_Name => "getgrgid";

  -- Search for an entry with a matching group name.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function getgrnam (uu_name : Interfaces.C.Strings.chars_ptr) return access group  -- /usr/include/grp.h:111
   with Import => True, 
        Convention => C, 
        External_Name => "getgrnam";

  -- Reasonable value for the buffer sized used in the reentrant
  --   functions below.  But better use `sysconf'.   

  -- Reentrant versions of some of the functions above.
  --   PLEASE NOTE: the `getgrent_r' function is not (yet) standardized.
  --   The interface may change in later versions of this library.  But
  --   the interface is designed following the principals used for the
  --   other reentrant functions so the chances are good this is what the
  --   POSIX people would choose.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function getgrent_r
     (uu_resultbuf : access group;
      uu_buffer : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t;
      uu_result : System.Address) return int  -- /usr/include/grp.h:135
   with Import => True, 
        Convention => C, 
        External_Name => "getgrent_r";

  -- Search for an entry with a matching group ID.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function getgrgid_r
     (uu_gid : x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_resultbuf : access group;
      uu_buffer : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t;
      uu_result : System.Address) return int  -- /usr/include/grp.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "getgrgid_r";

  -- Search for an entry with a matching group name.
  --   This function is a possible cancellation point and therefore not
  --   marked with __THROW.   

   function getgrnam_r
     (uu_name : Interfaces.C.Strings.chars_ptr;
      uu_resultbuf : access group;
      uu_buffer : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t;
      uu_result : System.Address) return int  -- /usr/include/grp.h:152
   with Import => True, 
        Convention => C, 
        External_Name => "getgrnam_r";

  -- Read a group entry from STREAM.  This function is not standardized
  --   an probably never will.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function fgetgrent_r
     (uu_stream : access stdio_h.FILE;
      uu_resultbuf : access group;
      uu_buffer : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t;
      uu_result : System.Address) return int  -- /usr/include/grp.h:165
   with Import => True, 
        Convention => C, 
        External_Name => "fgetgrent_r";

  -- Set the group set for the current user to GROUPS (N of them).   
   function setgroups (uu_n : stddef_h.size_t; uu_groups : access x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/grp.h:180
   with Import => True, 
        Convention => C, 
        External_Name => "setgroups";

  -- Store at most *NGROUPS members of the group set for USER into
  --   *GROUPS.  Also include GROUP.  The actual number of groups found is
  --   returned in *NGROUPS.  Return -1 if the if *NGROUPS is too small.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function getgrouplist
     (uu_user : Interfaces.C.Strings.chars_ptr;
      uu_group : x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_groups : access x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_ngroups : access int) return int  -- /usr/include/grp.h:190
   with Import => True, 
        Convention => C, 
        External_Name => "getgrouplist";

  -- Initialize the group set for the current user
  --   by reading the group database and using all groups
  --   of which USER is a member.  Also include GROUP.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

   function initgroups (uu_user : Interfaces.C.Strings.chars_ptr; uu_group : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int  -- /usr/include/grp.h:201
   with Import => True, 
        Convention => C, 
        External_Name => "initgroups";

end grp_h;
