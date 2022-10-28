pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
limited with x86_64_linux_gnu_bits_socket_h;
with Interfaces.C.Strings;
with System;

package ifaddrs_h is

   --  unsupported macro: ifa_broadaddr ifa_ifu.ifu_broadaddr
   --  unsupported macro: ifa_dstaddr ifa_ifu.ifu_dstaddr
  -- ifaddrs.h -- declarations for getting network interface addresses
  --   Copyright (C) 2002-2014 Free Software Foundation, Inc.
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

  -- The `getifaddrs' function generates a linked list of these structures.
  --   Each element of the list describes one network interface.   

  -- Pointer to the next structure.   
   type anon_115 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            ifu_broadaddr : access x86_64_linux_gnu_bits_socket_h.sockaddr;  -- /usr/include/ifaddrs.h:44
         when others =>
            ifu_dstaddr : access x86_64_linux_gnu_bits_socket_h.sockaddr;  -- /usr/include/ifaddrs.h:45
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type ifaddrs;
   type ifaddrs is record
      ifa_next : access ifaddrs;  -- /usr/include/ifaddrs.h:31
      ifa_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/ifaddrs.h:33
      ifa_flags : aliased unsigned;  -- /usr/include/ifaddrs.h:34
      ifa_addr : access x86_64_linux_gnu_bits_socket_h.sockaddr;  -- /usr/include/ifaddrs.h:36
      ifa_netmask : access x86_64_linux_gnu_bits_socket_h.sockaddr;  -- /usr/include/ifaddrs.h:37
      ifa_ifu : aliased anon_115;  -- /usr/include/ifaddrs.h:46
      ifa_data : System.Address;  -- /usr/include/ifaddrs.h:56
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/ifaddrs.h:29

  -- Name of this network interface.   
  -- Flags as from SIOCGIFFLAGS ioctl.   
  -- Network address of this interface.   
  -- Netmask of this interface.   
  -- At most one of the following two is valid.  If the IFF_BROADCAST
  --       bit is set in `ifa_flags', then `ifa_broadaddr' is valid.  If the
  --       IFF_POINTOPOINT bit is set, then `ifa_dstaddr' is valid.
  --       It is never the case that both these bits are set at once.   

  -- Broadcast address of this interface.  
  -- Point-to-point destination address.   
  -- These very same macros are defined by <net/if.h> for `struct ifaddr'.
  --     So if they are defined already, the existing definitions will be fine.   

  -- Address-specific data (may be unused).   
  -- Create a linked list of `struct ifaddrs' structures, one for each
  --   network interface on the host machine.  If successful, store the
  --   list in *IFAP and return 0.  On errors, return -1 and set `errno'.
  --   The storage returned in *IFAP is allocated dynamically and can
  --   only be properly freed by passing it to `freeifaddrs'.   

   function getifaddrs (uu_ifap : System.Address) return int  -- /usr/include/ifaddrs.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "getifaddrs";

  -- Reclaim the storage allocated by a previous `getifaddrs' call.   
   procedure freeifaddrs (uu_ifa : access ifaddrs)  -- /usr/include/ifaddrs.h:69
   with Import => True, 
        Convention => C, 
        External_Name => "freeifaddrs";

end ifaddrs_h;
