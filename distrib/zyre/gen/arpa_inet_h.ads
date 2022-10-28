pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with netinet_in_h;
with System;
with x86_64_linux_gnu_bits_socket_h;
with stddef_h;

package arpa_inet_h is

  -- Copyright (C) 1997-2014 Free Software Foundation, Inc.
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

  -- To define `struct in_addr'.   
  -- Type for length arguments in socket calls.   
  -- Convert Internet host address from numbers-and-dots notation in CP
  --   into binary data in network byte order.   

   function inet_addr (uu_cp : Interfaces.C.Strings.chars_ptr) return netinet_in_h.in_addr_t  -- /usr/include/arpa/inet.h:34
   with Import => True, 
        Convention => C, 
        External_Name => "inet_addr";

  -- Return the local host address part of the Internet address in IN.   
   function inet_lnaof (uu_in : netinet_in_h.in_addr) return netinet_in_h.in_addr_t  -- /usr/include/arpa/inet.h:37
   with Import => True, 
        Convention => C, 
        External_Name => "inet_lnaof";

  -- Make Internet host address in network byte order by combining the
  --   network number NET with the local address HOST.   

   function inet_makeaddr (uu_net : netinet_in_h.in_addr_t; uu_host : netinet_in_h.in_addr_t) return netinet_in_h.in_addr  -- /usr/include/arpa/inet.h:41
   with Import => True, 
        Convention => C, 
        External_Name => "inet_makeaddr";

  -- Return network number part of the Internet address IN.   
   function inet_netof (uu_in : netinet_in_h.in_addr) return netinet_in_h.in_addr_t  -- /usr/include/arpa/inet.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "inet_netof";

  -- Extract the network number in network byte order from the address
  --   in numbers-and-dots natation starting at CP.   

   function inet_network (uu_cp : Interfaces.C.Strings.chars_ptr) return netinet_in_h.in_addr_t  -- /usr/include/arpa/inet.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "inet_network";

  -- Convert Internet number in IN to ASCII representation.  The return value
  --   is a pointer to an internal array containing the string.   

   function inet_ntoa (uu_in : netinet_in_h.in_addr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/arpa/inet.h:53
   with Import => True, 
        Convention => C, 
        External_Name => "inet_ntoa";

  -- Convert from presentation format of an Internet number in buffer
  --   starting at CP to the binary network format and store result for
  --   interface type AF in buffer starting at BUF.   

   function inet_pton
     (uu_af : int;
      uu_cp : Interfaces.C.Strings.chars_ptr;
      uu_buf : System.Address) return int  -- /usr/include/arpa/inet.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "inet_pton";

  -- Convert a Internet address in binary network format for interface
  --   type AF in buffer starting at CP to presentation form and place
  --   result in buffer of length LEN astarting at BUF.   

   function inet_ntop
     (uu_af : int;
      uu_cp : System.Address;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_len : x86_64_linux_gnu_bits_socket_h.socklen_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/arpa/inet.h:64
   with Import => True, 
        Convention => C, 
        External_Name => "inet_ntop";

  -- The following functions are not part of XNS 5.2.   
  -- Convert Internet host address from numbers-and-dots notation in CP
  --   into binary data and store the result in the structure INP.   

   function inet_aton (uu_cp : Interfaces.C.Strings.chars_ptr; uu_inp : access netinet_in_h.in_addr) return int  -- /usr/include/arpa/inet.h:73
   with Import => True, 
        Convention => C, 
        External_Name => "inet_aton";

  -- Format a network number NET into presentation format and place result
  --   in buffer starting at BUF with length of LEN bytes.   

   function inet_neta
     (uu_net : netinet_in_h.in_addr_t;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_len : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/arpa/inet.h:77
   with Import => True, 
        Convention => C, 
        External_Name => "inet_neta";

  -- Convert network number for interface type AF in buffer starting at
  --   CP to presentation format.  The result will specifiy BITS bits of
  --   the number.   

   function inet_net_ntop
     (uu_af : int;
      uu_cp : System.Address;
      uu_bits : int;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_len : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/arpa/inet.h:82
   with Import => True, 
        Convention => C, 
        External_Name => "inet_net_ntop";

  -- Convert network number for interface type AF from presentation in
  --   buffer starting at CP to network format and store result int
  --   buffer starting at BUF of size LEN.   

   function inet_net_pton
     (uu_af : int;
      uu_cp : Interfaces.C.Strings.chars_ptr;
      uu_buf : System.Address;
      uu_len : stddef_h.size_t) return int  -- /usr/include/arpa/inet.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "inet_net_pton";

  -- Convert ASCII representation in hexadecimal form of the Internet
  --   address to binary form and place result in buffer of length LEN
  --   starting at BUF.   

   function inet_nsap_addr
     (uu_cp : Interfaces.C.Strings.chars_ptr;
      uu_buf : access unsigned_char;
      uu_len : int) return unsigned  -- /usr/include/arpa/inet.h:94
   with Import => True, 
        Convention => C, 
        External_Name => "inet_nsap_addr";

  -- Convert internet address in binary form in LEN bytes starting at CP
  --   a presentation form and place result in BUF.   

   function inet_nsap_ntoa
     (uu_len : int;
      uu_cp : access unsigned_char;
      uu_buf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/arpa/inet.h:99
   with Import => True, 
        Convention => C, 
        External_Name => "inet_nsap_ntoa";

end arpa_inet_h;
