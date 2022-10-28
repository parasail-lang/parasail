pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with netinet_in_h;

package x86_64_linux_gnu_bits_in_h is

   IP_OPTIONS : constant := 4;  --  /usr/include/x86_64-linux-gnu/bits/in.h:39
   IP_HDRINCL : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/in.h:40
   IP_TOS : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/in.h:41
   IP_TTL : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/in.h:42
   IP_RECVOPTS : constant := 6;  --  /usr/include/x86_64-linux-gnu/bits/in.h:43
   --  unsupported macro: IP_RECVRETOPTS IP_RETOPTS

   IP_RETOPTS : constant := 7;  --  /usr/include/x86_64-linux-gnu/bits/in.h:46
   IP_MULTICAST_IF : constant := 32;  --  /usr/include/x86_64-linux-gnu/bits/in.h:47
   IP_MULTICAST_TTL : constant := 33;  --  /usr/include/x86_64-linux-gnu/bits/in.h:48
   IP_MULTICAST_LOOP : constant := 34;  --  /usr/include/x86_64-linux-gnu/bits/in.h:49
   IP_ADD_MEMBERSHIP : constant := 35;  --  /usr/include/x86_64-linux-gnu/bits/in.h:50
   IP_DROP_MEMBERSHIP : constant := 36;  --  /usr/include/x86_64-linux-gnu/bits/in.h:51
   IP_UNBLOCK_SOURCE : constant := 37;  --  /usr/include/x86_64-linux-gnu/bits/in.h:52
   IP_BLOCK_SOURCE : constant := 38;  --  /usr/include/x86_64-linux-gnu/bits/in.h:53
   IP_ADD_SOURCE_MEMBERSHIP : constant := 39;  --  /usr/include/x86_64-linux-gnu/bits/in.h:54
   IP_DROP_SOURCE_MEMBERSHIP : constant := 40;  --  /usr/include/x86_64-linux-gnu/bits/in.h:55
   IP_MSFILTER : constant := 41;  --  /usr/include/x86_64-linux-gnu/bits/in.h:56

   MCAST_JOIN_GROUP : constant := 42;  --  /usr/include/x86_64-linux-gnu/bits/in.h:58
   MCAST_BLOCK_SOURCE : constant := 43;  --  /usr/include/x86_64-linux-gnu/bits/in.h:59
   MCAST_UNBLOCK_SOURCE : constant := 44;  --  /usr/include/x86_64-linux-gnu/bits/in.h:60
   MCAST_LEAVE_GROUP : constant := 45;  --  /usr/include/x86_64-linux-gnu/bits/in.h:61
   MCAST_JOIN_SOURCE_GROUP : constant := 46;  --  /usr/include/x86_64-linux-gnu/bits/in.h:62
   MCAST_LEAVE_SOURCE_GROUP : constant := 47;  --  /usr/include/x86_64-linux-gnu/bits/in.h:63
   MCAST_MSFILTER : constant := 48;  --  /usr/include/x86_64-linux-gnu/bits/in.h:64

   IP_UNICAST_IF : constant := 50;  --  /usr/include/x86_64-linux-gnu/bits/in.h:66

   MCAST_EXCLUDE : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/in.h:68
   MCAST_INCLUDE : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/in.h:69

   IP_ROUTER_ALERT : constant := 5;  --  /usr/include/x86_64-linux-gnu/bits/in.h:72
   IP_PKTINFO : constant := 8;  --  /usr/include/x86_64-linux-gnu/bits/in.h:73
   IP_PKTOPTIONS : constant := 9;  --  /usr/include/x86_64-linux-gnu/bits/in.h:74
   IP_PMTUDISC : constant := 10;  --  /usr/include/x86_64-linux-gnu/bits/in.h:75
   IP_MTU_DISCOVER : constant := 10;  --  /usr/include/x86_64-linux-gnu/bits/in.h:76
   IP_RECVERR : constant := 11;  --  /usr/include/x86_64-linux-gnu/bits/in.h:77
   IP_RECVTTL : constant := 12;  --  /usr/include/x86_64-linux-gnu/bits/in.h:78
   IP_RECVTOS : constant := 13;  --  /usr/include/x86_64-linux-gnu/bits/in.h:79
   IP_MTU : constant := 14;  --  /usr/include/x86_64-linux-gnu/bits/in.h:80
   IP_FREEBIND : constant := 15;  --  /usr/include/x86_64-linux-gnu/bits/in.h:81
   IP_IPSEC_POLICY : constant := 16;  --  /usr/include/x86_64-linux-gnu/bits/in.h:82
   IP_XFRM_POLICY : constant := 17;  --  /usr/include/x86_64-linux-gnu/bits/in.h:83
   IP_PASSSEC : constant := 18;  --  /usr/include/x86_64-linux-gnu/bits/in.h:84
   IP_TRANSPARENT : constant := 19;  --  /usr/include/x86_64-linux-gnu/bits/in.h:85
   IP_MULTICAST_ALL : constant := 49;  --  /usr/include/x86_64-linux-gnu/bits/in.h:86

   IP_ORIGDSTADDR : constant := 20;  --  /usr/include/x86_64-linux-gnu/bits/in.h:89
   --  unsupported macro: IP_RECVORIGDSTADDR IP_ORIGDSTADDR

   IP_MINTTL : constant := 21;  --  /usr/include/x86_64-linux-gnu/bits/in.h:92

   IP_PMTUDISC_DONT : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/in.h:96
   IP_PMTUDISC_WANT : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/in.h:97
   IP_PMTUDISC_DO : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/in.h:98
   IP_PMTUDISC_PROBE : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/in.h:99

   SOL_IP : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/in.h:102

   IP_DEFAULT_MULTICAST_TTL : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/in.h:104
   IP_DEFAULT_MULTICAST_LOOP : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/in.h:105
   IP_MAX_MEMBERSHIPS : constant := 20;  --  /usr/include/x86_64-linux-gnu/bits/in.h:106

   IPV6_ADDRFORM : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/in.h:138
   IPV6_2292PKTINFO : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/in.h:139
   IPV6_2292HOPOPTS : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/in.h:140
   IPV6_2292DSTOPTS : constant := 4;  --  /usr/include/x86_64-linux-gnu/bits/in.h:141
   IPV6_2292RTHDR : constant := 5;  --  /usr/include/x86_64-linux-gnu/bits/in.h:142
   IPV6_2292PKTOPTIONS : constant := 6;  --  /usr/include/x86_64-linux-gnu/bits/in.h:143
   IPV6_CHECKSUM : constant := 7;  --  /usr/include/x86_64-linux-gnu/bits/in.h:144
   IPV6_2292HOPLIMIT : constant := 8;  --  /usr/include/x86_64-linux-gnu/bits/in.h:145
   --  unsupported macro: SCM_SRCRT IPV6_RXSRCRT

   IPV6_NEXTHOP : constant := 9;  --  /usr/include/x86_64-linux-gnu/bits/in.h:149
   IPV6_AUTHHDR : constant := 10;  --  /usr/include/x86_64-linux-gnu/bits/in.h:150
   IPV6_UNICAST_HOPS : constant := 16;  --  /usr/include/x86_64-linux-gnu/bits/in.h:151
   IPV6_MULTICAST_IF : constant := 17;  --  /usr/include/x86_64-linux-gnu/bits/in.h:152
   IPV6_MULTICAST_HOPS : constant := 18;  --  /usr/include/x86_64-linux-gnu/bits/in.h:153
   IPV6_MULTICAST_LOOP : constant := 19;  --  /usr/include/x86_64-linux-gnu/bits/in.h:154
   IPV6_JOIN_GROUP : constant := 20;  --  /usr/include/x86_64-linux-gnu/bits/in.h:155
   IPV6_LEAVE_GROUP : constant := 21;  --  /usr/include/x86_64-linux-gnu/bits/in.h:156
   IPV6_ROUTER_ALERT : constant := 22;  --  /usr/include/x86_64-linux-gnu/bits/in.h:157
   IPV6_MTU_DISCOVER : constant := 23;  --  /usr/include/x86_64-linux-gnu/bits/in.h:158
   IPV6_MTU : constant := 24;  --  /usr/include/x86_64-linux-gnu/bits/in.h:159
   IPV6_RECVERR : constant := 25;  --  /usr/include/x86_64-linux-gnu/bits/in.h:160
   IPV6_V6ONLY : constant := 26;  --  /usr/include/x86_64-linux-gnu/bits/in.h:161
   IPV6_JOIN_ANYCAST : constant := 27;  --  /usr/include/x86_64-linux-gnu/bits/in.h:162
   IPV6_LEAVE_ANYCAST : constant := 28;  --  /usr/include/x86_64-linux-gnu/bits/in.h:163
   IPV6_IPSEC_POLICY : constant := 34;  --  /usr/include/x86_64-linux-gnu/bits/in.h:164
   IPV6_XFRM_POLICY : constant := 35;  --  /usr/include/x86_64-linux-gnu/bits/in.h:165

   IPV6_RECVPKTINFO : constant := 49;  --  /usr/include/x86_64-linux-gnu/bits/in.h:167
   IPV6_PKTINFO : constant := 50;  --  /usr/include/x86_64-linux-gnu/bits/in.h:168
   IPV6_RECVHOPLIMIT : constant := 51;  --  /usr/include/x86_64-linux-gnu/bits/in.h:169
   IPV6_HOPLIMIT : constant := 52;  --  /usr/include/x86_64-linux-gnu/bits/in.h:170
   IPV6_RECVHOPOPTS : constant := 53;  --  /usr/include/x86_64-linux-gnu/bits/in.h:171
   IPV6_HOPOPTS : constant := 54;  --  /usr/include/x86_64-linux-gnu/bits/in.h:172
   IPV6_RTHDRDSTOPTS : constant := 55;  --  /usr/include/x86_64-linux-gnu/bits/in.h:173
   IPV6_RECVRTHDR : constant := 56;  --  /usr/include/x86_64-linux-gnu/bits/in.h:174
   IPV6_RTHDR : constant := 57;  --  /usr/include/x86_64-linux-gnu/bits/in.h:175
   IPV6_RECVDSTOPTS : constant := 58;  --  /usr/include/x86_64-linux-gnu/bits/in.h:176
   IPV6_DSTOPTS : constant := 59;  --  /usr/include/x86_64-linux-gnu/bits/in.h:177

   IPV6_RECVTCLASS : constant := 66;  --  /usr/include/x86_64-linux-gnu/bits/in.h:179
   IPV6_TCLASS : constant := 67;  --  /usr/include/x86_64-linux-gnu/bits/in.h:180
   --  unsupported macro: IPV6_ADD_MEMBERSHIP IPV6_JOIN_GROUP
   --  unsupported macro: IPV6_DROP_MEMBERSHIP IPV6_LEAVE_GROUP
   --  unsupported macro: IPV6_RXHOPOPTS IPV6_HOPOPTS
   --  unsupported macro: IPV6_RXDSTOPTS IPV6_DSTOPTS

   IPV6_PMTUDISC_DONT : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/in.h:189
   IPV6_PMTUDISC_WANT : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/in.h:190
   IPV6_PMTUDISC_DO : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/in.h:191
   IPV6_PMTUDISC_PROBE : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/in.h:192

   SOL_IPV6 : constant := 41;  --  /usr/include/x86_64-linux-gnu/bits/in.h:195
   SOL_ICMPV6 : constant := 58;  --  /usr/include/x86_64-linux-gnu/bits/in.h:196

   IPV6_RTHDR_LOOSE : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/in.h:199
   IPV6_RTHDR_STRICT : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/in.h:200

   IPV6_RTHDR_TYPE_0 : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/in.h:202

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

  -- Linux version.   
  -- If the application has already included linux/in6.h from a linux-based
  --   kernel then we will not define the IPv6 IPPROTO_* defines, in6_addr (nor the
  --   defines), sockaddr_in6, or ipv6_mreq.  The ABI used by the linux-kernel and
  --   glibc match exactly.  Neither the linux kernel nor glibc should break this
  --   ABI without coordination.   

  -- This is not quite the same API since the kernel always defines s6_addr16 and
  --   s6_addr32. This is not a violation of POSIX since POSIX says "at least the
  --   following member" and that holds true.   

  -- Options for use with `getsockopt' and `setsockopt' at the IP level.
  --   The first word in the comment at the right is the data type used;
  --   "bool" means a boolean value stored in an `int'.   

  -- For BSD compatibility.   
  -- TProxy original addresses  
  -- IP_MTU_DISCOVER arguments.   
  -- To select the IP level.   
  -- Structure used to describe IP options for IP_OPTIONS and IP_RETOPTS.
  --   The `ip_dst' field is used for the first-hop gateway when using a
  --   source route (this gets put into the header proper).   

  -- First hop; zero without source route.   
   subtype ip_opts_array1470 is Interfaces.C.char_array (0 .. 39);
   type ip_opts is record
      ip_dst : aliased netinet_in_h.in_addr;  -- /usr/include/x86_64-linux-gnu/bits/in.h:114
      ip_opts : aliased ip_opts_array1470;  -- /usr/include/x86_64-linux-gnu/bits/in.h:115
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/in.h:112

  -- Actually variable in size.   
  -- Like `struct ip_mreq' but including interface specification by index.   
  -- IP multicast address of group  
   type ip_mreqn is record
      imr_multiaddr : aliased netinet_in_h.in_addr;  -- /usr/include/x86_64-linux-gnu/bits/in.h:121
      imr_address : aliased netinet_in_h.in_addr;  -- /usr/include/x86_64-linux-gnu/bits/in.h:122
      imr_ifindex : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/in.h:123
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/in.h:119

  -- local IP address of interface  
  -- Interface index  
  -- Structure used for IP_PKTINFO.   
  -- Interface index   
   type in_pktinfo is record
      ipi_ifindex : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/in.h:129
      ipi_spec_dst : aliased netinet_in_h.in_addr;  -- /usr/include/x86_64-linux-gnu/bits/in.h:130
      ipi_addr : aliased netinet_in_h.in_addr;  -- /usr/include/x86_64-linux-gnu/bits/in.h:131
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/in.h:127

  -- Routing destination address   
  -- Header destination address   
  -- Options for use with `getsockopt' and `setsockopt' at the IPv6 level.
  --   The first word in the comment at the right is the data type used;
  --   "bool" means a boolean value stored in an `int'.   

  -- Obsolete synonyms for the above.   
  -- IPV6_MTU_DISCOVER values.   
  -- Socket level values for IPv6.   
  -- Routing header options for IPv6.   
end x86_64_linux_gnu_bits_in_h;
