pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with stdint_h;
with x86_64_linux_gnu_bits_sockaddr_h;
with x86_64_linux_gnu_bits_socket_h;
with System;

package netinet_in_h is

   --  unsupported macro: IPPROTO_IP IPPROTO_IP
   --  unsupported macro: IPPROTO_ICMP IPPROTO_ICMP
   --  unsupported macro: IPPROTO_IGMP IPPROTO_IGMP
   --  unsupported macro: IPPROTO_IPIP IPPROTO_IPIP
   --  unsupported macro: IPPROTO_TCP IPPROTO_TCP
   --  unsupported macro: IPPROTO_EGP IPPROTO_EGP
   --  unsupported macro: IPPROTO_PUP IPPROTO_PUP
   --  unsupported macro: IPPROTO_UDP IPPROTO_UDP
   --  unsupported macro: IPPROTO_IDP IPPROTO_IDP
   --  unsupported macro: IPPROTO_TP IPPROTO_TP
   --  unsupported macro: IPPROTO_DCCP IPPROTO_DCCP
   --  unsupported macro: IPPROTO_IPV6 IPPROTO_IPV6
   --  unsupported macro: IPPROTO_RSVP IPPROTO_RSVP
   --  unsupported macro: IPPROTO_GRE IPPROTO_GRE
   --  unsupported macro: IPPROTO_ESP IPPROTO_ESP
   --  unsupported macro: IPPROTO_AH IPPROTO_AH
   --  unsupported macro: IPPROTO_MTP IPPROTO_MTP
   --  unsupported macro: IPPROTO_BEETPH IPPROTO_BEETPH
   --  unsupported macro: IPPROTO_ENCAP IPPROTO_ENCAP
   --  unsupported macro: IPPROTO_PIM IPPROTO_PIM
   --  unsupported macro: IPPROTO_COMP IPPROTO_COMP
   --  unsupported macro: IPPROTO_SCTP IPPROTO_SCTP
   --  unsupported macro: IPPROTO_UDPLITE IPPROTO_UDPLITE
   --  unsupported macro: IPPROTO_RAW IPPROTO_RAW
   --  unsupported macro: IPPROTO_HOPOPTS IPPROTO_HOPOPTS
   --  unsupported macro: IPPROTO_ROUTING IPPROTO_ROUTING
   --  unsupported macro: IPPROTO_FRAGMENT IPPROTO_FRAGMENT
   --  unsupported macro: IPPROTO_ICMPV6 IPPROTO_ICMPV6
   --  unsupported macro: IPPROTO_NONE IPPROTO_NONE
   --  unsupported macro: IPPROTO_DSTOPTS IPPROTO_DSTOPTS
   --  unsupported macro: IPPROTO_MH IPPROTO_MH
   --  arg-macro: function IN_CLASSA (a)
   --    return (((in_addr_t)(a)) and 16#80000000#) = 0;
   IN_CLASSA_NET : constant := 16#ff000000#;  --  /usr/include/netinet/in.h:165
   IN_CLASSA_NSHIFT : constant := 24;  --  /usr/include/netinet/in.h:166
   --  unsupported macro: IN_CLASSA_HOST (0xffffffff & ~IN_CLASSA_NET)

   IN_CLASSA_MAX : constant := 128;  --  /usr/include/netinet/in.h:168
   --  arg-macro: function IN_CLASSB (a)
   --    return (((in_addr_t)(a)) and 16#c0000000#) = 16#80000000#;

   IN_CLASSB_NET : constant := 16#ffff0000#;  --  /usr/include/netinet/in.h:171
   IN_CLASSB_NSHIFT : constant := 16;  --  /usr/include/netinet/in.h:172
   --  unsupported macro: IN_CLASSB_HOST (0xffffffff & ~IN_CLASSB_NET)

   IN_CLASSB_MAX : constant := 65536;  --  /usr/include/netinet/in.h:174
   --  arg-macro: function IN_CLASSC (a)
   --    return (((in_addr_t)(a)) and 16#e0000000#) = 16#c0000000#;

   IN_CLASSC_NET : constant := 16#ffffff00#;  --  /usr/include/netinet/in.h:177
   IN_CLASSC_NSHIFT : constant := 8;  --  /usr/include/netinet/in.h:178
   --  unsupported macro: IN_CLASSC_HOST (0xffffffff & ~IN_CLASSC_NET)
   --  arg-macro: function IN_CLASSD (a)
   --    return (((in_addr_t)(a)) and 16#f0000000#) = 16#e0000000#;
   --  arg-macro: procedure IN_MULTICAST (a)
   --    IN_CLASSD(a)
   --  arg-macro: function IN_EXPERIMENTAL (a)
   --    return (((in_addr_t)(a)) and 16#e0000000#) = 16#e0000000#;
   --  arg-macro: function IN_BADCLASS (a)
   --    return (((in_addr_t)(a)) and 16#f0000000#) = 16#f0000000#;
   --  unsupported macro: INADDR_ANY ((in_addr_t) 0x00000000)
   --  unsupported macro: INADDR_BROADCAST ((in_addr_t) 0xffffffff)
   --  unsupported macro: INADDR_NONE ((in_addr_t) 0xffffffff)

   IN_LOOPBACKNET : constant := 127;  --  /usr/include/netinet/in.h:195
   --  unsupported macro: INADDR_LOOPBACK ((in_addr_t) 0x7f000001)
   --  unsupported macro: INADDR_UNSPEC_GROUP ((in_addr_t) 0xe0000000)
   --  unsupported macro: INADDR_ALLHOSTS_GROUP ((in_addr_t) 0xe0000001)
   --  unsupported macro: INADDR_ALLRTRS_GROUP ((in_addr_t) 0xe0000002)
   --  unsupported macro: INADDR_MAX_LOCAL_GROUP ((in_addr_t) 0xe00000ff)
   --  unsupported macro: s6_addr __in6_u.__u6_addr8
   --  unsupported macro: s6_addr16 __in6_u.__u6_addr16
   --  unsupported macro: s6_addr32 __in6_u.__u6_addr32
   --  unsupported macro: IN6ADDR_ANY_INIT { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 } } }
   --  unsupported macro: IN6ADDR_LOOPBACK_INIT { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 } } }

   INET_ADDRSTRLEN : constant := 16;  --  /usr/include/netinet/in.h:232
   INET6_ADDRSTRLEN : constant := 46;  --  /usr/include/netinet/in.h:233
   --  arg-macro: function IP_MSFILTER_SIZE (numsrc)
   --    return sizeof (struct ip_msfilter) - sizeof (struct in_addr) + (numsrc) * sizeof (struct in_addr);
   --  arg-macro: function GROUP_FILTER_SIZE (numsrc)
   --    return sizeof (struct group_filter) - sizeof (struct sockaddr_storage) + ((numsrc) * sizeof (struct sockaddr_storage));
   --  arg-macro: function IN6_IS_ADDR_UNSPECIFIED (a)
   --    return __extension__ ({ const struct in6_addr *__a := (const struct in6_addr *) (a); __a.s6_addr32(0) = 0  and then  __a.s6_addr32(1) = 0  and then  __a.s6_addr32(2) = 0  and then  __a.s6_addr32(3) = 0; });
   --  arg-macro: function IN6_IS_ADDR_LOOPBACK (a)
   --    return __extension__ ({ const struct in6_addr *__a := (const struct in6_addr *) (a); __a.s6_addr32(0) = 0  and then  __a.s6_addr32(1) = 0  and then  __a.s6_addr32(2) = 0  and then  __a.s6_addr32(3) = htonl (1); });
   --  arg-macro: function IN6_IS_ADDR_LINKLOCAL (a)
   --    return __extension__ ({ const struct in6_addr *__a := (const struct in6_addr *) (a); (__a.s6_addr32(0) and htonl (16#ffc00000#)) = htonl (16#fe800000#); });
   --  arg-macro: function IN6_IS_ADDR_SITELOCAL (a)
   --    return __extension__ ({ const struct in6_addr *__a := (const struct in6_addr *) (a); (__a.s6_addr32(0) and htonl (16#ffc00000#)) = htonl (16#fec00000#); });
   --  arg-macro: function IN6_IS_ADDR_V4MAPPED (a)
   --    return __extension__ ({ const struct in6_addr *__a := (const struct in6_addr *) (a); __a.s6_addr32(0) = 0  and then  __a.s6_addr32(1) = 0  and then  __a.s6_addr32(2) = htonl (16#ffff#); });
   --  arg-macro: function IN6_IS_ADDR_V4COMPAT (a)
   --    return __extension__ ({ const struct in6_addr *__a := (const struct in6_addr *) (a); __a.s6_addr32(0) = 0  and then  __a.s6_addr32(1) = 0  and then  __a.s6_addr32(2) = 0  and then  ntohl (__a.s6_addr32(3)) > 1; });
   --  arg-macro: function IN6_ARE_ADDR_EQUAL (a, b)
   --    return __extension__ ({ const struct in6_addr *__a := (const struct in6_addr *) (a); const struct in6_addr *__b := (const struct in6_addr *) (b); __a.s6_addr32(0) = __b.s6_addr32(0)  and then  __a.s6_addr32(1) = __b.s6_addr32(1)  and then  __a.s6_addr32(2) = __b.s6_addr32(2)  and then  __a.s6_addr32(3) = __b.s6_addr32(3); });
   --  arg-macro: function IN6_IS_ADDR_MULTICAST (a)
   --    return ((const uint8_t *) (a))(0) = 16#ff#;
   --  arg-macro: function IN6_IS_ADDR_MC_NODELOCAL (a)
   --    return IN6_IS_ADDR_MULTICAST(a)  and then  ((((const uint8_t *) (a))(1) and 16#f#) = 16#1#);
   --  arg-macro: function IN6_IS_ADDR_MC_LINKLOCAL (a)
   --    return IN6_IS_ADDR_MULTICAST(a)  and then  ((((const uint8_t *) (a))(1) and 16#f#) = 16#2#);
   --  arg-macro: function IN6_IS_ADDR_MC_SITELOCAL (a)
   --    return IN6_IS_ADDR_MULTICAST(a)  and then  ((((const uint8_t *) (a))(1) and 16#f#) = 16#5#);
   --  arg-macro: function IN6_IS_ADDR_MC_ORGLOCAL (a)
   --    return IN6_IS_ADDR_MULTICAST(a)  and then  ((((const uint8_t *) (a))(1) and 16#f#) = 16#8#);
   --  arg-macro: function IN6_IS_ADDR_MC_GLOBAL (a)
   --    return IN6_IS_ADDR_MULTICAST(a)  and then  ((((const uint8_t *) (a))(1) and 16#f#) = 16#e#);

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

  -- Internet address.   
   subtype in_addr_t is stdint_h.uint32_t;  -- /usr/include/netinet/in.h:30

   type in_addr is record
      s_addr : aliased in_addr_t;  -- /usr/include/netinet/in.h:33
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:31

  -- Get system-specific definitions.   
  -- Standard well-defined IP protocols.   
  -- Dummy protocol for TCP.   
  -- Internet Control Message Protocol.   
  -- Internet Group Management Protocol.  
  -- IPIP tunnels (older KA9Q tunnels use 94).   
  -- Transmission Control Protocol.   
  -- Exterior Gateway Protocol.   
  -- PUP protocol.   
  -- User Datagram Protocol.   
  -- XNS IDP protocol.   
  -- SO Transport Protocol Class 4.   
  -- Datagram Congestion Control Protocol.   
  -- IPv6 header.   
  -- Reservation Protocol.   
  -- General Routing Encapsulation.   
  -- encapsulating security payload.   
  -- authentication header.   
  -- Multicast Transport Protocol.   
  -- IP option pseudo header for BEET.   
  -- Encapsulation Header.   
  -- Protocol Independent Multicast.   
  -- Compression Header Protocol.   
  -- Stream Control Transmission Protocol.   
  -- UDP-Lite protocol.   
  -- Raw IP packets.   
  -- If __USE_KERNEL_IPV6_DEFS is defined then the user has included the kernel
  --   network headers first and we should use those ABI-identical definitions
  --   instead of our own.   

  -- IPv6 Hop-by-Hop options.   
  -- IPv6 routing header.   
  -- IPv6 fragmentation header.   
  -- ICMPv6.   
  -- IPv6 no next header.   
  -- IPv6 destination options.   
  -- IPv6 mobility header.   
  -- Type to represent a port.   
   subtype in_port_t is stdint_h.uint16_t;  -- /usr/include/netinet/in.h:117

  -- Standard well-known ports.   
  -- Echo service.   
  -- Discard transmissions service.   
  -- System status service.   
  -- Time of day service.   
  -- Network status service.   
  -- File Transfer Protocol.   
  -- Telnet protocol.   
  -- Simple Mail Transfer Protocol.   
  -- Timeserver service.   
  -- Domain Name Service.   
  -- Internet Whois service.   
  -- Trivial File Transfer Protocol.   
  -- Finger service.   
  -- SUPDUP protocol.   
  -- execd service.   
  -- rlogind service.   
  -- UDP ports.   
  -- Ports less than this value are reserved for privileged processes.   
  -- Ports greater this value are reserved for (non-privileged) servers.   
  -- Definitions of the bits in an Internet address integer.
  --   On subnets, host and network parts are found according to
  --   the subnet mask, not these masks.   

  -- Address to accept any incoming messages.   
  -- Address to send to all hosts.   
  -- Address indicating an error return.   
  -- Network number for local host loopback.   
  -- Address to loopback in software to local host.   
  -- Defines for Multicast INADDR.   
  -- IPv6 address  
   type in6_addr_array3180 is array (0 .. 15) of aliased stdint_h.uint8_t;
   type in6_addr_array3182 is array (0 .. 7) of aliased stdint_h.uint16_t;
   type in6_addr_array3183 is array (0 .. 3) of aliased stdint_h.uint32_t;
   type anon_96 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_u6_addr8 : aliased in6_addr_array3180;  -- /usr/include/netinet/in.h:213
         when 1 =>
            uu_u6_addr16 : aliased in6_addr_array3182;  -- /usr/include/netinet/in.h:215
         when others =>
            uu_u6_addr32 : aliased in6_addr_array3183;  -- /usr/include/netinet/in.h:216
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type in6_addr is record
      uu_in6_u : aliased anon_96;  -- /usr/include/netinet/in.h:218
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:209

  -- ::  
   in6addr_any : aliased constant in6_addr  -- /usr/include/netinet/in.h:227
   with Import => True, 
        Convention => C, 
        External_Name => "in6addr_any";

  -- ::1  
   in6addr_loopback : aliased constant in6_addr  -- /usr/include/netinet/in.h:228
   with Import => True, 
        Convention => C, 
        External_Name => "in6addr_loopback";

  -- Structure describing an Internet socket address.   
   type sockaddr_in_array3187 is array (0 .. 7) of aliased unsigned_char;
   type sockaddr_in is record
      sin_family : aliased x86_64_linux_gnu_bits_sockaddr_h.sa_family_t;  -- /usr/include/netinet/in.h:239
      sin_port : aliased in_port_t;  -- /usr/include/netinet/in.h:240
      sin_addr : aliased in_addr;  -- /usr/include/netinet/in.h:241
      sin_zero : aliased sockaddr_in_array3187;  -- /usr/include/netinet/in.h:244
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:237

  -- Port number.   
  -- Internet address.   
  -- Pad to size of `struct sockaddr'.   
  -- Ditto, for IPv6.   
   type sockaddr_in6 is record
      sin6_family : aliased x86_64_linux_gnu_bits_sockaddr_h.sa_family_t;  -- /usr/include/netinet/in.h:254
      sin6_port : aliased in_port_t;  -- /usr/include/netinet/in.h:255
      sin6_flowinfo : aliased stdint_h.uint32_t;  -- /usr/include/netinet/in.h:256
      sin6_addr : aliased in6_addr;  -- /usr/include/netinet/in.h:257
      sin6_scope_id : aliased stdint_h.uint32_t;  -- /usr/include/netinet/in.h:258
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:252

  -- Transport layer port #  
  -- IPv6 flow information  
  -- IPv6 address  
  -- IPv6 scope-id  
  -- IPv4 multicast request.   
  -- IP multicast address of group.   
   type ip_mreq is record
      imr_multiaddr : aliased in_addr;  -- /usr/include/netinet/in.h:267
      imr_interface : aliased in_addr;  -- /usr/include/netinet/in.h:270
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:264

  -- Local IP address of interface.   
  -- IP multicast address of group.   
   type ip_mreq_source is record
      imr_multiaddr : aliased in_addr;  -- /usr/include/netinet/in.h:276
      imr_interface : aliased in_addr;  -- /usr/include/netinet/in.h:279
      imr_sourceaddr : aliased in_addr;  -- /usr/include/netinet/in.h:282
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:273

  -- IP address of source.   
  -- IP address of interface.   
  -- Likewise, for IPv6.   
  -- IPv6 multicast address of group  
   type ipv6_mreq is record
      ipv6mr_multiaddr : aliased in6_addr;  -- /usr/include/netinet/in.h:291
      ipv6mr_interface : aliased unsigned;  -- /usr/include/netinet/in.h:294
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:288

  -- local interface  
  -- Multicast group request.   
  -- Interface index.   
   type group_req is record
      gr_interface : aliased stdint_h.uint32_t;  -- /usr/include/netinet/in.h:303
      gr_group : aliased x86_64_linux_gnu_bits_socket_h.sockaddr_storage;  -- /usr/include/netinet/in.h:306
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:300

  -- Group address.   
  -- Interface index.   
   type group_source_req is record
      gsr_interface : aliased stdint_h.uint32_t;  -- /usr/include/netinet/in.h:312
      gsr_group : aliased x86_64_linux_gnu_bits_socket_h.sockaddr_storage;  -- /usr/include/netinet/in.h:315
      gsr_source : aliased x86_64_linux_gnu_bits_socket_h.sockaddr_storage;  -- /usr/include/netinet/in.h:318
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:309

  -- Group address.   
  -- Source address.   
  -- Full-state filter operations.   
  -- IP multicast address of group.   
   type ip_msfilter_array3202 is array (0 .. 0) of aliased in_addr;
   type ip_msfilter is record
      imsf_multiaddr : aliased in_addr;  -- /usr/include/netinet/in.h:326
      imsf_interface : aliased in_addr;  -- /usr/include/netinet/in.h:329
      imsf_fmode : aliased stdint_h.uint32_t;  -- /usr/include/netinet/in.h:332
      imsf_numsrc : aliased stdint_h.uint32_t;  -- /usr/include/netinet/in.h:335
      imsf_slist : aliased ip_msfilter_array3202;  -- /usr/include/netinet/in.h:337
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:323

  -- Local IP address of interface.   
  -- Filter mode.   
  -- Number of source addresses.   
  -- Source addresses.   
  -- Interface index.   
   type group_filter_array3205 is array (0 .. 0) of aliased x86_64_linux_gnu_bits_socket_h.sockaddr_storage;
   type group_filter is record
      gf_interface : aliased stdint_h.uint32_t;  -- /usr/include/netinet/in.h:347
      gf_group : aliased x86_64_linux_gnu_bits_socket_h.sockaddr_storage;  -- /usr/include/netinet/in.h:350
      gf_fmode : aliased stdint_h.uint32_t;  -- /usr/include/netinet/in.h:353
      gf_numsrc : aliased stdint_h.uint32_t;  -- /usr/include/netinet/in.h:356
      gf_slist : aliased group_filter_array3205;  -- /usr/include/netinet/in.h:358
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:344

  -- Group address.   
  -- Filter mode.   
  -- Number of source addresses.   
  -- Source addresses.   
  -- Functions to convert between host and network byte order.
  --   Please note that these functions normally take `unsigned long int' or
  --   `unsigned short int' values as arguments and also return them.  But
  --   this was a short-sighted decision since on different systems the types
  --   may have different representations but the values are always the same.   

   function ntohl (uu_netlong : stdint_h.uint32_t) return stdint_h.uint32_t  -- /usr/include/netinet/in.h:374
   with Import => True, 
        Convention => C, 
        External_Name => "ntohl";

   function ntohs (uu_netshort : stdint_h.uint16_t) return stdint_h.uint16_t  -- /usr/include/netinet/in.h:375
   with Import => True, 
        Convention => C, 
        External_Name => "ntohs";

   function htonl (uu_hostlong : stdint_h.uint32_t) return stdint_h.uint32_t  -- /usr/include/netinet/in.h:377
   with Import => True, 
        Convention => C, 
        External_Name => "htonl";

   function htons (uu_hostshort : stdint_h.uint16_t) return stdint_h.uint16_t  -- /usr/include/netinet/in.h:379
   with Import => True, 
        Convention => C, 
        External_Name => "htons";

  -- Get machine dependent optimized versions of byte swapping functions.   
  -- We can optimize calls to the conversion functions.  Either nothing has
  --   to be done or we are using directly the byte-swapping functions which
  --   often can be inlined.   

  -- The host byte order is the same as network byte order,
  --   so these functions are all just identity.   

  -- Bind socket to a privileged IP port.   
   function bindresvport (uu_sockfd : int; uu_sock_in : access sockaddr_in) return int  -- /usr/include/netinet/in.h:501
   with Import => True, 
        Convention => C, 
        External_Name => "bindresvport";

  -- The IPv6 version of this function.   
   function bindresvport6 (uu_sockfd : int; uu_sock_in : access sockaddr_in6) return int  -- /usr/include/netinet/in.h:504
   with Import => True, 
        Convention => C, 
        External_Name => "bindresvport6";

  -- Forward declaration.   
  -- IPv6 packet information.   
  -- src/dst IPv6 address  
   type in6_pktinfo is record
      ipi6_addr : aliased in6_addr;  -- /usr/include/netinet/in.h:536
      ipi6_ifindex : aliased unsigned;  -- /usr/include/netinet/in.h:537
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:534

  -- send/recv interface index  
  -- IPv6 MTU information.   
  -- dst address including zone ID  
   type ip6_mtuinfo is record
      ip6m_addr : aliased sockaddr_in6;  -- /usr/include/netinet/in.h:543
      ip6m_mtu : aliased stdint_h.uint32_t;  -- /usr/include/netinet/in.h:544
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/in.h:541

  -- path MTU in host byte order  
  -- Obsolete hop-by-hop and Destination Options Processing (RFC 2292).   
   function inet6_option_space (uu_nbytes : int) return int  -- /usr/include/netinet/in.h:549
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_option_space";

   function inet6_option_init
     (uu_bp : System.Address;
      uu_cmsgp : System.Address;
      uu_type : int) return int  -- /usr/include/netinet/in.h:551
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_option_init";

   function inet6_option_append
     (uu_cmsg : access x86_64_linux_gnu_bits_socket_h.cmsghdr;
      uu_typep : access stdint_h.uint8_t;
      uu_multx : int;
      uu_plusy : int) return int  -- /usr/include/netinet/in.h:553
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_option_append";

   function inet6_option_alloc
     (uu_cmsg : access x86_64_linux_gnu_bits_socket_h.cmsghdr;
      uu_datalen : int;
      uu_multx : int;
      uu_plusy : int) return access stdint_h.uint8_t  -- /usr/include/netinet/in.h:556
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_option_alloc";

   function inet6_option_next (uu_cmsg : access constant x86_64_linux_gnu_bits_socket_h.cmsghdr; uu_tptrp : System.Address) return int  -- /usr/include/netinet/in.h:559
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_option_next";

   function inet6_option_find
     (uu_cmsg : access constant x86_64_linux_gnu_bits_socket_h.cmsghdr;
      uu_tptrp : System.Address;
      uu_type : int) return int  -- /usr/include/netinet/in.h:562
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_option_find";

  -- Hop-by-Hop and Destination Options Processing (RFC 3542).   
   function inet6_opt_init (uu_extbuf : System.Address; uu_extlen : x86_64_linux_gnu_bits_socket_h.socklen_t) return int  -- /usr/include/netinet/in.h:568
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_opt_init";

   function inet6_opt_append
     (uu_extbuf : System.Address;
      uu_extlen : x86_64_linux_gnu_bits_socket_h.socklen_t;
      uu_offset : int;
      uu_type : stdint_h.uint8_t;
      uu_len : x86_64_linux_gnu_bits_socket_h.socklen_t;
      uu_align : stdint_h.uint8_t;
      uu_databufp : System.Address) return int  -- /usr/include/netinet/in.h:569
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_opt_append";

   function inet6_opt_finish
     (uu_extbuf : System.Address;
      uu_extlen : x86_64_linux_gnu_bits_socket_h.socklen_t;
      uu_offset : int) return int  -- /usr/include/netinet/in.h:572
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_opt_finish";

   function inet6_opt_set_val
     (uu_databuf : System.Address;
      uu_offset : int;
      uu_val : System.Address;
      uu_vallen : x86_64_linux_gnu_bits_socket_h.socklen_t) return int  -- /usr/include/netinet/in.h:574
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_opt_set_val";

   function inet6_opt_next
     (uu_extbuf : System.Address;
      uu_extlen : x86_64_linux_gnu_bits_socket_h.socklen_t;
      uu_offset : int;
      uu_typep : access stdint_h.uint8_t;
      uu_lenp : access x86_64_linux_gnu_bits_socket_h.socklen_t;
      uu_databufp : System.Address) return int  -- /usr/include/netinet/in.h:576
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_opt_next";

   function inet6_opt_find
     (uu_extbuf : System.Address;
      uu_extlen : x86_64_linux_gnu_bits_socket_h.socklen_t;
      uu_offset : int;
      uu_type : stdint_h.uint8_t;
      uu_lenp : access x86_64_linux_gnu_bits_socket_h.socklen_t;
      uu_databufp : System.Address) return int  -- /usr/include/netinet/in.h:579
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_opt_find";

   function inet6_opt_get_val
     (uu_databuf : System.Address;
      uu_offset : int;
      uu_val : System.Address;
      uu_vallen : x86_64_linux_gnu_bits_socket_h.socklen_t) return int  -- /usr/include/netinet/in.h:582
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_opt_get_val";

  -- Routing Header Option (RFC 3542).   
   function inet6_rth_space (uu_type : int; uu_segments : int) return x86_64_linux_gnu_bits_socket_h.socklen_t  -- /usr/include/netinet/in.h:587
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_rth_space";

   function inet6_rth_init
     (uu_bp : System.Address;
      uu_bp_len : x86_64_linux_gnu_bits_socket_h.socklen_t;
      uu_type : int;
      uu_segments : int) return System.Address  -- /usr/include/netinet/in.h:588
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_rth_init";

   function inet6_rth_add (uu_bp : System.Address; uu_addr : access constant in6_addr) return int  -- /usr/include/netinet/in.h:590
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_rth_add";

   function inet6_rth_reverse (uu_in : System.Address; uu_out : System.Address) return int  -- /usr/include/netinet/in.h:591
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_rth_reverse";

   function inet6_rth_segments (uu_bp : System.Address) return int  -- /usr/include/netinet/in.h:592
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_rth_segments";

   function inet6_rth_getaddr (uu_bp : System.Address; uu_index : int) return access in6_addr  -- /usr/include/netinet/in.h:593
   with Import => True, 
        Convention => C, 
        External_Name => "inet6_rth_getaddr";

  -- Multicast source filter support.   
  -- Get IPv4 source filter.   
   function getipv4sourcefilter
     (uu_s : int;
      uu_interface_addr : in_addr;
      uu_group : in_addr;
      uu_fmode : access stdint_h.uint32_t;
      uu_numsrc : access stdint_h.uint32_t;
      uu_slist : access in_addr) return int  -- /usr/include/netinet/in.h:600
   with Import => True, 
        Convention => C, 
        External_Name => "getipv4sourcefilter";

  -- Set IPv4 source filter.   
   function setipv4sourcefilter
     (uu_s : int;
      uu_interface_addr : in_addr;
      uu_group : in_addr;
      uu_fmode : stdint_h.uint32_t;
      uu_numsrc : stdint_h.uint32_t;
      uu_slist : access constant in_addr) return int  -- /usr/include/netinet/in.h:606
   with Import => True, 
        Convention => C, 
        External_Name => "setipv4sourcefilter";

  -- Get source filter.   
   function getsourcefilter
     (uu_s : int;
      uu_interface_addr : stdint_h.uint32_t;
      uu_group : access constant x86_64_linux_gnu_bits_socket_h.sockaddr;
      uu_grouplen : x86_64_linux_gnu_bits_socket_h.socklen_t;
      uu_fmode : access stdint_h.uint32_t;
      uu_numsrc : access stdint_h.uint32_t;
      uu_slist : access x86_64_linux_gnu_bits_socket_h.sockaddr_storage) return int  -- /usr/include/netinet/in.h:614
   with Import => True, 
        Convention => C, 
        External_Name => "getsourcefilter";

  -- Set source filter.   
   function setsourcefilter
     (uu_s : int;
      uu_interface_addr : stdint_h.uint32_t;
      uu_group : access constant x86_64_linux_gnu_bits_socket_h.sockaddr;
      uu_grouplen : x86_64_linux_gnu_bits_socket_h.socklen_t;
      uu_fmode : stdint_h.uint32_t;
      uu_numsrc : stdint_h.uint32_t;
      uu_slist : access constant x86_64_linux_gnu_bits_socket_h.sockaddr_storage) return int  -- /usr/include/netinet/in.h:621
   with Import => True, 
        Convention => C, 
        External_Name => "setsourcefilter";

end netinet_in_h;
