pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package x86_64_linux_gnu_bits_socket_type_h is

   --  unsupported macro: SOCK_STREAM SOCK_STREAM
   --  unsupported macro: SOCK_DGRAM SOCK_DGRAM
   --  unsupported macro: SOCK_RAW SOCK_RAW
   --  unsupported macro: SOCK_RDM SOCK_RDM
   --  unsupported macro: SOCK_SEQPACKET SOCK_SEQPACKET
   --  unsupported macro: SOCK_DCCP SOCK_DCCP
   --  unsupported macro: SOCK_PACKET SOCK_PACKET
   --  unsupported macro: SOCK_CLOEXEC SOCK_CLOEXEC
   --  unsupported macro: SOCK_NONBLOCK SOCK_NONBLOCK
  -- Define enum __socket_type for generic Linux.
  --   Copyright (C) 1991-2014 Free Software Foundation, Inc.
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

  -- Types of sockets.   
   subtype uu_socket_type is unsigned;
   SOCK_STREAM : constant unsigned := 1;
   SOCK_DGRAM : constant unsigned := 2;
   SOCK_RAW : constant unsigned := 3;
   SOCK_RDM : constant unsigned := 4;
   SOCK_SEQPACKET : constant unsigned := 5;
   SOCK_DCCP : constant unsigned := 6;
   SOCK_PACKET : constant unsigned := 10;
   SOCK_CLOEXEC : constant unsigned := 524288;
   SOCK_NONBLOCK : constant unsigned := 2048;  -- /usr/include/x86_64-linux-gnu/bits/socket_type.h:24

  -- Sequenced, reliable, connection-based
  --				   byte streams.   

  -- Connectionless, unreliable datagrams
  --				   of fixed maximum length.   

  -- Raw protocol interface.   
  -- Reliably-delivered messages.   
  -- Sequenced, reliable, connection-based,
  --				   datagrams of fixed maximum length.   

  -- Datagram Congestion Control Protocol.   
  -- Linux specific way of getting packets
  --				   at the dev level.  For writing rarp and
  --				   other similar things on the user level.  

  -- Flags to be ORed into the type parameter of socket and socketpair and
  --     used for the flags parameter of paccept.   

  -- Atomically set close-on-exec flag for the
  --				   new descriptor(s).   

  -- Atomically mark descriptor(s) as
  --				   non-blocking.   

end x86_64_linux_gnu_bits_socket_type_h;
