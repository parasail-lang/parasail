pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
with x86_64_linux_gnu_bits_sockaddr_h;
with System;
limited with x86_64_linux_gnu_bits_uio_h;
with stddef_h;
with x86_64_linux_gnu_sys_types_h;

package x86_64_linux_gnu_bits_socket_h is

   PF_UNSPEC : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:41
   PF_LOCAL : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:42
   --  unsupported macro: PF_UNIX PF_LOCAL
   --  unsupported macro: PF_FILE PF_LOCAL

   PF_INET : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:45
   PF_AX25 : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:46
   PF_IPX : constant := 4;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:47
   PF_APPLETALK : constant := 5;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:48
   PF_NETROM : constant := 6;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:49
   PF_BRIDGE : constant := 7;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:50
   PF_ATMPVC : constant := 8;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:51
   PF_X25 : constant := 9;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:52
   PF_INET6 : constant := 10;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:53
   PF_ROSE : constant := 11;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:54
   PF_DECnet : constant := 12;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:55
   PF_NETBEUI : constant := 13;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:56
   PF_SECURITY : constant := 14;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:57
   PF_KEY : constant := 15;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:58
   PF_NETLINK : constant := 16;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:59
   --  unsupported macro: PF_ROUTE PF_NETLINK

   PF_PACKET : constant := 17;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:61
   PF_ASH : constant := 18;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:62
   PF_ECONET : constant := 19;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:63
   PF_ATMSVC : constant := 20;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:64
   PF_RDS : constant := 21;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:65
   PF_SNA : constant := 22;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:66
   PF_IRDA : constant := 23;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:67
   PF_PPPOX : constant := 24;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:68
   PF_WANPIPE : constant := 25;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:69
   PF_LLC : constant := 26;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:70
   PF_CAN : constant := 29;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:71
   PF_TIPC : constant := 30;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:72
   PF_BLUETOOTH : constant := 31;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:73
   PF_IUCV : constant := 32;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:74
   PF_RXRPC : constant := 33;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:75
   PF_ISDN : constant := 34;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:76
   PF_PHONET : constant := 35;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:77
   PF_IEEE802154 : constant := 36;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:78
   PF_CAIF : constant := 37;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:79
   PF_ALG : constant := 38;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:80
   PF_NFC : constant := 39;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:81
   PF_VSOCK : constant := 40;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:82
   PF_MAX : constant := 41;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:83
   --  unsupported macro: AF_UNSPEC PF_UNSPEC
   --  unsupported macro: AF_LOCAL PF_LOCAL
   --  unsupported macro: AF_UNIX PF_UNIX
   --  unsupported macro: AF_FILE PF_FILE
   --  unsupported macro: AF_INET PF_INET
   --  unsupported macro: AF_AX25 PF_AX25
   --  unsupported macro: AF_IPX PF_IPX
   --  unsupported macro: AF_APPLETALK PF_APPLETALK
   --  unsupported macro: AF_NETROM PF_NETROM
   --  unsupported macro: AF_BRIDGE PF_BRIDGE
   --  unsupported macro: AF_ATMPVC PF_ATMPVC
   --  unsupported macro: AF_X25 PF_X25
   --  unsupported macro: AF_INET6 PF_INET6
   --  unsupported macro: AF_ROSE PF_ROSE
   --  unsupported macro: AF_DECnet PF_DECnet
   --  unsupported macro: AF_NETBEUI PF_NETBEUI
   --  unsupported macro: AF_SECURITY PF_SECURITY
   --  unsupported macro: AF_KEY PF_KEY
   --  unsupported macro: AF_NETLINK PF_NETLINK
   --  unsupported macro: AF_ROUTE PF_ROUTE
   --  unsupported macro: AF_PACKET PF_PACKET
   --  unsupported macro: AF_ASH PF_ASH
   --  unsupported macro: AF_ECONET PF_ECONET
   --  unsupported macro: AF_ATMSVC PF_ATMSVC
   --  unsupported macro: AF_RDS PF_RDS
   --  unsupported macro: AF_SNA PF_SNA
   --  unsupported macro: AF_IRDA PF_IRDA
   --  unsupported macro: AF_PPPOX PF_PPPOX
   --  unsupported macro: AF_WANPIPE PF_WANPIPE
   --  unsupported macro: AF_LLC PF_LLC
   --  unsupported macro: AF_CAN PF_CAN
   --  unsupported macro: AF_TIPC PF_TIPC
   --  unsupported macro: AF_BLUETOOTH PF_BLUETOOTH
   --  unsupported macro: AF_IUCV PF_IUCV
   --  unsupported macro: AF_RXRPC PF_RXRPC
   --  unsupported macro: AF_ISDN PF_ISDN
   --  unsupported macro: AF_PHONET PF_PHONET
   --  unsupported macro: AF_IEEE802154 PF_IEEE802154
   --  unsupported macro: AF_CAIF PF_CAIF
   --  unsupported macro: AF_ALG PF_ALG
   --  unsupported macro: AF_NFC PF_NFC
   --  unsupported macro: AF_VSOCK PF_VSOCK
   --  unsupported macro: AF_MAX PF_MAX

   SOL_RAW : constant := 255;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:134
   SOL_DECNET : constant := 261;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:135
   SOL_X25 : constant := 262;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:136
   SOL_PACKET : constant := 263;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:137
   SOL_ATM : constant := 264;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:138
   SOL_AAL : constant := 265;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:139
   SOL_IRDA : constant := 266;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:140

   SOMAXCONN : constant := 128;  --  /usr/include/x86_64-linux-gnu/bits/socket.h:143
   --  unsupported macro: MSG_OOB MSG_OOB
   --  unsupported macro: MSG_PEEK MSG_PEEK
   --  unsupported macro: MSG_DONTROUTE MSG_DONTROUTE
   --  unsupported macro: MSG_TRYHARD MSG_DONTROUTE
   --  unsupported macro: MSG_CTRUNC MSG_CTRUNC
   --  unsupported macro: MSG_PROXY MSG_PROXY
   --  unsupported macro: MSG_TRUNC MSG_TRUNC
   --  unsupported macro: MSG_DONTWAIT MSG_DONTWAIT
   --  unsupported macro: MSG_EOR MSG_EOR
   --  unsupported macro: MSG_WAITALL MSG_WAITALL
   --  unsupported macro: MSG_FIN MSG_FIN
   --  unsupported macro: MSG_SYN MSG_SYN
   --  unsupported macro: MSG_CONFIRM MSG_CONFIRM
   --  unsupported macro: MSG_RST MSG_RST
   --  unsupported macro: MSG_ERRQUEUE MSG_ERRQUEUE
   --  unsupported macro: MSG_NOSIGNAL MSG_NOSIGNAL
   --  unsupported macro: MSG_MORE MSG_MORE
   --  unsupported macro: MSG_WAITFORONE MSG_WAITFORONE
   --  unsupported macro: MSG_FASTOPEN MSG_FASTOPEN
   --  unsupported macro: MSG_CMSG_CLOEXEC MSG_CMSG_CLOEXEC
   --  arg-macro: function CMSG_DATA (cmsg)
   --    return (cmsg).__cmsg_data;
   --  arg-macro: procedure CMSG_NXTHDR (mhdr, cmsg)
   --    __cmsg_nxthdr (mhdr, cmsg)
   --  arg-macro: function CMSG_FIRSTHDR (mhdr)
   --    return (size_t) (mhdr).msg_controllen >= sizeof (struct cmsghdr) ? (struct cmsghdr *) (mhdr).msg_control : (struct cmsghdr *) 0;
   --  arg-macro: function CMSG_ALIGN (len)
   --    return ((len) + sizeof (size_t) - 1) and (size_t) ~(sizeof (size_t) - 1);
   --  arg-macro: function CMSG_SPACE (len)
   --    return CMSG_ALIGN (len) + CMSG_ALIGN (sizeof (struct cmsghdr));
   --  arg-macro: function CMSG_LEN (len)
   --    return CMSG_ALIGN (sizeof (struct cmsghdr)) + (len);
   --  unsupported macro: SCM_RIGHTS SCM_RIGHTS
   --  unsupported macro: SCM_CREDENTIALS SCM_CREDENTIALS

  -- System-specific socket constants and types.  Linux version.
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

  -- Type for length arguments in socket calls.   
   subtype socklen_t is x86_64_linux_gnu_bits_types_h.uu_socklen_t;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:33

  -- Get the architecture-dependent definition of enum __socket_type.   
  -- Protocol families.   
  -- Address families.   
  -- Socket level values.  Others are defined in the appropriate headers.
  --   XXX These definitions also should go into the appropriate headers as
  --   far as they are available.   

  -- Maximum queue length specifiable by listen.   
  -- Get the definition of the macro to define the common sockaddr members.   
  -- Structure describing a generic socket address.   
  -- Common data: address family and length.   
   subtype sockaddr_array1623 is Interfaces.C.char_array (0 .. 13);
   type sockaddr is record
      sa_family : aliased x86_64_linux_gnu_bits_sockaddr_h.sa_family_t;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:151
      sa_data : aliased sockaddr_array1623;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:152
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:149

  -- Address data.   
  -- Structure large enough to hold any socket address (with the historical
  --   exception of AF_UNIX).  We reserve 128 bytes.   

  -- Address family, etc.   
   subtype sockaddr_storage_array3092 is Interfaces.C.char_array (0 .. 111);
   type sockaddr_storage is record
      ss_family : aliased x86_64_linux_gnu_bits_sockaddr_h.sa_family_t;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:164
      uu_ss_align : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:165
      uu_ss_padding : aliased sockaddr_storage_array3092;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:166
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:162

  -- Force desired alignment.   
  -- Bits in the FLAGS argument to `send', `recv', et al.   
  -- Process out-of-band data.   
  -- Peek at incoming messages.   
  -- Don't use local routing.   
  -- DECnet uses a different name.   
  -- Control data lost before delivery.   
  -- Supply or ask second address.   
  -- Nonblocking IO.   
  -- End of record.   
  -- Wait for a full request.   
  -- Confirm path validity.   
  -- Fetch message from error queue.   
  -- Do not generate SIGPIPE.   
  -- Sender will send more.   
  -- Wait for at least one packet to return. 
  -- Send data in TCP SYN.   
  -- Set close_on_exit for file
  --					   descriptor received through
  --					   SCM_RIGHTS.   

  -- Structure describing messages sent by
  --   `sendmsg' and received by `recvmsg'.   

  -- Address to send to/receive from.   
   type msghdr is record
      msg_name : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:226
      msg_namelen : aliased socklen_t;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:227
      msg_iov : access x86_64_linux_gnu_bits_uio_h.iovec;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:229
      msg_iovlen : aliased stddef_h.size_t;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:230
      msg_control : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:232
      msg_controllen : aliased stddef_h.size_t;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:233
      msg_flags : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:238
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:224

  -- Length of address data.   
  -- Vector of data to send/receive into.   
  -- Number of elements in the vector.   
  -- Ancillary data (eg BSD filedesc passing).  
  -- Ancillary data buffer length.
  --				   !! The type should be socklen_t but the
  --				   definition of the kernel is incompatible
  --				   with this.   

  -- Flags on received message.   
  -- Structure used for storage of ancillary data object information.   
  -- Length of data in cmsg_data plus length
  --				   of cmsghdr structure.
  --				   !! The type should be socklen_t but the
  --				   definition of the kernel is incompatible
  --				   with this.   

   type cmsghdr_array3099 is array (size_t) of aliased unsigned_char;
   type cmsghdr is record
      cmsg_len : aliased stddef_h.size_t;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:244
      cmsg_level : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:249
      cmsg_type : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:250
      uu_cmsg_data : aliased cmsghdr_array3099;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:252
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:242

  -- Originating protocol.   
  -- Protocol specific type.   
  -- Ancillary data.   
  -- Ancillary data object manipulation macros.   
   --  skipped func __cmsg_nxthdr

  -- The kernel header does this so there may be a reason.   
  -- No more entries.   
  -- Socket level message types.  This must match the definitions in
  --   <linux/socket.h>.   

  -- Transfer file descriptors.   
  -- Credentials passing.   
  -- User visible structure for SCM_CREDENTIALS message  
  -- PID of sending process.   
   type ucred is record
      pid : aliased x86_64_linux_gnu_sys_types_h.pid_t;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:313
      uid : aliased x86_64_linux_gnu_sys_types_h.uid_t;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:314
      gid : aliased x86_64_linux_gnu_sys_types_h.gid_t;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:315
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:311

  -- UID of sending process.   
  -- GID of sending process.   
  -- Ugly workaround for unclean kernel headers.   
  -- Get socket manipulation related informations from kernel headers.   
  -- Structure used to manipulate the SO_LINGER option.   
  -- Nonzero to linger on close.   
   type linger is record
      l_onoff : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:381
      l_linger : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:382
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/socket.h:379

  -- Time to linger.   
end x86_64_linux_gnu_bits_socket_h;
