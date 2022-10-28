pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_sys_types_h;
with Interfaces.C.Extensions;
with x86_64_linux_gnu_bits_socket_h;

package netinet_tcp_h is

   TCP_NODELAY : constant := 1;  --  /usr/include/netinet/tcp.h:40
   TCP_MAXSEG : constant := 2;  --  /usr/include/netinet/tcp.h:41
   TCP_CORK : constant := 3;  --  /usr/include/netinet/tcp.h:42
   TCP_KEEPIDLE : constant := 4;  --  /usr/include/netinet/tcp.h:43
   TCP_KEEPINTVL : constant := 5;  --  /usr/include/netinet/tcp.h:44
   TCP_KEEPCNT : constant := 6;  --  /usr/include/netinet/tcp.h:45
   TCP_SYNCNT : constant := 7;  --  /usr/include/netinet/tcp.h:46
   TCP_LINGER2 : constant := 8;  --  /usr/include/netinet/tcp.h:47
   TCP_DEFER_ACCEPT : constant := 9;  --  /usr/include/netinet/tcp.h:48
   TCP_WINDOW_CLAMP : constant := 10;  --  /usr/include/netinet/tcp.h:49
   TCP_INFO : constant := 11;  --  /usr/include/netinet/tcp.h:50
   TCP_QUICKACK : constant := 12;  --  /usr/include/netinet/tcp.h:51
   TCP_CONGESTION : constant := 13;  --  /usr/include/netinet/tcp.h:52
   TCP_MD5SIG : constant := 14;  --  /usr/include/netinet/tcp.h:53
   TCP_COOKIE_TRANSACTIONS : constant := 15;  --  /usr/include/netinet/tcp.h:54
   TCP_THIN_LINEAR_TIMEOUTS : constant := 16;  --  /usr/include/netinet/tcp.h:55
   TCP_THIN_DUPACK : constant := 17;  --  /usr/include/netinet/tcp.h:56
   TCP_USER_TIMEOUT : constant := 18;  --  /usr/include/netinet/tcp.h:57
   TCP_REPAIR : constant := 19;  --  /usr/include/netinet/tcp.h:58
   TCP_REPAIR_QUEUE : constant := 20;  --  /usr/include/netinet/tcp.h:59
   TCP_QUEUE_SEQ : constant := 21;  --  /usr/include/netinet/tcp.h:60
   TCP_REPAIR_OPTIONS : constant := 22;  --  /usr/include/netinet/tcp.h:61
   TCP_FASTOPEN : constant := 23;  --  /usr/include/netinet/tcp.h:62
   TCP_TIMESTAMP : constant := 24;  --  /usr/include/netinet/tcp.h:63

   TH_FIN : constant := 16#01#;  --  /usr/include/netinet/tcp.h:93
   TH_SYN : constant := 16#02#;  --  /usr/include/netinet/tcp.h:94
   TH_RST : constant := 16#04#;  --  /usr/include/netinet/tcp.h:95
   TH_PUSH : constant := 16#08#;  --  /usr/include/netinet/tcp.h:96
   TH_ACK : constant := 16#10#;  --  /usr/include/netinet/tcp.h:97
   TH_URG : constant := 16#20#;  --  /usr/include/netinet/tcp.h:98

   TCPOPT_EOL : constant := 0;  --  /usr/include/netinet/tcp.h:154
   TCPOPT_NOP : constant := 1;  --  /usr/include/netinet/tcp.h:155
   TCPOPT_MAXSEG : constant := 2;  --  /usr/include/netinet/tcp.h:156
   TCPOLEN_MAXSEG : constant := 4;  --  /usr/include/netinet/tcp.h:157
   TCPOPT_WINDOW : constant := 3;  --  /usr/include/netinet/tcp.h:158
   TCPOLEN_WINDOW : constant := 3;  --  /usr/include/netinet/tcp.h:159
   TCPOPT_SACK_PERMITTED : constant := 4;  --  /usr/include/netinet/tcp.h:160
   TCPOLEN_SACK_PERMITTED : constant := 2;  --  /usr/include/netinet/tcp.h:161
   TCPOPT_SACK : constant := 5;  --  /usr/include/netinet/tcp.h:162
   TCPOPT_TIMESTAMP : constant := 8;  --  /usr/include/netinet/tcp.h:163
   TCPOLEN_TIMESTAMP : constant := 10;  --  /usr/include/netinet/tcp.h:164
   --  unsupported macro: TCPOLEN_TSTAMP_APPA (TCPOLEN_TIMESTAMP+2)
   --  unsupported macro: TCPOPT_TSTAMP_HDR (TCPOPT_NOP<<24|TCPOPT_NOP<<16|TCPOPT_TIMESTAMP<<8|TCPOLEN_TIMESTAMP)

   TCP_MSS : constant := 512;  --  /usr/include/netinet/tcp.h:176

   TCP_MAXWIN : constant := 65535;  --  /usr/include/netinet/tcp.h:178

   TCP_MAX_WINSHIFT : constant := 14;  --  /usr/include/netinet/tcp.h:180

   SOL_TCP : constant := 6;  --  /usr/include/netinet/tcp.h:182

   TCPI_OPT_TIMESTAMPS : constant := 1;  --  /usr/include/netinet/tcp.h:185
   TCPI_OPT_SACK : constant := 2;  --  /usr/include/netinet/tcp.h:186
   TCPI_OPT_WSCALE : constant := 4;  --  /usr/include/netinet/tcp.h:187
   TCPI_OPT_ECN : constant := 8;  --  /usr/include/netinet/tcp.h:188
   TCPI_OPT_ECN_SEEN : constant := 16;  --  /usr/include/netinet/tcp.h:189
   TCPI_OPT_SYN_DATA : constant := 32;  --  /usr/include/netinet/tcp.h:190

   TCP_MD5SIG_MAXKEYLEN : constant := 80;  --  /usr/include/netinet/tcp.h:247

   TCP_COOKIE_MIN : constant := 8;  --  /usr/include/netinet/tcp.h:275
   TCP_COOKIE_MAX : constant := 16;  --  /usr/include/netinet/tcp.h:276
   --  unsupported macro: TCP_COOKIE_PAIR_SIZE (2*TCP_COOKIE_MAX)

   TCP_COOKIE_IN_ALWAYS : constant := (2 ** 0);  --  /usr/include/netinet/tcp.h:280
   TCP_COOKIE_OUT_NEVER : constant := (2 ** 1);  --  /usr/include/netinet/tcp.h:281

   TCP_S_DATA_IN : constant := (2 ** 2);  --  /usr/include/netinet/tcp.h:285
   TCP_S_DATA_OUT : constant := (2 ** 3);  --  /usr/include/netinet/tcp.h:286

   TCP_MSS_DEFAULT : constant := 536;  --  /usr/include/netinet/tcp.h:288
   TCP_MSS_DESIRED : constant := 1220;  --  /usr/include/netinet/tcp.h:289

  -- * Copyright (c) 1982, 1986, 1993
  -- *	The Regents of the University of California.  All rights reserved.
  -- *
  -- * Redistribution and use in source and binary forms, with or without
  -- * modification, are permitted provided that the following conditions
  -- * are met:
  -- * 1. Redistributions of source code must retain the above copyright
  -- *    notice, this list of conditions and the following disclaimer.
  -- * 2. Redistributions in binary form must reproduce the above copyright
  -- *    notice, this list of conditions and the following disclaimer in the
  -- *    documentation and/or other materials provided with the distribution.
  -- * 4. Neither the name of the University nor the names of its contributors
  -- *    may be used to endorse or promote products derived from this software
  -- *    without specific prior written permission.
  -- *
  -- * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  -- * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  -- * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  -- * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  -- * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  -- * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  -- * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  -- * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  -- * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  -- * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  -- * SUCH DAMAGE.
  -- *
  -- *	@(#)tcp.h	8.1 (Berkeley) 6/10/93
  --  

  -- * User-settable options (used with setsockopt).
  --  

   subtype tcp_seq is x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:69

  -- * TCP header.
  -- * Per RFC 793, September, 1981.
  --  

   type anon_117 is record
      th_sport : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:80
      th_dport : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:81
      th_seq : aliased tcp_seq;  -- /usr/include/netinet/tcp.h:82
      th_ack : aliased tcp_seq;  -- /usr/include/netinet/tcp.h:83
      th_x2 : Extensions.Unsigned_4;  -- /usr/include/netinet/tcp.h:85
      th_off : Extensions.Unsigned_4;  -- /usr/include/netinet/tcp.h:86
      th_flags : aliased x86_64_linux_gnu_sys_types_h.u_int8_t;  -- /usr/include/netinet/tcp.h:92
      th_win : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:99
      th_sum : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:100
      th_urp : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:101
   end record
   with Convention => C_Pass_By_Copy;
   type anon_118 is record
      source : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:105
      dest : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:106
      seq : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:107
      ack_seq : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:108
      res1 : Extensions.Unsigned_4;  -- /usr/include/netinet/tcp.h:110
      doff : Extensions.Unsigned_4;  -- /usr/include/netinet/tcp.h:111
      fin : Extensions.Unsigned_1;  -- /usr/include/netinet/tcp.h:112
      syn : Extensions.Unsigned_1;  -- /usr/include/netinet/tcp.h:113
      rst : Extensions.Unsigned_1;  -- /usr/include/netinet/tcp.h:114
      psh : Extensions.Unsigned_1;  -- /usr/include/netinet/tcp.h:115
      ack : Extensions.Unsigned_1;  -- /usr/include/netinet/tcp.h:116
      urg : Extensions.Unsigned_1;  -- /usr/include/netinet/tcp.h:117
      res2 : Extensions.Unsigned_2;  -- /usr/include/netinet/tcp.h:118
      window : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:132
      check : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:133
      urg_ptr : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:134
   end record
   with Convention => C_Pass_By_Copy;
   type anon_116 (discr : unsigned := 0) is record
      case discr is
            parent : aliased anon_117;
            field_2 : aliased anon_118;
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type tcphdr is record
      parent : aliased anon_116;
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/tcp.h:74

  -- source port  
  -- destination port  
  -- sequence number  
  -- acknowledgement number  
  -- (unused)  
  -- data offset  
  -- data offset  
  -- (unused)  
  -- window  
  -- checksum  
  -- urgent pointer  
  -- now a valid state  
  -- * Default maximum segment size for TCP.
  -- * With an IP MSS of 576, this is 536,
  -- * but 512 is probably more convenient.
  -- * This should be defined as MIN(512, IP_MSS - sizeof (struct tcpiphdr)).
  --  

  -- Values for tcpi_state.   
   type tcp_ca_state is 
     (TCP_CA_Open,
      TCP_CA_Disorder,
      TCP_CA_CWR,
      TCP_CA_Recovery,
      TCP_CA_Loss)
   with Convention => C;  -- /usr/include/netinet/tcp.h:193

   type tcp_info is record
      tcpi_state : aliased x86_64_linux_gnu_sys_types_h.u_int8_t;  -- /usr/include/netinet/tcp.h:204
      tcpi_ca_state : aliased x86_64_linux_gnu_sys_types_h.u_int8_t;  -- /usr/include/netinet/tcp.h:205
      tcpi_retransmits : aliased x86_64_linux_gnu_sys_types_h.u_int8_t;  -- /usr/include/netinet/tcp.h:206
      tcpi_probes : aliased x86_64_linux_gnu_sys_types_h.u_int8_t;  -- /usr/include/netinet/tcp.h:207
      tcpi_backoff : aliased x86_64_linux_gnu_sys_types_h.u_int8_t;  -- /usr/include/netinet/tcp.h:208
      tcpi_options : aliased x86_64_linux_gnu_sys_types_h.u_int8_t;  -- /usr/include/netinet/tcp.h:209
      tcpi_snd_wscale : Extensions.Unsigned_4;  -- /usr/include/netinet/tcp.h:210
      tcpi_rcv_wscale : Extensions.Unsigned_4;  -- /usr/include/netinet/tcp.h:210
      tcpi_rto : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:212
      tcpi_ato : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:213
      tcpi_snd_mss : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:214
      tcpi_rcv_mss : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:215
      tcpi_unacked : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:217
      tcpi_sacked : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:218
      tcpi_lost : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:219
      tcpi_retrans : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:220
      tcpi_fackets : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:221
      tcpi_last_data_sent : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:224
      tcpi_last_ack_sent : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:225
      tcpi_last_data_recv : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:226
      tcpi_last_ack_recv : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:227
      tcpi_pmtu : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:230
      tcpi_rcv_ssthresh : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:231
      tcpi_rtt : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:232
      tcpi_rttvar : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:233
      tcpi_snd_ssthresh : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:234
      tcpi_snd_cwnd : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:235
      tcpi_advmss : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:236
      tcpi_reordering : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:237
      tcpi_rcv_rtt : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:239
      tcpi_rcv_space : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:240
      tcpi_total_retrans : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:242
   end record
   with Convention => C_Pass_By_Copy,
        Pack => True;  -- /usr/include/netinet/tcp.h:202

  -- Times.  
  -- Not remembered, sorry.   
  -- Metrics.  
  -- For TCP_MD5SIG socket option.   
  -- Address associated.   
   type tcp_md5sig_array4442 is array (0 .. 79) of aliased x86_64_linux_gnu_sys_types_h.u_int8_t;
   type tcp_md5sig is record
      tcpm_addr : aliased x86_64_linux_gnu_bits_socket_h.sockaddr_storage;  -- /usr/include/netinet/tcp.h:251
      uu_tcpm_pad1 : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:252
      tcpm_keylen : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:253
      uu_tcpm_pad2 : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:254
      tcpm_key : aliased tcp_md5sig_array4442;  -- /usr/include/netinet/tcp.h:255
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/tcp.h:249

  -- Zero.   
  -- Key length.   
  -- Zero.   
  -- Key (binary).   
  -- For socket repair options.   
   type tcp_repair_opt is record
      opt_code : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:261
      opt_val : aliased x86_64_linux_gnu_sys_types_h.u_int32_t;  -- /usr/include/netinet/tcp.h:262
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/tcp.h:259

  -- Queue to repair, for TCP_REPAIR_QUEUE.   
  -- For cookie transactions socket options.   
  -- Flags for both getsockopt and setsockopt  
  -- Flags for getsockopt  
   type tcp_cookie_transactions_array4451 is array (0 .. 535) of aliased x86_64_linux_gnu_sys_types_h.u_int8_t;
   type tcp_cookie_transactions is record
      tcpct_flags : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:293
      uu_tcpct_pad1 : aliased x86_64_linux_gnu_sys_types_h.u_int8_t;  -- /usr/include/netinet/tcp.h:294
      tcpct_cookie_desired : aliased x86_64_linux_gnu_sys_types_h.u_int8_t;  -- /usr/include/netinet/tcp.h:295
      tcpct_s_data_desired : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:296
      tcpct_used : aliased x86_64_linux_gnu_sys_types_h.u_int16_t;  -- /usr/include/netinet/tcp.h:297
      tcpct_value : aliased tcp_cookie_transactions_array4451;  -- /usr/include/netinet/tcp.h:298
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/netinet/tcp.h:291

end netinet_tcp_h;
