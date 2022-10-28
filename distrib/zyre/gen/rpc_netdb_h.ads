pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with stddef_h;

package rpc_netdb_h is

  -- @(#)netdb.h	2.1 88/07/29 3.9 RPCSRC  
  -- * Copyright (c) 2010, Oracle America, Inc.
  -- * Redistribution and use in source and binary forms, with or without
  -- * modification, are permitted provided that the following conditions are
  -- * met:
  -- *
  -- *     * Redistributions of source code must retain the above copyright
  -- *       notice, this list of conditions and the following disclaimer.
  -- *     * Redistributions in binary form must reproduce the above
  -- *       copyright notice, this list of conditions and the following
  -- *       disclaimer in the documentation and/or other materials
  -- *       provided with the distribution.
  -- *     * Neither the name of the "Oracle America, Inc." nor the names of its
  -- *       contributors may be used to endorse or promote products derived
  -- *       from this software without specific prior written permission.
  -- *
  -- *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  -- *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  -- *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  -- *   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  -- *   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
  -- *   INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  -- *   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
  -- *   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  -- *   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  -- *   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  -- *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  -- *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  --  

  -- Cleaned up for GNU C library roland@gnu.ai.mit.edu:
  --   added multiple inclusion protection and use of <sys/cdefs.h>.
  --   In GNU this file is #include'd by <netdb.h>.   

  -- Name of server for this rpc program.   
   type rpcent is record
      r_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/rpc/netdb.h:48
      r_aliases : System.Address;  -- /usr/include/rpc/netdb.h:49
      r_number : aliased int;  -- /usr/include/rpc/netdb.h:50
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/rpc/netdb.h:46

  -- Alias list.   
  -- RPC program number.   
   procedure setrpcent (uu_stayopen : int)  -- /usr/include/rpc/netdb.h:53
   with Import => True, 
        Convention => C, 
        External_Name => "setrpcent";

   procedure endrpcent  -- /usr/include/rpc/netdb.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "endrpcent";

   function getrpcbyname (uu_name : Interfaces.C.Strings.chars_ptr) return access rpcent  -- /usr/include/rpc/netdb.h:55
   with Import => True, 
        Convention => C, 
        External_Name => "getrpcbyname";

   function getrpcbynumber (uu_number : int) return access rpcent  -- /usr/include/rpc/netdb.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "getrpcbynumber";

   function getrpcent return access rpcent  -- /usr/include/rpc/netdb.h:57
   with Import => True, 
        Convention => C, 
        External_Name => "getrpcent";

   function getrpcbyname_r
     (uu_name : Interfaces.C.Strings.chars_ptr;
      uu_result_buf : access rpcent;
      uu_buffer : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t;
      uu_result : System.Address) return int  -- /usr/include/rpc/netdb.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "getrpcbyname_r";

   function getrpcbynumber_r
     (uu_number : int;
      uu_result_buf : access rpcent;
      uu_buffer : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t;
      uu_result : System.Address) return int  -- /usr/include/rpc/netdb.h:64
   with Import => True, 
        Convention => C, 
        External_Name => "getrpcbynumber_r";

   function getrpcent_r
     (uu_result_buf : access rpcent;
      uu_buffer : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t;
      uu_result : System.Address) return int  -- /usr/include/rpc/netdb.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "getrpcent_r";

end rpc_netdb_h;
