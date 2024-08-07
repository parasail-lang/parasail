pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
limited with zyre_library_h;
with System;
with Interfaces.C.Strings;
limited with czmq_library_h;
with Interfaces.C.Extensions;

package zyre_event_h is

  --  =========================================================================
  --    zyre_event - Parsing Zyre messages
  --    Copyright (c) the Contributors as noted in the AUTHORS file.
  --    This file is part of Zyre, an open-source framework for proximity-based
  --    peer-to-peer applications -- See http://zyre.org.
  --    This Source Code Form is subject to the terms of the Mozilla Public
  --    License, v. 2.0. If a copy of the MPL was not distributed with this
  --    file, You can obtain one at http://mozilla.org/MPL/2.0/.
  --    =========================================================================
  -- 

  --  @warning THE FOLLOWING @INTERFACE BLOCK IS AUTO-GENERATED BY ZPROJECT
  --  @warning Please edit the model at "api/zyre_event.api" to make changes.
  --  @interface
  --  This is a stable class, and may not change except for emergencies. It
  --  is provided in stable builds.
  --  Constructor: receive an event from the zyre node, wraps zyre_recv.
  --  The event may be a control message (ENTER, EXIT, JOIN, LEAVE) or
  --  data (WHISPER, SHOUT).
   function zyre_event_new (node : access zyre_library_h.zyre_t) return access zyre_library_h.zyre_event_t  -- include/zyre_event.h:32
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_new";

  --  Destructor; destroys an event instance
   procedure zyre_event_destroy (self_p : System.Address)  -- include/zyre_event.h:36
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_destroy";

  --  Returns event type, as printable uppercase string. Choices are:
  --  "ENTER", "EXIT", "JOIN", "LEAVE", "EVASIVE", "WHISPER" and "SHOUT"
  --  and for the local node: "STOP"
   function zyre_event_type (self : access zyre_library_h.zyre_event_t) return Interfaces.C.Strings.chars_ptr  -- include/zyre_event.h:42
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_type";

  --  Return the sending peer's uuid as a string
   function zyre_event_peer_uuid (self : access zyre_library_h.zyre_event_t) return Interfaces.C.Strings.chars_ptr  -- include/zyre_event.h:46
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_peer_uuid";

  --  Return the sending peer's public name as a string
   function zyre_event_peer_name (self : access zyre_library_h.zyre_event_t) return Interfaces.C.Strings.chars_ptr  -- include/zyre_event.h:50
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_peer_name";

  --  Return the sending peer's ipaddress as a string
   function zyre_event_peer_addr (self : access zyre_library_h.zyre_event_t) return Interfaces.C.Strings.chars_ptr  -- include/zyre_event.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_peer_addr";

  --  Returns the event headers, or NULL if there are none
   function zyre_event_headers (self : access zyre_library_h.zyre_event_t) return access czmq_library_h.zhash_t  -- include/zyre_event.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_headers";

  --  Returns value of a header from the message headers
  --  obtained by ENTER. Return NULL if no value was found.
   function zyre_event_header (self : access zyre_library_h.zyre_event_t; name : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- include/zyre_event.h:63
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_header";

  --  Returns the group name that a SHOUT event was sent to
   function zyre_event_group (self : access zyre_library_h.zyre_event_t) return Interfaces.C.Strings.chars_ptr  -- include/zyre_event.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_group";

  --  Returns the incoming message payload; the caller can modify the
  --  message but does not own it and should not destroy it.
   function zyre_event_msg (self : access zyre_library_h.zyre_event_t) return access czmq_library_h.zmsg_t  -- include/zyre_event.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_msg";

  --  Returns the incoming message payload, and pass ownership to the
  --  caller. The caller must destroy the message when finished with it.
  --  After called on the given event, further calls will return NULL.
  --  Caller owns return value and must destroy it when done.
   function zyre_event_get_msg (self : access zyre_library_h.zyre_event_t) return access czmq_library_h.zmsg_t  -- include/zyre_event.h:79
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_get_msg";

  --  Print event to zsys log
   procedure zyre_event_print (self : access zyre_library_h.zyre_event_t)  -- include/zyre_event.h:83
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_print";

  --  Self test of this class.
   procedure zyre_event_test (verbose : Extensions.bool)  -- include/zyre_event.h:87
   with Import => True, 
        Convention => C, 
        External_Name => "zyre_event_test";

  --  @end
end zyre_event_h;
