--  =========================================================================
--  zframe - working with single message frames

--  Copyright (c) the Contributors as noted in the AUTHORS file.
--  This file is part of CZMQ, the high-level C binding for 0MQ:
--  http://czmq.zeromq.org.

--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
--  =========================================================================

with zframe_h;
with stddef_h;
with Interfaces.C;
package body ZMQ.Zframes is
   --

   NYI : exception;

   --  @warning THE FOLLOWING @INTERFACE BLOCK IS AUTO-GENERATED BY ZPROJECT
   --  @warning Please edit the model at "api/zframe.api" to make changes.
   --  @interface
   --  This is a stable class, and may not change except for emergencies. It
   --  is provided in stable builds.
   --  This class has draft methods, which may change over time. They are not
   --  in stable releases, by default. Use --enable-drafts to enable.

   --  Create a new frame. If size is not null, allocates the frame data
   --  to the specified size. If additionally, data is not null, copies
   --  size octets from the specified data into the frame body.
   --  CZMQ_EXPORT zframe_t *
   --  zframe_new (const void *data, size_t size);

   function Zframe_New (Data : System.Address; Size : Long_Integer)
     return Zframe is
   begin
      return
        Zframe'(Raw => zframe_h.zframe_new (Data, stddef_h.size_t (Size)));
   end;

   --  Create an empty (zero-sized) frame
   --  CZMQ_EXPORT zframe_t *
   --  zframe_new_empty (void);

   function Zframe_New_Empty return Zframe is
   begin
      return raise NYI;
   end;

   --  Create a frame with a specified string content.
   --  CZMQ_EXPORT zframe_t *
   --  zframe_from (const char *string);

   function Zframe_From (Str : String) return Zframe is
   begin
      return raise NYI;
   end;

   --  Create a new frame from a stream-element array, based on zframe_new.
   --  CZMQ_EXPORT zframe_t *
   --  zframe_new (const void *data, size_t size);

   function Zframe_From_Stream (Stream : Ada.Streams.Stream_Element_Array)
     return Zframe is
   begin
      return
        Zframe'(Raw => zframe_h.zframe_new
          (Data => Stream (Stream'First)'Address,
           Size => stddef_h.size_t (Stream'Length *
                      (Stream'Component_Size / System.Storage_Unit))));
   end;
      

   --  Return frame data as a stream-element array, based on zframe_size/data.
   --  CZMQ_EXPORT size_t
   --  zframe_size (zframe_t *self);

   function Zframe_As_Stream (Self : Zframe)
     return Ada.Streams.Stream_Element_Array is
      use Ada.Streams;
      Len : constant Stream_Element_Count :=
        Stream_Element_Count (zframe_h.zframe_size (Self.Raw)) /
          (Stream_Element_Array'Component_Size / System.Storage_Unit);
      Data_As_Stream : Stream_Element_Array (1 .. Len)
        with Address => zframe_h.zframe_data (Self.Raw).all'Address;
   begin
      return Data_As_Stream;
   end;

   --  Receive frame from socket, returns zframe_t object or NULL if the recv
   --  was interrupted. Does a blocking recv, if you want to not block then use
   --  zpoller or zloop.
   --  CZMQ_EXPORT zframe_t *
   --  zframe_recv (void *source);

   function Zframe_Recv (Source : Socket) return Zframe is
   begin
      return raise NYI;
   end;

   --  Destroy a frame
   --  CZMQ_EXPORT void
   --  zframe_destroy (zframe_t **self_p);

   procedure Zframe_Destroy (Self_P : in out Zframe) is
   begin
      zframe_h.zframe_destroy (Self_P.Raw'Address);
   end;

   --  Send a frame to a socket, destroy frame after sending.
   --  Return -1 on error, 0 on success.
   --  CZMQ_EXPORT int
   --  zframe_send (zframe_t **self_p, void *dest, int flags);

   function Zframe_Send
     (Self_P : in out Zframe; Dest : Socket; Flags : Integer)
     return Return_Status is
   begin
      return raise NYI;
   end;

   --  Return number of bytes in frame data
   --  CZMQ_EXPORT size_t
   --  zframe_size (zframe_t *self);

   function Zframe_Size (Self : Zframe) return Long_Integer is
   begin
      return Long_Integer (zframe_h.zframe_size (Self.Raw));
   end;

   --  Return address of frame data
   --  CZMQ_EXPORT byte *
   --  zframe_data (zframe_t *self);

   function Zframe_Data (Self : Zframe) return System.Address is
   begin
      return zframe_h.zframe_data (Self.Raw).all'Address;
   end;

   --  Return meta data property for frame
   --  The caller shall not modify or free the returned value, which shall be
   --  owned by the message.
   --  CZMQ_EXPORT const char *
   --  zframe_meta (zframe_t *self, const char *property);

   --  Create a new frame that duplicates an existing frame. If frame is null,
   --  or memory was exhausted, returns null.
   --  Caller owns return value and must destroy it when done.
   --  CZMQ_EXPORT zframe_t *
   --  zframe_dup (zframe_t *self);

   function Zframe_Dup (Self : Zframe) return Zframe is
   begin
      return raise NYI;
   end;

   --  Move a zframe to a new frame holder
   procedure Zframe_Move (Self : in out Zframe; From : in out Zframe) is
   begin
      pragma Assert (Self.Raw = null);
      Self.Raw := From.Raw;
      From.Raw := null;
   end Zframe_Move;

   --  Return frame data encoded as printable hex string, useful for 0MQ UUIDs.
   --  Caller must free string when finished with it.
   --  Caller owns return value and must destroy it when done.
   --  CZMQ_EXPORT char *
   --  zframe_strhex (zframe_t *self);

   --  Return frame data copied into freshly allocated string
   --  Caller must free string when finished with it.
   --  Caller owns return value and must destroy it when done.
   --  CZMQ_EXPORT char *
   --  zframe_strdup (zframe_t *self);

   --  Return TRUE if frame body is equal to string, excluding terminator
   --  CZMQ_EXPORT bool
   --  zframe_streq (zframe_t *self, const char *string);

   --  Return frame MORE indicator (1 or 0), set when reading frame from socket
   --  or by the zframe_set_more() method
   --  CZMQ_EXPORT int
   --  zframe_more (zframe_t *self);

   --  Set frame MORE indicator (1 or 0). Note this is NOT used when sending
   --  frame to socket, you have to specify flag explicitly.
   --  CZMQ_EXPORT void
   --  zframe_set_more (zframe_t *self, int more);

   --  Return TRUE if two frames have identical size and data
   --  If either frame is NULL, equality is always false.
   --  CZMQ_EXPORT bool
   --  zframe_eq (zframe_t *self, zframe_t *other);

   --  Set new contents for frame
   --  CZMQ_EXPORT void
   --  zframe_reset (zframe_t *self, const void *data, size_t size);

   --  Send message to zsys log sink (may be stdout, or system facility as
   --  configured by zsys_set_logstream). Prefix shows before frame,
   --  if not null.
   --  Long messages are truncated.
   --  CZMQ_EXPORT void
   --  zframe_print (zframe_t *self, const char *prefix);

   --  Probe the supplied object, and report if it looks like a zframe_t.
   --  CZMQ_EXPORT bool
   --  zframe_is (void *self);

   --  Self test of this class.
   --  CZMQ_EXPORT void
   --  zframe_test (bool verbose);

end ZMQ.Zframes;
