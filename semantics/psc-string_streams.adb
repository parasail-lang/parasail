------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation. See           --
-- documentation/COPYING3 and documentation/GCC_RUNTIME3_1 for details.     --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with     --
-- the ParaSail, Sparkel, Javallel, or Parython compiler, including any     --
-- required library run-time units written in Ada or in any of the above    --
-- languages, using any licensing terms  of your choosing.                  --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;
package body PSC.String_Streams is
   --  A stream which saves up the output and spits
   --  it back out as a string.

   procedure Read
     (Stream : in out String_Stream;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset) is
      Len : constant Stream_Element_Offset :=
        Stream_Element_Offset'Min
           (Stream.Write_Count - Stream.Read_Count,
            Item'Length);
      --  Read up to Write_Count
      Start : constant Stream_Element_Offset := Stream.Read_Count + 1;
   begin
      --  Update Last and the Read counter
      Last := Item'First + Len - 1;
      Stream.Read_Count := Stream.Read_Count + Len;

      --  Transfer the data
      Item (Item'First .. Last) := Stream.Data (Start .. Stream.Read_Count);

      if Stream.Read_Count = Stream.Write_Count then
         --  Buffer now empty, set counters back to zero
         Stream.Read_Count := 0;
         Stream.Write_Count := 0;
      end if;
   end Read;

   procedure Write
     (Stream : in out String_Stream;
      Item : Stream_Element_Array) is
   begin
      --  Transfer the data
      if Stream.Write_Count + Item'Length > Stream.Data'Last then
         --  Need to truncate
         Stream.Data (Stream.Write_Count + 1 .. Stream.Data'Last) :=
           Item (
           Item'First ..
           Item'First + (Stream.Data'Last - Stream.Write_Count - 1));
         --  Put in "..."
         Stream.Data (Stream.Data'Last - 2 .. Stream.Data'Last) :=
           (others => Character'Pos ('.'));
         Stream.Write_Count := Stream.Data'Length;
      else
         Stream.Data (
           Stream.Write_Count + 1 .. Stream.Write_Count + Item'Length) :=
           Item;
         --  Update Write counter (only after successful data transfer)
         Stream.Write_Count := Stream.Write_Count + Item'Length;
      end if;

   end Write;

   function String_Of (Stream : access String_Stream) return String is
      --  Return what's left in buffer as a string and set back to empty
      Len : constant Natural :=
        Natural (Stream.Write_Count - Stream.Read_Count) *
        Stream_Element'Size /
        Character'Size;
      Result : String (1 .. Len);
   begin
      --  Read the characters from the stream
      String'Read (Stream, Result);

      return Result;
   end String_Of;

   --------------------------------
   -- Buffered_Stream operations --
   --------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Stream_Element_Array, Stream_Element_Array_Ptr);

   ------------------
   -- Copy_To_Heap --
   ------------------

   function Copy_To_Heap (Stream : access Buffered_Stream)
     return Stream_Element_Array_Ptr is
   --  Copy contents of stream into the heap and reset stream to empty.
   --  NOTE: This includes only amount written but not yet read.
      Result : Stream_Element_Array_Ptr;
   begin
      if Stream.Contents = null then
         --  Nothing there
         Result := new Stream_Element_Array (1 .. 0);
      elsif Stream.Amount_Read = 0
        and then Stream.Contents'Length = Stream.Amount_Written
      then
         --  Exactly what we want
         Result := Stream.Contents;
      else
         --  Copy unread portion into heap and then free the original
         declare
            subtype Cur_Stream_Subtype is Stream_Element_Array
              (1 .. Stream.Amount_Written - Stream.Amount_Read);
         begin
            Result :=
              new Stream_Element_Array'(Cur_Stream_Subtype
                (Stream.Contents
                   (Stream.Contents'First + Stream.Amount_Read ..
                    Stream.Contents'First + Stream.Amount_Written - 1)));
            Free (Stream.Contents);
         end;
      end if;
      --  Reset stream
      Stream.Contents := null;
      Stream.Amount_Written := 0;
      Stream.Amount_Read := 0;

      --  Return the stream now in the heap
      return Result;
   end Copy_To_Heap;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Buffered_Stream; Data : out Stream_Element_Array;
      Last : out Stream_Element_Offset) is
      Len : constant Stream_Element_Count := Data'Length;
   begin
      if Stream.Contents = null then
         --  Nothing to read
         Last := Data'First - 1;
      elsif Stream.Amount_Written < Stream.Amount_Read + Len then
         --  Reached end of the stream
         Last := Data'First +
           Stream.Amount_Written - Stream.Amount_Read - 1;
         Data (Data'First .. Last) :=
           Stream.Contents (Stream.Contents'First + Stream.Amount_Read ..
             Stream.Contents'First + Stream.Amount_Written - 1);

         --  Reset Amount_Written and Amount_Read
         Stream.Amount_Written := 0;
         Stream.Amount_Read := 0;
      else
         --  Enough to fill Data
         Last := Data'Last;
         Data :=
           Stream.Contents (Stream.Contents'First + Stream.Amount_Read ..
             Stream.Contents'First + Stream.Amount_Read + Len - 1);

         --  Bump amount read
         Stream.Amount_Read := Stream.Amount_Read + Len;
         if Stream.Amount_Read = Stream.Amount_Written then
            --  Reset Amount_Written and Amount_Read
            Stream.Amount_Written := 0;
            Stream.Amount_Read := 0;
         end if;
      end if;
   end Read;

   ------------------
   -- Reset_Stream --
   ------------------

   procedure Reset_Stream (Stream : in out Buffered_Stream) is
   --  Release all of the storage of a buffered stream
   begin
      Free (Stream.Contents);
      Stream.Amount_Written := 0;
      Stream.Amount_Read := 0;
   end Reset_Stream;

   -------------------
   -- Stream_Length --
   -------------------

   function Stream_Length (Stream : Buffered_Stream)
     return Stream_Element_Count is
   --  Return current amount of buffered stream that has not been read
   begin
      return Stream.Amount_Written - Stream.Amount_Read;
   end Stream_Length;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Buffered_Stream; Data : Stream_Element_Array) is
      Len : constant Stream_Element_Count := Data'Length;
   begin
      if Len = 0 then
         --  Nothing to do
         null;
      elsif Stream.Contents = null then
         --  Initialize stream from first write
         Stream.Contents := new Stream_Element_Array'(Data);
         Stream.Amount_Written := Len;
         Stream.Amount_Read := 0;
      else
         --  Extend if necessary
         if Stream.Contents'Length < Stream.Amount_Written + Len then
            --  Make it twice as big as it needs to be
            declare
               Old_Contents : Stream_Element_Array_Ptr := Stream.Contents;
            begin
               Stream.Contents :=
                 new Stream_Element_Array
                   (0 .. Stream.Amount_Written * 2 + Len * 2 - 1);
               --  Copy over old contents
               Stream.Contents (0 .. Stream.Amount_Written - 1) :=
                 Old_Contents (Old_Contents'First ..
                               Old_Contents'First +
                                 Stream.Amount_Written - 1);
               --  Reclaim old contents
               Free (Old_Contents);
            end;
         end if;
         --  Now write the new data
         Stream.Contents (Stream.Contents'First + Stream.Amount_Written ..
           Stream.Contents'First + Stream.Amount_Written + Len - 1) := Data;

         Stream.Amount_Written := Stream.Amount_Written + Len;
      end if;
   end Write;

   --------------------------------
   -- Buffered_Reader operations --
   --------------------------------

   procedure Write
     (Stream : in out Buffered_Reader; Data : Stream_Element_Array) is
   --  This will always raise Program_Error
   begin
      raise Program_Error;
   end Write;

   procedure Read
     (Stream : in out Buffered_Reader; Data : out Stream_Element_Array;
      Last : out Stream_Element_Offset) is
   --  This reads from Next_To_Read
      Len : constant Stream_Element_Count := Data'Length;
   begin
      Last := Data'Last;
      --  Read in next "Len" characters
      Data := Stream.Data (Stream.Next_To_Read ..
                             Stream.Next_To_Read + Len - 1);

      --  Bump past chars just read
      Stream.Next_To_Read := Stream.Next_To_Read + Len;
   end Read;

   ---------------------
   -- Read_Stream_Ptr --
   ---------------------

   --  Create 'Read which always produces a null value
   procedure Read_Stream_Ptr
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Stream_Element_Array_Ptr) is
   begin
      Item := null;
   end Read_Stream_Ptr;

   ----------------------
   -- Write_Stream_Ptr --
   ----------------------

   --  Create 'Write which writes nothing at all
   procedure Write_Stream_Ptr
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Stream_Element_Array_Ptr) is
   begin
      null;
   end Write_Stream_Ptr;

end PSC.String_Streams;
