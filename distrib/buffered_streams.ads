------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
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
package Buffered_Streams is
   --  A buffered stream which grows as needed

   type Buffered_Stream is new Root_Stream_Type with private;

   type Stream_Element_Array_Ptr is access Stream_Element_Array;

   procedure Write
     (Stream : in out Buffered_Stream; Data : Stream_Element_Array);

   procedure Read
     (Stream : in out Buffered_Stream; Data : out Stream_Element_Array;
      Last : out Stream_Element_Offset);

   function Contents (Stream : access Buffered_Stream)
     return Stream_Element_Array;
   --  Return contents of stream as an array, and reset stream to empty.

   function Copy_To_Heap (Stream : access Buffered_Stream)
     return Stream_Element_Array_Ptr;
   --  Copy contents of stream into the heap and reset stream to empty.
   --  NOTE: This includes only amount written but not yet read.

   function Stream_Length (Stream : Buffered_Stream)
     return Stream_Element_Count;
   --  Return current amount of buffered stream that has not been read

   procedure Reset_Stream (Stream : in out Buffered_Stream);
   --  Release all of the storage of a buffered stream

   --  Stream used for reading in from a stream array

   type Buffered_Reader (Data : not null access constant Stream_Element_Array)
     is new Root_Stream_Type with record
      Next_To_Read : Stream_Element_Offset := Data'First;
   end record;

   procedure Write
     (Stream : in out Buffered_Reader; Data : Stream_Element_Array);
   --  This will always raise Program_Error

   procedure Read
     (Stream : in out Buffered_Reader; Data : out Stream_Element_Array;
      Last : out Stream_Element_Offset);
   --  This reads from Next_To_Read

private

   type Buffered_Stream is new Root_Stream_Type with record
      Amount_Written : Stream_Element_Count := 0;
      Amount_Read    : Stream_Element_Count := 0;
      Contents       : Stream_Element_Array_Ptr := null;
   end record;

end Buffered_Streams;
