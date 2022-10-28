------------------------------------------------------------------------------
--                            Ada 202X Parallelism                          --
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
------------------------------------------------------------------------------
--  Provide version of Iterate for Vectors that returns a parallel iterator.

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package body Vector_Par_Iterators is
   use Vectors;
   use Vector_Par_Iterator_Interfaces;

   type Chunk_Info is record
      Last_Index : Index_Type := Index_Type'First;
   end record;

   type Chunk_Info_Array is array (Positive range <>) of Chunk_Info;

   type Chunk_Info_Record (Num_Chunks : Positive) is record
      Data : Chunk_Info_Array (1 .. Num_Chunks);
   end record;

   type Chunk_Info_Record_Ptr is access Chunk_Info_Record;

   type Vector_Par_Iterator is new Ada.Finalization.Limited_Controlled
     and Vector_Par_Iterator_Interfaces.Parallel_Iterator
     with record
      Container : not null access constant Vectors.Vector;
      Vector_Len : Index_Type'Base := 0;
      Chunks : Chunk_Info_Record_Ptr := null;
   end record;

   procedure Finalize (Iterator : in out Vector_Par_Iterator);

   function First (Object : Vector_Par_Iterator) return Cursor;
   function Next (Object : Vector_Par_Iterator; Position : Cursor)
     return Cursor;

   function Is_Split (Object : Vector_Par_Iterator)
     return Boolean;

   procedure Split_Into_Chunks (Object     : in out Vector_Par_Iterator;
                                Max_Chunks : Chunk_Index);

   function Chunk_Count (Object : Vector_Par_Iterator)
      return Chunk_Index;

   function First (Object : Vector_Par_Iterator;
                   Chunk  : Chunk_Index) return Cursor;

   function Next (Object   : Vector_Par_Iterator;
                  Position : Cursor;
                  Chunk    : Chunk_Index) return Cursor;

   -----

   procedure Finalize (Iterator : in out Vector_Par_Iterator) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Chunk_Info_Record, Chunk_Info_Record_Ptr);
   begin
      --  Free chunks, if any
      Free (Iterator.Chunks);
   end Finalize;

   function First (Object : Vector_Par_Iterator) return Cursor is
   begin
      if Object.Vector_Len = 0 then
         --  Reached end of vector
         return Vectors.No_Element;
      else
         return Object.Container.To_Cursor (Index_Type'First);
      end if;
   end First;

   function Next (Object : Vector_Par_Iterator; Position : Cursor)
     return Cursor is
      Index : constant Index_Type := To_Index (Position);
   begin
      if Index - (Index_Type'First - 1) >= Object.Vector_Len then
         --  Reached end of vector
         return Vectors.No_Element;
      else
         return Object.Container.To_Cursor (Index_Type'Succ (Index));
      end if;
   end Next;

   function Is_Split (Object : Vector_Par_Iterator)
     return Boolean is
   begin
      return Object.Chunks /= null;
   end Is_Split;

   procedure Split_Into_Chunks (Object     : in out Vector_Par_Iterator;
                                Max_Chunks : Chunk_Index) is
      --  Use user-requested chunk count unless is > vector length
      --  TBD: Might actually limit chunk count to vector-len/2, say.
      Num_Chunks : constant Chunk_Index :=
        Chunk_Index'Max (1,
          Chunk_Index'Min (Max_Chunks, Chunk_Index'Base (Object.Vector_Len)));
   begin
      if Object.Chunks /= null then
         raise Program_Error;
      end if;
      Object.Chunks := new Chunk_Info_Record (Num_Chunks);
      --  Note that it is further initialized by each call on First
   end Split_Into_Chunks;

   function Chunk_Count (Object : Vector_Par_Iterator)
      return Chunk_Index is
   begin
      if Object.Chunks = null then
         raise Program_Error;
      end if;
      return Object.Chunks.Num_Chunks;
   end Chunk_Count;

   function First (Object : Vector_Par_Iterator;
                   Chunk  : Chunk_Index) return Cursor is
   --  Return cursor to first element of chunk
   begin
      if Object.Chunks = null then
         raise Program_Error;
      end if;
      if Object.Vector_Len = 0 then
         --  Reached end of vector
         return Vectors.No_Element;
      else
         --  Chunk index must not exceed length of vector
         pragma Assert (Object.Vector_Len >= Index_Type'Base (Chunk));

         --  Compute last index of chunk
         Object.Chunks.Data (Chunk).Last_Index :=
           Index_Type'First - 1 +
              Index_Type'Base (Chunk) *
                Object.Vector_Len / Index_Type'Base
                                      (Object.Chunks.Num_Chunks);
         --  Compute first index of chunk and convert to Cursor
         return Object.Container.To_Cursor
           (Index_Type'First +
              Index_Type'Base (Chunk - 1) *
                Object.Vector_Len / Index_Type'Base
                                      (Object.Chunks.Num_Chunks));
      end if;
   end First;

   function Next (Object   : Vector_Par_Iterator;
                  Position : Cursor;
                  Chunk    : Chunk_Index) return Cursor is
      Index : constant Index_Type'Base := To_Index (Position);
   begin
      if Object.Chunks = null then
         raise Program_Error;
      end if;
      if Index >= Object.Chunks.Data (Chunk).Last_Index then
         --  Reached end of chunk
         return Vectors.No_Element;
      else
         return Object.Container.To_Cursor (Index_Type'Succ (Index));
      end if;
   end Next;

   function Par_Iterate (Container : Vectors.Vector)
     return Vector_Par_Iterator_Interfaces.Parallel_Iterator'Class is
   --  Return iterator over Container that can be split into chunks
   begin
      return Vector_Par_Iterator'(Ada.Finalization.Limited_Controlled with
                                  Container => Container'Unchecked_Access,
                                  Vector_Len =>
                                    Index_Type'Base (Container.Length),
                                  others => <>);
   end Par_Iterate;

end Vector_Par_Iterators;
