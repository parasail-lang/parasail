------------------------------------------------------------------------------
--                            Ada 202X Parallelism                          --
--                                                                          --
--                     Copyright (C) 2012-2025, AdaCore                     --
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
--  Provide Iterate for container that returns a parallel iterator.

with Ada.Unchecked_Deallocation;
with Ada.Finalization;
package body LWT.Generic_Par_Iterators is

   subtype Chunk_Index is Par_Iterator_Interfaces.Chunk_Index;

   type Cursor_Array is array (Positive range <>) of Cursor;

   type Chunk_Info_Record (Num_Chunks : Positive) is record
      First_Cursors : Cursor_Array (1 .. Num_Chunks);
   end record;

   type Chunk_Info_Record_Ptr is access Chunk_Info_Record;

   type Seq_Iter_Ptr is access Default_Iterator_Type;

   type Par_Iterator
    (Con : not null access constant Container;
     Seq_Iter : not null Seq_Iter_Ptr) is
       new Ada.Finalization.Limited_Controlled
         and Par_Iterator_Interfaces.Parallel_Iterator
     with record
      Chunks : Chunk_Info_Record_Ptr := null;
      No_Element : Cursor;
   end record;

   overriding
   procedure Finalize (Iterator : in out Par_Iterator);

   overriding
   function First
     (Object : Par_Iterator) return Cursor;
   overriding
   function Next
     (Object   : Par_Iterator;
      Position : Cursor) return Cursor;

   overriding
   function Is_Split (Object : Par_Iterator)
     return Boolean;
   
   overriding
   procedure Split_Into_Chunks (Object     : in out Par_Iterator;
                                Max_Chunks : Chunk_Index);
      
   overriding
   function Chunk_Count (Object : Par_Iterator)
      return Chunk_Index;
         
   overriding
   function First (Object : Par_Iterator;
                   Chunk  : Chunk_Index) return Cursor;

   overriding
   function Next (Object   : Par_Iterator;
                  Position : Cursor;
                  Chunk    : Chunk_Index) return Cursor;

   overriding
   procedure Finalize (Iterator : in out Par_Iterator) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Default_Iterator_Type, Seq_Iter_Ptr);
      procedure Free is new Ada.Unchecked_Deallocation
        (Chunk_Info_Record, Chunk_Info_Record_Ptr);

      Iter : Seq_Iter_Ptr := Iterator.Seq_Iter;  -- Make copy so can null out.
   begin
      --  Free chunks and seq iterator, if any
      Free (Iter);
      Free (Iterator.Chunks);
   end Finalize;

   overriding
   function First (Object : Par_Iterator) return Cursor is
   begin
      return Object.Seq_Iter.First;
   end First;

   overriding
   function Next (Object : Par_Iterator; Position : Cursor)
     return Cursor is
   begin
      return Object.Seq_Iter.Next (Position);
   end Next;

   overriding
   function Is_Split (Object : Par_Iterator)
     return Boolean is
   begin
      return Object.Chunks /= null;
   end Is_Split;

   subtype Count_Type is Ada.Containers.Count_Type;
   use type Count_Type;

   overriding
   procedure Split_Into_Chunks (Object     : in out Par_Iterator;
                                Max_Chunks : Chunk_Index) is
      --  Use user-requested chunk count unless is > Container length
      --  TBD: Might actually limit chunk count to Con_Len/2, say.
      Con_Len : constant Count_Type := Length (Object.Con.all);
      Num_Chunks : constant Chunk_Index :=
        Chunk_Index'Max (1,
          Chunk_Index'Min (Max_Chunks, Chunk_Index'Base (Con_Len)));
      Items_Per_Small_Chunk : constant Count_Type :=
        Con_Len / Count_Type (Num_Chunks);
      Num_Extra : constant Count_Type :=
        Con_Len - Items_Per_Small_Chunk * Count_Type (Num_Chunks);

      --  Compute how many chunks should be of size Items_Per_Small_Chunk
      --  (the others will have one additional item).
      Num_Small_Chunks : constant Chunk_Index'Base :=
        Num_Chunks - Chunk_Index'Base (Num_Extra);

      Next_Cursor : Cursor := Object.Seq_Iter.First;

   begin  --  Split_Into_Chunks

      if Object.Chunks /= null then
         --  Should not have already split the iterator.
         raise Program_Error;
      end if;

      --  Initialize chunk info records with first cursor for each chunk.
      Object.Chunks := new Chunk_Info_Record (Num_Chunks);
      for I in 1 .. Num_Chunks loop
         --  Initialize I'th chunk info with chunk's first cursor position.
         Object.Chunks.First_Cursors (I) := Next_Cursor;

         for J in 1 .. (if I <= Num_Small_Chunks
                        then Items_Per_Small_Chunk
                        else Items_Per_Small_Chunk + 1)
         loop
            if not Seq_Iterator_Interfaces.Has_Element (Next_Cursor) then
               --  Map must have been tampered with
               raise Program_Error;
            end if;
            --  Get next cursor position
            Next_Cursor := Object.Seq_Iter.Next (Next_Cursor);
         end loop;
      end loop;
      if Seq_Iterator_Interfaces.Has_Element (Next_Cursor) then
         --  Map must have been tampered with
         raise Program_Error;
      end if;

      --  Record value of "No_Element" for future use.
      Object.No_Element := Next_Cursor;
   end Split_Into_Chunks;

   overriding
   function Chunk_Count (Object : Par_Iterator)
      return Chunk_Index is
   begin
      if Object.Chunks = null then
         raise Program_Error;
      end if;
      return Object.Chunks.Num_Chunks;
   end Chunk_Count;

   overriding
   function First (Object : Par_Iterator;
                   Chunk  : Chunk_Index) return Cursor is
   --  Return cursor to first element of chunk
   begin
      if Object.Chunks = null then
         raise Program_Error;
      end if;
      return Object.Chunks.First_Cursors (Chunk);
   end First;

   overriding
   function Next (Object   : Par_Iterator;
                  Position : Cursor;
                  Chunk    : Chunk_Index) return Cursor is
      Result : Cursor;
   begin
      if Object.Chunks = null then
         raise Program_Error;
      end if;
      --  Get next cursor using sequential iterator
      Result := Object.Seq_Iter.Next (Position);
      if Chunk < Object.Chunks.Num_Chunks
        and then Result = Object.Chunks.First_Cursors (Chunk + 1)
      then
         --  Reached end of chunk
         return Object.No_Element;
      else
         return Result;
      end if;
   end Next;

   function Iterate (Container : Par_Iterable_Container)
     return Par_Iterator_Interfaces.Parallel_Iterator'Class is
   --  Return iterator over Container that can be split into chunks
   begin
      return Par_Iterator'(Ada.Finalization.Limited_Controlled with
                            Con => Container.Con,
                            Seq_Iter => new Default_Iterator_Type'
                              (Iterate (Container.Con.all)),
                            others => <>);
   end Iterate;

end LWT.Generic_Par_Iterators;
