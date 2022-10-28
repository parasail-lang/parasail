------------------------------------------------------------------------------
--                              Distributed Reduction                       --
--                                                                          --
--                     Copyright (C) 2012-2021, AdaCore                     --
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

--  pragma Ada_2020;

with Ada.Streams;
with Buffered_Streams;
with Ada.Text_IO; use Ada.Text_IO;
with System.Parallelism;
package body Distributed_Reduction is

   use System_Distrib;

   Debug_Distrib : constant Boolean := False;

   Next_Obj_Id_To_Assign : Distrib_Object_Id_Type :=
     Distrib_Object_Id_Type'Max (1, Distrib_Object_Id_Type'First);

   type Accum_Ptr is access all Accum_Type
     with Read => Accum_Ptr_Read, Write => Accum_Ptr_Write;

   procedure Accum_Ptr_Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Ptr : out Accum_Ptr)
     with Post => Ptr /= null;
   --  Use Accum_Type'Input to initialize an allocator

   procedure Accum_Ptr_Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Ptr : Accum_Ptr)
     with Pre => Ptr /= null;
   --  Use Accum_Type'Output to write out Ptr.all

   type Accum_Array is
     array (Node_Index_Type range <>) of Accum_Ptr;

   type Reduction_Message_Kind is (Update_One_Accum);
   type Reduction_Message_Type
     (Kind : Reduction_Message_Kind := Reduction_Message_Kind'First) is record
      case Kind is
         when Update_One_Accum =>
            Time_Stamp : Time_Stamp_Type;
            Node_Index : Node_Index_Type;
            Node_Value : Accum_Ptr;
               --  Cannot use "not null" here because
               --  the 'Input attribute does default
               --  initialization before calling Read,
               --  and so is certain to fail.
      end case;
   end record;

   package Distrib is
      --  Nested package where we define extenion of
      --  System_Distrib.Distrib_Object used for communication with peer nodes.

      type Reduction_Info
        (Self : not null access Collector;
         Num_Nodes : Node_Index_Type;
         Node_Index : Node_Index_Type) is new Distrib_Object
        with record
         Accums : Accum_Array (1 .. Num_Nodes) := (others => null);
         Time_Stamps : TS_Array (1 .. Num_Nodes) := (others => 0);
      end record;

      overriding
      function Distrib_Type_Id_Ptr (Dis_Obj : Reduction_Info)
        return String_Cptr;

      overriding
      function Distrib_Obj_Id (Dis_Obj : Reduction_Info)
        return Distrib_Object_Id_Type;

      overriding
      function Distrib_Obj_Group (Dis_Obj : Reduction_Info)
        return Group_Context_Ptr;
      --  Return reference to group context for given distributed object

      overriding
      procedure Handle_Obj_Message
        (Dis_Obj : in out Reduction_Info;
         Msg : aliased Ada.Streams.Stream_Element_Array);
      --  Message received that was directed to this distributed object
   end Distrib;

   package body Distrib is
      Type_Id : constant String_Cptr := new String'(Distrib_Type_Id);

      function Distrib_Type_Id_Ptr (Dis_Obj : Reduction_Info)
        return String_Cptr
        is (Type_Id);

      function Distrib_Obj_Id (Dis_Obj : Reduction_Info)
        return Distrib_Object_Id_Type
        is (Dis_Obj.Self.Distrib_Obj_Id);

      function Distrib_Obj_Group (Dis_Obj : Reduction_Info)
        return Group_Context_Ptr
      is (Dis_Obj.Self.GC);
      --  Return reference to group context for given distributed object

      procedure Handle_Obj_Message
        (Dis_Obj : in out Reduction_Info;
         Msg : aliased Ada.Streams.Stream_Element_Array) is

         use Buffered_Streams;
         Reader : aliased Buffered_Reader (Msg'Access);
         --  Read message out of stream
         Message : constant Reduction_Message_Type :=
           Reduction_Message_Type'Input (Reader'Access);

      begin

         if Debug_Distrib then
            Put_Line (Dis_Obj.Self.GC.Node_Index'Image &
              ": message received: " & Message.Kind'Image);
         end if;

         --  Handle message
         case Message.Kind is
            when Update_One_Accum =>
               if Debug_Distrib then
                  Put_Line (Dis_Obj.Self.GC.Node_Index'Image & ": obj " &
                    Distrib_Type_Id & Dis_Obj.Self.Distrib_Obj_Id'Image &
                    " received message from node =" &
                      Message.Node_Index'Image &
                    ", time stamp =" & Message.Time_Stamp'Image);
               end if;
               if Message.Node_Index /= Dis_Obj.Self.GC.Node_Index
                 and then Message.Time_Stamp >
                   Dis_Obj.Time_Stamps (Message.Node_Index)
               then
                  --  This message is not from current node,
                  --  and it is at a higher time stamp than past
                  --  update from this node, so update
                  --  the node info.
                  Dis_Obj.Accums (Message.Node_Index) := Message.Node_Value;
                  Dis_Obj.Time_Stamps (Message.Node_Index) :=
                    Message.Time_Stamp;
               else
                  if Debug_Distrib then
                     Put_Line (Dis_Obj.Self.GC.Node_Index'Image &
                       ": message ignored");
                  end if;
               end if;
         end case;
      end Handle_Obj_Message;
   end Distrib;

   type Reduction_Info is limited new Distrib.Reduction_Info with null record;

   function Init_Info (Self : not null access Collector)
     return not null Reduction_Info_Ptr is
   begin
      return Result : constant not null Reduction_Info_Ptr :=
        new Reduction_Info
          (Self, Self.GC.Num_Nodes, Node_Index => Self.GC.Node_Index) do
         if Self.Distrib_Obj_Id >= Next_Obj_Id_To_Assign then
            Next_Obj_Id_To_Assign := Self.Distrib_Obj_Id + 1;
         end if;
         Create_Distrib_Obj
           (Self.GC.all, Result.all'Unchecked_Access);
      end return;
   end Init_Info;

   function Next_Obj_Id return Distrib_Object_Id_Type is
      --  This can be used to assign the next sequential object id
      --  presuming the Distrib_Type_Id is unique to each instance
      --  of this package.
      Result : constant Distrib_Object_Id_Type := Next_Obj_Id_To_Assign;
   begin
      Next_Obj_Id_To_Assign := Next_Obj_Id_To_Assign + 1;
      return Result;
   end Next_Obj_Id;

   procedure Share_And_Wait
     (Dis_Obj : in out Collector; Accum : Accum_Type) is
   --  Share value of accum with other nodes and then wait for updates
      Time_Stamp : constant Time_Stamp_Type :=
         Dis_Obj.Info.Time_Stamps (Dis_Obj.GC.Node_Index) + 1;

      Message : constant Reduction_Message_Type :=
        (Kind => Update_One_Accum,
         Time_Stamp => Time_Stamp,
         Node_Index => Dis_Obj.GC.Node_Index,
         Node_Value => new Accum_Type'(Accum));
      Stream : aliased Buffered_Streams.Buffered_Stream;
   begin
      Dis_Obj.Info.Accums (Dis_Obj.GC.Node_Index) := Message.Node_Value;
      Dis_Obj.Info.Time_Stamps (Dis_Obj.GC.Node_Index) := Time_Stamp;
      Reduction_Message_Type'Write (Stream'Access, Message);

      System_Distrib.Share_With_Peers
        (Dis_Obj.GC.all, Dis_Obj.Info.all, Stream.Contents);
      if Debug_Distrib then
         Put_Line ("Node " & Dis_Obj.GC.Node_Index'Image &
           " about to wait until Time stamp for obj " &
           Dis_Obj.Info.Distrib_Type_Id &
           Dis_Obj.Info.Distrib_Obj_Id'Image & " >=" &
           Time_Stamp'Image);
      end if;

      --  Keep receiving messages as long as some node has an old time stamp.
      while (for some I in Dis_Obj.Info.Time_Stamps'Range =>
                Dis_Obj.Info.Time_Stamps (I) < Time_Stamp)
      loop
         --  Wait for a message and then re-check
         Dis_Obj.Info.Handle_Queued_Messages (Wait_For_Message => True);
      end loop;
   end Share_And_Wait;

   procedure Distrib_Reduce
     (Dis_Obj : in out Collector;
      Result : in out Accum_Type;
      First : Index_Type;
      Last : Index_Type'Base;
      Num_Chunks : Natural := 0;
      Add_One_Value : access procedure
        (Accum : in out Accum_Type; Index : Index_Type)) is
      --  Iterate in parallel over local indices First to Last, in chunks.
      --  Call Add_One_Value to add a single value to the accumulator.
      --  Will call Combiner to combine accumulators built up
      --  in parallel (some day doing a tree-wise combination in case
      --  the number of threads on a given node is very large).
      --  The node/shard-level result is shared with other nodes/shards
      --  and then combined into a single global accumulator, namely Result.

      Accum_Identity : constant Accum_Type := Result;
         --  Accum initially has "identity" value.
         --  TBD: Might be better to pass in an Identity separately.

      Local_Accum : Accum_Type := Accum_Identity;
         --  Local accumulator for this shard

      protected Prot_Combiner is
         procedure Combine (New_Accum : in out Accum_Type);
      end Prot_Combiner;

      protected body Prot_Combiner is
         procedure Combine (New_Accum : in out Accum_Type) is
         begin
            Combiner (Left => Local_Accum, Right => New_Accum);
         end Combine;
      end Prot_Combiner;

      use System.Parallelism;

      procedure Reduce_Over_One_Chunk
        (Low, High : Longest_Integer; Chunk_Index : Positive) is
         Accum : Accum_Type := Accum_Identity;
      begin
         --  Iterate over the chunk, accumulating using Add_One_Value
         for I in Low .. High loop
            Add_One_Value (Accum, Index_Type (I));
         end loop;
         --  Now combine into the per-shard accumulator
         Prot_Combiner.Combine (Accum);
      end Reduce_Over_One_Chunk;

   begin  --  Distrib_Reduce

      Par_Range_Loop
        (Low => Longest_Integer (First), High => Longest_Integer (Last),
         Num_Chunks => Num_Chunks,
         Loop_Body => Reduce_Over_One_Chunk'Access);

      --  Share accumulator with other nodes
      Share_And_Wait (Dis_Obj, Local_Accum);

      --  Now combine the accumulators left to right into Result.
      for I in 1 .. Dis_Obj.GC.Num_Nodes loop
         Combiner (Left => Result, Right => Dis_Obj.Info.Accums (I).all);
      end loop;
   end Distrib_Reduce;

   procedure Accum_Ptr_Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Ptr : out Accum_Ptr) is
   --  with Post => Ptr /= null;
   --  Use Accum_Type'Input to initialize an allocator
   begin
      Ptr := new Accum_Type'(Accum_Type'Input (Stream));
   end Accum_Ptr_Read;

   procedure Accum_Ptr_Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Ptr : Accum_Ptr) is
   --  with Pre => Ptr /= null;
   --  Use Accum_Type'Output to write out Ptr.all
   begin
      Accum_Type'Output (Stream, Ptr.all);
   end Accum_Ptr_Write;

end Distributed_Reduction;
