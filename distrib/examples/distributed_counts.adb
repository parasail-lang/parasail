------------------------------------------------------------------------------
--                              Distributed Counts                          --
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

with System.Atomic_Operations.Integer_Arithmetic;
with Ada.Streams;
with Buffered_Streams;
with Ada.Text_IO; use Ada.Text_IO;
package body Distributed_Counts is

   Debug_Distrib : constant Boolean := False;

   package Atomic_Arith is
     new System.Atomic_Operations.Integer_Arithmetic (Atomic_Integer);

   type Count_Array is
     array (Node_Index_Type range <>) of Atomic_Integer;

   type Count_Message_Kind is (Update_One_Count);
   type Count_Message_Type
     (Kind : Count_Message_Kind := Count_Message_Kind'First) is record
      case Kind is
         when Update_One_Count =>
            Time_Stamp : Time_Stamp_Type;
            Node_Index : Node_Index_Type;
            Node_Value : Count_Type;
      end case;
   end record;

   package Distrib is
      --  Nested package where we define extenion of
      --  System_Distrib.Distrib_Object used for communication with peer nodes.

      type Count_Info
        (Self : not null access Count;
         Num_Nodes : Node_Index_Type;
         Node_Index : Node_Index_Type) is new Distrib_Object
        with record
         Counts : Count_Array (1 .. Num_Nodes) := (others => 0);
         Time_Stamps : TS_Array (1 .. Num_Nodes) := (others => 0);
      end record;

      overriding
      function Distrib_Type_Id_Ptr (Dis_Obj : Count_Info) return String_Cptr;

      overriding
      function Distrib_Obj_Id (Dis_Obj : Count_Info)
        return Distrib_Object_Id_Type;

      overriding
      function Distrib_Obj_Group (Dis_Obj : Count_Info)
        return Group_Context_Ptr;
      --  Return reference to group context for given distributed object

      overriding
      procedure Handle_Obj_Message
        (Dis_Obj : in out Count_Info;
         Msg : aliased Ada.Streams.Stream_Element_Array);
      --  Message received that was directed to this distributed object
   end Distrib;

   package body Distrib is
      Type_Id : constant String_Cptr := new String'(Distrib_Type_Id);

      function Distrib_Type_Id_Ptr (Dis_Obj : Count_Info) return String_Cptr
        is (Type_Id);

      function Distrib_Obj_Id (Dis_Obj : Count_Info)
        return Distrib_Object_Id_Type
        is (Dis_Obj.Self.Distrib_Obj_Id);

      function Distrib_Obj_Group (Dis_Obj : Count_Info)
        return Group_Context_Ptr
      is (Dis_Obj.Self.GC.all'Unchecked_Access);
      --  Return reference to group context for given distributed object

      procedure Handle_Obj_Message
        (Dis_Obj : in out Count_Info;
         Msg : aliased Ada.Streams.Stream_Element_Array) is

         use Buffered_Streams;
         Reader : aliased Buffered_Reader (Msg'Access);
         Message : Count_Message_Type;

      begin
         --  Read message out of stream
         Count_Message_Type'Read (Reader'Access, Message);

         if Debug_Distrib then
            Put_Line (Dis_Obj.Self.GC.Node_Index'Image &
              ": message received: " & Message.Kind'Image);
         end if;

         --  Handle message
         case Message.Kind is
            when Update_One_Count =>
               if Debug_Distrib then
                  Put_Line (Dis_Obj.Self.GC.Node_Index'Image & ": obj " &
                    Distrib_Type_Id & Dis_Obj.Self.Distrib_Obj_Id'Image &
                    " received message from node =" &
                      Message.Node_Index'Image &
                    ", time stamp =" & Message.Time_Stamp'Image &
                    ", value =" & Message.Node_Value'Image);
               end if;
               if Message.Node_Index /= Dis_Obj.Self.GC.Node_Index
                 and then Message.Time_Stamp >
                   Dis_Obj.Time_Stamps (Message.Node_Index)
               then
                  --  This message is not from current node,
                  --  and it is at a higher time stamp than past
                  --  update from this node, so update
                  --  the node info.
                  Dis_Obj.Counts (Message.Node_Index) :=
                    Count_Type'Pos (Message.Node_Value);
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

   type Count_Info is limited new Distrib.Count_Info with null record;

   function Init_Info (Self : not null access Count)
     return not null Count_Info_Ptr is
   begin
      return Result : constant not null Count_Info_Ptr :=
        new Count_Info
          (Self, Self.GC.Num_Nodes, Node_Index => Self.GC.Node_Index) do
         Create_Distrib_Obj
           (Self.GC.all, Result.all'Unchecked_Access);
      end return;
   end Init_Info;

   procedure Add (C : in out Count; Amount : Count_Type) is
   begin
      Atomic_Arith.Atomic_Add (C.Val, Count_Type'Pos (Amount));
      --  Share increment with other nodes?
   end Add;

   procedure Subtract (C : in out Count; Amount : Count_Type) is
   begin
      Atomic_Arith.Atomic_Subtract (C.Val, Count_Type'Pos (Amount));
   end Subtract;

   procedure Share_And_Wait (C : Count) is
   --  Share value of count with other nodes and then wait for updates
      Time_Stamp : constant Time_Stamp_Type :=
         C.Info.Time_Stamps (C.GC.Node_Index) + 1;

      Message : constant Count_Message_Type :=
        (Kind => Update_One_Count,
         Time_Stamp => Time_Stamp,
         Node_Index => C.GC.Node_Index,
         Node_Value => Count_Type'Val (C.Val));
      Stream : aliased Buffered_Streams.Buffered_Stream;
   begin
      C.Info.Counts (C.GC.Node_Index) := C.Val;
      C.Info.Time_Stamps (C.GC.Node_Index) := Time_Stamp;
      Count_Message_Type'Write (Stream'Access, Message);

      System_Distrib.Share_With_Peers
        (C.GC.all, C.Info.all, Stream.Contents);
      if Debug_Distrib then
         Put_Line ("Node " & C.GC.Node_Index'Image &
           " about to wait until Time stamp for obj " &
           C.Info.Distrib_Type_Id & C.Info.Distrib_Obj_Id'Image & " >=" &
           Time_Stamp'Image);
      end if;

      --  Keep receiving messages as long as some node has an old time stamp.
      while (for some I in C.Info.Time_Stamps'Range =>
                C.Info.Time_Stamps (I) < Time_Stamp)
      loop
         --  Wait for a message and then re-check
         C.Info.Handle_Queued_Messages (Wait_For_Message => True);
      end loop;
   end Share_And_Wait;

   function Value
     (C : Count; Peer_Node_Index : Node_Index_Type;
      Share_First : Boolean := False)
     return Count_Type is
   --  Return value of count for a given node
   --  If Share_First, or if have never shared value, do so now.
   --  If Share_First is False, should not have updated the
   --  value since a prior call on Value, unless fetching own value.
      Node_Index : constant Node_Index_Type := C.GC.Node_Index;
   begin
      if Peer_Node_Index = Node_Index then
         return Count_Type'Val (C.Val);
      end if;

      if Share_First
        or else C.Info.Time_Stamps (Node_Index) = 0
      then
         --  Initialize our own slot in the Counts array
         C.Info.Counts (C.GC.Node_Index) := C.Val;

         --  Share and wait for results
         Share_And_Wait (C);
      else
         --  Value should not have changed since last call on Value
         pragma Assert (C.Info.Counts (Node_Index) = C.Val);
      end if;
      return Count_Type'Val (C.Info.Counts (Peer_Node_Index));
   end Value;

   function Value
     (C : Count; Share_First : Boolean := False)
     return Count_Type is
   --  Return sum of all counts on all nodes
   --  If Share_First, or if have never shared value, do so now.
   --  If Share_First is False, should not have updated the
   --  value since a prior call on Value.
      Sum : Count_Type := Count_Type'Val (0);
      Node_Index : constant Node_Index_Type := C.GC.Node_Index;
   begin
      if Share_First
        or else C.Info.Time_Stamps (Node_Index) = 0
      then
         --  Initialize our own slot in the Counts array
         C.Info.Counts (Node_Index) := C.Val;

         --  Share and wait for results
         Share_And_Wait (C);
      else
         --  Value should not have changed since last call on Value
         pragma Assert (C.Info.Counts (Node_Index) = C.Val);
      end if;

      for Count of C.Info.Counts loop
         Sum := Count_Type'Val (Count_Type'Pos (Sum) +
           Long_Integer (Count));
      end loop;

      return Sum;
   end Value;
end Distributed_Counts;
