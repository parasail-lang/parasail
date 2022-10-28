------------------------------------------------------------------------------
--                              Distributed Vectors                         --
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
package body Distributed_Vectors is

   Debug_Distrib : constant Boolean := False;

   Max_Distrib_Obj_Id : Distrib_Object_Id_Type := 0;
   --  This keeps track of the maximum distributed obj-id type in use

   type Atomic_Integer is new Integer with Atomic;

   package Atomic_Arith is
     new System.Atomic_Operations.Integer_Arithmetic (Atomic_Integer);

   type Count_Array is
     array (Node_Index_Type range <>) of Atomic_Integer;

   type Vec_Message_Kind is (Update_One_Count, Append_Slice);
   type Vec_Message_Type
     (Kind : Vec_Message_Kind := Vec_Message_Kind'First) is record
      Time_Stamp : Time_Stamp_Type;
      Node_Index : Node_Index_Type;
      case Kind is
         when Update_One_Count =>
            Node_Value : Extended_Index;
         when Append_Slice =>
            Slice_Length : Extended_Index;
      end case;
   end record;

   procedure Set_Nth_Element
     (Loc_Vec : in out Local_Vector; Index : Index_Type; Val : Element_Type) is
   begin
      Set_Nth_Element (Loc_Vec, Seq_Vecs.Elem_Index (Index), Val);
   end Set_Nth_Element;

   procedure Assign_Slice
     (To : in out Local_Vector; To_Index : Index_Type;
      From : Local_Vector; From_Index : Index_Type;
      Count : Extended_Index) is
   --  Assign slice of length Count of vector From into vector To at given
   --  indices.
   begin
      Assign_Slice (To, Seq_Vecs.Elem_Index (To_Index),
                    From, Seq_Vecs.Elem_Index (From_Index),
                    Seq_Vecs.Elem_Index (Count));
   end Assign_Slice;

   package Distrib is
      --  Nested package where we define extension of
      --  System_Distrib.Distrib_Object used for communication with peer nodes.

      protected type Prot_Vec (Self : not null access Vector) is
         procedure Append (New_Item : Element_Type);
         --  Append one item to local shard

         procedure Append_Slice
           (From : Vector; First : Index_Type; Last : Extended_Index);
         --  Append (local) slice of another vector onto local shard

         procedure Append_From_Stream
           (From_Stream : in out Buffered_Streams.Buffered_Reader;
            Count : Extended_Index);
         --  Append to local shard from a stream of elements
      end Prot_Vec;

      type Vec_Info
        (Self : not null access Vector;
         Num_Nodes : Node_Index_Type;
         Node_Index : Node_Index_Type) is new Distrib_Object
        with record
         Locked_Vec : Prot_Vec (Self);
         Is_Dirty : Boolean := False;
         Counts : Count_Array (1 .. Num_Nodes) := (others => 0);
         Time_Stamps : TS_Array (1 .. Num_Nodes) := (others => 0);
      end record;

      overriding
      function Distrib_Type_Id_Ptr (Dis_Obj : Vec_Info) return String_Cptr;

      overriding
      function Distrib_Obj_Id (Dis_Obj : Vec_Info)
        return Distrib_Object_Id_Type;

      overriding
      function Distrib_Obj_Group (Dis_Obj : Vec_Info)
        return Group_Context_Ptr;
      --  Return reference to group context for given distributed object

      overriding
      procedure Handle_Obj_Message
        (Dis_Obj : in out Vec_Info;
         Msg : aliased Ada.Streams.Stream_Element_Array);
      --  Message received that was directed to this distributed object
   end Distrib;

   package body Distrib is
      Type_Id : constant String_Cptr := new String'(Distrib_Type_Id);

      function Distrib_Type_Id_Ptr (Dis_Obj : Vec_Info) return String_Cptr
        is (Type_Id);

      function Distrib_Obj_Id (Dis_Obj : Vec_Info)
        return Distrib_Object_Id_Type
        is (Dis_Obj.Self.Distrib_Obj_Id);

      function Distrib_Obj_Group (Dis_Obj : Vec_Info)
        return Group_Context_Ptr
      is (Dis_Obj.Self.GC);
      --  Return reference to group context for given distributed object

      procedure Handle_Obj_Message
        (Dis_Obj : in out Vec_Info;
         Msg : aliased Ada.Streams.Stream_Element_Array) is

         --  NOTE: This procedure is called as a result of a call to
         --        Handle_Queued_Messages so is expected to be on the same
         --        Ada task as other operations in this package,
         --        so race conditions should not occur.

         use Buffered_Streams;
         Reader : aliased Buffered_Reader (Msg'Access);
         Message : Vec_Message_Type;

         My_Index : constant Node_Index_Type := Dis_Obj.Self.GC.Node_Index;
      begin
         --  Read message out of stream
         Vec_Message_Type'Read (Reader'Access, Message);

         if Debug_Distrib then
            Put_Line (My_Index'Image &
              ": message received: " & Message.Kind'Image);
         end if;

         --  Handle message
         case Message.Kind is
            when Update_One_Count =>
               if Debug_Distrib then
                  Put_Line (My_Index'Image & ": obj " &
                    Distrib_Type_Id & Dis_Obj.Self.Distrib_Obj_Id'Image &
                    " received Update_One_Count message from node =" &
                      Message.Node_Index'Image &
                    ", time stamp =" & Message.Time_Stamp'Image &
                    ", value =" & Message.Node_Value'Image);
               end if;
               if Message.Node_Index /= My_Index
                 and then Message.Time_Stamp >
                   Dis_Obj.Time_Stamps (Message.Node_Index)
               then
                  --  This message is not from current node,
                  --  and it is at a higher time stamp than past
                  --  update from this node, so update
                  --  the node info.
                  Dis_Obj.Counts (Message.Node_Index) :=
                    Extended_Index'Pos (Message.Node_Value);
                  Dis_Obj.Time_Stamps (Message.Node_Index) :=
                    Message.Time_Stamp;
               else
                  if Debug_Distrib then
                     Put_Line (My_Index'Image & ": message ignored");
                  end if;
               end if;

            when Append_Slice =>
               if Debug_Distrib then
                  Put_Line (My_Index'Image & ": obj " &
                    Distrib_Type_Id & Dis_Obj.Self.Distrib_Obj_Id'Image &
                    " received Append_Slice message from node =" &
                      Message.Node_Index'Image &
                    ", time stamp =" & Message.Time_Stamp'Image &
                    ", length =" & Message.Slice_Length'Image);
               end if;
               if Message.Node_Index /= My_Index
                 and then Message.Time_Stamp >
                   Dis_Obj.Time_Stamps (Message.Node_Index)
               then
                  --  This message is not from current node,
                  --  and it is at a higher time stamp than past
                  --  update from this node, so append the slice
                  --  onto local shard.
                  Dis_Obj.Locked_Vec.Append_From_Stream
                    (Reader, Message.Slice_Length);
                  Dis_Obj.Time_Stamps (Message.Node_Index) :=
                    Message.Time_Stamp;
               else
                  if Debug_Distrib then
                     Put_Line (My_Index'Image & ": message ignored");
                  end if;
               end if;
         end case;
      end Handle_Obj_Message;

      protected body Prot_Vec is
         procedure Append (New_Item : Element_Type) is
         begin
            Self.Loc_Vec.Add_Element (New_Item);
         end Append;

         procedure Append_Slice
           (From : Vector; First : Index_Type; Last : Extended_Index) is
            Count : constant Extended_Index := Last - First + 1;
         begin
            if Count > 0 then
               Self.Loc_Vec.Reserve_Capacity
                 (Self.Loc_Vec.Length + Count);
               for I in First .. Last loop
                  Self.Loc_Vec.Add_Element (From (I));
               end loop;
            end if;
         end Append_Slice;

         procedure Append_From_Stream
           (From_Stream : in out Buffered_Streams.Buffered_Reader;
            Count : Extended_Index) is
         --  Append to local shard from a stream of elements
         begin
            if Count > 0 then
               declare
                  New_Item : Element_Type;
               begin
                  Self.Loc_Vec.Reserve_Capacity
                    (Self.Loc_Vec.Length + Count);
                  for I in 1 .. Count loop
                     Element_Type'Read (From_Stream'Access, New_Item);
                     Self.Loc_Vec.Add_Element (New_Item);
                  end loop;
               end;
            end if;
         end Append_From_Stream;
      end Prot_Vec;
   end Distrib;

   type Vec_Info is limited new Distrib.Vec_Info with null record;

   function Init_Info (Self : not null access Vector)
     return not null Vec_Info_Ptr is
   begin
      --  Make sure that obj-ids keep increasing
      pragma Assert (Self.Distrib_Obj_Id > Max_Distrib_Obj_Id);
      Max_Distrib_Obj_Id := Self.Distrib_Obj_Id;

      return Result : constant not null Vec_Info_Ptr :=
        new Vec_Info
          (Self, Self.GC.Num_Nodes, Node_Index => Self.GC.Node_Index) do
         Create_Distrib_Obj
           (Self.GC.all, Result.all'Unchecked_Access);
      end return;
   end Init_Info;

   procedure Bump_And_Share_Time_Stamp (V : Vector;
                                        Time_Stamp : Time_Stamp_Type := 0);
   --  Share value of count with other nodes and then wait for updates

   procedure Bump_And_Share_Time_Stamp (V : Vector;
                                        Time_Stamp : Time_Stamp_Type := 0) is
      Time_Stamp_To_Use : constant Time_Stamp_Type :=
         (if Time_Stamp > 0 then Time_Stamp
          else V.Info.Time_Stamps (V.GC.Node_Index) + 1);

      Message : constant Vec_Message_Type :=
        (Kind => Update_One_Count,
         Time_Stamp => Time_Stamp_To_Use,
         Node_Index => V.GC.Node_Index,
         Node_Value => V.Loc_Vec.Length);
      Stream : aliased Buffered_Streams.Buffered_Stream;
   begin
      V.Info.Counts (V.GC.Node_Index) :=
        Index_Type'Pos (V.Loc_Vec.Length);
      V.Info.Time_Stamps (V.GC.Node_Index) := Time_Stamp_To_Use;
      Vec_Message_Type'Write (Stream'Access, Message);

      if Debug_Distrib then
         Put_Line ("Node " & V.GC.Node_Index'Image &
           " about to share Time stamp for obj " &
           V.Info.Distrib_Type_Id & V.Info.Distrib_Obj_Id'Image & " =" &
           Time_Stamp'Image);
      end if;

      System_Distrib.Share_With_Peers
        (V.GC.all, V.Info.all, Stream.Contents);

      if Debug_Distrib then
         Put_Line ("Node " & V.GC.Node_Index'Image &
           " just shared Time stamp for obj " &
           V.Info.Distrib_Type_Id & V.Info.Distrib_Obj_Id'Image & " =" &
           Time_Stamp'Image);
      end if;
   end Bump_And_Share_Time_Stamp;

   function Empty_Vector
     (GC : not null System_Distrib.Group_Context_Ptr;
      Distrib_Obj_Id : System_Distrib.Distrib_Object_Id_Type := 0;
      Capacity : Extended_Index := 0) return Vector is

      New_Obj_Id : constant System_Distrib.Distrib_Object_Id_Type :=
        (if Distrib_Obj_Id /= 0
         then Distrib_Obj_Id
         else Max_Distrib_Obj_Id + 1);
   begin
      return Result : Vector (GC, Distrib_Obj_Id) do
         Result.Loc_Vec := Empty_Vector
                         (Capacity => Seq_Vecs.Elem_Index'Base (Capacity));
      end return;
   end Empty_Vector;

   function Copy_Vector
     (Existing_Vector : Vector;
      Distrib_Obj_Id : System_Distrib.Distrib_Object_Id_Type := 0)
     return Vector is
   --  Make a copy of Existing_Vector with the given obj-id.
   --  If Distrib_Obj_Id = 0, then assign a new one based on state
   --  of Existing_Vector.
      New_Obj_Id : constant System_Distrib.Distrib_Object_Id_Type :=
        (if Distrib_Obj_Id /= 0
         then Distrib_Obj_Id
         else Max_Distrib_Obj_Id + 1);
   begin
      return Result : Vector
        (Existing_Vector.GC, New_Obj_Id) do
         Result.Loc_Vec := Existing_Vector.Loc_Vec;
      end return;
   end Copy_Vector;

   procedure Set_Empty (Vec : in out Vector) is
   --  Set vector back to the empty state
   begin
      Vec.Loc_Vec.Set_Empty;
   end Set_Empty;

   procedure Append (Container : in out Vector;
                     New_Item  :        Element_Type) is
   begin
      Container.Info.Locked_Vec.Append (New_Item);
   end Append;

   procedure Append_Slice (Onto : in out Vector;
                           From : Vector;
                           First : Index_Type;
                           Last : Extended_Index;
                           Onto_Shard : System_Distrib.Shard_Index_Type := 0;
                           Time_Stamp : System_Distrib.Time_Stamp_Type := 0) is
   --  Append a slice of the local shard of vector "From" onto
   --  shard with index "Onto_Shard" of distributed vector "Onto".
   --  If "Onto_Shard" is zero this appends on the local shard.
   --  If "Onto_Shard" is non-zero, it appends on the given shard
   --  with the given timestamp (which should not be zero in this case).
   begin
      if Onto_Shard = 0
        or else Onto_Shard = Shard_Index_Type (Onto.GC.Node_Index)
      then
         --  Append onto local shard
         --  TBD: Presuming shard index = node index
         Onto.Info.Locked_Vec.Append_Slice (From, First, Last);

         if Time_Stamp /= 0 then
            --  Set new time stamp for this node
            Onto.Info.Time_Stamps (Onto.GC.Node_Index) := Time_Stamp;
         end if;
      else
         --  Send a message to node holding shard with index "Onto_Shard"
         declare
            Message : constant Vec_Message_Type :=
              (Kind => Append_Slice,
               Time_Stamp =>
                 (if Time_Stamp = 0
                  then Onto.Info.Time_Stamps (Onto.GC.Node_Index) + 1
                  else Time_Stamp),
               Node_Index => Onto.GC.Node_Index,
               Slice_Length => Last - First + 1);
            Stream : aliased Buffered_Streams.Buffered_Stream;
         begin
            --  Write out the message header
            Vec_Message_Type'Write (Stream'Access, Message);

            --  Write out the elements of the slice
            for I in First .. Last loop
               Element_Type'Write
                 (Stream'Access, From (I));
            end loop;

            --  Send the message to appropriate peer node
            --  TBD: Presuming Shard index = Node index for now
            System_Distrib.Send_To_Peer
              (Onto.GC.all, Onto.Info.all,
               Node_Index_Type (Onto_Shard), Stream.Contents);

            if Debug_Distrib then
               Put_Line ("Node" & Onto.GC.Node_Index'Image &
                 " just sent slice of vector of bounds" &
                 First'Image & " .." & Last'Image & " to Node" &
                 Onto_Shard'Image & " with Time stamp for obj " &
                 Onto.Info.Distrib_Type_Id &
                 Onto.Info.Distrib_Obj_Id'Image & " =" &
                 Message.Time_Stamp'Image & ", local length =" &
                 Onto.Length'Image);
            end if;
         end;
      end if;
   end Append_Slice;

   procedure Append_Local_Vector (Onto : in out Vector;
                                  From : Local_Vector'Class) is
   --  Append local vector onto current shard of distributed vector.
   begin
      for I in 1 .. From.Length loop
         Onto.Append (From (I));
      end loop;
   end Append_Local_Vector;

   procedure Await_Incoming_Updates
     (Onto : in out Vector;
      Time_Stamp : System_Distrib.Time_Stamp_Type := 0) is
      Time_Stamp_To_Use : constant Time_Stamp_Type :=
        (if Time_Stamp = 0
         then Onto.Info.Time_Stamps (Onto.GC.Node_Index) + 1
         else Time_Stamp);
   begin
      Onto.Info.Time_Stamps (Onto.GC.Node_Index) := Time_Stamp_To_Use;
      --  Keep receiving messages if the time stamp of any node has not
      --  reached Time_Stamp_To_Use.
      while (for some TS of Onto.Info.Time_Stamps => TS < Time_Stamp_To_Use)
      loop
         Onto.Info.Handle_Queued_Messages (Wait_For_Message => True);
      end loop;
   end Await_Incoming_Updates;

   function First_Index (Container : Vector) return Index_Type is
   begin
      return Index_Type'First;
   end First_Index;

   function Last_Index (Container : Vector) return Extended_Index is
   begin
      return Container.Loc_Vec.Length;
   end Last_Index;

   function Length
     (Container : Vector;
      Time_Stamp : System_Distrib.Time_Stamp_Type := 0)
     return Extended_Index is
   begin
      if Time_Stamp = 0 then
         --  Return length of local shard
         return Container.Loc_Vec.Length;
      else
         --  Share length with other nodes
         Bump_And_Share_Time_Stamp (Container, Time_Stamp);

         --  Wait for all nodes to reach time stamp of this node.
         while (for some TS of Container.Info.Time_Stamps =>
                  TS < Time_Stamp)
         loop
            Container.Info.Handle_Queued_Messages (Wait_For_Message => True);
         end loop;

         declare
            Overall_Length : Extended_Index :=
              Container.Loc_Vec.Length;
            My_Index : constant Node_Index_Type :=
              Container.GC.Node_Index;
         begin
            for I in 1 .. Container.GC.Num_Nodes loop
               if I /= My_Index then
                  Overall_Length := Overall_Length +
                    Extended_Index (Container.Info.Counts (I));
               end if;
            end loop;

            return Overall_Length;
         end;
      end if;
   end Length;

   function Element (Container : Vector;
                     Index     : Index_Type)
      return Element_Type is
   begin
      return Container.Loc_Vec.Nth_Element (Index);
   end Element;

   procedure Process_Shard_As_Local_Vector
     (Container : in out Vector;
      Process : not null access procedure (Vec : in out Local_Vector);
      Shard : System_Distrib.Shard_Index_Type := 0) is
   --  Process shard as a "local" vector
   --  Shard = 0 implies local shard.
   begin
      --  TBD: Only handles local vector currently
      pragma Assert (Shard = 0);
      Process (Container.Loc_Vec);
   end Process_Shard_As_Local_Vector;

   procedure For_Each_Shard
     (Container : Vector;
      Process_One_Shard : not null access procedure
        (Shard : System_Distrib.Shard_Index_Type);
      Sequential : Boolean := False) is
   --  Process each shard of vector.
   --  If Sequential = True, then call Process_One_Shard in sequence
   --  for each shard.  If Sequential = False, then process all of the
   --  shards in parallel.
   --  Return when the shards on the current node are done.
      My_Index : constant Node_Index_Type :=
        Container.GC.Node_Index;
   begin
      if Sequential and then My_Index > 1 then
         --  Wait for prior node to complete its iteration
         while Container.Info.Time_Stamps (My_Index - 1) <=
            Container.Info.Time_Stamps (My_Index)
         loop
            Container.Info.Handle_Queued_Messages (Wait_For_Message => True);
         end loop;
      end if;
      Process_One_Shard (Shard_Index_Type (My_Index));
      if Sequential and then My_Index < Container.GC.Num_Nodes then
         --  Signal next node that we are done
         Bump_And_Share_Time_Stamp (Container);
      end if;
   end For_Each_Shard;

   procedure Iterate (Container : Vector;
                      Num_Chunks_Per_Shard : Natural := 1;
                      Process : not null access procedure
                        (Element : Element_Type)) is
   --  Iterate over vector.
   --  If Num_Chunks_Per_Shard = 1 then this is a sequential
   --  iteration over the entire distributed vector, but with
   --  an initial wait for the "left" neighboring node (if any)
   --  and a final signaling of the "right" neighboring node (if any).
      My_Index : constant Node_Index_Type :=
        Container.GC.Node_Index;
   begin
      if My_Index > 1 then
         --  Wait for prior node to complete its iteration
         while Container.Info.Time_Stamps (My_Index - 1) <=
            Container.Info.Time_Stamps (My_Index)
         loop
            Container.Info.Handle_Queued_Messages (Wait_For_Message => True);
         end loop;
      end if;
      for I in Container.First_Index .. Container.Last_Index loop
         Process (Container (I));
      end loop;
      if My_Index < Container.GC.Num_Nodes then
         --  Signal next node that we are done
         Bump_And_Share_Time_Stamp (Container);
      end if;
   end Iterate;

   procedure Var_Iterate (Container : in out Vector;
                          Num_Chunks_Per_Shard : Natural := 1;
                          Process : not null access procedure
                            (Element : in out Element_Type)) is
   --  Iterate over vector.
   --  If Num_Chunks_Per_Shard = 1 then this is a sequential
   --  iteration over the entire distributed vector, but with
   --  an initial wait for the "left" neighboring node (if any)
   --  and a final signaling of the "right" neighboring node (if any).
   begin
      --  TBD: Wait first
      for I in Container.First_Index .. Container.Last_Index loop
         Process (Container.Loc_Vec.Nth_Reference (I));
      end loop;
      --  TBD: Signal after
   end Var_Iterate;

end Distributed_Vectors;
