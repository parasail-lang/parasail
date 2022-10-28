------------------------------------------------------------------------------
--                          System_Distrib                                  --
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

--  Package defining an interface to distributed computing.

--  pragma Ada_2020;

with ZMQ.Zpollers;
with ZMQ.String_Zlists;
use ZMQ;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Buffered_Streams;
with Interfaces.C.Strings;
with System;
package body System_Distrib is
   --  A central switchboard to support distributed computing.

   pragma Linker_Options ("-lzyre");
   pragma Linker_Options ("-lczmq");
   pragma Linker_Options ("-lzmq");

   Debug_Distrib : constant Boolean := False;

   type Distrib_Object_Key is record
      Type_Id : String_Cptr := null;
      Obj_Id : Distrib_Object_Id_Type := 0;
   end record;

   function "<" (Left, Right : Distrib_Object_Key) return Boolean is
     (Left.Type_Id.all < Right.Type_Id.all
       or else
      (Left.Type_Id.all = Right.Type_Id.all
        and then Left.Obj_Id < Right.Obj_Id));

   --  Map from distributed object key (type-id, object-id) to actual
   --  distributed object handle.
   package Live_Object_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Distrib_Object_Key, Element_Type => Distrib_Object_Ptr);

   --  We use a Zmsg to keep a list of (sub) messages received
   --  for a given object before it is created.
   type Saved_Msgs_Ptr is access Zmq.Zmsgs.Zmsg;

   --  Map from distributed object key (type-id, object-id) to set of
   --  messages received before the object was created
   package Saved_Msgs_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Distrib_Object_Key, Element_Type => Saved_Msgs_Ptr);

   task type Zyre_Comm_Task_Type
     (GC : not null Group_Context_Ptr) is
   --  This task type joins a group, initializes
   --  the Group_Context, and then
   --  receives messages and hands them out to the
   --  appropriate distributed object handlers.
      entry Join_Group
        (Group_Name : String; Num_Cores : Positive; Min_Num_Nodes : Positive);

      entry Wait_For_Shutdown;
   end Zyre_Comm_Task_Type;

   function Join_Group
     (Group_Name : String; Num_Cores : Positive; Min_Num_Nodes : Positive)
     return Group_Context is
      use ZMQ.Zyre;
   begin
      return GC : aliased Group_Context
      do
         GC.Zyre_Comm_Task := new Zyre_Comm_Task_Type
           (GC => GC'Unchecked_Access);

         GC.Zyre_Comm_Task.Join_Group (Group_Name,
                        Num_Cores => Num_Cores,
                        Min_Num_Nodes => Min_Num_Nodes);

         if Debug_Distrib then
            Put_Line (GC.Node_Index'Image & ": Name = " & Zyre_Name (GC.Node) &
              "; returned from Join_Group entry call");
         end if;
      end return;
   end Join_Group;

   procedure Create_Distrib_Obj
     (GC : in out Group_Context;
      Dis_Obj : not null Distrib_Object_Ptr) is
   --  Create a new distributed object, represented by the given
   --  access-to-distrib-object value.
      Mesg : Zmsgs.Zmsg := Zmsgs.Zmsg_New;
      Stream : aliased Buffered_Streams.Buffered_Stream;
   begin
      --  Send "NEW_OBJ" message to Zyre_Comm_Task
      Distrib_Object_Ptr'Write (Stream'Access, Dis_Obj);
      if Zmsgs.Zmsg_Addstr (Mesg, "NEW_OBJ") /= 0 then
         raise ZMQ_Error;
      end if;
      if Zmsgs.Zmsg_Addstr (Mesg, Dis_Obj.Distrib_Type_Id) /= 0 then
         raise ZMQ_Error;
      end if;
      if Zmsgs.Zmsg_Addstr (Mesg, Dis_Obj.Distrib_Obj_Id'Image) /= 0 then
         raise ZMQ_Error;
      end if;

      declare
         Stream_Contents : Ada.Streams.Stream_Element_Array
           renames Stream.Contents;
         Frame : Zframes.Zframe :=
                   Zframes.Zframe_From_Stream (Stream_Contents);
      begin
         if Zmsgs.Zmsg_Append (Mesg, Frame) /= 0 then
            raise ZMQ_Error;
         end if;

         if Debug_Distrib then
            Put_Line (GC.Index'Image & ": Sending NEW_OBJ message: " &
              Dis_Obj.Distrib_Type_Id & Dis_Obj.Distrib_Obj_Id'Image);
         end if;

         if Zmsgs.Zmsg_Send (Mesg, Zsocks.Zsock_Addr (GC.Share_Socket)) /= 0
         then
            raise ZMQ_Error;
         end if;
      end;
   end Create_Distrib_Obj;

   procedure Share_With_Peers
     (GC : in out Group_Context;
      Dis_Obj : Distrib_Object'Class;
      Stream : Ada.Streams.Stream_Element_Array) is
   --  Share the info embedded in the stream with peers
      Mesg : Zmsgs.Zmsg := Zmsgs.Zmsg_New;
      Frame : Zframes.Zframe := Zframes.Zframe_From_Stream (Stream);
   begin
      --  Shout the information from the stream to all members of group.
      --  It should be identified by the distrib type-id/obj-id plus
      --  some operation-id in the stream.
      if Zmsgs.Zmsg_Addstr (Mesg, "SHARE") /= 0 then
         raise ZMQ_Error;
      end if;
      if Zmsgs.Zmsg_Addstr (Mesg, Dis_Obj.Distrib_Type_Id) /= 0 then
         raise ZMQ_Error;
      end if;
      if Zmsgs.Zmsg_Addstr (Mesg, Dis_Obj.Distrib_Obj_Id'Image) /= 0 then
         raise ZMQ_Error;
      end if;
      if Zmsgs.Zmsg_Append (Mesg, Frame) /= 0 then
         raise ZMQ_Error;
      end if;
      if Debug_Distrib then
         Put_Line (GC.Index'Image & ": Sharing data of " &
           Dis_Obj.Distrib_Type_Id & Dis_Obj.Distrib_Obj_Id'Image);
      end if;
      if Zmsgs.Zmsg_Send (Mesg, Zsocks.Zsock_Addr (GC.Share_Socket)) /= 0 then
         raise ZMQ_Error;
      end if;
   end Share_With_Peers;

   procedure Send_To_Peer
     (GC : in out Group_Context;
      Dis_Obj : Distrib_Object'Class;
      Peer : Node_Index_Type;
      Stream : Ada.Streams.Stream_Element_Array) is
   --  Send the info embedded in the stream to the specified peer node.
      Mesg : Zmsgs.Zmsg := Zmsgs.Zmsg_New;
      Frame : Zframes.Zframe := Zframes.Zframe_From_Stream (Stream);
   begin
      --  Whisper the information from the stream to specified peer node.
      --  It should be identified by the distrib type-id/obj-id plus
      --  some operation-id in the stream.
      if Zmsgs.Zmsg_Addstr (Mesg, "SEND_TO_PEER") /= 0 then
         raise ZMQ_Error;
      end if;
      if Zmsgs.Zmsg_Addstr (Mesg, Peer'Image) /= 0 then
         raise ZMQ_Error;
      end if;
      if Zmsgs.Zmsg_Addstr (Mesg, Dis_Obj.Distrib_Type_Id) /= 0 then
         raise ZMQ_Error;
      end if;
      if Zmsgs.Zmsg_Addstr (Mesg, Dis_Obj.Distrib_Obj_Id'Image) /= 0 then
         raise ZMQ_Error;
      end if;
      if Zmsgs.Zmsg_Append (Mesg, Frame) /= 0 then
         raise ZMQ_Error;
      end if;
      if Debug_Distrib then
         Put_Line (GC.Index'Image & ": Sending data of " &
           Dis_Obj.Distrib_Type_Id & Dis_Obj.Distrib_Obj_Id'Image &
           " to peer node" & Peer'Image);
      end if;
      if Zmsgs.Zmsg_Send (Mesg, Zsocks.Zsock_Addr (GC.Share_Socket)) /= 0 then
         raise ZMQ_Error;
      end if;
   end Send_To_Peer;

   procedure Handle_Queued_Messages
     (Dis_Obj : in out Distrib_Object'Class;
      Wait_For_Message : Boolean := False) is
      --  This class-wide operation calls "Handle_Obj_Message"
      --  on all of the queued messages for the given object.
      --  If Wait_For_Message is True, this will block the caller
      --  if there are no messages.

      Message_Frame : Zframes.Zframe;
   begin
      if Wait_For_Message then
         --  Make "regular" entry call to wait for a message
         if Debug_Distrib then
            Put_Line (Dis_Obj.Distrib_Obj_Group.Index'Image &
              ": About to wait for message for obj " &
              Dis_Obj.Distrib_Type_Id & Dis_Obj.Distrib_Obj_Id'Image);
         end if;
         Dis_Obj.Message_Queue.Retrieve_Frame (Message_Frame);
      else
         --  Make "selective" entry call so not blocked
         select
            Dis_Obj.Message_Queue.Retrieve_Frame (Message_Frame);
         else
            return;  --  No more messages
         end select;
      end if;

      loop
         --  Send message frame as a stream to handler
         declare
            use Zframes;
            Message_Stream : aliased constant
              Ada.Streams.Stream_Element_Array :=
                Zframe_As_Stream (Message_Frame);
                  --  TBD: Try to do this without multiple copies
                  --       of the data.  Perhaps return a pointer
                  --       to the stream element array, though that
                  --       is difficult for access-to-unconstrained.
                  --       Perhaps have generic which you instantiate
                  --       with the Zframe and it declares array subtype
                  --       and function which returns acc-to-constrained.
         begin
            if Debug_Distrib then
               Put_Line (Dis_Obj.Distrib_Obj_Group.Index'Image &
                 ": Saved message sent to obj: " &
                 Dis_Obj.Distrib_Type_Id & Dis_Obj.Distrib_Obj_Id'Image);
            end if;
            Dis_Obj.Handle_Obj_Message (Message_Stream);
            Zframe_Destroy (Message_Frame);
         end;

         --  Get next message frame
         --  Make "selective" entry call so not blocked
         select
            Dis_Obj.Message_Queue.Retrieve_Frame (Message_Frame);
         else
            return;  --  No more messages
         end select;
      end loop;
   end Handle_Queued_Messages;

   procedure Finish (GC : in out Group_Context) is
      Mesg : Zmsgs.Zmsg := Zmsgs.Zmsg_New;
   begin
      if Debug_Distrib then
         Put_Line (GC.Index'Image & ": Finishing up");
      end if;

      --  Send SHUTDOWN message
      if Zmsgs.Zmsg_Addstr (Mesg, "SHUTDOWN") /= 0 then
         raise ZMQ_Error;
      end if;
      if Zmsgs.Zmsg_Send (Mesg, Zsocks.Zsock_Addr (GC.Share_Socket)) /= 0 then
         raise ZMQ_Error;
      end if;

      --  Now wait for task to complete its work
      GC.Zyre_Comm_Task.Wait_For_Shutdown;

      if Debug_Distrib then
         Put_Line (GC.Index'Image & ": Comm task has shutdown");
      end if;

      --  And stop and destroy the node
      ZMQ.Zyre.Zyre_Stop (GC.Node);
      ZMQ.Zyre.Zyre_Destroy (GC.Node);
      --  TBD: Recover space for Group_Name_Ptr
   exception
      when E : others =>
         Put_Line (GC.Index'Image & ": Exception while finishing: " &
           Ada.Exceptions.Exception_Information (E));
         ZMQ.Zyre.Zyre_Stop (GC.Node);
         ZMQ.Zyre.Zyre_Destroy (GC.Node);
   end Finish;

   protected body Prot_Message_Queue_Type is
      procedure Add_Frame (Frame : in out ZMQ.Zframes.Zframe) is
      begin
         if Zmsgs.Zmsg_Append (Frame_Queue, Frame) /= 0 then
            raise ZMQ_Error;
         end if;
      end Add_Frame;

      entry Retrieve_Frame (Frame : in out ZMQ.Zframes.Zframe)
        when Frame_Queue.Zmsg_Size > 0 is
         New_Frame : Zframes.Zframe := Frame_Queue.Zmsg_Pop;
      begin
         Frame.Zframe_Move (From => New_Frame);
      end Retrieve_Frame;
   end Prot_Message_Queue_Type;

   task body Zyre_Comm_Task_Type is
      use ZMQ.Zyre, ZMQ.Zsocks, ZMQ.Zpollers;
      Num_In_Group : Positive := 1;  -- Including self
      Num_Joins : Positive := 1;  -- Including self
      Total_Num_Cores : Natural := 0;
      My_Offset : Natural := 0;
      My_Index : Node_Index_Type := 1;
      Receive_Poller : Zpoller := Zpoller_New;
      Node : Zyre_Node := Zyre_New ("");
      Live_Objects : Live_Object_Maps.Map;
      Saved_Msgs : Saved_Msgs_Maps.Map;

      function Find_Live_Obj (Key : Distrib_Object_Key)
        return Distrib_Object_Ptr;
      --  Find handle on distributed object give its key

      function Find_Live_Obj (Key : Distrib_Object_Key)
        return Distrib_Object_Ptr is
         Pos : constant Live_Object_Maps.Cursor := Live_Objects.Find (Key);
      begin
         if Pos in Live_Object_Maps.No_Element then
            return null;
         else
            return Live_Object_Maps.Element (Pos);
         end if;
      end Find_Live_Obj;

      function Find_Saved_Msgs (Key : Distrib_Object_Key)
        return Saved_Msgs_Ptr;
      --  Find messages received before object was created, if any

      function Find_Saved_Msgs (Key : Distrib_Object_Key)
        return Saved_Msgs_Ptr is
         Pos : constant Saved_Msgs_Maps.Cursor := Saved_Msgs.Find (Key);
      begin
         if Pos in Saved_Msgs_Maps.No_Element then
            return null;
         else
            return Saved_Msgs_Maps.Element (Pos);
         end if;
      end Find_Saved_Msgs;

      procedure Clean_Up_Comm_Task;
      --  Clean up at end of task

      procedure Clean_Up_Comm_Task is
      begin
         Zpoller_Destroy (Receive_Poller);
         Zsock_Destroy (GC.Share_Socket);
         Zsock_Destroy (GC.Share_Forward_Socket);
         Live_Objects.Clear;
         Saved_Msgs.Clear;  --  TBD: Complain if not already empty?
      end Clean_Up_Comm_Task;

   begin  --  Zyre_Comm_Task_Type

      accept Join_Group
        (Group_Name : String; Num_Cores : Positive; Min_Num_Nodes : Positive)
      do
         GC.Share_Socket := Zsock_New_Pair ("@inproc://Share_Socket");
         GC.Share_Forward_Socket := Zsock_New_Pair (">inproc://Share_Socket");
         Total_Num_Cores := Num_Cores;  --  Counting this node only.
         GC.Size := Num_Cores;
         GC.Group_Name_Ptr := new String'(Group_Name);

         Zyre_Node_Move (From => Node, To => GC.Node);

         --  Zyre_Set_Verbose (GC.Node);
         Zyre_Set_Header (GC.Node, "Num_Cores", Num_Cores'Image);

         if Zyre_Start (GC.Node) /= 0 then
            raise ZMQ.ZMQ_Error;
         end if;
         if Zyre_Join (GC.Node, Group_Name) /= 0 then
            raise ZMQ.ZMQ_Error;
         end if;
         --  At this point, we want to figure out our offset and size
         --  within the total group worker space.
         --  We can loop doing a zyre_recv until we see the group
         --  reach the expected size via JOIN messages.
         --  Zyre_Peers_By_Group
         while Num_In_Group < Min_Num_Nodes loop
            declare
               Msg : constant Zmsgs.Zmsg := Zyre_Recv (GC.Node);
               Command : constant String := Zmsgs.Zmsg_Popstr (Msg);
               My_Name : constant String := Zyre_Name (GC.Node);
               use Interfaces.C.Strings;
            begin
               if Debug_Distrib then
                  if Command = "ENTER" or else Command = "JOIN" then
                     declare
                        From_UUID : constant String := Zmsgs.Zmsg_Popstr (Msg);
                        From_Name : constant String := Zmsgs.Zmsg_Popstr (Msg);
                     begin
                        Put_Line (My_Name & ": Got command " & Command &
                          " from " & From_Name);
                     end;
                  else
                     Put_Line (My_Name & ": Got command " & Command);
                  end if;
               end if;
               if Command = "JOIN" then
                  Num_Joins := Num_Joins + 1;
               end if;
               if Num_Joins >= Min_Num_Nodes then
                  --  Now get the list of peers
                  declare
                     Peers_In_Group : String_Zlists.Zlist :=
                       Zyre_Peers_By_Group (GC.Node, Group_Name);
                     Size : constant Natural := Natural
                       (String_Zlists.Zlist_Size (Peers_In_Group));
                     Peer : chars_ptr :=
                       String_Zlists.Zlist_First (Peers_In_Group);
                     Core_Sizes : Size_Array (1 .. Node_Index_Type (Size + 1));
                         --  Sizes, indexed by order in Peers_In_Group list,
                         --  plus one for local node.
                     List_Index : Node_Index_Type := 1;
                  begin
                     if Debug_Distrib then
                        Put_Line (My_Name & ": Zyre_Peers_By_Group of size" &
                          Size'Image);
                     end if;
                     Total_Num_Cores := Num_Cores;
                     My_Offset := 0;
                     My_Index := 1;
                     while Peer /= Null_Ptr loop
                        declare
                           Peer_UUID : constant String := Value (Peer);
                           Peer_Cores : constant Natural := Natural'Value
                             (Zyre_Peer_Header_Value
                               (GC.Node, Peer_UUID, "Num_Cores"));
                           Peer_Name : constant String (1 .. 6) :=
                             Peer_UUID (Peer_UUID'First ..
                               Peer_UUID'First + 5);
                        begin
                           if Debug_Distrib then
                              Put_Line (My_Name & ": Peer: " & Peer_Name &
                                ", Num_Cores =" & Peer_Cores'Image);
                           end if;
                           Total_Num_Cores := Total_Num_Cores + Peer_Cores;

                           if Peer_Name < My_Name then
                              My_Offset := My_Offset + Peer_Cores;
                              My_Index := My_Index + 1;
                           end if;

                           --  Remember number of cores of this peer
                           Core_Sizes (List_Index) := Peer_Cores;

                           Peer := String_Zlists.Zlist_Next (Peers_In_Group);
                           List_Index := List_Index + 1;
                        end;
                     end loop;

                     --  Last element holds size of current node
                     Core_Sizes (List_Index) := Num_Cores;

                     Num_In_Group := Size + 1;
                     if Debug_Distrib then
                        Put_Line (My_Name &
                          ": Num in Group:" & Num_In_Group'Image &
                          ", Total Cores:" & Total_Num_Cores'Image);
                     end if;

                     if Num_In_Group >= Min_Num_Nodes then
                        --  Save Node Name <-> Node Index mappings
                        GC.Node_Names :=
                          new String_Cptr_Array
                            (1 .. Node_Index_Type (Num_In_Group));

                        GC.Node_UUIDs :=
                          new String_Cptr_Array
                            (1 .. Node_Index_Type (Num_In_Group));

                        GC.Node_Sizes :=
                          new Size_Array
                            (1 .. Node_Index_Type (Num_In_Group));

                        --  Start back at the beginning of the list of peers
                        Peer :=
                          String_Zlists.Zlist_First (Peers_In_Group);

                        List_Index := 1;
                        while Peer /= Null_Ptr loop
                           declare
                              Peer_UUID : constant String := Value (Peer);
                              Peer_Name : constant String (1 .. 6) :=
                                Peer_UUID (Peer_UUID'First ..
                                  Peer_UUID'First + 5);
                              Key : constant String_CPtr :=
                                new String'(Peer_Name);
                           begin
                              --  Add name to ordered map
                              GC.Node_Name_Map.Include
                                (Key => Key,
                                 New_Item => List_Index);  --  Updated below

                              --  and to Node-to-UUID map
                              GC.Node_Name_To_UUID_Map.Include
                                (Key => Key,
                                 New_Item => new String'(Peer_UUID));

                              --  Advance to next peer
                              Peer :=
                                String_Zlists.Zlist_Next (Peers_In_Group);
                              List_Index := List_Index + 1;
                           end;
                        end loop;

                        declare
                           My_Name_Cptr : constant String_Cptr :=
                             new String'(My_Name);
                           Peer_Index : Node_Index_Type := 1;
                        begin
                           --  Add current node to map
                           GC.Node_Name_Map.Include
                             (Key => My_Name_Cptr,
                              New_Item =>  --  updated below
                                Node_Index_Type (Num_In_Group));

                           --  and to Node-to-UUID map
                           GC.Node_Name_To_UUID_Map.Include
                             (Key => My_Name_Cptr,
                              New_Item => new String'(Zyre_UUID (GC.Node)));

                           --  Now fill in the node index parts,
                           --  by iterating in lexical order of names.
                           for Curs in GC.Node_Name_Map.Iterate loop
                              --  Use the saved index into Core_Sizes
                              --  to fill in proper slot in Node_Sizes array.
                              GC.Node_Sizes (Peer_Index) :=
                                Core_Sizes
                                  (GC.Node_Name_Map (Curs));  --  list index
                              GC.Node_Names (Peer_Index) :=
                                String_To_Node_Index_Maps.Key (Curs);
                              GC.Node_UUIDs (Peer_Index) :=
                                GC.Node_Name_To_UUID_Map.Element
                                  (GC.Node_Names (Peer_Index));
                              --  Now fill in the correct index.
                              GC.Node_Name_Map.Replace_Element
                                (Curs, Peer_Index);
                              Peer_Index := Peer_Index + 1;
                           end loop;

                           --  Make sure we ended up with same index
                           --  chosen earlier.
                           pragma Assert
                             (GC.Node_Names (My_Index).all = My_Name);
                           pragma Assert
                             (GC.Node_Sizes (My_Index) = Num_Cores);
                        end;

                     end if;

                     String_Zlists.Zlist_Destroy (Peers_In_Group);
                  end;
               end if;
            end;
         end loop;
         GC.Total := Total_Num_Cores;
         GC.Offset := My_Offset;
         GC.Index := My_Index;
         if GC.Node_Sizes = null then
            GC.Node_Sizes := new Size_Array'(My_Index => GC.Size);
         end if;
         GC.Num_Nodes := Node_Index_Type (Num_In_Group);

      exception
         when E : others =>
            Put_Line (My_Index'Image &
               ": exception in Join_Group rendezvous: " &
               Ada.Exceptions.Exception_Information (E));
            Zsock_Destroy (GC.Share_Socket);
            Zsock_Destroy (GC.Share_Forward_Socket);
      end Join_Group;

      if Zpoller_Add (Receive_Poller, Zsock_Addr (Zyre_Socket (GC.Node))) /= 0
        or else
         Zpoller_Add
           (Receive_Poller, Zsock_Addr (GC.Share_Forward_Socket)) /= 0
      then
         raise ZMQ_Error;
      end if;

      declare
         Which_Socket : Zmsgs.Socket;
         Node_Is_Done : array (Node_Index_Type range 1 .. GC.Num_Nodes)
           of Boolean := (others => False);
      begin
         while (for some Is_Done of Node_Is_Done => not Is_Done) loop
            --  Pick up next item to either shout out, or handle
            Which_Socket := Zpoller_Wait (Receive_Poller, -1);

            exit when Which_Socket in System.Null_Address;
               --  Sockets seem to have been closed

            declare
               Msg : Zmsgs.Zmsg := Zmsgs.Zmsg_Recv (Which_Socket);
               Command : constant String := Zmsgs.Zmsg_Popstr (Msg);
            begin
               if Debug_Distrib then
                  Put_Line (GC.Index'Image &
                    ": Zyre comm task got command " & Command);
               end if;
               --  Check for a shutdown
               if Command = "SHUTDOWN" then
                  declare
                     Control_Msg : Zmsgs.Zmsg := Zmsgs.Zmsg_New;
                  begin
                     --  Shout a control message indicating we are done
                     Node_Is_Done (GC.Node_Index) := True;
                     if Zmsgs.Zmsg_Addstr (Control_Msg, "CONTROL") /= 0 then
                        raise ZMQ_Error;
                     end if;
                     --  Send the node index rather than an obj-id
                     if Zmsgs.Zmsg_Addstr
                       (Control_Msg, GC.Node_Index'Image) /= 0
                     then
                        raise ZMQ_Error;
                     end if;
                     if Zyre_Shout (GC.Node, GC.Group_Name, Control_Msg) /= 0
                     then
                        raise ZMQ_Error;
                     end if;
                     --  Will exit loop when we receive a CONTROL message
                     --  from all other nodes.
                  end;

               elsif Command = "NEW_OBJ" then
                  --  A new distributed object was just created
                  --  Read Type-Id, Obj-Id, and Handle from message
                  --  and enter into Live_Objects map.
                  declare
                     use Zmsgs, Zframes;
                     Type_Id : aliased constant String := Zmsg_Popstr (Msg);
                     Obj_Id : constant Distrib_Object_Id_Type :=
                       Distrib_Object_Id_Type'Value (Zmsg_Popstr (Msg));
                     Data : Zframe := Zmsg_Pop (Msg);
                     Stream : aliased constant
                       Ada.Streams.Stream_Element_Array :=
                         Zframe_As_Stream (Data);
                     Reader : aliased Buffered_Streams.Buffered_Reader
                                        (Stream'Access);
                     Dis_Obj : constant not null Distrib_Object_Ptr :=
                       Distrib_Object_Ptr'Input (Reader'Access);
                     Saved : constant Saved_Msgs_Ptr :=
                       Find_Saved_Msgs ((Type_Id'Unchecked_Access, Obj_Id));
                  begin
                     Zframe_Destroy (Data);
                     --  Enter object into Live_Objects map
                     Live_Objects.Insert
                       (Key => (new String'(Type_Id), Obj_Id),
                        New_Item => Dis_Obj);

                     if Debug_Distrib then
                        Put_Line (GC.Index'Image &
                          ": New object added to Live_Objects map: " &
                          Type_Id & Obj_Id'Image);
                     end if;
                     --  Now pass the object any messages that arrived early
                     if Saved /= null then
                        loop
                           declare
                              Next_Frame : Zframe := Zmsg_Pop (Saved.all);
                           begin
                              exit when Zframe_Is_Null (Next_Frame);

                              Dis_Obj.Message_Queue.Add_Frame (Next_Frame);

                              if Debug_Distrib then
                                 Put_Line (GC.Index'Image &
                                   ": Saved message sent to obj: " &
                                   Type_Id & Obj_Id'Image);
                              end if;
                           exception
                              when E : others =>
                                 Put_Line ("Exception in message handler: " &
                                   Ada.Exceptions.Exception_Information (E));

                                 Zframe_Destroy (Next_Frame);
                           end;
                        end loop;

                        Zmsgs.Zmsg_Destroy (Saved.all);

                        --  We have sent all of the saved-up messages
                        --  TBD: Recover heap storage used by key first.
                        Saved_Msgs.Delete
                          ((Type_Id'Unchecked_Access, Obj_Id));
                     end if;
                  end;

               elsif Command = "SHARE" then
                  --  This is a message we are supposed to shout to our peers
                  if Zyre_Shout (GC.Node, GC.Group_Name, Msg) /= 0 then
                     raise ZMQ_Error;
                  end if;

               elsif Command = "SEND_TO_PEER" then
                  --  This is a message we are supposed to whisper to a peer
                  declare
                     Peer_Index : constant Node_Index_Type :=
                       Node_Index_Type'Value (Zmsgs.Zmsg_Popstr (Msg));
                  begin
                     if Zyre_Whisper
                       (GC.Node, GC.Node_UUIDs (Peer_Index).all, Msg) /= 0
                     then
                        raise ZMQ_Error;
                     end if;
                  end;
               elsif Command = "ENTER" then
                  --  This is a message indicating a new peer.
                  --  TBD:  For now, we just ignore it.
                  declare
                     use Zmsgs;
                     Node_UUID : constant String := Zmsg_Popstr (Msg);
                     Node_Name : constant String := Zmsg_Popstr (Msg);
                  begin
                     if Debug_Distrib then
                        Put_Line (GC.Index'Image & ": Received ENTER from " &
                          Node_Name);
                     end if;
                  end;

               elsif Command = "EXIT" or else Command = "LEAVE" then
                  --  Report the exit or leaving of a peer
                  declare
                     use Zmsgs;
                     Node_UUID : constant String := Zmsg_Popstr (Msg);
                     Node_Name : constant String := Zmsg_Popstr (Msg);
                     Node_Name_Ptr : constant String_Cptr :=
                       new String'(Node_Name);
                     Node_Index : constant Node_Index_Type'Base :=
                       (if GC.Node_Name_Map.Find (Node_Name_Ptr) in
                          String_To_Node_Index_Maps.No_Element
                        then 0 else GC.Node_Name_Map.Element (Node_Name_Ptr));
                  begin
                     --  TBD: if Debug_Distrib then ...
                     Put_Line (GC.Index'Image & ": Received " & Command &
                       " from" & Node_Index'Image & ":" & Node_Name);

                     if Node_Index /= 0 and then Command = "EXIT" then
                        --  Node exited before we received its
                        --  shutting-down message.
                        Node_Is_Done (Node_Index) := True;
                     end if;
                  end;

               elsif Command = "SHOUT" or else Command = "WHISPER" then
                  --  This is a SHOUT or WHISPER we received from some peer.
                  declare
                     use Zmsgs, Zframes;
                     Is_Shout : constant Boolean := Command = "SHOUT";

                     Node_UUID : constant String := Zmsg_Popstr (Msg);
                     Node_Name : constant String := Zmsg_Popstr (Msg);

                     Group_Name : constant String :=
                       (if Is_Shout then Zmsg_Popstr (Msg) else "");
                     pragma Assert (Group_Name in "" | GC.Group_Name);

                     Type_Id : aliased constant String := Zmsg_Popstr (Msg);
                     Obj_Id : constant Distrib_Object_Id_Type :=
                       Distrib_Object_Id_Type'Value (Zmsg_Popstr (Msg));
                     Dis_Obj : constant Distrib_Object_Ptr :=
                       Find_Live_Obj
                         (Key => (Type_Id'Unchecked_Access, Obj_Id));
                  begin
                     if Dis_Obj = null then
                        --  Check for a CONTROL message
                        if Type_Id = "CONTROL" then
                           --  We only use this for shutting down now.
                           Node_Is_Done (Node_Index_Type (Obj_Id)) := True;

                           --  Will exit loop if all nodes are done
                        elsif Obj_Id > 0 then
                           --  Save up the message since it seems to have
                           --  arrived before the object was created.
                           if Debug_Distrib then
                              Put_Line (GC.Index'Image &
                                ": Not found in Live_Objects: " &
                                Type_Id & Obj_Id'Image);
                           end if;

                           declare
                              Saved : Saved_Msgs_Ptr :=
                                Find_Saved_Msgs
                                  ((Type_Id'Unchecked_Access, Obj_Id));
                           begin
                              if Saved = null then
                                 --  This is the first such message;
                                 --  Create an empty Zmsg which will hold
                                 --  the data streams received early.
                                 Saved := new Zmsg'(Zmsg_New);
                                 Saved_Msgs.Include
                                   (Key => (new String'(Type_Id), Obj_Id),
                                    New_Item => Saved);
                              end if;

                              --  Now add the message to the list of
                              --  saved "early" messages for this object.
                              declare
                                 Frame_To_Save : Zframe := Zmsg_Pop (Msg);
                              begin
                                 if Debug_Distrib then
                                    Put_Line (GC.Node_Index'Image &
                                      ": Saving " & Command & " message for " &
                                      Type_Id & Obj_Id'Image);
                                 end if;
                                 if Zmsg_Append
                                      (Saved.all, Frame_To_Save) /= 0
                                 then
                                    raise ZMQ_Error;
                                 end if;
                              end;
                           end;
                        else
                           --  Doesn't look like an early message
                           --  since obj-id = 0
                           Put_Line (GC.Index'Image &
                             ": Unrecognized object: " &
                             Type_Id & Obj_Id'Image);
                        end if;
                     else
                        --  Call handler
                        declare
                           Data : Zframe := Zmsg_Pop (Msg);
                        begin
                           if Debug_Distrib then
                              Put_Line (GC.Index'Image &
                                ": Shout received: " &
                                Type_Id & Obj_Id'Image);
                           end if;
                           Dis_Obj.Message_Queue.Add_Frame (Data);
                        exception
                           when E : others =>
                              Put_Line ("Exception in message handler: " &
                                Ada.Exceptions.Exception_Information (E));

                              Zframe_Destroy (Data);
                        end;
                     end if;
                  end;
               end if;
               Zmsgs.Zmsg_Destroy (Msg);
            exception
               when E : others =>
                  Put_Line ("Exception in Zyre_Comm task loop for command: " &
                    Command & ", " &
                    Ada.Exceptions.Exception_Information (E));

                  Zmsgs.Zmsg_Destroy (Msg);
            end;
         end loop;

         Clean_Up_Comm_Task;

      exception
         when E : others =>
            Put_Line (GC.Node_Index'Image &
              ": Exception in Zyre_Comm task while receiving: " &
              Ada.Exceptions.Exception_Information (E));
         Clean_Up_Comm_Task;
      end;

      --  Indicate task is shutting down
      accept Wait_For_Shutdown;

   exception
      when E : others =>
         Put_Line (GC.Node_Index'Image & ": Exception in Zyre_Comm task: " &
           Ada.Exceptions.Exception_Information (E));
   end Zyre_Comm_Task_Type;

end System_Distrib;
