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

with Ada.Streams;
private with ZMQ.Zyre;
private with ZMQ.Zsocks;
private with ZMQ.Zmsgs;
private with ZMQ.Zframes;
private with Ada.Containers.Ordered_Maps;
package System_Distrib is
   --  A central switchboard to support distributed computing.

   type Group_Context is tagged limited private;
   type Group_Context_Ptr is access all Group_Context;

   function Join_Group
     (Group_Name : String; Num_Cores : Positive; Min_Num_Nodes : Positive)
     return Group_Context;
   --  Join group with given Group_Name, indicating Num_Cores as the
   --  number of (effective) cores that this node provides.
   --  Min_Num_Nodes is the minimum number of nodes in group before
   --  returning from Join_Group.

   procedure Finish (GC : in out Group_Context);
   --  Leave the given group.

   --  As measured in "effective" cores within group:
   function Node_Offset (GC : Group_Context) return Natural;
      --  Offset of this node's cores within all cores of group

   function Node_Size (GC : Group_Context) return Natural;
      --  Number of effective cores of current node.

   function Total_Size (GC : Group_Context) return Positive;
      --  Total number of effective cores of group.

   --  Indices of nodes within group:
   type Node_Index_Type is new Integer range 1 .. Integer'Last;

   function Node_Index (GC : Group_Context) return Node_Index_Type;
   --  Node index of current node.

   function Node_Size (GC : Group_Context; Node : Node_Index_Type)
     return Natural;
      --  Number of effective cores of specified node.

   function Num_Nodes (GC : Group_Context) return Node_Index_Type;
   --  Total number of nodes in the group.

   type String_Cptr is access constant String;

   function Group_Name_Ptr (GC : Group_Context) return String_Cptr;
   --  Pointer to name of current group.

   function Group_Name (GC : Group_Context'Class) return String;
   --  Name of current group

   ------------------------------------------------------

   type Distrib_Object_Id_Type is new Natural;
   --  A Distrib_Obj_Id of zero is used to request the assignment
   --  of the next sequential obj-id for the given distributed obj type.

   type Distrib_Object is abstract tagged limited private;
   --  Per-distributed-object info used for handling messages
   --  from peer nodes.

   type Distrib_Object_Ptr is access all Distrib_Object'Class;

   function Distrib_Type_Id_Ptr (Dis_Obj : Distrib_Object)
     return String_Cptr
     is abstract;
      --  Returns pointer to name of type of distributed object.

   function Distrib_Type_Id (Dis_Obj : Distrib_Object'Class) return String
     is (Distrib_Type_Id_Ptr (Dis_Obj).all);

   function Distrib_Obj_Id (Dis_Obj : Distrib_Object)
     return Distrib_Object_Id_Type
     is abstract;
      --  Returns index of object among all objects of given type.

   function Distrib_Obj_Group (Dis_Obj : Distrib_Object)
     return Group_Context_Ptr is abstract;
      --  Return reference to group context for given distributed object

   subtype Shard_Index_Type is Node_Index_Type'Base
     range 0 .. Node_Index_Type'Last;
      --  Distributed objects are broken up into "shards", with typically
      --  one shard per node.  By default, the shard index matches the
      --  node index of the node where the shard resides, though that could
      --  change over time, or if the shard is replicated on multiple nodes.
      --  A shard index of "0" means the "local" shard.

   subtype Chunk_Index_Type is Positive;
   subtype Chunk_Count_Type is Chunk_Index_Type'Base
     range 0 .. Chunk_Index_Type'Last;
      --  Local arrays are broken into chunks for parallel processing.
      --  A chunk count of zero implies no specified number of chunks.

   procedure Handle_Obj_Message
     (Dis_Obj : in out Distrib_Object;
      Msg : aliased Ada.Streams.Stream_Element_Array)
     is abstract;
      --  Handler for messages directed at this particular distributed object.

   procedure Handle_Queued_Messages
     (Dis_Obj : in out Distrib_Object'Class;
      Wait_For_Message : Boolean := False);
      --  This class-wide operation calls "Handle_Obj_Message"
      --  on all of the queued messages for the given object.
      --  If Wait_For_Message is True, this will block the caller
      --  if there are no messages.

   procedure Create_Distrib_Obj
     (GC : in out Group_Context;
      Dis_Obj : not null Distrib_Object_Ptr);
   --  Create a new distributed object, represented by the given
   --  access-to-distrib-object value.

   procedure Share_With_Peers
     (GC : in out Group_Context;
      Dis_Obj : Distrib_Object'Class;
      Stream : Ada.Streams.Stream_Element_Array);
   --  Share the info embedded in the stream with peers

   procedure Send_To_Peer
     (GC : in out Group_Context;
      Dis_Obj : Distrib_Object'Class;
      Peer : Node_Index_Type;
      Stream : Ada.Streams.Stream_Element_Array);
   --  Send the info embedded in the stream to the specified peer node.

   type Time_Stamp_Type is new Natural;
   --  Type used to represent a logical time stamp since the
   --  beginning of a given distributed object's life.

   type TS_Array is array (Node_Index_Type range <>) of Time_Stamp_Type;
   --  Array of times stamps, one per node.

private

   type Zyre_Comm_Task_Type;
   --  This task type receives messages and hands them out to the
   --  appropriate distributed object handlers.

   type Zyre_Comm_Task_Ptr is access Zyre_Comm_Task_Type;

   type String_Cptr_Array is array (Node_Index_Type range <>) of String_Cptr;
   type String_Cptr_Array_Ptr is access String_Cptr_Array;

   function "<"(Left, Right : String_Cptr) return Boolean
     is (Left.all < Right.all);

   package String_To_Node_Index_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type => String_Cptr,
        Element_Type => Node_Index_Type);

   subtype String_To_Node_Index_Map is String_To_Node_Index_Maps.Map;

   package String_To_String_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type => String_Cptr,
        Element_Type => String_Cptr);

   subtype String_To_String_Map is String_To_String_Maps.Map;

   type Size_Array is array (Node_Index_Type range <>) of Natural;
   type Size_Array_Ptr is access Size_Array;

   --  Descriptor of group
   type Group_Context is tagged limited record
      Node : ZMQ.Zyre.Zyre_Node;
      Offset : Natural := 0;  --  Within overall set of cores in group
      Size : Natural := 0;    --  Number of "effective" cores on this node
      Total : Positive := 1;  --  Total number of "effective" cores in group
      Index : Node_Index_Type := 1;  --  Index among nodes in group
      Num_Nodes : Node_Index_Type := 1;  --  Number of nodes in group
      Group_Name_Ptr : String_Cptr := null;
      Zyre_Comm_Task : Zyre_Comm_Task_Ptr := null;
      Share_Socket : ZMQ.Zsocks.Zsock;
      Share_Forward_Socket : ZMQ.Zsocks.Zsock;
      Node_Names : String_Cptr_Array_Ptr := null;  --  Index => Node Name
      Node_UUIDS : String_Cptr_Array_Ptr := null;  --  Index => UUID
      Node_Sizes : Size_Array_Ptr := null;         --  Index => Size
      Node_Name_Map : String_To_Node_Index_Map;    --  Node Name => Index
      Node_Name_To_UUID_Map : String_To_String_Map;  -- Node_Name => UUID
   end record;

   function Node_Offset (GC : Group_Context) return Natural is (GC.Offset);
   function Node_Size (GC : Group_Context) return Natural is (GC.Size);
   function Total_Size (GC : Group_Context) return Positive is (GC.Total);

   function Node_Index (GC : Group_Context) return Node_Index_Type
     is (GC.Index);
   function Num_Nodes (GC : Group_Context) return Node_Index_Type
     is (GC.Num_Nodes);

   function Node_Size (GC : Group_Context; Node : Node_Index_Type)
     return Natural is (GC.Node_Sizes (Node));

   function Group_Name_Ptr (GC : Group_Context) return String_Cptr
     is (GC.Group_Name_Ptr);

   function Group_Name (GC : Group_Context'Class) return String
     is (if GC.Group_Name_Ptr = null then "" else GC.Group_Name_Ptr.all);

   protected type Prot_Message_Queue_Type is
      procedure Add_Frame (Frame : in out ZMQ.Zframes.Zframe);
      entry Retrieve_Frame (Frame : in out ZMQ.Zframes.Zframe);
   private
      Frame_Queue : ZMQ.Zmsgs.Zmsg := ZMQ.Zmsgs.Zmsg_New;
      --  We use a Zmsg to hold a queue of Zframes, one per
      --  message directed at this object.
   end Prot_Message_Queue_Type;

   type Distrib_Object is abstract tagged limited record
      --  Per-distributed-object info used for handling messages
      --  from peer nodes.
      Message_Queue : Prot_Message_Queue_Type;
   end record;

end System_Distrib;
