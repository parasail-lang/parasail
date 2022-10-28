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

with System_Distrib;
private with Local_Vectors;
private with Distributed_Counts;
generic
   type Index_Type is range <>;
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;
   Distrib_Type_Id : String;
package Distributed_Vectors is
   --  A "distributed" vector is one that is broken up into "shards"
   --  which can reside on different nodes of a distributed "group" of nodes.
   --  By default, there is one shard on each node, where the size
   --  of the shard is intended to be approximately proportional to the
   --  "size" of the node (as determined by its number of "effective" cores).

   type Vector
     (GC : not null System_Distrib.Group_Context_Ptr;
      Distrib_Obj_Id : System_Distrib.Distrib_Object_Id_Type) is
      tagged limited private
      with Constant_Indexing => Element;

   subtype Extended_Index is
      Index_Type'Base range
         Index_Type'First - 1 ..
         Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   function Empty_Vector
     (GC : not null System_Distrib.Group_Context_Ptr;
      Distrib_Obj_Id : System_Distrib.Distrib_Object_Id_Type := 0;
      Capacity : Extended_Index := 0) return Vector;
   --  Create a new distributed vector with the given obj-id and
   --  with the local shard having the given capacity.
   --  Zero Distrib_Obj_Id means to assign the next sequential ID for
   --  the given instance of this package.
   --  Zero Capacity means to use some default capacity.

   function Copy_Vector
     (Existing_Vector : Vector;
      Distrib_Obj_Id : System_Distrib.Distrib_Object_Id_Type := 0)
     return Vector;
   --  Make a copy of Existing_Vector with the given obj-id.
   --  If Distrib_Obj_Id = 0, then assign the next sequential ID
   --  for this type.

   procedure Set_Empty (Vec : in out Vector);
   --  Set vector back to the empty state

   procedure Append (Container : in out Vector;
                     New_Item  :        Element_Type);
   --  Append onto local shard of given vector

   procedure Append_Slice (Onto : in out Vector;
                           From : Vector;
                           First : Index_Type;
                           Last : Extended_Index;
                           Onto_Shard : System_Distrib.Shard_Index_Type := 0;
                           Time_Stamp : System_Distrib.Time_Stamp_Type := 0);
   --  Append a slice of the local shard of vector "From" onto
   --  shard with index "Onto_Shard" of distributed vector "Onto".
   --  If "Onto_Shard" is zero this appends on the local shard.
   --  If "Onto_Shard" is non-zero, it appends on the given shard
   --  with the given timestamp (which should not be zero in this case).

   procedure Await_Incoming_Updates
     (Onto : in out Vector;
      Time_Stamp : System_Distrib.Time_Stamp_Type := 0);
   --  Wait for updates to be received from all peers with given
   --  Time_Stamp (or if zero, with current node's time stamp + 1).
   --  Set current node's time stamp to awaited time stamp value.

   function First_Index (Container : Vector) return Index_Type;
   --  First index of local shard

   function Last_Index (Container : Vector) return Extended_Index;
   --  Last index of local shard

   function Length
     (Container : Vector;
      Time_Stamp : System_Distrib.Time_Stamp_Type := 0)
     return Extended_Index;
   --  Length of local shard if Time_Stamp = 0; otherwise
   --  overall length of vector as of the given time stamp.

   function Element (Container : Vector;
                     Index     : Index_Type)
      return Element_Type;
   --  Return element of local shard

   procedure For_Each_Shard
     (Container : Vector;
      Process_One_Shard : not null access procedure
        (Shard : System_Distrib.Shard_Index_Type);
      Sequential : Boolean := False);
   --  Process each shard of vector.
   --  If Sequential = True, then call Process_One_Shard in sequence
   --  for each shard.  If Sequential = False, then process all of the
   --  shards in parallel.
   --  Return when the shards on the current node are done.

   procedure Iterate (Container : Vector;
                      Num_Chunks_Per_Shard : Natural := 1;
                      Process : not null access procedure
                        (Element : Element_Type));
   --  Iterate over vector.
   --  If Num_Chunks_Per_Shard = 1 then this is a sequential
   --  iteration over the entire distributed vector, but with
   --  an initial wait for the "left" neighboring shard (if any)
   --  and a final signaling of the "right" neighboring shard (if any).

   procedure Var_Iterate (Container : in out Vector;
                          Num_Chunks_Per_Shard : Natural := 1;
                          Process : not null access procedure
                            (Element : in out Element_Type));
   --  Iterate over vector.
   --  If Num_Chunks_Per_Shard = 1 then this is a sequential
   --  iteration over the entire distributed vector, but with
   --  an initial wait for the "left" neighboring shard (if any)
   --  and a final signaling of the "right" neighboring shard (if any).

   type Local_Vector is tagged private
      ;  --  tbd: with Constant_Indexing => Nth_Element;

   function Nth_Element (Loc_Vec : Local_Vector; Index : Index_Type)
     return Element_Type;

   function Length (Loc_Vec : Local_Vector) return Extended_Index;

   procedure Set_Nth_Element
     (Loc_Vec : in out Local_Vector; Index : Index_Type; Val : Element_Type);

   procedure Append (To : in out Local_Vector; Val : Element_Type);
   --  Add a new element to local vector; no locking performed

   procedure Assign_Slice
     (To : in out Local_Vector; To_Index : Index_Type;
      From : Local_Vector; From_Index : Index_Type;
      Count : Extended_Index);
   --  Assign slice of length Count of vector From into vector To at given
   --  indices.

   procedure Process_Shard_As_Local_Vector
     (Container : in out Vector;
      Process : not null access procedure (Vec : in out Local_Vector);
      Shard : System_Distrib.Shard_Index_Type := 0);
   --  Process shard as a "local" vector
   --  Shard = 0 implies local shard.

   procedure Append_Local_Vector (Onto : in out Vector;
                                  From : Local_Vector'Class);
   --  Append local vector onto current shard of distributed vector.

private

   package Seq_Vecs is
     new Local_Vectors (Element_Type);

   type Local_Vector is new Seq_Vecs.Vector with null record;

   --  Provide overloadings that use Index_Type rather than Elem_Index
   --  for indexing.
   procedure Reserve_Capacity
     (Loc_Vec : in out Local_Vector;
      Capacity : Extended_Index) is null;
   --  This is a no-op for the current version of "local" vectors.

   function Nth_Reference (Loc_Vec : in out Local_Vector; Index : Index_Type)
     return Seq_Vecs.Ref_To_Element
     is (Nth_Reference (Loc_Vec, Seq_Vecs.Elem_Index (Index)));
   --  Return a writable reference to the Nth element.

   function Nth_Element (Loc_Vec : Local_Vector; Index : Index_Type)
     return Element_Type
       is (Nth_Element (Loc_Vec, Seq_Vecs.Elem_Index (Index)));

   function Length (Loc_Vec : Local_Vector) return Extended_Index
     is (Seq_Vecs.Elem_Index'Pos (Num_Elements (Loc_Vec)));

   procedure Append (To : in out Local_Vector; Val : Element_Type)
     renames Add_Element;

   type Vec_Info;
   type Vec_Info_Ptr is access all Vec_Info;

   use System_Distrib;

   function Init_Info (Self : not null access Vector)
     return not null Vec_Info_Ptr;

   type Vector (GC : not null Group_Context_Ptr;
                Distrib_Obj_Id : Distrib_Object_Id_Type) is
     tagged limited record
      Loc_Vec : Local_Vector;
      Info : not null Vec_Info_Ptr := Init_Info (Vector'Unchecked_Access);
   end record;

end Distributed_Vectors;
