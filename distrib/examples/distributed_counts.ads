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

with System_Distrib;
generic
   type Count_Type is (<>);
   Distrib_Type_Id : String;
package Distributed_Counts is
   type Count (GC : not null System_Distrib.Group_Context_Ptr;
               Distrib_Obj_Id : System_Distrib.Distrib_Object_Id_Type) is
     tagged limited private;

   procedure Add (C : in out Count; Amount : Count_Type);
   procedure Subtract (C : in out Count; Amount : Count_Type);

   function Value
     (C : Count; Share_First : Boolean := False)
     return Count_Type;
   --  Return sum of all counts on all nodes
   --  If Share_First, or if have never shared value, do so now.
   --  If Share_First is False, should not have updated the
   --  value since a prior call on Value.

   function Value
     (C : Count; Peer_Node_Index : System_Distrib.Node_Index_Type;
      Share_First : Boolean := False)
     return Count_Type;
   --  Return value of count for a given node
   --  If Share_First, or if have never shared value, do so now.
   --  If Share_First is False, should not have updated the
   --  value since a prior call on Value, unless fetching own value.

private
   type Atomic_Integer is new Integer with Atomic;

   type Count_Info;
   type Count_Info_Ptr is access all Count_Info;

   use System_Distrib;

   function Init_Info (Self : not null access Count)
     return not null Count_Info_Ptr;

   type Count (GC : not null Group_Context_Ptr;
               Distrib_Obj_Id : Distrib_Object_Id_Type) is
     tagged limited record
      Val : aliased Atomic_Integer := 0;
      Info : not null Count_Info_Ptr := Init_Info (Count'Unchecked_Access);
   end record;
end Distributed_Counts;
