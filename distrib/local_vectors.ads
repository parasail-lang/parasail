------------------------------------------------------------------------------
--                              P A R A S A I L                             --
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
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with     --
-- the ParaSail, Sparkel, Javallel, or Parython compiler, including any     --
-- required library run-time units written in Ada or in any of the above    --
-- languages, using any licensing terms  of your choosing.                  --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

private with Ada.Finalization;
generic
   type Element_Type is private;
package Local_Vectors is

   --  Extensible Vector of elements

   Max_Elem_Index : constant := 2_000_000_000;
   --  Max number of elements in a vector

   type Elem_Index is range 0 .. Max_Elem_Index;

   No_Elem_Index : constant Elem_Index := 0;

   type Vector is tagged private
     with Constant_Indexing => Nth_Element,
          Variable_Indexing => Nth_Reference;
   --  Vector of elements

   function Num_Elements (Vec : Vector) return Elem_Index;
   --  Return number of Elements in Vec

   procedure Add_Element
     (Vec : in out Vector;
      Elem : Element_Type);
   --  Add Element to Vector

   procedure Add_Element
     (Vec : in out Vector;
      Elem : Element_Type;
      Index : out Elem_Index);
   --  Add Element to Vector and return its Index

   function Nth_Element
     (Vec : Vector;
      Index : Elem_Index)
      return Element_Type;
   --  Retrieve Element at given index.

   procedure Set_Nth_Element
     (Vec : in out Vector;
      Index : Elem_Index;
      Elem : Element_Type);
   --  Overwrite nth element.

   type Ref_To_Element (Item : access Element_Type) is null record
     with Implicit_Dereference => Item;
   --  Implicitly dereferencable reference

   function Nth_Reference (Vec : in out Vector; Index : Elem_Index)
     return Ref_To_Element;
   --  Return a writable reference to the Nth element.

   procedure Assign_Slice
     (To : in out Vector; To_Index : Elem_Index;
      From : Vector; From_Index : Elem_Index;
      Count : Elem_Index);
   --  Assign slice of length Count of Vector From into Vector To at given
   --  indices.

   procedure Set_Empty (Vec : in out Vector);
   --  Set vector back to empty and recover any allocated storage.

   function Create (Num_Elements : Elem_Index; Value : Element_Type)
     return Vector;
   --  Create a vector of the the given length initialized
   --  with the given value.

   function Empty_Vector (Capacity : Elem_Index := 0) return Vector;
   --  Create an empty vector; if Capacity > 0 then
   --  pre-reserve room for the specified number of elements,
   --  if possible.

private

   type Vector_Rec;  --  completed in package body
   type Vector_Ptr is access Vector_Rec;

   type Vector is new Ada.Finalization.Controlled with record
      Count : Elem_Index := 0;  --  Count of Elements in list
      Data : Vector_Ptr;     --  actual list of Vectors
   end record;

   procedure Finalize (Vec : in out Vector) renames Set_Empty;

   procedure Adjust (Vec : in out Vector);

   function "=" (Left, Right : Vector) return Boolean;

end Local_Vectors;
