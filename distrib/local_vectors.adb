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

with Ada.Text_IO;
with System.Storage_Elements;
with Ada.Unchecked_Deallocation;
package body Local_Vectors is

   ------- Vector implementation -------

   Debug : constant Boolean := False;

   subtype Vec_Capacity is Elem_Index'Base range
     Elem_Index'First .. Elem_Index'Base'Last;

   subtype Vec_Index is Vec_Capacity range 1 .. Vec_Capacity'Last;

   type Element_Array is array (Vec_Capacity range <>) of aliased Element_Type;

   type Vector_Array is array (Vec_Capacity range <>) of Vector_Ptr;
   --  For large Vectors, we break them up into sub-Vecs

   Max_Vector_Size : constant := 250 *
                                 System.Storage_Elements.Integer_Address'Size;
   --  Max of 250 pointers per Vec

   Initial_Len : constant Vec_Capacity := 10;

   Sub_Vec_Capacity : constant Vec_Capacity :=
     Max_Vector_Size / Element_Array'Component_Size;
   --  Max length before we go to using Sub_Vecs

   Vec_Of_Vec_Count : constant Vec_Capacity :=
     Max_Vector_Size / Vector_Array'Component_Size;
   --  Max number of Sub_Vecs before we go to second, third, ...
   --  level of indirection.

   subtype Level_Range is Natural range 0 .. 3;
   --  Level indicates number of number of levels of indirection.
   --  This allows Vecs of up to 250**3 * Sub_Vec_Capacity Elements.
   --  Should be enough!

   --  Capacity of subvectors based on level in Vector tree
   Level_1_Subvecs : constant Vec_Capacity := Sub_Vec_Capacity;
   Level_2_Subvecs : constant Vec_Capacity :=
     Level_1_Subvecs * Vec_Capacity'Min
        (Vec_Of_Vec_Count, Vec_Capacity'Last / Level_1_Subvecs);
   Level_3_Subvecs : constant Vec_Capacity :=
     Level_2_Subvecs * Vec_Capacity'Min
        (Vec_Of_Vec_Count, Vec_Capacity'Last / Level_2_Subvecs);

   --  Array of capacities of a subvector at each level
   Subvec_Capacities : constant array (1 .. Level_Range'Last)
     of Vec_Capacity :=
     (1 => Level_1_Subvecs, 2 => Level_2_Subvecs, 3 => Level_3_Subvecs);

   Level_Lengths : constant array (1 .. Level_Range'Last) of Vec_Capacity :=
     (1 => Level_2_Subvecs / Level_1_Subvecs,
      2 => Level_3_Subvecs / Level_2_Subvecs,
      3 => Vec_Capacity'Min
             (Vec_Of_Vec_Count, Vec_Capacity'Last / Level_3_Subvecs));

   type Vector_Rec (Level : Level_Range; Len : Vec_Capacity) is
   --
   --  The single discriminant "Len" is being used
   --  in two different ways:
   --   If Level = 0, as the capacity of the Vec.
   --   If Level > 0, as the number of Sub_Vecs.
   record
      case Level is
         when 0 =>
            Elements : Element_Array (1 .. Len);
         when others =>
            Total_Capacity : Vec_Capacity := 0;
            Sub_Vecs : Vector_Array (1 .. Len);
      end case;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Vector_Rec, Vector_Ptr);

   ------- Local Subprograms ----------

   function Capacity (Vec : Vector_Ptr) return Vec_Capacity is
   --  Return overall capacity of Vec, as opposed to number of current
   --  Vectors.
   begin
      if Vec = null then
         return 0;
      elsif Vec.Level = 0 then
         return Vec.Len;
      else
         return Vec.Total_Capacity;
      end if;
   end Capacity;

   function Nth_Element
     (Vec : Vector_Rec;
      Index : Elem_Index)
      return Element_Type
   is
   --  Recursive routine to return Nth element in Vec
   begin
      --  NOTE: We break this down by case to micro-optimize this
      --        very time-critical function.
      case Vec.Level is
         when 0 =>
            --  Return appropriate element
            return Vec.Elements (Index);
         when 1 =>
            --  Select from appropriate sub-sub vec
            return Vec.Sub_Vecs
              ((Index + (Level_1_Subvecs - 1)) / Level_1_Subvecs).Elements
                ((Index - 1) rem Level_1_Subvecs + 1);
         when 2 =>
            --  Recurse with appropriate sub-Vec
            return Nth_Element
                     (Vec =>
                        Vec.Sub_Vecs ((Index + (Level_2_Subvecs - 1)) /
                                      Level_2_Subvecs).all,
                      Index => (Index - 1) rem Level_2_Subvecs + 1);
         when 3 =>
            --  Recurse with appropriate sub-Vec
            return Nth_Element
                     (Vec =>
                        Vec.Sub_Vecs ((Index + (Level_3_Subvecs - 1)) /
                                      Level_3_Subvecs).all,
                      Index => (Index - 1) rem Level_3_Subvecs + 1);
      end case;
   end Nth_Element;

   procedure Set_Nth_Element
     (Vec : in out Vector_Rec;
      Index : Elem_Index;
      To : Element_Type) is
   --  Recursive routine to set Nth element in Vec
   begin
      case Vec.Level is
         when 0 =>
            --  Set appropriate element
            Vec.Elements (Index) := To;
         when 1 =>
            --  Assign to appropriate sub-sub vec
            Vec.Sub_Vecs
              ((Index + (Level_1_Subvecs - 1)) / Level_1_Subvecs).Elements
                ((Index - 1) rem Level_1_Subvecs + 1) := To;
         when 2 =>
            --  Recurse with appropriate sub-Vec
            Set_Nth_Element
                     (Vec =>
                        Vec.Sub_Vecs ((Index + (Level_2_Subvecs - 1)) /
                                      Level_2_Subvecs).all,
                      Index => (Index - 1) rem Level_2_Subvecs + 1,
                      To => To);
         when 3 =>
            --  Recurse with appropriate sub-Vec
            Set_Nth_Element
                     (Vec =>
                        Vec.Sub_Vecs ((Index + (Level_3_Subvecs - 1)) /
                                      Level_3_Subvecs).all,
                      Index => (Index - 1) rem Level_3_Subvecs + 1,
                      To => To);
      end case;
   end Set_Nth_Element;

   function Nth_Reference
     (Vec : not null access Vector_Rec; Index : Elem_Index)
     return Ref_To_Element is
   --  Return a writable reference to the Nth element.
   begin
      case Vec.Level is
         when 0 =>
            --  Return appropriate element
            return (Item => Vec.Elements (Index)'Unrestricted_Access);
                  --  TBD: 'Access should work; GNAT bug in version on Mac
         when 1 =>
            --  Select from appropriate sub-sub vec
            return (Item => Vec.Sub_Vecs
              ((Index + (Level_1_Subvecs - 1)) / Level_1_Subvecs).Elements
                ((Index - 1) rem Level_1_Subvecs + 1)'Unrestricted_Access);
                  --  TBD: 'Access should work; GNAT bug in version on Mac
         when 2 =>
            --  Recurse with appropriate sub-Vec
            return Nth_Reference
                     (Vec =>
                        Vec.Sub_Vecs ((Index + (Level_2_Subvecs - 1)) /
                                      Level_2_Subvecs),
                      Index => (Index - 1) rem Level_2_Subvecs + 1);
         when 3 =>
            --  Recurse with appropriate sub-Vec
            return Nth_Reference
                     (Vec =>
                        Vec.Sub_Vecs ((Index + (Level_3_Subvecs - 1)) /
                                      Level_3_Subvecs),
                      Index => (Index - 1) rem Level_3_Subvecs + 1);
      end case;
   end Nth_Reference;

   procedure Expand_Vec (Vec : in out Vector_Ptr;
                         Recursion : Natural := 0) is
   --  Grow Vec to accommodate at least one more element
      pragma Assert (Recursion <= 5);
   begin
      if Vec = null then
         Vec := new Vector_Rec (Level => 0, Len => Initial_Len);
         return;   --  All done
      elsif Vec.Level = 0 then
         --  We have a simple one-level Vec, extend it if possible
         if Vec.Len < Sub_Vec_Capacity then
            --  Extend this sub-Vec
            declare
               New_Vec_Len : constant Vec_Capacity :=
                 Vec_Capacity'Min (2 * Vec.Len, Sub_Vec_Capacity);
               New_Vec : constant Vector_Ptr :=
                 new Vector_Rec'
                 (Level => 0,
                  Len => New_Vec_Len,
                  Elements => Vec.Elements &
                              (Vec.Len + 1 .. New_Vec_Len => <>));
            begin
               --  Reclaim old Vec storage and set to point to new Vec
               Free (Vec);
               Vec := New_Vec;
               return;   --  All done
            end;
         end if;

         --  This subVec is full, need to create a multi-level structure
         declare
            New_Multilevel_Vec : constant Vector_Ptr :=
              new Vector_Rec (Level => 1, Len => Level_Lengths (1));
         --  NOTE: We start with full length Vec-of-Vec since
         --       we know this is already a pretty big Vec
         begin
            --  Put existing Vec as first element of new Vec
            New_Multilevel_Vec.Sub_Vecs (1) := Vec;
            New_Multilevel_Vec.Total_Capacity := Sub_Vec_Capacity;
            Vec := New_Multilevel_Vec;
            --  Fall through to create a new sub-Vec
         end;

      elsif Vec.Level < Level_Range'Last
        and then Vec.Total_Capacity = Subvec_Capacities (Vec.Level + 1)
      then
         --  We already have a multi-level structure, but it is full
         --  so need to add a level.
         declare
            New_Multilevel_Vec : constant Vector_Ptr :=
              new Vector_Rec
              (Level => Vec.Level + 1,
               Len => Level_Lengths (Vec.Level + 1));
         --  NOTE: We start with full length Vec-of-Vec since
         --       we know this is already a pretty big Vec
         begin
            --  Put existing Vec as first element of new Vec
            New_Multilevel_Vec.Sub_Vecs (1) := Vec;
            New_Multilevel_Vec.Total_Capacity := Vec.Total_Capacity;
            Vec := New_Multilevel_Vec;
            --  Fall through to create a new sub-Vec
         end;
      end if;

      --  We now have a multi-level structure that needs expanding
      pragma Assert (Vec.Level > 0);

      declare
         New_Sub_Vec_Index : constant Vec_Index :=
           Vec.Total_Capacity / Subvec_Capacities (Vec.Level) + 1;
      begin
         if Vec.Level = 1 then
            --  Create new level 0 subVec
            Vec.Sub_Vecs (New_Sub_Vec_Index) :=
              new Vector_Rec (Level => 0, Len => Sub_Vec_Capacity);
         else
            if Vec.Total_Capacity mod Subvec_Capacities (Vec.Level) = 0 then
               --  Create new intermediate level Vec-of-Vecs
               Vec.Sub_Vecs (New_Sub_Vec_Index) :=
                 new Vector_Rec
                 (Level => Vec.Level - 1,
                  Len => Level_Lengths (Vec.Level - 1));
               --  Default initial "Total_Capacity" is zero
            end if;

            if False and then Debug
              and then (Recursion > 0 or else Vec.Level > 2)
            then
               Ada.Text_IO.Put_Line (" Recursing when already at level:" &
                 Recursion'Image);
               Ada.Text_IO.Put_Line (" Vec.Level =" & Vec.Level'Image);
               Ada.Text_IO.Put_Line (" Vec.Total_Capacity =" &
                                     Vec.Total_Capacity'Image);
               Ada.Text_IO.Put_Line (" New_Sub_Vec_Index =" &
                                     New_Sub_Vec_Index'Image);
            end if;
            --  Recurse to expand last sub vec
            Expand_Vec (Vec.Sub_Vecs (New_Sub_Vec_Index),
                        Recursion => Recursion + 1);
         end if;
         --  Bump up capacity at each level (on the way out of any recursion)
         Vec.Total_Capacity := Vec.Total_Capacity + Sub_Vec_Capacity;
      end;
   end Expand_Vec;

   procedure Free_Vec_Data (Vec : in out Vector_Ptr) is
   --  Recursively free vector data
   begin
      if Vec /= null then
         if Vec.Level > 0 then
            --  Recurse with sub-vectors
            for I in Vec.Sub_Vecs'Range loop
               Free_Vec_Data (Vec.Sub_Vecs (I));
            end loop;
         end if;
         --  Now free this vector.
         Free (Vec);
      end if;
   end Free_Vec_Data;

   procedure Copy_Vec_Data (Vec : in out Vector_Ptr) is
   --  Recursively copy vector data
   begin
      if Vec /= null then
         Vec := new Vector_Rec'(Vec.all);
         if Vec.Level > 0 then
            for I in Vec.Sub_Vecs'Range loop
               Copy_Vec_Data (Vec.Sub_Vecs (I));
            end loop;
         end if;
      end if;
   end Copy_Vec_Data;

   function Equal_Vec_Data (Left, Right : Vector_Ptr) return Boolean is
   --  Recursively compare vector data
   begin
      if Left = null or else Right = null then
         return Left = Right;
      elsif Left.Level /= Right.Level
        or else Left.Len /= Right.Len
      then
         --  Should never get here since we already verified that overall
         --  Count was the same.
         return False;
      else
         if Left.Level = 0 then
            --  Check elements directly in level 0
            return Left.Elements = Right.Elements;
         else
            for I in Left.Sub_Vecs'Range loop
               if Left.Sub_Vecs (I) = null then
                  --  Reached end of subvectors
                  return Right.Sub_Vecs (I) = null;
               elsif not Equal_Vec_Data  --  Check recursively
                           (Left.Sub_Vecs (I), Right.Sub_Vecs (I))
               then
                  --  Recursive equality test failed
                  return False;
               end if;
            end loop;
            --  All subvectors are equal
            return True;
         end if;
      end if;
   end Equal_Vec_Data;

   ------- Visible Subprograms ----------

   function Num_Elements (Vec : Vector) return Elem_Index is
   --  Return number of Elements in Vec
   begin
      return Vec.Count;
   end Num_Elements;

   procedure Add_Element
     (Vec : in out Vector;
      Elem : Element_Type;
      Index : out Elem_Index) is
   --  Add element to Vec and return its index
   begin
      if Vec.Count >= Capacity (Vec.Data) then
         --  Need to expand Vec
         declare
            Old_Level : constant Level_Range'Base :=
              (if Vec.Data = null then -1 else Vec.Data.Level);
         begin
            Expand_Vec (Vec.Data);
            if Debug and then Vec.Data.Level > Old_Level then
               Ada.Text_IO.Put_Line (" * Total_Capacity =" &
                 Vec.Data.Total_Capacity'Image);
            end if;
         end;
      end if;
      Vec.Count := Vec.Count + 1;

      --  Return the Index
      Index := Vec.Count;

      --  Call recursive routine to actually set Nth element
      Set_Nth_Element (Vec.Data.all, Index, To => Elem);
   end Add_Element;

   procedure Add_Element
     (Vec : in out Vector;
      Elem : Element_Type) is
   --  Add Element to Vector
      Ignore : Elem_Index;
   begin
      Add_Element (Vec, Elem, Ignore);
   end Add_Element;

   function Nth_Element
     (Vec : Vector;
      Index : Elem_Index)
      return Element_Type
   is
      --  Retrieve element at given index.
      pragma Assert (Index <= Vec.Count);
   begin
      --  Call recursive routine
      return Nth_Element (Vec.Data.all, Index);
   end Nth_Element;

   procedure Set_Nth_Element
     (Vec : in out Vector;
      Index : Elem_Index;
      Elem : Element_Type) is
   --  Overwrite nth element.
   begin
      --  Call recursive routine
      Set_Nth_Element (Vec.Data.all, Index, To => Elem);
   end Set_Nth_Element;

   function Nth_Reference (Vec : in out Vector; Index : Elem_Index)
     return Ref_To_Element is
   begin
      --  Call recursive routine
      return Nth_Reference (Vec.Data, Index);
   end Nth_Reference;

   procedure Assign_Slice
     (To : in out Vector; To_Index : Elem_Index;
      From : Vector; From_Index : Elem_Index;
      Count : Elem_Index) is
   begin
      if Count > 0 then
         pragma Assert (To.Count >= To_Index + Count - 1);
         pragma Assert (From.Count >= From_Index + Count - 1);
         for I in 0 .. Count - 1 loop
            --  TBD: Use a more efficient mechanism at some point
            Set_Nth_Element (To.Data.all, To_Index + I,
              Nth_Element (From.Data.all, From_Index + I));
         end loop;
      end if;
   end Assign_Slice;

   procedure Set_Empty (Vec : in out Vector) is
   begin
      if Vec.Data = null then
         return;
      else
         --  Call recursive routine
         Free_Vec_Data (Vec.Data);
         pragma Assert (Vec.Data = null);
         Vec.Count := 0;
      end if;
   end Set_Empty;

   function Create (Num_Elements : Elem_Index; Value : Element_Type)
     return Vector is
   --  Create a vector of the the given length initialized
   --  with the given value.
      Ignore : Elem_Index;
   begin
      return Result : Vector do
         for I in 1 .. Num_Elements loop
            Add_Element (Result, Value, Ignore);
         end loop;
      end return;
   end Create;

   function Empty_Vector (Capacity : Elem_Index := 0) return Vector is
   --  Create an empty vector; if Capacity > 0 then
   --  pre-reserve room for the specified number of elements,
   --  if possible.
      pragma Unreferenced (Capacity);  --  Not used in this representation
   begin
      return (Ada.Finalization.Controlled with Count => 0, Data => null);
   end Empty_Vector;

   procedure Adjust (Vec : in out Vector) is
   begin
      --  Fixup Vec after assignment
      if Vec.Data /= null then
         --  Recursively copy vector data
         Copy_Vec_Data (Vec.Data);
      end if;
   end Adjust;

   function "=" (Left, Right : Vector) return Boolean is
   begin
      if Left.Count /= Right.Count then
         --  Count needs to match to be equal
         return False;
      elsif Left.Count = 0 then
         --  Both are empty
         return True;
      else
         --  Call recursive equality routine
         return Equal_Vec_Data (Left.Data, Right.Data);
      end if;
   end "=";

begin
   if Debug then
      for I in 1 .. Level_Range'Last loop
         Ada.Text_IO.Put_Line (I'Image & ": Length =" &
           Level_Lengths (I)'Image & ", Subvec_Capacity =" &
           Subvec_Capacities (I)'Image);
      end loop;
   end if;
end Local_Vectors;
