------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
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

with System.Storage_Elements;
with Ada.Unchecked_Deallocation;
package body PSC.Vectors is

   ------- Vector implementation -------

   Default_Elem : Element_Type;
   pragma Unmodified (Default_Elem);  --  Used for its default init only.

   subtype Vec_Capacity is Elem_Index'Base range
     Elem_Index'First .. Elem_Index'Base'Last;

   subtype Vec_Index is Vec_Capacity range 1 .. Vec_Capacity'Last;

   type Element_Array is array (Vec_Capacity range <>) of Element_Type;

   type Vector_Array is array (Vec_Capacity range <>) of Vector_Ptr;
   --  For large Vectors, we break them up into sub-Vecs

   Max_Vector_Size : constant := 250 *
                                 System.Storage_Elements.Integer_Address'Size;
   --  Max of 250 pointers per Vec

   Initial_Len : constant Vec_Capacity := 10;

   Sub_Vec_Capacity : constant Vec_Capacity :=
     Max_Vector_Size / Element_Array'Component_Size;
   --  Max length before we go to using Sub_Vecs

   Vec_Of_Vec_Capacity : constant Vec_Capacity :=
     Max_Vector_Size / Vector_Array'Component_Size;
   --  Max number of Sub_Vecs before we go to second, third, ...
   --  level of indirection.

   subtype Level_Range is Natural range 0 .. 3;
   --  Level indicates number of number of levels of indirection.
   --  This allows Vecs of up to 250**3 * Sub_Vec_Capacity Elements.
   --  Should be enough!

   --  Capacity of each subvector based on level in Vector tree
   Level_1_Capacity : constant Vec_Capacity := Sub_Vec_Capacity;
   Level_2_Capacity : constant Vec_Capacity :=
     Level_1_Capacity * Vec_Capacity'Min
             (Vec_Of_Vec_Capacity, Vec_Capacity'Last / Level_1_Capacity);
   Level_3_Capacity : constant Vec_Capacity :=
     Level_2_Capacity * Vec_Capacity'Min
             (Vec_Of_Vec_Capacity, Vec_Capacity'Last / Level_2_Capacity);

   --  Array of level subvector capacities
   Level_Capacities : constant array (1 .. Level_Range'Last) of Vec_Capacity :=
     (1 => Level_1_Capacity, 2 => Level_2_Capacity, 3 => Level_3_Capacity);

   --  Array of level lengths
   Level_Lengths : constant array (1 .. Level_Range'Last) of Vec_Capacity :=
     (1 => Level_2_Capacity / Level_1_Capacity,
      2 => Level_3_Capacity / Level_2_Capacity,
      3 => Vec_Capacity'Min
             (Vec_Of_Vec_Capacity, Vec_Capacity'Last / Level_3_Capacity));

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
              ((Index + (Level_1_Capacity - 1)) / Level_1_Capacity).Elements
                ((Index - 1) rem Level_1_Capacity + 1);
         when 2 =>
            --  Recurse with appropriate sub-Vec
            return Nth_Element
                     (Vec =>
                        Vec.Sub_Vecs ((Index + (Level_2_Capacity - 1)) /
                                      Level_2_Capacity).all,
                      Index => (Index - 1) rem Level_2_Capacity + 1);
         when 3 =>
            --  Recurse with appropriate sub-Vec
            return Nth_Element
                     (Vec =>
                        Vec.Sub_Vecs ((Index + (Level_3_Capacity - 1)) /
                                      Level_3_Capacity).all,
                      Index => (Index - 1) rem Level_3_Capacity + 1);
      end case;
   end Nth_Element;

   procedure Set_Nth_Element
     (Vec : in out Vector_Rec;
      Index : Elem_Index;
      To : Element_Type) is
   --  Recursive routine to set Nth element in Vec
   begin
      if Vec.Level = 0 then
         Vec.Elements (Index) := To;
      else
         declare
            Total_Sub_Vec_Capacity : constant Vec_Capacity :=
              Level_Capacities (Vec.Level);
         begin
            --  Recurse with appropriate sub-Vec
            Set_Nth_Element
              (Vec =>
                 Vec.Sub_Vecs ((Index + Total_Sub_Vec_Capacity - 1) /
                               Total_Sub_Vec_Capacity).all,
               Index => (Index - 1) rem Total_Sub_Vec_Capacity + 1,
               To => To);
         end;
      end if;
   end Set_Nth_Element;

   procedure Expand_Vec (Vec : in out Vector_Ptr) is
   --  Grow Vec to accommodate at least one more element
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
                              (Vec.Len + 1 .. New_Vec_Len => Default_Elem));
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
        and then Vec.Total_Capacity = Level_Capacities (Vec.Level + 1)
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
           Vec.Total_Capacity / Level_Capacities (Vec.Level) + 1;
      begin
         if Vec.Level = 1 then
            --  Create new level 0 subVec
            Vec.Sub_Vecs (New_Sub_Vec_Index) :=
              new Vector_Rec (Level => 0, Len => Sub_Vec_Capacity);
         else
            if Vec.Total_Capacity mod Level_Capacities (Vec.Level) = 0 then
               --  Create new intermediate level Vec-of-Vecs
               Vec.Sub_Vecs (New_Sub_Vec_Index) :=
                 new Vector_Rec
                 (Level => Vec.Level - 1,
                  Len => Level_Lengths (Vec.Level - 1));
               --  Default initial "Total_Capacity" is zero
            end if;

            --  Recurse to expand last sub vec
            Expand_Vec (Vec.Sub_Vecs (New_Sub_Vec_Index));
         end if;
         --  Bump up capacity at each level (on the way out of any recursion)
         Vec.Total_Capacity := Vec.Total_Capacity + Sub_Vec_Capacity;
      end;
   end Expand_Vec;

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
         Expand_Vec (Vec.Data);
      end if;
      Vec.Count := Vec.Count + 1;

      --  Return the Index
      Index := Vec.Count;

      --  Call recursive routine to actually set Nth element
      Set_Nth_Element (Vec.Data.all, Index, To => Elem);
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

   procedure Set_Empty
     (Vec : in out Vector) is
   --  Set the vector back to the initial, empty state.
   --  Release any heap storage in use.

      procedure Set_Empty_Ptr (Ptr : in out Vector_Ptr) is
      --  Recursive routine to set the entire vector "tree" to empty
      begin
         if Ptr = null then
            --  Nothing to do
            return;
         elsif Ptr.Level > 0 then
            --  Recurse on each of the sub-vectors.
            for I in 1 .. Ptr.Len loop
               Set_Empty_Ptr (Ptr.Sub_Vecs (I));
            end loop;
         end if;
         --  Now free this level
         Free (Ptr);
      end Set_Empty_Ptr;

   begin  --  Set_Empty

      --  Free the entire tree
      Set_Empty_Ptr (Vec.Data);

      --  Set the count back to zero.
      Vec.Count := 0;
   end Set_Empty;

end PSC.Vectors;
