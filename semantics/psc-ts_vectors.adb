------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

with System.Storage_Elements;
with Ada.Unchecked_Deallocation;

package body PSC.TS_Vectors is

   --  ----------------- Vector implementation -------------------------------

   pragma Warnings (Off);  -- Avoid warning on unitialized variable
   Default_Elem : Element_Type;
   pragma Warnings (On);

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
   --  This allows Vecs of up to 250**3 * Vec_Of_Vec_Capacity Elements.
   --  Should be enough!

   Level_Capacities : constant array (1 .. Level_Range'Last) of Vec_Capacity
      :=
     (1 => Sub_Vec_Capacity,
      2 => Vec_Capacity'Min
             (Vec_Capacity'Last,
              Vec_Of_Vec_Capacity * Sub_Vec_Capacity),
      3 => Vec_Capacity'Min
             (Vec_Capacity'Last,
              Vec_Of_Vec_Capacity ** 2 * Sub_Vec_Capacity));
   --  Divisor to use based on level in Vector tree

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

   protected type Locked_Vec_Ops is
      --  These operations provide locking as a side-effect of their invocation

      procedure Expand_Vec (Vec : in out Locked_Vector_Ptr);
      --  Grow Vec to accommodate at least one more element

      function Nth_Element
        (Vec : Locked_Vector_Ptr;
         Index : Elem_Index)
         return Element_Type;
      --  Recursive routine to return Nth element in Vec

      procedure Set_Nth_Element
        (Vec   : in out Locked_Vector_Ptr;
         Index : Elem_Index;
         To    : Element_Type);
      --  Recursive routine to set Nth element in Vec
   end Locked_Vec_Ops;

   type Locked_Vector_Rec is record
      Lock      : Locked_Vec_Ops;
      Vector    : Vector_Ptr;
   end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Vector_Rec, Vector_Ptr);

   --  -------------------- Local Subprograms --------------------------------

   procedure Expand_Vec (Vec : in out Vector_Ptr);
   --  Grow Vec to accommodate at least one more element

   procedure Expand_Vec (Vec : in out Locked_Vector_Ptr);
   --  Grow Vec to accommodate at least one more element; lack

   function Nth_Element
     (Vec : Vector_Rec;
      Index : Elem_Index)
      return Element_Type;
   --  Recursive routine to return Nth element in Vec

   procedure Set_Nth_Element
     (Vec   : in out Vector_Rec;
      Index : Elem_Index;
      To    : Element_Type);
   --  Recursive routine to set Nth element in Vec

   ----------------
   -- Expand_Vec --
   ----------------

   procedure Expand_Vec (Vec : in out Vector_Ptr) is
      --  Grow Vec to accommodate at least one more element
   begin
      pragma Assert (Vec /= null);

      if Vec.Level = 0 then
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
              new Vector_Rec (Level => 1, Len => Vec_Of_Vec_Capacity);
            --  NOTE: We start with full length Vec-of-Vec since
            --       we know this is already a pretty big Vec
         begin
            --  Put existing Vec as first element of new Vec
            New_Multilevel_Vec.Sub_Vecs (1) := Vec;
            New_Multilevel_Vec.Total_Capacity := Sub_Vec_Capacity;
            Vec := New_Multilevel_Vec;
            --  Fall through to create a new sub-Vec
         end;

      elsif Vec.Sub_Vecs (Vec.Sub_Vecs'Last) /= null then

         --  We already have a multi-level structure, but it is full
         --  need to add a level
         declare
            New_Multilevel_Vec : constant Vector_Ptr :=
              new Vector_Rec
                (Level => Vec.Level + 1,
                 Len => Vec_Of_Vec_Capacity);
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
                    Len => Vec_Of_Vec_Capacity);
               --  Default initial "Total_Capacity" is zero
            end if;

            --  Recurse to expand last sub vec
            Expand_Vec (Vec.Sub_Vecs (New_Sub_Vec_Index));
         end if;

         --  Bump up capacity at each level (on the way out of any
         --  recursion)
         Vec.Total_Capacity := Vec.Total_Capacity + Sub_Vec_Capacity;
      end;
   end Expand_Vec;

   procedure Expand_Vec (Vec : in out Locked_Vector_Ptr) is
      --  Grow Vec to accommodate at least one more element
   begin
      if Vec = null then
         Vec := new Locked_Vector_Rec;
         Vec.Vector := new Vector_Rec (Level => 0, Len => Initial_Len);
         return;   --  All done
      end if;

      if not Thread_Safe then
         Expand_Vec (Vec.Vector);
      else
         Vec.Lock.Expand_Vec (Vec);
      end if;
   end Expand_Vec;

   -----------------
   -- Nth_Element --
   -----------------

   function Nth_Element
     (Vec : Vector_Rec;
      Index : Elem_Index)
      return Element_Type
   is
   begin
      if Vec.Level = 0 then
         return Vec.Elements (Index);
      else
         declare
            Total_Sub_Vec_Capacity : constant Vec_Capacity :=
              Level_Capacities (Vec.Level);
         begin
            --  Recurse with appropriate sub-Vec
            return Nth_Element
                     (Vec =>
                        Vec.Sub_Vecs ((Index + Total_Sub_Vec_Capacity - 1) /
                                      Total_Sub_Vec_Capacity).all,
                      Index => (Index - 1) rem Total_Sub_Vec_Capacity + 1);
         end;
      end if;
   end Nth_Element;

   ---------------------
   -- Set_Nth_Element --
   ---------------------

   procedure Set_Nth_Element
     (Vec : in out Vector_Rec;
      Index : Elem_Index;
      To : Element_Type) is
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

   protected body Locked_Vec_Ops is
      --  These operations provide locking as a side-effect of their invocation

      procedure Expand_Vec (Vec : in out Locked_Vector_Ptr) is
      --  Grow Vec to accommodate at least one more element
      begin
         TS_Vectors.Expand_Vec (Vec.Vector);
      end Expand_Vec;

      function Nth_Element
        (Vec : Locked_Vector_Ptr;
         Index : Elem_Index)
         return Element_Type is
      --  Recursive routine to return Nth element in Vec
      begin
         return TS_Vectors.Nth_Element (Vec.Vector.all, Index);
      end Nth_Element;

      procedure Set_Nth_Element
        (Vec   : in out Locked_Vector_Ptr;
         Index : Elem_Index;
         To    : Element_Type) is
      --  Recursive routine to set Nth element in Vec
      begin
         TS_Vectors.Set_Nth_Element (Vec.Vector.all, Index, To);
      end Set_Nth_Element;

   end Locked_Vec_Ops;

   --  ------------------- Visible Subprograms --------------------------------

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (Vec : Vector) return Elem_Index is
   --  Return number of Elements in Vec
   begin
      return Vec.Count;
   end Num_Elements;

   -----------------
   -- Add_Element --
   -----------------

   procedure Add_Element
     (Vec : in out Vector;
      Elem : Element_Type;
      Index : out Elem_Index) is
   --  Add element to Vec and return its index

      function Capacity (Vec : Locked_Vector_Ptr) return Vec_Capacity;
      --  Return overall capacity of Vec, as opposed to number of current
      --  Vectors.

      function Capacity (Vec : Locked_Vector_Ptr) return Vec_Capacity is
      begin
         if Vec = null then
            return 0;
         elsif Vec.Vector.Level = 0 then
            return Vec.Vector.Len;
         else
            return Vec.Vector.Total_Capacity;
         end if;
      end Capacity;

   begin
      if Vec.Count >= Capacity (Vec.Data) then
         --  Need to expand Vec
         Expand_Vec (Vec.Data);
      end if;
      Vec.Count := Vec.Count + 1;

      --  Return the Index
      Index := Vec.Count;

      --  Call recursive routine to actually set Nth element
      Set_Nth_Element (Vec.Data.Vector.all, Index, To => Elem);
   end Add_Element;

   -----------------
   -- Nth_Element --
   -----------------

   function Nth_Element
     (Vec : Vector;
      Index : Elem_Index)
      return Element_Type
   is
      --  Retrieve element at given index.
      pragma Assert (Index <= Vec.Count);
   begin
      if not Thread_Safe then
         return Nth_Element (Vec.Data.Vector.all, Index);
      else
         --  Call locking recursive routine Nth_Element
         return Vec.Data.Lock.Nth_Element (Vec.Data, Index);
      end if;
   end Nth_Element;

   ---------------------
   -- Set_Nth_Element --
   ---------------------

   procedure Set_Nth_Element
     (Vec : in out Vector;
      Index : Elem_Index;
      Elem : Element_Type) is
   --  Overwrite nth element.
   begin
      if not Thread_Safe then
         Set_Nth_Element (Vec.Data.Vector.all, Index, To => Elem);
      else
         --  Call locking recursive routine Set_Nth_Element
         Vec.Data.Lock.Set_Nth_Element (Vec.Data, Index, To => Elem);
      end if;
   end Set_Nth_Element;

end PSC.TS_Vectors;
