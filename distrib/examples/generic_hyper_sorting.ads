---------------------------------------------------
--  Generic Hyper Sorting routine

--  Copyright (C) 2011-2020, AdaCore
--  This program is provided "as is" with no warranty.
--  Report errors at:
--    http://groups.google.com/group/parasail-programming-language

generic
   type Elem_Type is private;
   with function "<" (Left, Right : Elem_Type) return Boolean is <>;
   with function "+" (Left, Right : Elem_Type) return Elem_Type is <>;
   with function "-" (Left, Right : Elem_Type) return Elem_Type is <>;
   with function "/" (Left : Elem_Type; Right : Integer)
     return Elem_Type is <>;
   with function "**" (Left : Elem_Type; Right : Integer)
     return Elem_Type is <>;
   Elem_First : Elem_Type;
   Elem_Last : Elem_Type;
   Elem_Zero : Elem_Type;
   with function Image (Val : Elem_Type) return String;
   type Index_Type is range <>;
   type Part_Index_Type is range <>;
   type Indexable_Type (<>) is limited private;
   with function Length (A : Indexable_Type) return Index_Type;
   with function First (A : Indexable_Type) return Index_Type;
   with function Element
     (A : Indexable_Type; Index : Index_Type;
      Part : Part_Index_Type'Base := 0)
     return Elem_Type;
   with function Move
     (A : in out Indexable_Type) return Indexable_Type;
   with procedure Copy_Slice
     (From : Indexable_Type;
      From_Part : Part_Index_Type;
      From_Index : Index_Type;
      To : in out Indexable_Type;
      To_Part : Part_Index_Type;
      To_Index : Index_Type;
      Count : Index_Type);
   with procedure Sort_One_Part
     (A : in out Indexable_Type;
      First, Last : Index_Type;
      Part : Part_Index_Type'Base := 0);
   with procedure For_Each_Part
     (Num_Parts : Part_Index_Type;
      Loop_Body : access procedure (Part_Index : Part_Index_Type));
package Generic_Hyper_Sorting is

   procedure Hyper_Qsort
     (A : in out Indexable_Type; Num_Parts : Part_Index_Type);
      --  Break indexable obj into parts; sort each part
      --  Come up with multiple pivots, and partition into a new
      --  set of parts using these pivots
      --  Shuffle data into new partitioning (probably using a temp array).
      --  Sort each new part.

end Generic_Hyper_Sorting;
