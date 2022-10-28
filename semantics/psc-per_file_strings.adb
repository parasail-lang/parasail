------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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
with Ada.Unchecked_Deallocation;

package body PSC.Per_File_Strings is
   ------------------------------------------------
   --  Per_File_String_Table type and operations --
   ------------------------------------------------

   --  Compile-time Per-file String table

   use Per_File_String_Maps;

   --  Operations on a per-file string table

   function Create return Per_File_String_Table_Ptr is
   --  Create an empty per-file string table
   begin
      return new Per_File_String_Table;
   end Create;

   function Get_Local_Index (PFS : Per_File_String_Table_Ptr;
                             Str : Strings.U_String_Index)
     return Local_String_Index is
   --  Get index into per-file string table (starting counting from 1)
      Existing_Str_Ref : Element_Ref;
      New_Local_Index : Local_String_Index;
      U_Str : constant Strings.U_String := Strings.To_U_String (Str);
   begin

      New_Local_Index :=
        Local_String_Count (Num_Entries (PFS.Hash_Tab)) + 1;
      --  Add string to table unless already there
      Enter_Element (PFS.Hash_Tab,
        U_Str, New_Local_Index, Existing_Str_Ref);

      if Existing_Str_Ref /= null then
         --  Already in table
         New_Local_Index := Existing_Str_Ref.all;
      end if;
      --  else: It has been added to table

      --  Make sure things are copacetic.
      pragma Assert (Find_Element (PFS.Hash_Tab, U_Str).all = New_Local_Index);
      pragma Assert (New_Local_Index <=
                        Local_String_Count (Num_Entries (PFS.Hash_Tab)));

      return New_Local_Index;
   end Get_Local_Index;

   function Num_Strings (PFS : Per_File_String_Table_Ptr)
     return Local_String_Count is
   --  Number of strings in string table
   begin
      return Local_String_Count (Num_Entries (PFS.Hash_Tab));
   end Num_Strings;

   function Nth_String (PFS : Per_File_String_Table_Ptr;
                        Index : Local_String_Index)
     return Strings.U_String_Index is
   --  Nth element of string table (starting counting from 1)

      procedure Fill_In_Vec_Elem (Pair : Pair_Ref);
      --  Fill in the appropriate element of PFS.Str_Vec

      procedure Fill_In_Vec_Elem (Pair : Pair_Ref) is
      --  Fill in the appropriate element of PFS.Str_Vec
      begin
         PFS.Str_Vec (Element (Pair).all) := Strings.Index (Key (Pair).all);
      end Fill_In_Vec_Elem;

      procedure Fill_In_Vec is new Iterate (Fill_In_Vec_Elem);

   begin  --  Nth_String

      if PFS.Str_Vec = null then
         --  Build the string vector now
         PFS.Str_Vec := new Str_Index_Array (1 .. Num_Strings (PFS));
         --  Fill in the elements of the vector
         Fill_In_Vec (PFS.Hash_Tab);
      end if;

      --  Return the Nth element
      return PFS.Str_Vec (Index);
   end Nth_String;

   procedure Reset (PFS : in out Per_File_String_Table_Ptr) is
   --  Reclaim storage and set PFS to null
      procedure Free is new Ada.Unchecked_Deallocation
        (Str_Index_Array, Str_Index_Array_Ptr);
      procedure Free is new Ada.Unchecked_Deallocation
        (Per_File_String_Table, Per_File_String_Table_Ptr);
   begin
      --  Reclaim storage and set everything to null
      Reclaim (PFS.Hash_Tab);
      Free (PFS.Str_Vec);
      Free (PFS);
   end Reset;
end PSC.Per_File_Strings;
