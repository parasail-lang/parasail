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

with Ada.Streams;
with PSC.Hash_Tables;
with PSC.String_Streams;
with PSC.Strings;

pragma Elaborate (PSC.Hash_Tables);
pragma Elaborate (PSC.String_Streams);
package PSC.Per_File_Strings is

   ------------------------------------------------
   --  Per_File_String_Table type and operations --
   ------------------------------------------------

   --  Compile-time Per-file String table

   type Local_String_Index_Base is  -- Allows negative values
     range -2 ** 15 .. 2 ** 15 - 1;

   subtype Local_String_Count is Local_String_Index_Base
     range 0 .. 2 ** 15 - 1;

   subtype Local_String_Index is
     Local_String_Count range 1 .. Local_String_Count'Last;

   type Per_File_String_Table is private;
   type Per_File_String_Table_Ptr is access all Per_File_String_Table;
   pragma No_Strict_Aliasing (Per_File_String_Table_Ptr);

   --  Operations on a per-file string table
   function Create return Per_File_String_Table_Ptr;
   --  Create an empty per-file string table

   function Get_Local_Index (PFS : Per_File_String_Table_Ptr;
                             Str : Strings.U_String_Index)
     return Local_String_Index;
   --  Get index into per-file string table (starting counting from 1)

   function Num_Strings (PFS : Per_File_String_Table_Ptr)
     return Local_String_Count;
   --  Number of strings in string table

   function Nth_String (PFS : Per_File_String_Table_Ptr;
                        Index : Local_String_Index)
     return Strings.U_String_Index;
   --  Nth element of string table (starting counting from 1)

   procedure Reset (PFS : in out Per_File_String_Table_Ptr);
   --  Reclaim storage and set PFS to null

   --  Stream type which has per-file string table for saving
   --  strings in a separate table rather than writing them out
   --  at each occurrence.
   type Buffered_Stream_With_PFS (PFS : access Per_File_String_Table)
     is new String_Streams.Buffered_Stream with null record;

   ------------------------------------
   -- Run-time per-file string table --
   ------------------------------------

   --  Buffered reader which also takes a mapping from local string indices
   --  to a 64-bit "Word_Type" representation for a string, where the
   --  low-order 32 bits are the U_String_Index.
   --  Map from a per-file local string index to the global U_String index

   type String_As_Word_Type is  --  Low-order 32 bits match U_String_Index
     range -2**63 .. +2**63 - 1;

   type Local_String_Map_Type is
     array (Local_String_Index) of String_As_Word_Type;

   type Buffered_Reader_With_Strings
     (Data     : access String_Streams.Stream_Rep_Array;
      Strings  : access Local_String_Map_Type)
     is new String_Streams.Buffered_Reader (Data) with null record;

private
   package Per_File_String_Maps is new Hash_Tables
     (Element_Type => Local_String_Index,
      Key_Type     => Strings.U_String,
      Hash_Type    => Strings.Hash_Type,
      Hash         => Strings.Hash);

   type Str_Index_Array is
     array (Local_String_Index range <>) of Strings.U_String_Index;

   type Str_Index_Array_Ptr is access all Str_Index_Array;

   type Per_File_String_Table is record
      Hash_Tab : Per_File_String_Maps.Hash_Table;
      Str_Vec  : Str_Index_Array_Ptr := null;
         --  This isn't filled in until
         --  Nth_String is called for the first time.
   end record;

end PSC.Per_File_Strings;
