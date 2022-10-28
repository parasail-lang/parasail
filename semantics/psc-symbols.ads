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
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

with PSC.Hash_Tables;
with PSC.Languages;
with PSC.Source_Positions;
with PSC.Strings;
with PSC.Trees;
with PSC.Trees.Lists;
with PSC.Vectors;
package PSC.Symbols is

   --  Symbol table for ParaSail

   type Sym_Kind_Enum is (
     No_Sym_Kind,
     Module_Sym_Kind,
     Module_Ancestor_Sym_Kind,
     Type_Sym_Kind,
     Param_Sym_Kind,
     Loop_Param_Sym_Kind,
     Loop_Key_Param_Sym_Kind,
     Object_Sym_Kind,
     Component_Sym_Kind,
     Operation_Sym_Kind,
     Literal_Sym_Kind,
     Statement_Sym_Kind);

   type Region;
   type Region_Ptr is access all Region;
   pragma No_Strict_Aliasing (Region_Ptr);

   type Symbol;
   type Sym_Ptr is access Symbol;

   package Symbol_Lists is new PSC.Vectors (Sym_Ptr);

   subtype Sym_Index is Symbol_Lists.Elem_Index;
   use type Sym_Index;

   No_Sym_Index : constant Sym_Index := Symbol_Lists.No_Elem_Index;

   type Symbol is record
      Kind : Sym_Kind_Enum := No_Sym_Kind;
      Str : Strings.U_String := Strings.Null_U_String;
      Full_Name : Strings.U_String := Strings.Null_U_String;
         --  NOTE: Not filled in until call "Compute_Sym_Full_Name"
      Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position;
      Enclosing_Region : Region_Ptr;
      Nested_Region : Region_Ptr;
      Sem_Info : Trees.Root_Sem_Ptr;
      Definition : Trees.Optional_Tree;
      Completion_Of : Symbols.Sym_Ptr;
      Index : Sym_Index := No_Sym_Index;
      Next_Homonym : Sym_Index := No_Sym_Index;
      Import_Clauses : Trees.Lists.List;  --  Non-empty for stand-alone units
   end record;

   type Symbol_List is private;
   --  List of symbols associated with region, in order of declaration

   type Symbol_Table is private;
   --  Hash table, mapping identifier => symbol index.

   package Region_Vectors is new PSC.Vectors (Region_Ptr);
   subtype Region_Index is Region_Vectors.Elem_Index;
   use type Region_Index;

   No_Region_Index : constant Region_Index := Region_Vectors.No_Elem_Index;

   type Region_Kind_Enum is (
     No_Region_Kind,
     Library_Region_Kind,
     Module_Region_Kind,
     Operation_Param_Region_Kind,
     Operation_Body_Region_Kind,
     Loop_Param_Region_Kind,
     Loop_Body_Region_Kind,
     Block_Stmt_Region_Kind,
     If_Stmt_Region_Kind,
     Case_Stmt_Region_Kind,
     Return_Stmt_Region_Kind,
     Parallel_Stmt_Region_Kind,
     Type_Annotation_Region_Kind);

   subtype Stmt_Region_Kind is Region_Kind_Enum range
     Loop_Param_Region_Kind .. Parallel_Stmt_Region_Kind;

   subtype Exitable_Stmt_Region_Kind is Region_Kind_Enum range
     Loop_Body_Region_Kind .. Case_Stmt_Region_Kind;

   subtype Sequential_Stmt_Region_Kind is Region_Kind_Enum range
     Block_Stmt_Region_Kind .. Return_Stmt_Region_Kind;

   type Region_Symbols is limited record
      Local_Symbols : Symbol_List;
      --  Declarations of region, in order of declaration
      Local_Symbol_Table : Symbol_Table;
      --  Declarations of region, indexed by U_String
   end record;

   type Region_Symbols_Ptr is access Region_Symbols;

   type Region (Kind : Region_Kind_Enum) is limited record
      Index : Region_Index := No_Region_Index;  --  Unique Id of Region
      Stmt_List : Trees.Lists.List;
      --  List of statement trees associated with region.
      --  NOTE: For a for-loop param region this is the list of iterators.
      Syms : Region_Symbols_Ptr;  --  Symbols declared in region
      Associated_Symbol : Sym_Ptr;
      --  Symbol associated with this region, if any
      Enclosing_Region : Region_Ptr;
      --  Enclosing region, if any
      Next_Sibling_Region : Region_Ptr;
      --  Next region at same level of multi-arm construct
      --  (such as an "if" or "case" statement).
   end record;

   ------------------------------------

   Library_Region : constant Region_Ptr;

   ------------------------------------

   function Enclosing_Module_Full_Name (Op_Sym : Sym_Ptr;
     Separator : String := Languages.Module_Name_Separator)
     return Strings.U_String;
   --  Return U_String for full name of module, with
   --  Languages.Module_Name_Separator (e.g. "::") separating names
   --  by default.

   function Sym_Full_Name (Sym : Sym_Ptr;
     Separator : String := Languages.Module_Name_Separator)
     return String;
   --  Return String for full name of sym, with given separator
   --  (Languages.Module_Name_Separator (e.g. "::") by default)

   procedure Compute_Sym_Full_Name (Sym : Sym_Ptr;
     Separator : String := Languages.Module_Name_Separator);
   --  Compute the full name of the symbol and save for later reference

   function Sym_Name (Sym : Sym_Ptr) return String;
   --  Return name of symbol, or "null" if Sym is null

   function Num_Prior_Homonyms (Sym : Sym_Ptr) return Sym_Index;
   --  Return count of number of prior symbols in same region
   --  which this overloads (is a homonym).  Result is 0 if no prior homonyms.
   --  If this sym has a non-null Completion_Of field, it starts from
   --  that symbol.
   --  This ignores syms with Completion_Of non-null.

   function Num_Symbols (List : Symbol_List) return Sym_Index;
   --  Return number of symbols in list

   procedure Add_Symbol (List : in out Symbol_List; Sym : Sym_Ptr);
   --  Add symbol to list and fill in its Index field.

   function Nth_Symbol
     (List : Symbol_List;
      Index : Sym_Index)
      return Sym_Ptr;
   --  Retrieve symbol at given index.

   procedure Enter_Symbol (Table : in out Symbol_Table; Sym : Sym_Ptr);
   --  Add symbol to symbol table.  If there is an overloading already
   --  in symbol table, replace preexisting symbol in table and
   --  set Next_Homonym to be index of prior symbol.
   --  Requires: Index field has already been filled in (via Add_Symbol).

   function Lookup_Symbol
     (Table : Symbol_Table;
      Str : Strings.U_String)
      return Sym_Index;
   --  Return index of most recent symbol added to Table with given
   --  Str as name.
   --  Any homonym can be reached via Next_Homonym field.

   procedure Add_To_Region (Specified_Region : Region_Ptr; Sym : Sym_Ptr);
   --  Add symbol to region.  Fill in Index and Next_Homonym fields.

   function Num_Symbols_In_Region (Specified_Region : Region_Ptr)
     return Sym_Index;
   --  Return number of symbols in region symbol table

   function Nth_Symbol_In_Region
     (Specified_Region : Region_Ptr;
      Index : Sym_Index)
      return Sym_Ptr;
   --  Retrieve symbol of region at given index.

   function Lookup_In_Region
     (Specified_Region : Region_Ptr;
      Str : Strings.U_String;
      Orig_Region : Region_Ptr := null)
      return Sym_Ptr;
   --  Lookup string in specified region.
   --  Return first symbol found, or null if no symbol found.
   --  Orig_Region is used to determine visibility of standalone items.
   --  If null, means all are visible.

   function Next_Homonym_In_Region
     (This_Sym : Sym_Ptr; Orig_Region : Region_Ptr := null) return Sym_Ptr;
   --  Look for next homonym in this region.
   --  Return next homonym if found, or null if no symbol found.
   --  Orig_Region is used to determine visibility of standalone items.
   --  If null, means all are visible.

   function Lookup_In_Region_Chain
     (Starting_Region : Region_Ptr;
      Str : Strings.U_String)
      return Sym_Ptr;
   --  Lookup string in given chain of regions.
   --  Return first symbol found, or null if no symbol found.
   --  Use symbols made directly visible by Imports as last resort.

   function Next_Homonym_In_Region_Chain
     (This_Sym : Sym_Ptr;
      Orig_Region : Region_Ptr)
      return Sym_Ptr;
   --  Look for next homonym in region chain
   --  Return next homonym if found, or null if no symbol found.
   --  Orig_Region identifies region where lookup started, as this
   --  may determine what "import"ed symbols are visible.

   function Lookup_Standalone_Item
     (Decl_Region : Region_Ptr;
      Item_Name : Trees.Optional_Tree;
      Region_For_Imports : Region_Ptr := null)
      return Sym_Ptr;
   --  Lookup standalone item, possibly of the form "A::B::C"
   --  If Region_For_Imports is non-null, then imports associated
   --  with given region apply to lookup.

   function Region_Encloses_Region
     (Encloser : Region_Ptr;
      Enclosed : Region_Ptr)
      return Boolean;
   --  Return True if Encloser region matches or encloses Enclosed regions,
   --  treating interface and class defining a module as equivalent.

   procedure Assign_Region_Index (Region : Region_Ptr);
   --  Assign unique index to region

   function Region_At_Index (Index : Region_Index) return Region_Ptr;
   --  Return region with given index

   subtype Str_Array is Strings.U_String_Array;

   function Sym_Strings (Sym : Sym_Ptr) return Str_Array;
   --  Return array of strings corresponding to full name of symbol

   function Name_Strings (Import_Clause : Trees.Optional_Tree)
     return Str_Array;
   --  Return array of strings corresponding to import clause

   function Generate_Unique_Label
     (Pos : Source_Positions.Source_Position;
      Preceding : String;
      Include_File : Boolean := False) return Strings.U_String;
   --  Return a U_String that is a concatenation of "Preceding" and
   --  "[FFFF_]LLLL_CCCC" where "FFFF" is a unique number assigned
   --  to the file, "LLLL" is the line number, and "CCCC" is the column
   --  number.

   procedure Reset_Standard_Imports;
   --  When we switch to a new language, we need to
   --  reset the standard imports.

private

   type Symbol_List is new Symbol_Lists.Vector;

   function Num_Symbols (List : Symbol_List) return Sym_Index renames
     Num_Elements;
   --  Return number of symbols in list

   function Nth_Symbol
     (List : Symbol_List;
      Index : Sym_Index)
      return Sym_Ptr renames Nth_Element;
   --  Retrieve symbol at given index.

   -- Symbol table --

   package Symbol_Tables is new PSC.Hash_Tables (
      Element_Type => Sym_Index,
      Key_Type => Strings.U_String,
      Hash_Type => Strings.Hash_Type,
      Hash => Strings.Hash);

   type Symbol_Table is new Symbol_Tables.Hash_Table;

   Library_Region : constant Region_Ptr :=
     new Region (Kind => Symbols.Library_Region_Kind);

end PSC.Symbols;
