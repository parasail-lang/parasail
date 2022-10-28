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

with System.Storage_Elements;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with PSC.Languages;
with PSC.Strings; pragma Elaborate (PSC.Strings);
with PSC.Trees;  use PSC.Trees;
with PSC.Trees.Lists;
with PSC.Trees.Identifier;
with PSC.Trees.Semantics;
with PSC.Trees.Qualified_Name;
package body PSC.Symbols is

   subtype Sym_Table_Entry_Ref is Symbol_Tables.Element_Ref;
   use type Sym_Table_Entry_Ref;

   Debug_Import_Clauses : constant Boolean := False;

   --  These are used for the default and standard import clauses
   Star_Str : constant Strings.U_String := Strings.String_Lookup ("*");
   Standard_Import_Clauses : Lists.List := Lists.Empty_List;
   --  Initialized on first use

   Region_Table : Region_Vectors.Vector;

   ------- Local Subprograms ----------

   procedure Init_Standard_Imports is
   --  Initialize the Standard_Import_Clauses to
   --  PSL::Core::* and PSL::Containers::*
   begin
      if not Lists.Is_Empty (Standard_Import_Clauses) then
         --  Already done
         return;
      end if;

      declare
         Std_Lib_Str : constant Strings.U_String := Strings.String_Lookup
           (Languages.Standard_Library_Prefix);
         Core_Str : constant Strings.U_String := Strings.String_Lookup
           (Languages.Standard_Library_Core_Module);
         Containers_Str : constant Strings.U_String :=
           Strings.String_Lookup
             (Languages.Standard_Library_Containers_Module);
      begin
         if Languages.Language in Languages.Ada_Ish then
            --  Include Standard.*
            Lists.Append (Standard_Import_Clauses,
              Qualified_Name.Make
                (Prefix => Identifier.Make
                   (Strings.String_Lookup ("Standard")),
                 Id => Identifier.Make (Star_Str)));
         end if;
         Lists.Append (Standard_Import_Clauses,
           Qualified_Name.Make
             (Prefix => Qualified_Name.Make
                (Prefix => Identifier.Make (Std_Lib_Str),
                 Id => Identifier.Make (Core_Str)),
              Id => Identifier.Make (Star_Str)));
         Lists.Append (Standard_Import_Clauses,
           Qualified_Name.Make
             (Prefix => Qualified_Name.Make
                (Prefix => Identifier.Make (Std_Lib_Str),
                 Id => Identifier.Make (Containers_Str)),
              Id => Identifier.Make (Star_Str)));
      end;
   end Init_Standard_Imports;

   function Sym_Strings (Sym : Sym_Ptr) return Str_Array is
   --  Return array of strings corresponding to full name of symbol
      use type Strings.U_String;

      function Count_Strings return Positive is
         --  Return count of strings in full name of Sym
         Next_Region : Region_Ptr := Sym.Enclosing_Region;
         Num_Strings : Positive := 1;
      begin
         --  Count number of non-null symbols in enclosing scopes
         while Next_Region /= null loop
            if Next_Region.Associated_Symbol /= null
              and then
                Next_Region.Associated_Symbol.Str /= Strings.Null_U_String
            then
               Num_Strings := Num_Strings + 1;
            end if;
            Next_Region := Next_Region.Enclosing_Region;
         end loop;
         return Num_Strings;
      end Count_Strings;

      Result : Str_Array (1 .. Count_Strings);

      Next_Region : Region_Ptr := Sym.Enclosing_Region;
      Index : Positive := Result'Last;
   begin
      --  Put strings into array
      Result (Index) := Sym.Str;
      while Next_Region /= null loop
         if Next_Region.Associated_Symbol /= null
           and then
             Next_Region.Associated_Symbol.Str /= Strings.Null_U_String
         then
            Index := Index - 1;
            Result (Index) := Next_Region.Associated_Symbol.Str;
         end if;
         Next_Region := Next_Region.Enclosing_Region;
      end loop;

      pragma Assert (Index = 1);  --  Array fully initialized

      return Result;
   end Sym_Strings;

   function Name_Strings (Import_Clause : Optional_Tree) return Str_Array is
   --  Return array of strings corresponding to import clause
      use type Strings.U_String;

      function Count_Strings return Positive is
         --  Return count of strings in name in Import_Clause

         Num_Strings : Natural := 0;
         Name : Optional_Tree := Import_Clause;
      begin
         --  Count number of names in qualified/selected/property name
         --  (or 1 if simple ident)
         while Not_Null (Name) loop
            Num_Strings := Num_Strings + 1;
            Name := Semantics.Prefix (Name);
         end loop;
         return Num_Strings;
      end Count_Strings;

      Result : Str_Array (1 .. Count_Strings);

      Index : Positive := Result'Last + 1;
      Name : Optional_Tree := Import_Clause;
   begin
      --  Put identifier strings into array, in reverse order
      while Not_Null (Name) loop
         Index := Index - 1;
         Result (Index) := Identifier.Tree
           (Tree_Ptr_Of (Semantics.Suffix (Name)).all).Str;

         Name := Semantics.Prefix (Name);
      end loop;

      pragma Assert (Index = 1);  --  Array fully initialized

      return Result;
   end Name_Strings;

   function Symbol_Included_In_Import
     (Sym : Sym_Ptr; Import_Clause : Optional_Tree) return Boolean
   is
   --  Return True if Sym is covered by Import_Clause
      Import_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Import_Clause).all;
      Import_Strs : constant Str_Array := Name_Strings (Import_Clause);
      pragma Assert (Import_Strs'First = 1);
      Sym_Strs : constant Str_Array := Sym_Strings (Sym);
      pragma Assert (Sym_Strs'First = 1);
      use type Strings.U_String;
   begin
      for I in 1 .. Sym_Strs'Length loop
         if I > Import_Strs'Length then
            --  Import_Strs ran out too soon
            return False;
         elsif Import_Strs (I) = Star_Str then
            --  Import_Strs ends with a "*" so we are all set
            pragma Assert (I = Import_Strs'Length);  --  "*" should be last
            return True;
         elsif Import_Strs (I) /= Sym_Strs (I) then
            --  Mismatch
            return False;
         end if;
      end loop;

      --  All strings match
      return True;
   end Symbol_Included_In_Import;

   function Symbol_Included_In_Import_Clauses
     (Sym : Sym_Ptr; Import_Clauses : Lists.List) return Boolean
   is
   --  Return True if Sym is covered by one of the Import_Clauses
      use Ada.Text_IO;
   begin
      for I in 1 ..  Trees.Lists.Length (Import_Clauses) loop
         if Symbol_Included_In_Import (Sym,
           Trees.Lists.Nth_Element (Import_Clauses, I))
         then
            if Debug_Import_Clauses then
               Put_Line (Sym_Name (Sym) &
                 " is included in import of " &
                 Trees.Subtree_Image
                   (Trees.Lists.Nth_Element
                      (Import_Clauses, I)));
            end if;
            return True;
         end if;
      end loop;
      return False;
   end Symbol_Included_In_Import_Clauses;

   function Symbol_Included_In_Standard_Imports (Sym : Sym_Ptr)
     return Boolean is
   --  Return True if Sym is covered by standard imports
   begin
      --  Do one-time initialization of "standard" imports
      Init_Standard_Imports;

      return Symbol_Included_In_Import_Clauses (Sym, Standard_Import_Clauses);
   end Symbol_Included_In_Standard_Imports;

   function Innermost_Import_Clauses (Starting_Region : Region_Ptr)
     return Trees.Lists.List is
   --  Return Import_Clauses from innermost standalone module
   --  or operation including Starting_Region.  Return empty list
   --  if none.
      Next_Region : Region_Ptr := Starting_Region;
      use Trees;
   begin
      while Next_Region /= null loop
         if Next_Region.Associated_Symbol /= null and then
           not Lists.Is_Empty (Next_Region.Associated_Symbol.Import_Clauses)
         then
            --  Found a non-null list of import clauses
            return Next_Region.Associated_Symbol.Import_Clauses;
         end if;
         Next_Region := Next_Region.Enclosing_Region;
      end loop;
      --  No standalone enclosing module found (not expected!)
      pragma Assert (Starting_Region = null or else
        Starting_Region.Enclosing_Region = null);
      return Lists.Empty_List;
   end Innermost_Import_Clauses;

   function Apply_Import_Filter
     (Sym : Sym_Ptr;
      Orig_Region : Region_Ptr)
      return Sym_Ptr is
   --  Return Sym if Orig_Region is null or it is not a standalone item
   --  or if import clauses make it visible.
   --  Otherwise return null.
   begin
      if Orig_Region = null
        or else Trees.Lists.Is_Empty (Sym.Import_Clauses)
      then
         --  No Orig_Region specified, or Sym not a standalone item.
         return Sym;
      else
         --  This corresponds to a standalone operation/module.
         --  Only return it if it is "visible" via an import
         --  or encloses the Orig_Region, or Orig_Region is
         --  Library_Region
         if Orig_Region = Library_Region
           or else Region_Encloses_Region
             (Encloser => Sym.Nested_Region,
              Enclosed => Orig_Region)
         then
            --  Visible because is enclosing or we are
            --  at library level.
            return Sym;
         end if;

         declare
            Import_Clauses : constant Trees.Lists.List :=
              Innermost_Import_Clauses (Orig_Region);
            use Ada.Text_IO;
         begin
            if Symbol_Included_In_Import_Clauses
              (Sym, Import_Clauses)
            then
               return Sym;
            end if;

            if Symbol_Included_In_Standard_Imports (Sym) then
               return Sym;
            end if;

            if Debug_Import_Clauses then
               Put_Line (Sym_Name (Sym) & " NOT included in:");
               for I in 1 ..
                 Trees.Lists.Length (Import_Clauses) loop
                  Put_Line ("  import " & Trees.Subtree_Image
                    (Trees.Lists.Nth_Element (Import_Clauses, I)));
               end loop;
               Put_Line (" nor in standard imports:");
               for I in 1 ..
                 Trees.Lists.Length (Standard_Import_Clauses) loop
                  Put_Line ("  import " & Trees.Subtree_Image
                    (Trees.Lists.Nth_Element
                      (Standard_Import_Clauses, I)));
               end loop;
            end if;
            --  Not visible
            return null;
         end;
      end if;
   end Apply_Import_Filter;

   function Lookup_In_Import_Clauses
     (Str : Strings.U_String; Import_Clauses : Lists.List;
      Orig_Region : Region_Ptr) return Sym_Ptr
   is
   --  See if the given Str denotes a name made directly visible
   --  by an import clause.
   --  Complain if there is more than one (use Orig_Region for Source-Pos).
   --  Return non-null Sym_Ptr if found.
      Result : Sym_Ptr := null;
   begin
      for I in 1 .. Lists.Length (Import_Clauses) loop
         declare
            Import_Clause : constant Optional_Tree :=
              Lists.Nth_Element (Import_Clauses, I);
            Import_Strs : constant Str_Array := Name_Strings (Import_Clause);
            use type Strings.U_String;
         begin
            if Import_Strs'Length > 1
              and then
                (Import_Strs (Import_Strs'Last) = Str
                 or else Import_Strs (Import_Strs'Last) = Star_Str)
            then
               --  Only interested in cases like X::<str> or X::*
               declare
                  Prefix : constant Optional_Tree :=
                    Semantics.Prefix (Import_Clause);
                  Prefix_Sym : constant Sym_Ptr :=
                    Lookup_Standalone_Item (Library_Region, Prefix);
               begin
                  if Prefix_Sym /= null
                    and then not Region_Encloses_Region
                      (Encloser => Prefix_Sym.Nested_Region,
                       Enclosed => Orig_Region)
                  then
                     --  Lookup in region, unless is an enclosing region
                     declare
                        New_Result : constant Sym_Ptr :=
                          Lookup_In_Region (Prefix_Sym.Nested_Region, Str);
                        use Ada.Text_IO;
                     begin
                        if Debug_Import_Clauses then
                           Put_Line ("Looking up " & Strings.To_String (Str) &
                             " against import " &
                             Subtree_Image (Import_Clause) & "; found = " &
                             Boolean'Image (New_Result /= null));
                        end if;
                        if New_Result /= null then
                           if Result = null then
                              --  First match
                              Result := New_Result;
                           else
                              --  Ambiguous result
                              Semantics.Sem_Error (
                                "Ambiguous definitions for " &
                                  Strings.To_String (Str) & " at " &
                                  Source_Positions.Image (Result.Source_Pos) &
                                  " and " &
                                  Source_Positions.Image
                                    (New_Result.Source_Pos),
                                Src_Pos =>
                                  Orig_Region.Associated_Symbol.Source_Pos);
                           end if;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      return Result;
   end Lookup_In_Import_Clauses;

   function Lookup_In_Imports
     (Str : Strings.U_String;
      Orig_Region : Region_Ptr)
      return Sym_Ptr
   is
   --  Lookup Str in symbols made directly visible by imports
      Import_Clauses : constant Lists.List :=
        Innermost_Import_Clauses (Orig_Region);
      Result : constant Sym_Ptr :=
        Lookup_In_Import_Clauses (Str, Import_Clauses, Orig_Region);
   begin
      if Result /= null then
         return Result;
      end if;
      --  Look up in standard import clauses
      Init_Standard_Imports;
      return Lookup_In_Import_Clauses
        (Str, Standard_Import_Clauses, Orig_Region);
   end Lookup_In_Imports;

   function Lookup_Next_In_Region_Chain
     (Starting_Region : Region_Ptr;
      Str : Strings.U_String;
      Orig_Region : Region_Ptr;
      Last_Region : Region_Ptr)
      return Sym_Ptr
   is
      --  Lookup next string in given chain of regions.
      --  Return first symbol found, or null if no symbol found.
      --  Orig_Region indicates where lookup originated.
      --  Last_Region indicates where prior symbol was found.
      --  If no symbols found in enclosing regions, look in imported
      --  regions as well, presuming Last_Region is not itself an
      --  imported region (as determined by whether or not it is in the
      --  chain of regions starting at Orig_Region).
      Next_Region : Region_Ptr := Starting_Region;
   begin
      while Next_Region /= null loop
         declare
            Result : constant Sym_Ptr := Lookup_In_Region
              (Next_Region, Str, Orig_Region => Orig_Region);
         begin
            if Result /= null then
               --  Found visible item in this region
               return Result;
            end if;

            Next_Region := Next_Region.Enclosing_Region;
         end;
      end loop;

      --  Not found in chain of regions.
      if Orig_Region /= null
        and then Region_Encloses_Region
           (Encloser => Last_Region,
            Enclosed => Orig_Region)
      then
         --  Consider names made directly visible by import clauses,
         --  but only if Last_Region is same as, or enclosing
         --  Orig_Region.
         return Lookup_In_Imports (Str, Orig_Region);
      else
         --  Give up now
         return null;
      end if;

   end Lookup_Next_In_Region_Chain;

   function Lookup_Symbol_In_Region
     (Specified_Region : Region_Ptr;
      Str : Strings.U_String)
      return Sym_Index is
   --  Look up in region's symbol table, if any.
   --  Return No_Sym_Index if no symbol table created yet.
   begin
      if Specified_Region = null or else Specified_Region.Syms = null then
         return No_Sym_Index;
      else
         return Lookup_Symbol (Specified_Region.Syms.Local_Symbol_Table, Str);
      end if;
   end Lookup_Symbol_In_Region;

   ------- Visible Subprograms ----------

   function Enclosing_Module_Full_Name (Op_Sym : Sym_Ptr;
     Separator : String := Languages.Module_Name_Separator)
     return Strings.U_String is
   --  Return U_String for full name of module, with
   --  Languages.Module_Name_Separator (e.g. "::") separating names
   --  by default.
      Reg : Symbols.Region_Ptr := Op_Sym.Enclosing_Region;
      use type Strings.U_String;
   begin
      while Reg /= null and then Reg.Kind /= Library_Region_Kind loop
         if Reg.Associated_Symbol /= null
           and then Reg.Associated_Symbol.Str /= Strings.Null_U_String
         then
            --  Found a named enclosing region; return its full name.
            if Reg.Associated_Symbol.Full_Name /= Strings.Null_U_String then
               --  Full name is pre-computed
               return Reg.Associated_Symbol.Full_Name;
            else
               --  Need to compute full name now
               return Strings.String_Lookup
                 (Sym_Full_Name (Reg.Associated_Symbol, Separator));
            end if;
         end if;
         --  Ignore unnamed regions
         Reg := Reg.Enclosing_Region;
      end loop;
      --  No named enclosing module
      return Strings.Null_U_String;
   end Enclosing_Module_Full_Name;

   function Sym_Full_Name (Sym : Sym_Ptr;
     Separator : String := Languages.Module_Name_Separator)
     return String is
   --  Return String for full name of sym, with given separator
   --  (Languages.Module_Name_Separator (e.g. "::") by default)
      Reg : Symbols.Region_Ptr := Sym.Enclosing_Region;
      use type Strings.U_String;
   begin
      if Sym.Full_Name /= Strings.Null_U_String then
         --  See whether already computed
         return Strings.To_String (Sym.Full_Name);
      end if;

      while Reg /= null and then Reg.Kind /= Library_Region_Kind loop
         if Reg.Associated_Symbol /= null
           and then Reg.Associated_Symbol.Str /= Strings.Null_U_String
         then
            return Sym_Full_Name (Reg.Associated_Symbol, Separator) &
              Separator & Strings.To_String (Sym.Str);
         end if;
         --  Ignore unnamed region
         Reg := Reg.Enclosing_Region;
      end loop;

      --  No named enclosing region
      return Strings.To_String (Sym.Str);
   end Sym_Full_Name;

   procedure Compute_Sym_Full_Name (Sym : Sym_Ptr;
     Separator : String := Languages.Module_Name_Separator) is
   --  Compute the full name of the symbol and save for later reference
      use type Strings.U_String;
   begin
      if Sym.Full_Name = Strings.Null_U_String then
         Sym.Full_Name :=
           Strings.String_Lookup (Sym_Full_Name (Sym, Separator));
      end if;
   end Compute_Sym_Full_Name;

   function Sym_Name (Sym : Sym_Ptr) return String is
   --  Return name of symbol, or "null" if Sym is null
   begin
      if Sym = null then
         return "null";
      else
         return Strings.To_String (Sym.Str);
      end if;
   end Sym_Name;

   procedure Add_Symbol (List : in out Symbol_List; Sym : Sym_Ptr) is
   --  Add symbol to list and fill in its Index field.
   begin
      --  Just pass the buck to the Vector routine
      Add_Element (List, Sym, Sym.Index);
   end Add_Symbol;

   procedure Enter_Symbol (Table : in out Symbol_Table; Sym : Sym_Ptr) is
      --  Add symbol to symbol table.  If there is an overloading already
      --  in symbol table, replace preexisting symbol in table and
      --  set Next_Homonym to be index of prior symbol.
      --  Requires: Index field has already been filled in (via Add_Symbol).
      Existing_Sym : Sym_Table_Entry_Ref;
   begin
      --  Add to table unless already there
      Enter_Element (Table, Sym.Str, Sym.Index, Existing_Sym);

      if Existing_Sym /= null then
         --  Already in table
         --  Add to chain of homonyms
         Sym.Next_Homonym := Existing_Sym.all;
         Existing_Sym.all := Sym.Index;
      else
         --  First sym with given Str.
         Sym.Next_Homonym := No_Sym_Index;
      end if;
   end Enter_Symbol;

   function Lookup_Symbol
     (Table : Symbol_Table;
      Str : Strings.U_String)
      return Sym_Index
   is
      --  Return index of most recent symbol added to Table with given
      --  Str as name.
      --  Any homonym can be reached via Next_Homonym field.
      Ref : constant Sym_Table_Entry_Ref := Find_Element (Table, Str);
   begin
      if Ref /= null then
         --  Found
         return Ref.all;
      else
         --  Str not in table
         return No_Sym_Index;
      end if;
   end Lookup_Symbol;

   procedure Add_To_Region (Specified_Region : Region_Ptr; Sym : Sym_Ptr) is
      --  Add symbol to region.  Fill in Index and Next_Homonym fields.
      --  Don't actually enter symbol in hash table
      --  if it's name is the null string.
      use type Strings.U_String;
   begin
      if Specified_Region.Syms = null then
         --  First symbol in region; create symbol table
         Specified_Region.Syms := new Region_Symbols;
      end if;
      Add_Symbol (Specified_Region.Syms.Local_Symbols, Sym);
      if Sym.Str /= Strings.Null_U_String
        and then Sym.Str /= Strings.Empty_U_String
      then
         Enter_Symbol (Specified_Region.Syms.Local_Symbol_Table, Sym);
      end if;

      if Specified_Region.Associated_Symbol /= null then
         --  Pre-compute the full name now that we know there are local symbols
         Compute_Sym_Full_Name (Specified_Region.Associated_Symbol);
      end if;
   end Add_To_Region;

   function Num_Prior_Homonyms (Sym : Sym_Ptr) return Sym_Index is
   --  Return count of number of prior symbols in same region
   --  which this overloads (is a homonym).  Result is 0 if no prior homonyms.
   --  If this sym has a non-null Completion_Of field, it starts from
   --  that symbol.
   --  This ignores syms with Completion_Of non-null.
      Result : Sym_Index := 0;
      Prior_Sym : Sym_Ptr := Next_Homonym_In_Region (Sym);
   begin
      if Sym.Completion_Of /= null then
         --  Start from decl if this is a completion
         Prior_Sym := Next_Homonym_In_Region (Sym.Completion_Of);
      end if;
      while Prior_Sym /= null loop
         if Prior_Sym.Completion_Of = null then
            --  Only count symbols that are not completions of something else
            Result := Result + 1;
         end if;
         Prior_Sym := Next_Homonym_In_Region (Prior_Sym);
      end loop;
      return Result;
   end Num_Prior_Homonyms;

   function Num_Symbols_In_Region (Specified_Region : Region_Ptr)
     return Sym_Index is
   --  Return number of symbols in region symbol table
   begin
      if Specified_Region.Syms = null then
         return 0;
      else
         return Num_Symbols (Specified_Region.Syms.Local_Symbols);
      end if;
   end Num_Symbols_In_Region;

   function Nth_Symbol_In_Region
     (Specified_Region : Region_Ptr;
      Index : Sym_Index)
      return Sym_Ptr is
   --  Retrieve symbol of region at given index.
   begin
      return Nth_Symbol (Specified_Region.Syms.Local_Symbols, Index);
   end Nth_Symbol_In_Region;

   function Lookup_In_Region
     (Specified_Region : Region_Ptr;
      Str : Strings.U_String;
      Orig_Region : Region_Ptr := null)
      return Sym_Ptr
   is
   --  Lookup string in specified region.
   --  Return first symbol found, or null if no symbol found.
   --  Orig_Region is used to determine visibility of standalone items.
   --  If null, means all are visible.
      Index : constant Sym_Index :=
        Lookup_Symbol_In_Region (Specified_Region, Str);
   begin
      if Index /= No_Sym_Index then
         declare
            Sym : constant Sym_Ptr :=
              Nth_Symbol_In_Region (Specified_Region, Index);
            Result : constant Sym_Ptr :=
              Apply_Import_Filter (Sym, Orig_Region);
         begin
            if Result /= null then
               return Result;
            else
               return Next_Homonym_In_Region (Sym, Orig_Region);
            end if;
         end;
      end if;
      --  Not found
      return null;
   end Lookup_In_Region;

   function Next_Homonym_In_Region
     (This_Sym : Sym_Ptr; Orig_Region : Region_Ptr := null) return Sym_Ptr is
   --  Look for next homonym in this region.
   --  Return next homonym if found, or null if no symbol found.
   --  Orig_Region is used to determine visibility of standalone items.
   --  If null, means all are visible.
      Index : Sym_Index := This_Sym.Next_Homonym;
   begin
      while Index /= No_Sym_Index loop
         declare
            Sym : constant Sym_Ptr :=
              Nth_Symbol_In_Region (This_Sym.Enclosing_Region, Index);
            Result : constant Sym_Ptr :=
              Apply_Import_Filter (Sym, Orig_Region);
         begin
            if Result /= null then
               --  This one is visible
               return Result;
            else
               --  Keep looking
               Index := Sym.Next_Homonym;
            end if;
         end;
      end loop;
      --  That's it, no more homonyms in this region
      return null;
   end Next_Homonym_In_Region;

   function Lookup_In_Region_Chain
     (Starting_Region : Region_Ptr;
      Str : Strings.U_String)
      return Sym_Ptr
   is
      --  Lookup string in given chain of regions.
      --  Return first symbol found, or null if no symbol found.
      --  Use symbols made directly visible by Imports as last resort.
   begin
      return Lookup_Next_In_Region_Chain (Starting_Region, Str,
        Orig_Region => Starting_Region,
        Last_Region => Starting_Region);
   end Lookup_In_Region_Chain;

   function Next_Homonym_In_Region_Chain
     (This_Sym : Sym_Ptr;
      Orig_Region : Region_Ptr)
      return Sym_Ptr
   is
      --  Look for next homonym in region chain
      --  Return next homonym if found, or null if no symbol found.
      --  Orig_Region identifies region where lookup started, as this
      --  may determine what "import"ed symbols are visible.
      Next_Sym : constant Sym_Ptr := Next_Homonym_In_Region (This_Sym,
        Orig_Region => Orig_Region);
      use type Strings.U_String;
   begin
      --  NOTE: We use Orig_Region to determine what has been "import"ed.
      if Next_Sym /= null then
         --  There was another homonym in this region
         return Next_Sym;
      elsif This_Sym.Str = Strings.Null_U_String then
         --  Special case of "implicit" module
         return null;
      elsif Orig_Region /= null
        and then not Region_Encloses_Region
           (Encloser => This_Sym.Enclosing_Region,
            Enclosed => Orig_Region)
      then
         --  Stop now, because we must have been looking in an imported region
         --  and we only allow symbols to come from a single imported region
         --  in a given lookup.
         return null;
      else
         --  Look in enclosing region or imported regions.
         --  NOTE: We are relying on Lookup_Next_In_Region_Chain returning
         --       null if given a null Starting_Region
         return Lookup_Next_In_Region_Chain
                  (This_Sym.Enclosing_Region.Enclosing_Region,
                   This_Sym.Str,
                   Orig_Region => Orig_Region,
                   Last_Region => This_Sym.Enclosing_Region);
      end if;
   end Next_Homonym_In_Region_Chain;

   function Lookup_Standalone_Item
     (Decl_Region : Region_Ptr;
      Item_Name : Trees.Optional_Tree;
      Region_For_Imports : Region_Ptr := null)
      return Sym_Ptr
   is
      --  Lookup standalone item, possibly of the form "A::B::C"
      --  If Region_For_Imports is non-null, then imports associated
      --  with given region apply to lookup.
      Name_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Item_Name).all;
      Prefix : constant Optional_Tree := Semantics.Prefix (Item_Name);
   begin
      if Not_Null (Prefix) then
         --  Recurse if name is a qualified name
         declare
            Enc_Module : constant Sym_Ptr :=
              Lookup_Standalone_Item
                (Decl_Region,
                 Prefix,
                 Region_For_Imports);
         begin
            if Enc_Module = null then
               --  Not found
               return null;
            else
               --  Use enclosing module's region
               return Lookup_In_Region (Enc_Module.Nested_Region,
                 Identifier.Tree
                   (Tree_Ptr_Of (Semantics.Suffix (Item_Name)).all).Str);
            end if;
         end;
      elsif Name_Tree not in Identifier.Tree then
         --  Malformed qualified name
         Semantics.Sem_Error
           (Item_Name, "Not a valid fully-qualified name");
         return null;
      else
         declare
            Str : constant Strings.U_String :=
              Identifier.Tree (Name_Tree).Str;
            Item_Sym : Symbols.Sym_Ptr :=
              Lookup_Next_In_Region_Chain
                (Decl_Region, Str, Orig_Region => Region_For_Imports,
                                   Last_Region => Decl_Region);
            use type Symbols.Sym_Kind_Enum;
         begin
            while Item_Sym /= null
              and then Item_Sym.Kind /= Module_Sym_Kind
              and then Item_Sym.Kind /= Operation_Sym_Kind
            loop
               --  Keep looking for a module/operation with the given name
               Item_Sym := Next_Homonym_In_Region_Chain (Item_Sym,
                 Orig_Region => Region_For_Imports);
            end loop;

            return Item_Sym;
         end;
      end if;
   end Lookup_Standalone_Item;

   function Region_Encloses_Region
     (Encloser : Region_Ptr;
      Enclosed : Region_Ptr)
      return Boolean
   is
      --  Return True if Encloser region matches or encloses Enclosed regions,
      --  treating interface and class defining a module as equivalent.
      Next_Outer : Region_Ptr := Enclosed;
   begin
      if Encloser = null then
         --  Null doesn't match anything
         return False;
      end if;
      while Next_Outer /= null loop
         if Next_Outer = Encloser
           or else (Next_Outer.Syms = Encloser.Syms
                    and then Next_Outer.Syms /= null)
         then
            --  Found a match
            return True;
         end if;

         Next_Outer := Next_Outer.Enclosing_Region;
      end loop;
      --  Not an encloser
      return False;
   end Region_Encloses_Region;

   function Region_At_Index (Index : Region_Index) return Region_Ptr is
   --  Return region with given index
      pragma Assert (Index /= No_Region_Index);
   begin
      return Region_Vectors.Nth_Element (Region_Table, Index);
   end Region_At_Index;

   procedure Assign_Region_Index (Region : Region_Ptr) is
   --  Assign unique index to region
      pragma Assert (Region.Index = No_Region_Index);
   begin
      Region_Vectors.Add_Element (Region_Table, Region, Region.Index);
   end Assign_Region_Index;

   function Generate_Unique_Label
     (Pos : Source_Positions.Source_Position;
      Preceding : String;
      Include_File : Boolean := False) return Strings.U_String is
   --  Return a U_String that is a concatenation of "Preceding" and
   --  "[FFFF_]LLLL_CCCC" where "FFFF" is a unique number assigned
   --  to the file, "LLLL" is the line number, and "CCCC" is the column
   --  number.

      --  This image includes a leading space
      Line_Image_Space : constant String :=
         Source_Positions.Line_Number'Image (Pos.Line);
      --  Remove the space
      Line_Image : String renames Line_Image_Space
         (Line_Image_Space'First + 1 .. Line_Image_Space'Last);
      --  Same with Column
      Col_Image_Space : constant String :=
         Source_Positions.Column_Number'Image (Pos.Col);
      Col_Image : String renames Col_Image_Space
         (Col_Image_Space'First + 1 .. Col_Image_Space'Last);
      Result : Strings.U_String;
      use type Strings.U_String;
   begin
      if not Include_File then
         Result := Strings.String_Lookup
                  (Preceding & Line_Image & '_' & Col_Image,
                   Must_Be_New => True);
      else
         declare
            File_Image_Space : constant String :=
              Strings.U_String_Index'Image (Pos.File);
            File_Image : String renames File_Image_Space
              (File_Image_Space'First + 1 .. File_Image_Space'Last);
         begin
            Result := Strings.String_Lookup
              (Preceding & File_Image & '_' & Line_Image & '_' & Col_Image,
               Must_Be_New => True);
         end;
      end if;
      if Result /= Strings.Null_U_String then
         return Result;
      else
         --  Oh dear, not unique.
         --  Try again with a (much) larger column number.
         declare
            Updated_Source_Pos : Source_Positions.Source_Position := Pos;
            use Source_Positions;
         begin
            Updated_Source_Pos.Col := (Pos.Col + 100) mod
                (Max_Column_Number + 1);
            return Generate_Unique_Label
                     (Updated_Source_Pos, Preceding, Include_File);
         end;
      end if;
   end Generate_Unique_Label;

   procedure Reset_Standard_Imports is
   --  When we switch to a new language, we need to
   --  reset the standard imports.
   begin
      Standard_Import_Clauses := Lists.Empty_List;
   end Reset_Standard_Imports;

begin
   --  Assign first region index to the library region
   Assign_Region_Index (Library_Region);
end PSC.Symbols;
