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

with PSC.Source_Positions;
with PSC.Stream_Output;
with PSC.String_Streams;

with PSC.Strings;  pragma Elaborate (PSC.Strings);
with PSC.Symbols;  pragma Elaborate (PSC.Symbols);

with PSC.Trees.For_Loop_Construct;
with PSC.Trees.Identifier;
with PSC.Trees.Invocation;
with PSC.Trees.Module;
with PSC.Trees.Param_Decl;
with PSC.Trees.Qualifier;
with PSC.Trees.Reference;
with PSC.Trees.Type_Decl;
with PSC.Trees.Unary;

with PSC.Trees.Semantics.Debug; use PSC.Trees.Semantics.Debug;
with PSC.Trees.Semantics.Static;

with Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

package body PSC.Trees.Semantics.Info is

   Debug_Operation_Ids : constant Boolean := False;

   Debug_Types : constant Boolean := False;

   function Type_Sem_To_Word_Ptr is new Ada.Unchecked_Conversion
     (Type_Sem_Ptr, Interpreter.Word_Ptr);

   ------------- Visible subprograms --------------

   function Sem_Image
     (Sem : access Type_Semantic_Info;
      Use_Short_Form : Boolean := False)
      return String is
   --  Dispatching op to return image of type identified by semantic info
   begin
      --  Just pass the buck to Type_Sem_Image
      return Type_Sem_Image (Root_Sem_Ptr (Sem), Use_Short_Form);
   end Sem_Image;

   function Elem_Index_Hash (Index : Type_Sem_Vectors.Elem_Index)
      return Hash_Type is
   --  Hash function for index into Type_Sem_Vector
   begin
      return Hash_Type (Index);
   end Elem_Index_Hash;

   function Type_Sem_Hash (Type_Sem : Type_Sem_Ptr) return Hash_Type is
   --  Hash function for Type_Sem_Ptr
   begin
      return Hash_Type (Type_Sem.Type_Index);
   end Type_Sem_Hash;

   function Next_Type_Index return Unique_Type_Index is
   --  Assign a unique type index
   begin
      Last_Type_Index := Last_Type_Index + 1;
      return Last_Type_Index;
   end Next_Type_Index;

   function Type_Is_Parameterized (Type_Sem : Type_Sem_Ptr) return Boolean is
   --  Return True if module defining type has parameters or
   --  if Type_Is_Parameterized (Type_Sem.Enclosing_Type).
   begin
      if Type_Sem = null then
         --  null type
         return False;
      elsif Type_Sem.Associated_Module = null then
         --  Some kind of error.
         return False;
      elsif Static.Num_Module_Parameters (Type_Sem.Associated_Module) > 0 then
         --  Module has parameters
         return True;
      else
         --  Recurse on enclosing type
         return Type_Is_Parameterized (Type_Sem.Enclosing_Type);
      end if;
   end Type_Is_Parameterized;

   function Types_Match (Type1, Type2 : Type_Sem_Ptr) return Boolean is
   --  Return True if Type1 and Type2 are value-equivalent
   begin
      if Type1 = Type2 then
         return True;
      elsif Type1 = null or else Type2 = null then
         return False;
      elsif Type1.U_Base_Type = Type2.U_Base_Type then
         return True;
      elsif Type1.Root_Type = Type2.Root_Type
        and then Type1.Root_Type /= null
      then
         --  Root types match, one must be polymorphic
         return True;
      elsif Type1.External_View /= null
        and then Type2.External_View /= null
        and then Type1.External_View.U_Base_Type =
           Type2.External_View.U_Base_Type
      then
         return True;
      else
         if Debug_Types then
            Put_Line (" types do not match: " &
              Type_Image (Type1) & " /= " &
              Type_Image (Type2));
         end if;
         return False;
      end if;
   end Types_Match;

   function Treat_As_Type_Indicator (Mod_Sem : Module_Sem_Ptr)
     return String is
   --  Return " (as type)" if Treat_As_Type is True for module; "" otherwise
   begin
      if Mod_Sem /= null and then Not_Null (Mod_Sem.Definition) then
         declare
            Mod_Tree : Module.Tree renames Module.Tree (Tree_Ptr_Of
              (Mod_Sem.Definition).all);
         begin
            if Mod_Tree.Treat_As_Type then
               return " (as type)";
            end if;
         end;
      end if;
      return "";
   end Treat_As_Type_Indicator;

   function Actual_Parameters_Hash
     (Base_Type : Type_Sem_Ptr)
      return Hash_Type
   is
      --  Hash on actual parameter U_Types of instantiation
      Type_Def : Tree'Class renames Tree_Ptr_Of (Base_Type.Definition).all;
      use type Invocation.Invocation_Kind_Enum;
      use type Hash_Type;
      Result : Hash_Type := 1 + Hash_Type (Base_Type.New_Type_Counter);
   begin
      if Base_Type.Func_Type_Op_Sem /= null then
         --  We have a func type
         return Result *
           Static.Hash_Tree (Base_Type.Func_Type_Op_Sem.Definition);
      elsif Type_Def not in Invocation.Tree
        or else Invocation.Tree (Type_Def).Kind /=
                Invocation.Module_Instantiation
        or else All_Nulls (Base_Type.Actual_Sem_Infos)
      then
         --  Must be the "current instance" of the module
         return Result;  --  Should match instantiation with no actuals
      else
         --  NOTE: We don't bother hashing the module since
         --       these hash tables are module specific.
         for I in Base_Type.Actual_Sem_Infos'Range loop
            --  Combine the hashes of the actuals
            if Base_Type.Actual_Sem_Infos (I) /= null then
               Result := Result * 61 +
                         Static.Hash_Tree
                            (Base_Type.Actual_Sem_Infos (I).Definition);
            end if;
         end loop;
         return Result;
      end if;

   end Actual_Parameters_Hash;

   function Same_Actual_Parameters
     (Base_Type1, Base_Type2 : Type_Sem_Ptr)
      return Boolean
   is
      --  True if same actual parameter U_Types in instantiations.

      --  Note that we don't check that the associated modules match
      --  because the only time they *don't* match is when we intentionally
      --  insert a type into the table because we are parsing multiple
      --  languages, and we want them all to use the same Univ_*
      --  types, Any, Boolean, etc.
   begin
      if Base_Type1 = Base_Type2 then
         return True;
      elsif Base_Type1.Associated_Module = null
        or else Base_Type2.Associated_Module = null
      then
         --  Weird -- no associated module
         if Debug_Second_Pass then
            Put_Line (" Module null in ""Same_Actual_Parameters""");
         end if;
         return False;
      elsif Base_Type1.Enclosing_Type /= Base_Type2.Enclosing_Type
        and then Type_Is_Parameterized (Base_Type1.Enclosing_Type)
        and then not Types_Match (Base_Type1.Enclosing_Type,
          Base_Type2.Enclosing_Type)
      then
         if Debug_Matching then
            Put_Line
              (" Enclosing type mismatch " &
               "in ""Same_Actual_Parameters"":" &
               Type_Image (Base_Type1.Enclosing_Type) &
               " /= " &
               Type_Image (Base_Type2.Enclosing_Type));
         end if;
         return False;
      elsif Base_Type1.Associated_Generic_Op /=
            Base_Type2.Associated_Generic_Op
      then
         if Debug_Matching then
            Put_Line
              (" Associated_Generic_Op mismatch " &
               "in ""Same_Actual_Parameters"":" &
               Type_Image (Base_Type1) &
               " (AGO=" &
               Sym_Name (Base_Type1.Associated_Generic_Op) &
               ") /= " &
               Type_Image (Base_Type2) &
               " (AGO=" &
               Sym_Name (Base_Type2.Associated_Generic_Op) &
               ")");
         end if;
         return False;

      elsif Base_Type1.New_Type_Counter /= Base_Type2.New_Type_Counter then
         --  Types differ in count of "new"s applied
         if Debug_Matching then
            Put_Line
              (" New_Type_Counter mismatch " &
               "in ""Same_Actual_Parameters"":" &
               Type_Image (Base_Type1) &
               " (NTC =" &
               New_Type_Count_Type'Image (Base_Type1.New_Type_Counter) &
               ") /= " &
               Type_Image (Base_Type2) &
               " (NTC =" &
               New_Type_Count_Type'Image (Base_Type2.New_Type_Counter) &
               ")");
         end if;
         return False;
      elsif Base_Type1.Is_Polymorphic /= Base_Type2.Is_Polymorphic then
         --  One is polymorphic and the other isn't
         --  NOTE: As of 10-Jan-2016 we consider a polymophic type
         --        as being a different base type from the root type.
         return False;
      elsif Base_Type1.Func_Type_Op_Sem /= null
        and then Base_Type2.Func_Type_Op_Sem /= null
      then
         --  Compare two func types
         if Base_Type1.Func_Type_Op_Sem = Base_Type2.Func_Type_Op_Sem then
            return True;
         else
            return Static.Signatures_And_Modes_Match
              (Base_Type1.Func_Type_Op_Sem, Base_Type2.Func_Type_Op_Sem);
         end if;
      end if;

      --  Compare actual parameters
      if Base_Type1.Is_Formal_Type
        or else Base_Type2.Is_Formal_Type
      then
         if Debug_Matching then
            Put_Line
              (" Comparing Base_Type1 " &
               Type_Image (Base_Type1) &
               " vs. Base_Type2 " &
               Type_Image (Base_Type2) &
               " at least one is formal");
         end if;
         return False;
      else
         --  Instances of the same module
         declare
            Actuals1 : constant Sem_Info_Array_Ptr :=
              Base_Type1.Actual_Sem_Infos;
            Actuals2 : constant Sem_Info_Array_Ptr :=
              Base_Type2.Actual_Sem_Infos;
         begin
            if All_Nulls (Actuals1) or else All_Nulls (Actuals2) then
               if Debug_Matching then
                  Put_Line
                    (" One or both of the actual_sem_infos are null " &
                     Type_Image (Base_Type1) &
                     " vs. " &
                     Type_Image (Base_Type2));
               end if;
               return All_Nulls (Actuals1) = All_Nulls (Actuals2);
            elsif Actuals1'Length /= Actuals2'Length then
               if Debug_Second_Pass then
                  Put_Line
                    (" Actuals not same length for " &
                     Type_Image (Base_Type1) &
                     " vs. " &
                     Type_Image (Base_Type2));
               end if;
               return False;
            elsif Actuals1'Length /=
              Static.Num_Module_Parameters (Base_Type1.Associated_Module)
            then
               if Debug_Second_Pass then
                  Put_Line
                    (" Actuals not same length as formals for " &
                     Type_Image (Base_Type1) &
                     " vs. " &
                     Type_Image (Base_Type2));
               end if;
               return False;
            else
               if Debug_Matching then
                  Put_Line
                    (" Comparing actuals in ""Same_Actual_Parameters"":");
               end if;
               for I in Actuals1'Range loop
                  --  Compare the Actuals
                  if Actuals1 (I) = null or else Actuals2 (I) = null then
                     if Actuals1 (I) /= Actuals2 (I) then
                        if Debug_Second_Pass then
                           Put_Line
                             (" Missing actual" &
                              Integer'Image (I) &
                              " in " &
                              Type_Image (Base_Type1) &
                              " vs. " &
                              Type_Image (Base_Type2));
                        end if;
                        return False;
                     end if;
                  elsif not Static.Equiv_Tree
                              (Actuals1 (I).Definition,
                               Actuals2 (I).Definition)
                  then
                     if Debug_Matching then
                        Put_Line
                          (" Mismatched Actual " &
                           Subtree_Image (Actuals1 (I).Definition) &
                           " in " &
                           Type_Image (Base_Type1) &
                           " vs. " &
                           Subtree_Image (Actuals2 (I).Definition) &
                           " in " &
                           Type_Image (Base_Type2));
                     end if;
                     return False;
                  end if;
               end loop;
               --  All actuals are equivalent
               return True;
            end if;
         end;
      end if;

   end Same_Actual_Parameters;

   function Base_And_Constraints_Hash
     (Constrained_Type : Type_Sem_Ptr)
      return Hash_Type
   is
      --  Hash on base type and constraints
      use type Hash_Type;
      Result : Hash_Type :=
        Hash_Type (Constrained_Type.U_Base_Type.Type_Index) * 61;
   begin
      for I in 1 .. Lists.Length (Constrained_Type.Constraint_Annotations)
      loop
         --  Add in hash of each constraint.
         --  Don't multiply Result in between to preserve order independence.
         Result := Result +
                   Static.Hash_Tree
                      (Lists.Nth_Element
                          (Constrained_Type.Constraint_Annotations,
                           I));
      end loop;

      --  Include "Is_Optional," "Is_Concurrent," and "Is_Polymorphic" in hash.
      Result := Result * 61 +
                Boolean'Pos (Constrained_Type.Value_Is_Optional) +
                2 * Boolean'Pos (Constrained_Type.Obj_Is_Concurrent) +
                4 * Boolean'Pos (Constrained_Type.Is_Polymorphic);

      return Result;
   end Base_And_Constraints_Hash;

   function Same_Base_And_Constraints
     (Constrained_Type1, Constrained_Type2 : Type_Sem_Ptr)
      return Boolean
   is
   --  True if same base type and constraints
   begin
      if Constrained_Type1 = Constrained_Type2 then
         return True;
      elsif Constrained_Type1.U_Base_Type /=
            Constrained_Type2.U_Base_Type
      then
         --  Different base types
         return False;
      elsif Constrained_Type1.Value_Is_Optional /=
            Constrained_Type2.Value_Is_Optional
        or else Constrained_Type1.Obj_Is_Concurrent /=
                Constrained_Type2.Obj_Is_Concurrent
        or else Constrained_Type1.Is_Polymorphic /=
                Constrained_Type2.Is_Polymorphic
      then
         --  Different optionality/concurrency/polymorphism
         return False;
      elsif Lists.Length (Constrained_Type1.Constraint_Annotations) /=
            Lists.Length (Constrained_Type2.Constraint_Annotations)
      then
         --  Different number constraints
         return False;
      else
         if Debug_Matching then
            Put_Line
              (" Comparing constraints in ""Same_Base_And_Constraints"":");
         end if;
         --  Do pair wise compares
         for I in
              1 .. Lists.Length (Constrained_Type1.Constraint_Annotations)
         loop
            declare
               Ann1 : constant Optional_Tree :=
                 Lists.Nth_Element
                    (Constrained_Type1.Constraint_Annotations,
                     I);
               In_List2 : Boolean := False;
            begin
               for J in
                    1 ..
                    Lists.Length (Constrained_Type2.Constraint_Annotations)
               loop
                  declare
                     Ann2 : constant Optional_Tree :=
                       Lists.Nth_Element
                          (Constrained_Type2.Constraint_Annotations,
                           J);
                  begin
                     if Static.Equiv_Tree (Ann1, Ann2) then
                        In_List2 := True;
                        exit;
                     end if;
                  end;
               end loop;

               if not In_List2 then
                  --  Lists not the same
                  return False;
               end if;
            end;
         end loop;
         return True;
      end if;
   end Same_Base_And_Constraints;

   function Find_U_Base_Type (Some_Type : Type_Sem_Ptr) return Type_Sem_Ptr is
      --  Find unique base type for given type
      Existing_Base_Type : U_Base_Type_Tables.Element_Ref;
      use type U_Base_Type_Tables.Element_Ref;
   begin
      if Some_Type.Associated_Module = null then
         --  Some earlier error
         if Debug_Second_Pass then
            Put_Line (" Find_U_Base_Type Associated_Module is null");
         end if;
         Some_Type.U_Base_Type := Some_Type;
         return Some_Type;
      end if;

      if Debug_Second_Pass
        and then not Some_Type.Associated_Module.Is_Interface
      then
         Put_Line
           (" Find_U_Base_Type Associated_Module " &
            Sym_Name (Some_Type.Associated_Module.Associated_Symbol) &
            " not an interface ");
      end if;

      U_Base_Type_Tables.Enter_Element
        (Some_Type.Associated_Module.U_Base_Types,
         Some_Type,
         Some_Type,
         Existing_Base_Type);
      if Existing_Base_Type /= null then
         --  There is already an equivalent base type
         return Existing_Base_Type.all;
      else
         --  This is the first base type with these parameters
         if Debug_Second_Pass then
            Put_Line
              (" New base type " &
               Type_Image (Some_Type) &
               " for module " &
               Sym_Name (Some_Type.Associated_Module.Associated_Symbol) &
               Treat_As_Type_Indicator (Some_Type.Associated_Module));
         end if;
         return Some_Type;
      end if;
   end Find_U_Base_Type;

   function Find_U_Type (Some_Type : Type_Sem_Ptr) return Type_Sem_Ptr is
      --  Find unique type equivalent to given type
      Existing_U_Type : U_Type_Tables.Element_Ref;
      use type U_Type_Tables.Element_Ref;
   begin
      if Some_Type.Associated_Module = null then
         --  Some earlier error
         if Debug_Second_Pass then
            Put_Line (" Find_U_Type Associated_Module is null");
         end if;
         Some_Type.U_Type := Some_Type;
         return Some_Type;
      end if;

      if Debug_Second_Pass
        and then not Some_Type.Associated_Module.Is_Interface
      then
         Put_Line
           (" Find_U_Type Associated_Module " &
            Sym_Name (Some_Type.Associated_Module.Associated_Symbol) &
            " not an interface ");
      end if;

      U_Type_Tables.Enter_Element
        (Some_Type.Associated_Module.U_Types,
         Some_Type,
         Some_Type,
         Existing_U_Type);
      if Existing_U_Type /= null then
         --  There is already an equivalent type in the table
         return Existing_U_Type.all;
      else
         --  This is the first type with this base and constraints
         if Debug_Second_Pass then
            Put_Line
              (" New U type " &
               Type_Image (Some_Type) &
               " for module " &
               Sym_Name (Some_Type.Associated_Module.Associated_Symbol) &
               Treat_As_Type_Indicator (Some_Type.Associated_Module));
         end if;
         if Some_Type.Is_Polymorphic then
            --  Remember that we will need a polymorphic type descriptor
            --  TBD: This is not very useful.  A polymorphic type descriptor
            --       must have an associated formal type.
            --       We really need a mapping.
            Some_Type.Associated_Module.Needs_Polymorphic_Type_Desc := True;
         end if;
         return Some_Type;
      end if;
   end Find_U_Type;

   function All_Nulls
     (Actual_Sem_Infos : Sem_Info_Array)
      return Boolean
   is
   --  Return True if Actual_Sem_Infos is of zero length,
   --  or all elements are null.
   begin
      for I in Actual_Sem_Infos'Range loop
         if Actual_Sem_Infos (I) /= null then
            return False;
         end if;
      end loop;
      --  All nulls
      return True;
   end All_Nulls;

   function All_Nulls
     (Actual_Sem_Infos : Sem_Info_Array_Ptr)
      return Boolean
   is
   --  Return True if Actual_Sem_Infos is null,
   --  or zero length, or all elements are null.
   begin
      return Actual_Sem_Infos = null or else All_Nulls (Actual_Sem_Infos.all);
   end All_Nulls;

   function Some_Nulls
     (Actual_Sem_Infos : Sem_Info_Array)
      return Boolean
   is
   --  Return True if Actual_Sem_Infos is
   --  of zero length, or some of its elements are null.
   begin
      if Actual_Sem_Infos'Length = 0 then
         return True;
      else
         for I in Actual_Sem_Infos'Range loop
            if Actual_Sem_Infos (I) = null then
               return True;
            end if;
         end loop;
         --  All non-null
         return False;
      end if;

   end Some_Nulls;

   function Some_Nulls
     (Actual_Sem_Infos : Sem_Info_Array_Ptr)
      return Boolean
   is
   --  Return True if Actual_Sem_Infos is null,
   --  or zero length, or some of its elements are null.
   begin
      return Actual_Sem_Infos = null or else Some_Nulls (Actual_Sem_Infos.all);
   end Some_Nulls;

   function U_Base_Type_Region
     (Obj_Type : Type_Sem_Ptr)
      return Type_Region_Ptr
   is
   --  Return unique base type of type
   --  This strips away any constraint on top of a module instantiation,
   --  and returns a unique value for all instantiations with equivalent
   --  parameter types.  Each formal type get its own unique base type.
   begin
      if Obj_Type = null then
         return null;
      else
         return Type_Region_Ptr (Obj_Type.Root_Type);
      end if;
   end U_Base_Type_Region;

   function Underlying_Sem_Info (Orig_Sem : Sem_Ptr) return Sem_Ptr is
      --  Return Orig_Sem, unless Orig_Sem is a Sym_Reference_Info,
      --  in which case we return the Sem_Info of the Associated_Symbol.
      use type Symbols.Sym_Ptr;
      Sem : Sem_Ptr := Orig_Sem;
   begin
      while Sem /= null loop
         declare
            U_Sem : Sem_Ptr;
         begin
            --  Get underlying sem info if there is one
            if Sem.all in Sym_Reference_Info'Class then
               --  Get sem info from designated entity
               U_Sem := Sym_Reference_Info (Sem.all).Underlying_Sem_Info;
            else
               --  Return sem info of Tree
               exit;
            end if;

            exit when U_Sem = null or else U_Sem = Sem;
            --  Not making progress, return now

            --  Keep looking for underlying info
            Sem := U_Sem;
         end;
      end loop;
      --  End of the line
      return Sem;
   end Underlying_Sem_Info;

   function Underlying_Sem_Info (OT : Optional_Tree) return Sem_Ptr is
   --  Return Sem_Info of OT, unless OT is a Sym_Reference_Info,
   --  in which case we return the Sem_Info of the Associated_Symbol.
   begin
      return Underlying_Sem_Info (Sem_Ptr (Sem_Info (OT)));
   end Underlying_Sem_Info;

   function Underlying_Op_Sem_Info (Orig_Sem : Sem_Ptr) return Sem_Ptr is
      --  Return Orig_Sem, unless Orig_Sem is a Sym_Reference_Info,
      --  in which case we return the Sem_Info of the Associated_Symbol,
      --  or in case Orig_Sem is a Resolved_Operation_Info, in which
      --  case we return the underlying Operation_Info.
      U_Sem : constant Sem_Ptr := Underlying_Sem_Info (Orig_Sem);
   begin
      if U_Sem.all in Resolved_Operation_Info then
         --  Get sem info from underlying operation
         return Underlying_Op_Sem_Info (Sem_Ptr
           (Resolved_Operation_Info (U_Sem.all).Op_Sem));
      else
         return U_Sem;
      end if;
   end Underlying_Op_Sem_Info;

   function Underlying_Op_Sem_Info (OT : Optional_Tree) return Sem_Ptr is
   --  Return Sem_Info of OT, unless OT is a Sym_Reference_Info,
   --  in which case we return the Sem_Info of the Associated_Symbol,
   --  or in case Sem_Info is a Resolved_Operation_Info, in which
   --  case we return the underlying Operation_Info.
   begin
      return Underlying_Op_Sem_Info (Sem_Ptr (Sem_Info (OT)));
   end Underlying_Op_Sem_Info;

   function Follow_Op_Equiv (Op_Sem : Operation_Sem_Ptr)
     return Operation_Sem_Ptr is
   --  If Op_Sem.Equiv_To /= null, then return Op_Sem at end of chain
   --  of Equiv_To's.
      Result : Operation_Sem_Ptr := Op_Sem;
   begin
      while Result.Equiv_To /= null loop
         Result := Result.Equiv_To;
      end loop;
      return Result;
   end Follow_Op_Equiv;

   function Resolved_Type (OT : Optional_Tree) return Type_Sem_Ptr is
      --  Return resolved type for given optional tree, if determined
      Tree_Sem : constant Sem_Ptr := Underlying_Sem_Info (OT);
   begin
      if Tree_Sem /= null then
         if Tree_Sem.all in Operand_Semantic_Info'Class then
            return Operand_Sem_Ptr (Tree_Sem).Resolved_Type;
         elsif Tree_Sem.all in Type_Semantic_Info'Class then
            return Type_Sem_Ptr (Tree_Sem);
         end if;
      end if;
      return null;
   end Resolved_Type;

   function Resolved_Tree (T : Optional_Tree) return Optional_Tree is
      --  Return Resolved interp if present, else return original tree
      T_Sem : constant Sem_Ptr := Sem_Ptr (Sem_Info (T));
   begin
      if T_Sem /= null
        and then T_Sem.all in Operand_Semantic_Info'Class
        and then Not_Null (Operand_Sem_Ptr (T_Sem).Resolved_Interp)
      then
         --  Return the resolved tree
         return Operand_Sem_Ptr (T_Sem).Resolved_Interp;
      else
         --  Return the original tree
         return T;
      end if;
   end Resolved_Tree;

   procedure Visit_Resolved
     (T : Optional_Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
      --  If T has Sem_Info that has a Resolved_Interp, then
      --  visit that instead of T itself.

      T_Copy : Optional_Tree := Resolved_Tree (T);
   begin
      Visit (T_Copy, Visitor);
   end Visit_Resolved;

   function Sym_Name (Id : Identifier.Tree) return String is
   --  Return string representation of identifier
   begin
      return Strings.To_String (Id.Str);
   end Sym_Name;

   function Initial_Value_Operand (Loop_Body : Optional_Tree;
     Num_Found : access Natural) return Optional_Tree is
   --  Return operand of Initial_Value_Op found within loop body.
   --  Bump up Num_Found for each one found; caller should init to zero.
   --  Return Null_Optional_Tree if none found.
   begin
      if Is_Null (Loop_Body) then
         return Null_Optional_Tree;
      else
         declare
            Body_Tree : Trees.Tree'Class renames
              Tree_Ptr_Of (Resolved_Tree (Loop_Body)).all;
            Result : Optional_Tree;
            use type Unary.Unary_Operator_Enum;
            use type For_Loop_Construct.For_Loop_Kind_Enum;
         begin
            if Body_Tree in For_Loop_Construct.Tree
              and then For_Loop_Construct.Tree (Body_Tree).Kind =
               For_Loop_Construct.Map_Reduce_Expr
            then
               --  Found a nested map-reduce expr; don't look inside it
               return Null_Optional_Tree;

            elsif Body_Tree in Unary.Tree
              and then
                Unary.Tree (Body_Tree).Operator = Unary.Initial_Value_Op
            then
               --  Is an initial value operand (<...>)
               Result := Resolved_Tree (Unary.Tree (Body_Tree).Operand);
               Num_Found.all := Num_Found.all + 1;

            elsif Body_Tree in Reference.Tree then
               --  Skip over the "Key" of the Key => Referent notation.
               --  This is important because within a pattern, we use
               --  <A> as a wildcard that introduces the identifier "A".
               return Initial_Value_Operand
                        (Reference.Tree (Body_Tree).Referent, Num_Found);
            end if;

            for I in 1 .. Num_Operands (Body_Tree) loop
               declare
                  Init_Val : constant Optional_Tree :=
                    Initial_Value_Operand
                      (Nth_Operand (Body_Tree, I), Num_Found);
               begin
                  if Not_Null (Init_Val) then
                     --  Remember initial-value operand; overwrite
                     --  any earlier one so error message is more useful
                     --  if Num_Found ends up > 1.
                     Result := Init_Val;
                  end if;
               end;
            end loop;
            return Result;
         end;
      end if;
   end Initial_Value_Operand;

   function Contains_Initial_Value_Operand (Loop_Body : Optional_Tree)
     return Boolean is
   --  Return True if Loop_Body contains an initial-value operand
      Num_Found : aliased Natural := 0;
   begin
      return Not_Null (Initial_Value_Operand (Loop_Body, Num_Found'Access));
   end Contains_Initial_Value_Operand;

   procedure Put_Resolved_Param_Decls
      (Str : access Ada.Streams.Root_Stream_Type'Class; Params : Lists.List) is
   --  Display sequence of param decls, separated by ";"
   --  but display resolved type rather than original type.
      use Stream_Output;
      Num_Params : constant Natural := Lists.Length (Params);
   begin
      for I in 1 .. Num_Params loop
         declare
            Elem : constant Optional_Tree := Lists.Nth_Element (Params, I);
            Elem_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Elem).all;
         begin
            if Elem_Tree not in Param_Decl.Tree then
               --  Fall back to using Display_Subtree
               Display_Subtree (Elem_Tree, Str);
            else
               declare
                  Param_Tree : Param_Decl.Tree renames
                    Param_Decl.Tree (Elem_Tree);
                  Resolved_Param_Type : Type_Sem_Ptr := Resolved_Type (Elem);
                  Unresolved_Param_Type : constant Type_Sem_Ptr :=
                    Type_Sem_Ptr (Underlying_Sem_Info (Param_Tree.Param_Type));
               begin
                  Put (Str, Param_Decl.Param_Kind_Image
                    (Param_Tree.Kind, Param_Tree.Locking));
                  if Not_Null (Param_Tree.Name) then
                     Display_Subtree (Param_Tree.Name, Str);
                     Put (Str, " : ");
                  end if;

                  if Param_Tree.Is_Optional then
                     Put (Str, "optional ");
                  end if;

                  if Resolved_Param_Type = null then
                     Resolved_Param_Type :=
                      Resolved_Type (Param_Tree.Param_Type);
                  elsif Resolved_Param_Type /=
                    Resolved_Type (Param_Tree.Param_Type)
                  then
                     --  Weird, a mismatch
                     if Debug_Second_Pass then
                        Put_Line ("Put_Resolved_Param_Decls: " &
                          "Resolved_Type (Param) = " &
                          Type_Image (Resolved_Param_Type) &
                          " but Resolved_Type (Param_Type) = " &
                          Type_Image (Resolved_Type (Param_Tree.Param_Type)));
                     end if;
                     Resolved_Param_Type :=
                       Resolved_Type (Param_Tree.Param_Type);
                  end if;

                  if Resolved_Param_Type /= null
                    and then (Unresolved_Param_Type = null
                      or else Resolved_Param_Type.U_Base_Type /=
                        Unresolved_Param_Type.U_Base_Type)
                  then
                     --  It is worth displaying the resolved type
                     Put (Str, Canonical_Type_Name (Resolved_Param_Type));
                  else
                     --  Fall back on display subtree
                     Display_Subtree (Param_Tree.Param_Type, Str);
                  end if;

               end;
            end if;

            if I /= Num_Params then
               --  Separate with semicolons
               Put (Str, "; ");
            end if;
         end;
      end loop;
   end Put_Resolved_Param_Decls;

   function Type_Image
     (Expr_Type : Type_Sem_Ptr;
      Use_Short_Form : Boolean := True;
      Max_Chars : Positive := 2000)
      return String
   is
      --  Return image of type

      function Type_Prefix (Parens : Boolean := False) return String is
      --  Return "<enclosing_type>::" if Expr_Type.Enclosing_Type /= null
      begin
         if Expr_Type.Enclosing_Type /= null then
            if Parens then
               --  Parenthesize to indicate may be redundant
               return '(' &
                      Type_Image
                         (Expr_Type.Enclosing_Type,
                          Use_Short_Form => True, Max_Chars => Max_Chars) &
                      Languages.Module_Name_Separator &
                      ')';
            else
               return Type_Image
                         (Expr_Type.Enclosing_Type,
                          Use_Short_Form => True, Max_Chars => Max_Chars) &
                      Languages.Module_Name_Separator;
            end if;
         else
            return "";
         end if;
      end Type_Prefix;

      function Opt_Con return String is
      --  add "optional" and/or "concurrent" prefix
      begin
         if Expr_Type = null then
            return "";
         elsif Expr_Type.Value_Is_Optional then
            if Expr_Type.Obj_Is_Concurrent and then not Use_Short_Form then
               return "optional concurrent ";
            else
               return "optional ";
            end if;
         elsif Expr_Type.Obj_Is_Concurrent and then not Use_Short_Form then
            return "concurrent ";
         else
            return "";
         end if;
      end Opt_Con;

      function Unassignable return String is
      --  Return suffix to identify unassignable types
      begin
         if Expr_Type /= null
           and then not Expr_Type.Known_To_Be_Assignable
         then
            return " (unassignable)";
         else
            return "";
         end if;
      end Unassignable;

      function Is_Poly return String is
      --  Add "+" if is polymorphic
      begin
         if Expr_Type = null or else not Expr_Type.Is_Polymorphic then
            return "";
         else
            return "+";
         end if;
      end Is_Poly;

      function Type_Qualifier_Basic return String is
         --  Return suffix to identify universal and formal types
         use type Type_Sem_Vectors.Elem_Index;
      begin
         if Use_Short_Form or else Expr_Type = null then
            return "";
         elsif Expr_Type.Is_Formal_Type then
            if Expr_Type.Formal_Prefix /= Null_Optional_Tree then
               return " (formal with prefix " &
                      Subtree_Image (Expr_Type.Formal_Prefix,
                        Use_Short_Form, Max_Chars) &
                      ")" &
                      Unassignable;
            else
               return " (formal)" & Unassignable;
            end if;
         elsif Expr_Type.Nested_Type_Index > 0 then
            return " (nested)" & Unassignable;
         elsif not Expr_Type.All_Parameters_Known then
            if Expr_Type.Associated_Module /= null
              and then Expr_Type.Associated_Module.Cur_Inst_Sem =
                       Expr_Type.U_Base_Type
            then
               return " (cur-inst)" & Unassignable;
            else
               return " (incomplete?)" & Unassignable;
            end if;
         elsif Expr_Type.Is_Universal then
            return " (univ)";
         elsif Expr_Type.Known_To_Be_Small then
            return " (small)" & Unassignable;
         elsif Expr_Type.Associated_Module /= null
           and then Expr_Type.Associated_Module.Is_Concurrent
         then
            return " (concurrent)" & Unassignable;
         else
            return Unassignable;
         end if;
      end Type_Qualifier_Basic;

      function Type_Qualifier return String is
         --  Add in the address in Hex when debugging to result
         --  of Type_Qualifier_Basic.
      begin
         if Debug_Types and then Expr_Type /= null then
            declare
               Expr_Addr : constant Interpreter.Word_Ptr :=
                 Type_Sem_To_Word_Ptr (Expr_Type);
            begin
               return Type_Qualifier_Basic & " (" &
                 Interpreter.Hex_Image (Expr_Addr) & ')';
            end;
         else
            return Type_Qualifier_Basic;
         end if;
      end Type_Qualifier;

      function New_Type_Name (New_Type : Type_Sem_Ptr) return String is
      --  Return name used to declare a "new" type.
      begin
         if New_Type.U_Base_Type /= null
           and then New_Type /= New_Type.U_Base_Type
         then
            --  Recurse on base type
            return New_Type_Name (New_Type.U_Base_Type);
         elsif New_Type.Associated_Symbol = null then
            --  This shouldn't happen!
            return "<anon>";
         else
            --  Return name used to declare type
            return Sym_Name (New_Type.Associated_Symbol);
         end if;
      end New_Type_Name;

      function Func_Type_Image
        (Func_Type : Type_Sem_Ptr;
         Max_Chars : Positive := 2000)
         return String
      is
      --  Return string representing Func_Type, using Resolved_Type of
      --  parameters.
         Op_Tree : Operation.Tree renames
           Operation.Tree (Tree_Ptr_Of (Func_Type.Definition).all);
         use String_Streams;
         use Stream_Output;
         Str : aliased String_Stream
           (Ada.Streams.Stream_Element_Offset
              (Max_Chars * Character'Size / Ada.Streams.Stream_Element'Size));
      begin
         --  Put image into the stream
         Put (Str'Access, "func (");
         Put_Resolved_Param_Decls (Str'Access, Op_Tree.Operation_Inputs);
         case Lists.Length (Op_Tree.Operation_Outputs) is
            when 0 =>
               --  No outputs
               Put (Str'Access, ")");
            when 1 =>
               --  One output, no need to parenthesize
               Put (Str'Access, ") -> ");
               Put_Resolved_Param_Decls
                 (Str'Access, Op_Tree.Operation_Outputs);
            when others =>
               --  Parenthesize outputs
               Put (Str'Access, ") -> (");
               Put_Resolved_Param_Decls
                 (Str'Access, Op_Tree.Operation_Inputs);
               Put (Str'Access, ")");
         end case;

         --  Get stream as a string
         return String_Of (Str'Access);
      end Func_Type_Image;

      use type Symbols.Sym_Ptr;

   begin  --  Type_Image

      if Expr_Type = null then
         return "null type";
      elsif Expr_Type.New_Type_Counter > 0
        and then Expr_Type.U_Base_Structure /= null
      then
         --  a "new" type
         if Expr_Type.Parent_Type /= null
           and then Expr_Type.Parent_Type.New_Type_Counter > 0
         then
            --  Derived from another "new" type
            return Opt_Con & Type_Prefix (Parens => True) &
              New_Type_Name (Expr_Type) & " is new #" &
              New_Type_Count_Type'Image (Expr_Type.New_Type_Counter) & ' ' &
              New_Type_Name (Expr_Type.Parent_Type) &
              Type_Qualifier;
         else
            --  Derived directly from its "structural" type
            return Opt_Con & Type_Prefix (Parens => True) &
              New_Type_Name (Expr_Type) & " is new #" &
              New_Type_Count_Type'Image (Expr_Type.New_Type_Counter) & ' ' &
              Subtree_Image (Expr_Type.U_Base_Structure.Definition,
                Use_Short_Form, Max_Chars - 8) &
              Type_Qualifier;
         end if;
      elsif Not_Null (Expr_Type.Definition)
        and then
          Tree_Ptr_Of (Expr_Type.Definition).all in Invocation.Tree'Class
      then
         return Type_Prefix (Parens => True) &
                Subtree_Image (Expr_Type.Definition, Use_Short_Form,
                  Max_Chars) &
                Type_Qualifier;
      elsif Not_Null (Expr_Type.Definition)
        and then Tree_Ptr_Of (Expr_Type.Definition).all in Type_Decl.Tree'
           Class
      then
         if Type_Decl.Tree (Tree_Ptr_Of (Expr_Type.Definition).all).Is_New_Type
           or else
             (Expr_Type.U_Type = null and then Expr_Type.U_Base_Type = null)
         then
            --  This is a "new" type, or U_Type/U_Base_Type not filled in.
            return Type_Prefix (Parens => True) &
              Subtree_Image (Expr_Type.Definition, Use_Short_Form, Max_Chars) &
              Type_Qualifier;
         elsif Expr_Type.U_Type /= Expr_Type then
            --  Type declaration, get underlying type
            return Type_Image (Expr_Type.U_Type, Use_Short_Form, Max_Chars);
         else
            --  Type declaration, get base type
            return
              Type_Image (Expr_Type.U_Base_Type, Use_Short_Form, Max_Chars);
         end if;
      elsif Not_Null (Expr_Type.Definition)
        and then Tree_Ptr_Of (Expr_Type.Definition).all in Operation.Tree'Class
      then
         return Func_Type_Image (Expr_Type, Max_Chars);
      elsif Not_Null (Expr_Type.Definition)
        and then Tree_Ptr_Of (Expr_Type.Definition).all in Qualifier.Tree'Class
      then
         --  Type with a qualifier, get unqualified type and add
         --  "optional" or "concurrent" on the front, and "+" on the
         --  back.
         declare
            Operand_Sem : constant Sem_Ptr :=
              Underlying_Sem_Info
                (Qualifier.Tree
                   (Tree_Ptr_Of (Expr_Type.Definition).all).Operand);
         begin
            if Operand_Sem.all in Type_Semantic_Info then
               return Opt_Con &
                   Type_Image
                      (Type_Sem_Ptr (Operand_Sem),
                       Use_Short_Form => True, Max_Chars => Max_Chars) &
                   Is_Poly &
                   Type_Qualifier;
            else
               --  Underlying entity is not a type, so this is
               --  perhaps an optional func type, so just fall back on
               --  subtree image.
               return Subtree_Image (Expr_Type.Definition);
            end if;
         end;
      elsif Expr_Type.Associated_Module /= null
        and then Expr_Type.Associated_Module.Associated_Symbol /= null
      then
         return Type_Prefix &
                Sym_Name (Expr_Type.Associated_Module.Associated_Symbol) &
                "<...>" &
                Type_Qualifier;
      elsif Expr_Type.Associated_Symbol /= null then
         return Type_Prefix &
                Sym_Name (Expr_Type.Associated_Symbol) &
                " (unknown module)" &
                Type_Qualifier;
      else
         return Type_Prefix & "(unknown type)" & Type_Qualifier;
      end if;
   end Type_Image;

   function Non_Module_Encloser_Names
     (Decl_Region : Region_Ptr)
     return String is
      --  Return "X.Y." where X and Y are the unique names
      --  for Decl_Region and each Enclosing_Region thereof
      --  which is not a module
      use Symbols;
   begin
      if Decl_Region = null
        or else Decl_Region.Associated_Symbol = null
        or else Decl_Region.Associated_Symbol.Kind = Module_Sym_Kind
      then
         return "";
      else
         declare
            Enclosing_Region : Region_Ptr := Decl_Region.Enclosing_Region;
         begin
            while Enclosing_Region /= null
              and then Enclosing_Region.Associated_Symbol =
                          Decl_Region.Associated_Symbol
            loop
               --  Skip region with same associated symbol as Decl_Region
               Enclosing_Region := Enclosing_Region.Enclosing_Region;
            end loop;

            return Non_Module_Encloser_Names (Enclosing_Region) &
              Sym_Name (Decl_Region.Associated_Symbol) &
              Languages.Module_Name_Separator;
         end;
      end if;
   end Non_Module_Encloser_Names;

   function Canonical_Type_Name (Expr_Type : Type_Sem_Ptr) return String is
   --  Return a name that is unique and canonical for the given type.
   begin
      if Expr_Type = null then
         return "null type";
      elsif Expr_Type.U_Base_Type /= Expr_Type
        and then Expr_Type.U_Base_Type /= null
      then
         --  Go to base type, but indicate whether concurrent
         if Expr_Type.Obj_Is_Concurrent and then
           not Expr_Type.U_Base_Type.Obj_Is_Concurrent
         then
            --  This type is concurrent, but base type isn't
            return "concurrent " &
                     Canonical_Type_Name (Expr_Type.U_Base_Type);
         else
            --  Just use base type name
            return Canonical_Type_Name (Expr_Type.U_Base_Type);
         end if;
      elsif Expr_Type.Is_Polymorphic then
         --  Add a '+' after canonical name of root type.
         return Canonical_Type_Name (Expr_Type.Root_Type) & '+';
      elsif Expr_Type.Associated_Module = null then
         return Type_Image (Expr_Type);  --  Not fully defined
      elsif Expr_Type.New_Type_Counter > 0
        and then Expr_Type.U_Base_Structure /= null
      then
         --  Type is a "new" type, which means the type is defined
         --  by its name and the enclosing module.
         return Canonical_Type_Name (Expr_Type.Enclosing_Type) &
           Languages.Module_Name_Separator &
           Non_Module_Encloser_Names
             (Expr_Type.Associated_Symbol.Enclosing_Region) &
           Sym_Name (Expr_Type.Associated_Symbol);
      elsif Not_Null (Expr_Type.Definition)
        and then Tree_Ptr_Of (Expr_Type.Definition).all in Operation.Tree'Class
      then
         --  An operation type; fall back to Type_Image
         return Type_Image (Expr_Type);
      elsif Some_Nulls (Expr_Type.Actual_Sem_Infos) then
         --  Not a "new" type but module name is not parameterized
         --  or parameterization is not fully provided
         --  so name of module is adequate.
         --  NOTE: Formal types should come through here, probably.
         if Expr_Type.Enclosing_Type /= null then
            return Canonical_Type_Name (Expr_Type.Enclosing_Type) &
              Languages.Module_Name_Separator &
              Non_Module_Encloser_Names
                (Expr_Type.Associated_Module.Associated_Symbol.
                  Enclosing_Region) &
              Sym_Name (Expr_Type.Associated_Module.Associated_Symbol);
         else
            return Sym_Full_Name
              (Expr_Type.Associated_Module.Associated_Symbol);
         end if;
      else
         --  Type is not a "new" type so we need to expand into
         --  an image of the instantiation that defined it.
         declare
            use String_Streams;
            Result : aliased String_Stream (Max => 1000);
            Is_First : Boolean := True;
         begin
            for I in Expr_Type.Actual_Sem_Infos'Range loop
               declare
                  Actual_Sem : constant Sem_Ptr :=
                    Expr_Type.Actual_Sem_Infos (I);
               begin
                  if not Is_First then
                     String'Write (Result'Access, ", ");
                  else
                     Is_First := False;
                  end if;

                  if Actual_Sem.all in Type_Semantic_Info then
                     --  Use canonical type name for type parameters,
                     --  but add in "optional" if specified.
                     if Type_Sem_Ptr (Actual_Sem).Value_Is_Optional then
                        String'Write (Result'Access, "optional ");
                     end if;
                     String'Write (Result'Access,
                                   Canonical_Type_Name
                                     (Type_Sem_Ptr (Actual_Sem)));
                  else
                     --  Use image of tree
                     Display_Subtree (Actual_Sem.Definition,
                       On => Result'Access);
                  end if;
               end;
            end loop;
            if Expr_Type.Enclosing_Type /= null then
               return Canonical_Type_Name (Expr_Type.Enclosing_Type) &
                 Languages.Module_Name_Separator &
                 Sym_Name (Expr_Type.Associated_Module.Associated_Symbol)
                 & '<' & String_Of (Result'Access) & '>';
            else
               return Sym_Full_Name
                   (Expr_Type.Associated_Module.Associated_Symbol)
                 & '<' & String_Of (Result'Access) & '>';
            end if;
         end;
      end if;
   end Canonical_Type_Name;

   function Param_Map_Image (Map : Param_Mapping_Ptr) return String is
      --  Return an image in the form "(From => To, From => To, ...)"
      use String_Streams;
      use Stream_Output;
      Result : aliased String_Stream (Max => 1000);
      M : Param_Mapping_Ptr := Map;
   begin
      Put (Result'Access, "(");
      while M /= null and then M.From /= null loop
         if M.From.all in Type_Semantic_Info then
            Put (Result'Access, Type_Image (Type_Sem_Ptr (M.From)));
            Put (Result'Access, " => ");
            Put (Result'Access, Type_Image (Type_Sem_Ptr (M.To)));
         else
            Put
              (Result'Access,
               Subtree_Image (M.From.Definition, Use_Short_Form => True));
            Put (Result'Access, " => ");
            Put
              (Result'Access,
               Subtree_Image (M.To.Definition, Use_Short_Form => True));
         end if;
         M := M.Next;
         if M /= null then
            --  Another mapping coming
            Put (Result'Access, ", ");
         end if;
      end loop;
      Put (Result'Access, ")");
      --  Return result as a string
      return String_Of (Result'Access);
   end Param_Map_Image;

   Last_Op_Id : Natural := 0;

   function Unique_Operation_Id return Natural is
   --  Return a unique operation id and announce it if debugging.
   begin
      Last_Op_Id := Last_Op_Id + 1;
      if Debug_Operation_Ids then
         Put_Line (" Assigning operation id =" & Natural'Image (Last_Op_Id));
      end if;
      return Last_Op_Id;
   end Unique_Operation_Id;

   --  Types of the various kinds of literals
   function Univ_Types return Univ_Types_Ptr is
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return Builtin_Types.Univ_Types'Access;
   end Univ_Types;

   function Univ_Integer_Type return Type_Sem_Ptr is
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return Builtin_Types_Array (PSC.Languages.Language).
        Univ_Types (Integer_Literal);
   end Univ_Integer_Type;

   function Unsigned_64_Type return Type_Sem_Ptr is
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return Builtin_Types_Array (PSC.Languages.Language).Unsigned_64_Type;
   end Unsigned_64_Type;

   function Integer_64_Type return Type_Sem_Ptr is
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return Builtin_Types_Array (PSC.Languages.Language).Integer_64_Type;
   end Integer_64_Type;

   function Univ_Real_Type return Type_Sem_Ptr is
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return
        Builtin_Types.Univ_Real_Type;
   end Univ_Real_Type;

   function Univ_Character_Type return Type_Sem_Ptr is
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return Builtin_Types.Univ_Types (Char_Literal);
   end Univ_Character_Type;

   function Univ_String_Type return Type_Sem_Ptr is
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return Builtin_Types.Univ_Types (String_Literal);
   end Univ_String_Type;

   function Univ_Enumeration_Type return Type_Sem_Ptr is
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return Builtin_Types.Univ_Types (Enum_Literal);
   end Univ_Enumeration_Type;

   function Optional_Type return Type_Sem_Ptr is
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return Builtin_Types.Univ_Types (Null_Literal);
   end Optional_Type;

   function Integer_Module return Module_Sem_Ptr is
   --  Module for built-in Integer
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return
        Builtin_Types.Integer_Module;
   end Integer_Module;

   function Float_Module return Module_Sem_Ptr is
   --  Module for built-in Float
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return
        Builtin_Types.Float_Module;
   end Float_Module;

   function Basic_Array_Module return Module_Sem_Ptr is
   --  Module for built-in Basic_Array
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return
        Builtin_Types.Basic_Array_Module;
   end Basic_Array_Module;

   function Aliased_Object_Module return Module_Sem_Ptr is
   --  Module for built-in Aliased Object
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      return
        Builtin_Types.Aliased_Object_Module;
   end Aliased_Object_Module;

   function Func_Type_Module return Module_Sem_Ptr is
   --  A "pseudo" module to hold all of the func types
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Builtin_Types = null then
         return null;
      end if;

      declare
         Func_Type_Mod : constant Module_Sem_Ptr :=
           Builtin_Types.Func_Type_Module;
      begin
         if Func_Type_Mod.Associated_Symbol = null then
            --  Finish initialization of Func_Type_Module
            Func_Type_Mod.Num_Module_Parameters := 0;
            Func_Type_Mod.Num_Constructors := 0;
            Func_Type_Mod.Num_Visible_Components := 0;
            Func_Type_Mod.Is_Interface := True;

            Func_Type_Mod.Associated_Symbol := new Symbols.Symbol'
              (Kind => Symbols.Module_Sym_Kind,
               Str => Strings.String_Lookup ("func"),
               Full_Name => Strings.Null_U_String,
               Source_Pos => Source_Positions.Null_Source_Position,
               Enclosing_Region => null,
               Nested_Region => null,
               Sem_Info => Root_Sem_Ptr (Func_Type_Mod),
               Definition => Null_Optional_Tree,
               Completion_Of => null,
               Index => Symbols.No_Sym_Index,
               Next_Homonym => Symbols.No_Sym_Index,
               Import_Clauses => Lists.Empty_List);

            Func_Type_Mod.Definition := Module.Make
              (Name => Identifier.Make (Func_Type_Mod.Associated_Symbol.Str),
               Add_On_Label => Lists.Empty_List,
               Is_Interface => True,
               Is_Abstract => False,
               Is_Private => False,
               Is_Concurrent => False,
               Is_Limited => True,  --  Functions are not assignable
               Has_Formals => True,
               Module_Formals => Lists.Empty_List,
               Extends_Interface => Null_Optional_Tree,
               Implements_Interfaces => Lists.Empty_List,
               Class_Locals => Lists.Empty_List,
               Module_Exports => Lists.Empty_List,
               Module_New_Exports => Lists.Empty_List,
               Module_Implements => Lists.Empty_List);

            Func_Type_Mod.Associated_Symbol.Definition :=
              Func_Type_Mod.Definition;
         end if;
         return Func_Type_Mod;
      end;
   end Func_Type_Module;

end PSC.Trees.Semantics.Info;
