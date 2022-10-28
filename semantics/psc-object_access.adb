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
pragma Elaborate (PSC.Hash_Tables);
with PSC.Symbols;
with PSC.Trees;
with PSC.Trees.Semantics;

with Ada.Text_IO; use Ada.Text_IO;
package body PSC.Object_Access is
   --  Keep track of which sets of objects are read and written
   --  by different parts of a program tree.  This is used
   --  to detect illegal aliasing and possible race conditions.

   --  NOTE: We need a way to represent the set of objects
   --       that correspond to the "ref" result of a call
   --       on a ref-returning operation.  By default
   --       this is the union of all of the "ref" inputs.
   --       A subsequent "read" or "update" of this result
   --       is equivalent to a "read" or "update" of all of
   --       these "ref" inputs.  In some special cases,
   --       the output "ref" is considered a subset of the
   --       input "ref"(s), in particular for indexing and slicing.
   --       In fact, we need to distinguish ref-const, ref, and ref-var.
   --       A ref-const and ref are first equivalent to a "read"
   --       since inside the operation they can be read.
   --       A ref-var is equivalent to an "update" for a corresponding
   --       reason.  But in addition, these get bundled up into something
   --       which can be passed around, and which can be "read" and
   --       "written," and also "ref"ed further.

   --       We also need to distinguish unlocked-concurrent, as these
   --       incur no conflicts.

   --       This interface is best defined in a top-down fashion,
   --       based on how it will be used.

   Debug_Object_Access : Boolean := False;

   Empty_Mapping : constant Read_Write_Mapping := null;

   Num_Object_Ids : Natural := 0;

   --  The Read_Write_Info contains the following:
   --  The set of objects already read.
   --  The set of objects already written.
   --  The set of objects that are referenced by the result of the operation
   --   and are read-only or read/write.
   --  This could be organized either by object, or by access-mode.
   --  Presuming there are many fewer objects written than read,
   --  and even fewer that are "ref"ed, it makes sense to split first
   --  by access-mode.
   --  We move subtrees from one access-mode to another.
   --  The same object can appear in the "ref" mode as well as in the
   --  "read" or "written" mode.
   --  For conflicts we only look at the read/write modes.
   --  The "ref" mode is used for the result of an operation.

   --  Kind of node in Access_Tree
   type Component_Kind_Enum is
     (Whole_Object, Selected_Component, Indexed_Component);

   --  How two objects with the same enclosing object can alias
   type Object_Aliasing_Enum is (Disjoint, May_Alias, Must_Alias);

   type Access_Tree_Node (Kind : Component_Kind_Enum);
   type Access_Tree is access Access_Tree_Node;

   type Access_Tree_Node (Kind : Component_Kind_Enum) is record
      --  Representation of a subtree of objects.
      --  Generally we are only interested in keeping track of leaves.
      --  If we read all or write all of a component, then which
      --  subcomponents of the component we access is no longer of interest.
      First_Component : Access_Tree := null; --  Subcomponents
      Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position;
      case Kind is
         when Whole_Object =>
            Id : Object_Id_Type := Null_Object_Id;
         when Selected_Component | Indexed_Component =>
            Enclosing_Obj : Access_Tree := null;   --  Enclosing object
            Next_Component : Access_Tree := null;  --  Next comp of same obj
            case Kind is
               when Selected_Component =>
                  Selector : Symbols.Sym_Ptr;
               when Indexed_Component =>
                  Domain : Trees.Root_Sem_Ptr;
                  Index : Trees.Root_Sem_Ptr;
               when others =>
                  null;
            end case;
      end case;
   end record;

   type Obj_Num_Hash_Type is mod 2 ** Natural'Base'Size;

   function Hash_Object_Num (Obj_Num : Natural) return Obj_Num_Hash_Type is
   begin
      return Obj_Num_Hash_Type (Obj_Num);
   end Hash_Object_Num;

   --  Table of top-level object trees
   package Tree_Tables is new Hash_Tables (
      Element_Type => Access_Tree,
      Key_Type => Natural,
      Equiv => "=",
      Hash_Type => Obj_Num_Hash_Type,
      Hash => Hash_Object_Num);

   use Tree_Tables;

   --  Table of object trees referenced with the given mode.
   type Table_Array is
     array (Recorded_Access_Mode_Enum) of Tree_Tables.Hash_Table;

   type Read_Write_Mapping_Rec is record
      --  Record summary of objects read and written,
      --  and if they represent inputs to a function that returns a "ref,"
      --  what objects are read/written by reads/writes via the returned ref.
      Tables : Table_Array;
   end record;

   --  Indicate which "ref" table to use for given access mode
   Mode_For_Ref : constant array (Access_Mode_Enum) of Access_Mode_Enum :=
     (No_Access => No_Access,
      Param_Computation => No_Access,
      Ref_Access => Ref_Var_Access,
      Ref_Const_Access => Ref_Const_Access,
      Ref_Var_Access => Ref_Var_Access,
      Read_Access => No_Access,
      Update_Access => No_Access);

   --  Indicate which "use" table to use for given access mode
   Mode_For_Use : constant array (Access_Mode_Enum) of Access_Mode_Enum :=
     (No_Access => No_Access,
      Param_Computation => No_Access,
      Ref_Access => No_Access,
      Ref_Const_Access => Read_Access,
      Ref_Var_Access => Update_Access,
      Read_Access => Read_Access,
      Update_Access => Update_Access);

   generic
      with procedure Action (Ref : Access_Tree; Mode : Access_Mode_Enum);
   procedure Iterate_Leaves_Of_Mode
     (Mapping : Read_Write_Mapping; Mode : Access_Mode_Enum);
   --  Iterate over all access-tree leaves appearing in Mapping with given mode

   procedure Iterate_Leaves_Of_Mode
     (Mapping : Read_Write_Mapping; Mode : Access_Mode_Enum) is

      procedure Do_One_Tree (Tree : Access_Tree) is
         --  Walk tree of components, recursively, calling Action
         --  on the "leaves" of the tree.
      begin
         if Tree /= null then
            declare
               Component : Access_Tree := Tree.First_Component;
            begin
               if Component = null then
                  --  Only do action on whole object if it has no
                  --  subcomponents.
                  Action (Tree, Mode);
               else
                  --  Recurse to do action on leaves.
                  while Component /= null loop
                     Do_One_Tree (Component);
                     Component := Component.Next_Component;
                  end loop;
               end if;
            end;
         end if;
      end Do_One_Tree;

      procedure Do_One_Pair (Pair : Tree_Tables.Pair_Ref) is
         --  Process one element of the Tree table.
      begin
         --  Call recursive routine
         Do_One_Tree (Tree_Tables.Element (Pair).all);
      end Do_One_Pair;

      procedure Iterate_Trees is new Tree_Tables.Iterate (Do_One_Pair);

   begin  --  Iterate_Leaves_Of_Mode
      if Mapping /= null then
         Iterate_Trees (Mapping.Tables (Mode));
      end if;
   end Iterate_Leaves_Of_Mode;

   generic
      with procedure Action (Ref : Access_Tree; Mode : Access_Mode_Enum);
   procedure Iterate_References (Mapping : Read_Write_Mapping);
   --  Iterate over the references in the mapping.

   procedure Iterate_References (Mapping : Read_Write_Mapping) is
   --  Iterate over the references in the mapping.
      procedure Iterate_One_Mode is new Iterate_Leaves_Of_Mode (Action);
   begin
      Iterate_One_Mode (Mapping, Ref_Const_Access);
      Iterate_One_Mode (Mapping, Ref_Var_Access);
   end Iterate_References;

   function Check_Container_Indices_For_Aliasing
     (Domain : Trees.Root_Sem_Ptr; Left, Right : Trees.Root_Sem_Ptr)
     return Object_Aliasing_Enum
   --  Return indication of whether two indices for the same domain
   --  of the same container identify definite or possible aliases.
   is
      use type Trees.Root_Sem_Ptr;
   begin
      if Left = Right then
         return Must_Alias;
      else
         --  TBD: Handle case of literals, nonoverlapping sets, etc.
         --       Probably need to create an abstraction just for
         --       representing sets of container indices.
         return May_Alias;
      end if;
   end Check_Container_Indices_For_Aliasing;

   --  definite or possible aliases.
   function Check_Components_For_Aliasing (Left, Right : Access_Tree)
     return Object_Aliasing_Enum
   --  Return indication of whether two components of the same object are
   --  definite or possible aliases.
   is
      pragma Assert (Left.Kind /= Whole_Object);
      pragma Assert (Right.Kind /= Whole_Object);
      --  NOTE: We don't require Left.Enclosing_Obj = Right.Enclosing_Obj
      --        because these might be nodes from different tree tables.
      use type Symbols.Sym_Ptr;
      use type Trees.Root_Sem_Ptr;
   begin
      if Left.Kind /= Right.Kind then
         --  Different kinds of components, so must consider as aliases.
         return May_Alias;
      elsif Left.Kind = Selected_Component then
         if Left.Selector /= Right.Selector then
            --  They are disjoint objects
            return Disjoint;
         else
            --  They are identical components
            return Must_Alias;
         end if;
      else
         if Left.Domain /= Right.Domain then
            --  They are indexes from different domains
            --  so they might alias.
            return May_Alias;
         else
            --  Hand off to index comparator
            return Check_Container_Indices_For_Aliasing
              (Left.Domain, Left.Index, Right.Index);
         end if;
      end if;
   end Check_Components_For_Aliasing;

   procedure Get_Enclosing_Obj (Read_Write : in out Read_Write_Mapping;
     Mode : Access_Mode_Enum;
     Existing_Enclosing_Obj : Access_Tree;
     New_Enclosing_Obj : out Access_Tree)
   --  Find or create a tree to represent the object identified
   --  by the Existing_Enclosing_Obj for the given Mode in the
   --  specified Read_Write mapping.  If the object (or an ancestor
   --  thereof) is already in the table *without* any subcomponents,
   --  then set New_Enclosing_Obj to null.  If the object with
   --  subcomponents is already in the table, return it.  If the
   --  object is not in the table, then create it.
   is
   begin
      if Read_Write = null then
         --  Create initial tables
         Read_Write := new Read_Write_Mapping_Rec;
      end if;

      case Existing_Enclosing_Obj.Kind is
         when Whole_Object =>
            --  We are at the whole-object level; use hash-table "find"
            declare
               Existing_Elem : constant Tree_Tables.Element_Ref :=
                 Tree_Tables.Find_Element (Read_Write.Tables (Mode),
                   Existing_Enclosing_Obj.Id.Num);
            begin
               if Existing_Elem = null then
                  --  Create new enclosing object
                  declare
                     Ignore : Tree_Tables.Element_Ref;
                  begin
                     New_Enclosing_Obj :=
                       new Access_Tree_Node'(Kind => Whole_Object,
                         First_Component => null,
                         Source_Pos => Existing_Enclosing_Obj.Source_Pos,
                         Id => Existing_Enclosing_Obj.Id);

                     --  Enter it into the table
                     Tree_Tables.Enter_Element (Read_Write.Tables (Mode),
                       Key => Existing_Enclosing_Obj.Id.Num,
                       Elem => New_Enclosing_Obj,
                       Existing_Elem => Ignore);

                     pragma Assert (Ignore = null);
                  end;
               elsif Existing_Elem.all.First_Component = null then
                  --  The existing enclosing object is present in the table
                  --  as a "whole" so there is no need to add this new element.
                  New_Enclosing_Obj := null;
               else
                  New_Enclosing_Obj := Existing_Elem.all;
               end if;
            end;

         when Selected_Component | Indexed_Component =>
            declare
               New_Encloser_Encloser : Access_Tree;
            begin
               Get_Enclosing_Obj (Read_Write, Mode,
                  Existing_Enclosing_Obj =>
                    Existing_Enclosing_Obj.Enclosing_Obj,
                  New_Enclosing_Obj => New_Encloser_Encloser);

               if New_Encloser_Encloser = null then
                  --  Ancestor already accessed in this mode.
                  New_Enclosing_Obj := null;
               else
                  --  Look to see if matching component already present
                  New_Enclosing_Obj := New_Encloser_Encloser.First_Component;
                  while New_Enclosing_Obj /= null loop
                     declare
                        How_Aliased : constant Object_Aliasing_Enum :=
                          Check_Components_For_Aliasing
                            (New_Enclosing_Obj, Existing_Enclosing_Obj);
                     begin
                        case How_Aliased is
                           when Must_Alias =>
                              --  A perfect match.
                              if New_Enclosing_Obj.First_Component = null then
                                 --  No need to extend this enclosing obj
                                 --  because it is already accessed as
                                 --  a "whole-obj" in this mode.
                                 New_Enclosing_Obj := null;
                              end if;
                              return;      ----------- all done ----------

                           when May_Alias =>
                              --  Different kind of component,
                              --  or different domain, so we can
                              --  wipe out all of the components and
                              --  return null.
                              --  TBD: Reclaim old components
                              New_Encloser_Encloser.First_Component := null;

                              New_Enclosing_Obj := null;
                              return;      ----------- all done ----------

                           when Disjoint =>
                              --  Keep looking
                              null;
                        end case;

                        --  Advance to next component
                        New_Enclosing_Obj := New_Enclosing_Obj.Next_Component;
                     end;
                  end loop;

                  --  Not already there, so we can add it.
                  if Existing_Enclosing_Obj.Kind = Indexed_Component then
                     New_Enclosing_Obj :=
                       new Access_Tree_Node'(Kind => Indexed_Component,
                         First_Component => null,
                         Source_Pos => Existing_Enclosing_Obj.Source_Pos,
                         Enclosing_Obj => New_Encloser_Encloser,
                         Next_Component =>
                           New_Encloser_Encloser.First_Component,
                         Domain => Existing_Enclosing_Obj.Domain,
                         Index => Existing_Enclosing_Obj.Index);
                  else
                     New_Enclosing_Obj :=
                       new Access_Tree_Node'(Kind => Selected_Component,
                         First_Component => null,
                         Source_Pos => Existing_Enclosing_Obj.Source_Pos,
                         Enclosing_Obj => New_Encloser_Encloser,
                         Next_Component =>
                           New_Encloser_Encloser.First_Component,
                         Selector => Existing_Enclosing_Obj.Selector);
                  end if;

                  --  Add it to front of list of components
                  New_Encloser_Encloser.First_Component :=
                    New_Enclosing_Obj;

               end if;
            end;
      end case;

      --  New_Enclosing_Obj has been set properly, so we just
      --  return from the procedure.

   end Get_Enclosing_Obj;

   function Access_Tree_Image (Tree : Access_Tree) return String is
   --  Return image of tree using "." or "[...]" as appropriate
   begin
      if Tree = null then
         return "null";
      end if;

      case Tree.Kind is
         when Whole_Object =>
            return Symbols.Sym_Name (Tree.Id.Sym);
         when Selected_Component =>
            return Access_Tree_Image (Tree.Enclosing_Obj) &
              '.' & Symbols.Sym_Name (Tree.Selector);
         when Indexed_Component =>
            --  TBD: Use "domain" in a more useful way?
            return Access_Tree_Image (Tree.Enclosing_Obj) & '[' &
              Trees.Subtree_Image (Tree.Domain.Definition) & ':' &
              Trees.Subtree_Image (Tree.Index.Definition) & ']';
      end case;
   end Access_Tree_Image;

   procedure Dump_Access_Tree (Tree : Access_Tree; Indent : Natural := 0) is
      use Ada.Text_IO;
      Indent_Str : constant String := (1 .. 100 => ' ');
   begin
      Put (Indent_Str (1 .. Indent));
      if Tree = null then
         Put_Line ("null");
         return;
      end if;

      Put (Source_Positions.Image (Tree.Source_Pos, Sep => ": ") &
        Access_Tree_Image (Tree));

      case Tree.Kind is
         when Whole_Object =>
            Put_Line (" =" & Natural'Image (Tree.Id.Num));
         when Selected_Component | Indexed_Component =>
            New_Line;
      end case;

      --  Recurse on components
      declare
         Next : Access_Tree := Tree.First_Component;
      begin
         while Next /= null loop
            Dump_Access_Tree (Next, Indent + 2);
            Next := Next.Next_Component;
         end loop;
      end;
   end Dump_Access_Tree;

   procedure Dump_Access_Table (Access_Table : Tree_Tables.Hash_Table) is
   --  Dump all access trees in table

      procedure Dump_One (Table_Elem : Tree_Tables.Pair_Ref) is
      --  Dump one element of access table
         Obj_Num : constant Natural := Key (Table_Elem).all;
         Acc_Tree : constant Access_Tree := Element (Table_Elem).all;
      begin
         Dump_Access_Tree (Acc_Tree, Indent => 4);
      end Dump_One;

      procedure Dump_All is
        new Tree_Tables.Iterate (Dump_One);

   begin  --  Dump_Access_Table

      --  Iterate over the trees in the table.
      Dump_All (Access_Table);
   end Dump_Access_Table;

   procedure Check_For_Conflicts
     (Overall : Tree_Tables.Hash_Table;
      Addition : Tree_Tables.Hash_Table;
      Kind_Of_Conflict : String) is
   --  Report any parallel data-access conflicts between
   --  "Overall" and "Addition"

      procedure Report_Conflict
        (Existing_Component, New_Component : Access_Tree) is
      begin
         Trees.Semantics.Sem_Warning
           (Kind_Of_Conflict & " Data Race on " &
            Access_Tree_Image (Existing_Component) & " at " &
            Source_Positions.Image (Existing_Component.Source_Pos),
            New_Component.Source_Pos);
      end Report_Conflict;

      procedure Check_Components (Existing_Component : Access_Tree;
        New_Component : Access_Tree) is
      --  Check New_Component against Existing_Component
      begin
         if Existing_Component.First_Component = null or else
           New_Component.First_Component = null
         then
            --  One or both are whole components, so report a conflict.
            Report_Conflict (Existing_Component, New_Component);
         else
            --  Both trees have components; check them for conflicts
            declare
               New_Subcomp : Access_Tree := New_Component.First_Component;
               use type Symbols.Sym_Ptr, Trees.Root_Sem_Ptr;
            begin
               while New_Subcomp /= null loop
                  declare
                     Next_Existing_Subcomp : Access_Tree :=
                       Existing_Component.First_Component;
                     Next_New_Subcomp : constant Access_Tree :=
                       New_Subcomp.Next_Component;
                  begin
                     while Next_Existing_Subcomp /= null loop
                        if New_Subcomp.Kind /=
                          Next_Existing_Subcomp.Kind
                        then
                           --  Subcomponent is of different kind;
                           --  report a conlict.
                           Report_Conflict
                             (Next_Existing_Subcomp, New_Subcomp);
                           return;   --- Done now ---
                        else
                           case Next_Existing_Subcomp.Kind is
                              when Whole_Object =>
                                 --  Cannot happen
                                 pragma Assert (False); null;
                              when Selected_Component =>
                                 if Next_Existing_Subcomp.Selector =
                                   New_Subcomp.Selector
                                 then
                                    --  Selectors match.
                                    --  Subcomponent already in table.

                                    --  Check sub-subcomponents.
                                    Check_Components (Next_Existing_Subcomp,
                                      New_Subcomp);

                                    --  Set to null to inhibit insertion.
                                    New_Subcomp := null;
                                    exit;
                                 end if;
                                 --  Check next subcomponent.
                              when Indexed_Component =>
                                 if Next_Existing_Subcomp.Domain /=
                                     New_Subcomp.Domain
                                 then
                                    --  Domain mismatch;
                                    --  report a conlict.
                                    Report_Conflict
                                      (Next_Existing_Subcomp, New_Subcomp);
                                    return;   --- Done now ---
                                 elsif Next_Existing_Subcomp.Index =
                                     New_Subcomp.Index
                                 then
                                    --  Exact index match.
                                    --  Subcomponent already in table.

                                    --  Check sub-subcomponents.
                                    Check_Components (Next_Existing_Subcomp,
                                      New_Subcomp);

                                    --  Set to null to inhibit insertion.
                                    New_Subcomp := null;
                                    exit;
                                 end if;
                                 --  TBD: Should check for index overlap.
                                 --  Check next subcomponent.
                           end case;

                           --  Move on to the next existing subcomponent.
                           Next_Existing_Subcomp :=
                             Next_Existing_Subcomp.Next_Component;
                        end if;
                     end loop;

                     --  Move on to next new subcomponent
                     New_Subcomp := Next_New_Subcomp;
                  end;
               end loop;
            end;

         end if;
      end Check_Components;

      procedure Check_One (Table_Elem : Tree_Tables.Pair_Ref) is
      --  Check one element of table against corresponding element of "Overall"
         Obj_Num : constant Natural := Key (Table_Elem).all;
         New_Tree : constant Access_Tree := Element (Table_Elem).all;
         Existing_Tree : constant Tree_Tables.Element_Ref :=
           Find_Element (Overall, Obj_Num);
      begin
         if Existing_Tree /= null then
            --  Object already in table; Check components
            Check_Components (Existing_Tree.all, New_Tree);
         end if;
      end Check_One;

      procedure Check_All is
        new Tree_Tables.Iterate (Check_One);

   begin  --  Check_For_Conficts

      if Tree_Tables.Num_Entries (Overall) = 0
        or else Tree_Tables.Num_Entries (Addition) = 0
      then
         return;   -- Nothing to do
      end if;

      if Debug_Object_Access then
         Put_Line ("--> Check for " & Kind_Of_Conflict &
           " Conflicts between:");
         Dump_Access_Table (Overall);
         Put_Line ("--> and:");
         Dump_Access_Table (Addition);
      end if;

      --  Iterate over the trees in the "addition" table.
      Check_All (Addition);
   end Check_For_Conflicts;

   procedure Combine_Tree
     (Overall : in out Tree_Tables.Hash_Table;
      Addition : in out Tree_Tables.Hash_Table) is
   --  Combine object trees of Addition into Overall.
   --  Destroy "Addition" in the process.

      procedure Combine_Components (Existing_Component : Access_Tree;
        New_Component : Access_Tree) is
      --  Combine New_Component with Existing_Component
      begin
         if Existing_Component.First_Component = null then
            --  Whole component already in table.
            null;
         elsif New_Component.First_Component = null then
            --  Whole component being added to table
            if Debug_Object_Access then
               Put_Line ("  Combine: Discarding subtree of components");
            end if;
            Existing_Component.First_Component := null; --  TBD: Reclaim
         else
            --  Both trees have components; combine them
            declare
               New_Subcomp : Access_Tree := New_Component.First_Component;
               use type Symbols.Sym_Ptr, Trees.Root_Sem_Ptr;
            begin
               while New_Subcomp /= null loop
                  declare
                     Existing_Subcomp : Access_Tree :=
                       Existing_Component.First_Component;
                     Next_New_Subcomp : constant Access_Tree :=
                       New_Subcomp.Next_Component;
                  begin
                     while Existing_Subcomp /= null loop
                        if New_Subcomp.Kind /=
                          Existing_Subcomp.Kind
                        then
                           --  Subcomponent is of different kind;
                           --  revert to whole component.
                           Existing_Component.First_Component := null;
                           return;   --- Done now ---
                        else
                           case Existing_Subcomp.Kind is
                              when Whole_Object =>
                                 --  Cannot happen
                                 pragma Assert (False); null;
                              when Selected_Component =>
                                 if Existing_Subcomp.Selector =
                                   New_Subcomp.Selector
                                 then
                                    --  Selectors match.
                                    --  Subcomponent already in table.

                                    --  Combine sub-subcomponents.
                                    Combine_Components (Existing_Subcomp,
                                      New_Subcomp);

                                    --  Set to null to inhibit insertion.
                                    New_Subcomp := null;
                                    exit;
                                 end if;
                                 --  Check next subcomponent.
                              when Indexed_Component =>
                                 if Existing_Subcomp.Domain /=
                                     New_Subcomp.Domain
                                 then
                                    --  Domain mismatch.
                                    --  Revert to whole component.
                                    Existing_Component.First_Component := null;
                                    return;   --- Done now ---
                                 elsif Existing_Subcomp.Index =
                                     New_Subcomp.Index
                                 then
                                    --  Exact index match.
                                    --  Subcomponent already in table.

                                    --  Combine sub-subcomponents.
                                    Combine_Components (Existing_Subcomp,
                                      New_Subcomp);

                                    --  Set to null to inhibit insertion.
                                    New_Subcomp := null;
                                    exit;
                                 end if;
                                 --  TBD: Should check for index overlap.
                                 --  Check next subcomponent.
                           end case;

                           --  Move on to the next existing subcomponent.
                           Existing_Subcomp :=
                             Existing_Subcomp.Next_Component;
                        end if;
                     end loop;

                     if New_Subcomp /= null then
                        --  All existing subcomps of same type, none matching.
                        --  Add new subcomponent into tree.
                        New_Subcomp.Next_Component :=
                          Existing_Component.First_Component;
                        New_Subcomp.Enclosing_Obj := Existing_Component;
                        Existing_Component.First_Component := New_Subcomp;
                     end if;

                     --  Move on to next new subcomponent
                     New_Subcomp := Next_New_Subcomp;
                  end;
               end loop;
            end;

         end if;
      end Combine_Components;

      procedure Combine_One (Obj_Num : Natural; New_Tree : Access_Tree) is
      --  Merge New_Tree into Overall
         Existing_Tree : Tree_Tables.Element_Ref;
      begin
         Enter_Element (Overall, Obj_Num, New_Tree, Existing_Tree);
         if Existing_Tree /= null then
            --  Object already in table; combine components
            if Existing_Tree.all.First_Component = null then
               --  Whole object already in table.
               null;
            elsif New_Tree.First_Component = null then
               --  Whole object being added to table
               if Debug_Object_Access then
                  Put_Line ("  Combine: Discarding subtree of components");
               end if;
               Existing_Tree.all.First_Component := null; --  TBD: Reclaim
            else
               --  Both trees have component; combine
               Combine_Components (Existing_Tree.all, New_Tree);
            end if;
         end if;
      end Combine_One;

      procedure Combine_All is
        new Tree_Tables.Iterate_And_Remove (Combine_One);

   begin  --  Combine_Tree

      --  Iterate over the trees in the "addition" table.
      Combine_All (Addition);
   end Combine_Tree;

   function Copy_With_New_Source_Pos
     (Tree : Access_Tree;
      New_Source_Pos : Source_Positions.Source_Position;
      New_Enclosing_Obj : Access_Tree := null) return Access_Tree is
   --  Copy access tree, updating the source position
   begin
      if Tree = null then
         return null;
      else
         --  Copy tree recursively, putting in new source pos.
         declare
            Result : constant Access_Tree := new Access_Tree_Node'(Tree.all);
         begin
            Result.Source_Pos := New_Source_Pos;
            Result.First_Component :=
              Copy_With_New_Source_Pos (Tree.First_Component,
                                        New_Source_Pos,
                                        New_Enclosing_Obj => Result);
            case Result.Kind is
               when Whole_Object => null;
               when Selected_Component | Indexed_Component =>
                  --  Update enclosing obj then recurse on next component
                  Result.Enclosing_Obj := New_Enclosing_Obj;
                  Result.Next_Component :=
                    Copy_With_New_Source_Pos
                      (Tree.Next_Component,
                       New_Source_Pos,
                       New_Enclosing_Obj => New_Enclosing_Obj);
            end case;

            return Result;
         end;
      end if;
   end Copy_With_New_Source_Pos;

   procedure Copy_Table
     (From : Tree_Tables.Hash_Table; To : in out Tree_Tables.Hash_Table;
      New_Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position) is
   --  Copy entries in "From" table into "To" Table.
   --  If New_Source_Pos is not null, then shift source positions
   --  of the references to the given source position (this is used
   --  for uplevel references associated with a given call).

      procedure Copy_One (Table_Elem : Tree_Tables.Pair_Ref) is
      --  Copy one entry into "To" table
         use Tree_Tables;
         Existing_Elem : Tree_Tables.Pair_Ref;
         Obj_Acc : Access_Tree := Element (Table_Elem).all;
         use type Source_Positions.Line_Number;
      begin
         if New_Source_Pos.Line /= 0 then
            --  Update the source positions
            Obj_Acc := Copy_With_New_Source_Pos (Obj_Acc, New_Source_Pos);
         end if;

         Enter_Element_Pair
           (To, Key (Table_Elem).all, Obj_Acc,
            Existing_Pair => Existing_Elem);
      end Copy_One;

      procedure Copy_All is new Tree_Tables.Iterate (Copy_One);

   begin  --  Copy_Table

      --  Iterate over elements of "From" table
      Copy_All (From);
   end Copy_Table;

   function Sym_Enclosed_By
     (Sym : Symbols.Sym_Ptr; Loop_Region : Symbols.Region_Ptr)
     return Boolean is
   --  Return True if Sym is declared in Loop_Region
   --  (or one of its subregions).
      use type Symbols.Sym_Ptr;
   begin
      if Sym /= null then
         return Symbols.Region_Encloses_Region
           (Encloser => Loop_Region,
            Enclosed => Sym.Enclosing_Region);
      else
         --  TBD: why no sym?  Presume is a (very) local variable...
         return True;
      end if;
   end Sym_Enclosed_By;

   function Obj_Enclosed_By
     (Obj : Access_Tree; Loop_Region : Symbols.Region_Ptr) return Boolean is
   --  Return True if Sym of enclosing whole obj is declared in Loop_Region
   --  (or one of its subregions).
   begin
      case Obj.Kind is
      when Whole_Object =>
         return Sym_Enclosed_By (Obj.Id.Sym, Loop_Region);
      when Selected_Component |
           Indexed_Component =>
         return Obj_Enclosed_By (Obj.Enclosing_Obj, Loop_Region);
      end case;
   end Obj_Enclosed_By;

   procedure Copy_Uplevel_Refs
     (Op_Region : Symbols.Region_Ptr;
      From : Tree_Tables.Hash_Table; To : in out Tree_Tables.Hash_Table) is
   --  Copy uplevel entries in "From" table into "To" Table

      procedure Copy_One_Uplevel (Table_Elem : Tree_Tables.Pair_Ref) is
      --  Copy one entry into "To" table if it is a uplevel reference
         use Tree_Tables;
         Existing_Elem : Tree_Tables.Pair_Ref;
         Existing_Obj : constant Access_Tree := Element (Table_Elem).all;
      begin
         if not Obj_Enclosed_By (Existing_Obj, Op_Region) then
            Enter_Element_Pair
              (To, Key (Table_Elem).all, Element (Table_Elem).all,
               Existing_Pair => Existing_Elem);
         end if;
      end Copy_One_Uplevel;

      procedure Copy_Uplevels is new Tree_Tables.Iterate (Copy_One_Uplevel);

   begin  --  Copy_Uplevel_Refs

      --  Iterate over elements of "From" table
      Copy_Uplevels (From);
   end Copy_Uplevel_Refs;

   function Indexed_By_Loop_Var
     (Obj : Access_Tree; Loop_Region : Symbols.Region_Ptr) return Boolean is
   --  Return True if obj is an array with index that depends on
   --  a loop variable defined in given loop.
   begin
      case Obj.Kind is
      when Whole_Object =>
         return False;
      when Selected_Component =>
         return Indexed_By_Loop_Var (Obj.Enclosing_Obj, Loop_Region);
      when Indexed_Component =>
         --  TBD: See whether loop-var of Loop_Region appears
         --       somewhere within index expression.
         return True;
      end case;
   end Indexed_By_Loop_Var;

   --------------- visible subprograms ---------------

   procedure Initialize (Overall : out Read_Write_Mapping) is
   --  Initialize read/write mapping to empty
   begin
      Overall := Empty_Mapping;  --  TBD: Reclaim old mapping?
   end Initialize;

   procedure Move (From : in out Read_Write_Mapping;
     To : out Read_Write_Mapping) is
   --  Move mapping "From" to "To" leaving "From" empty.
   begin
      Initialize (To);
      To := From;
      From := null;
   end Move;

   procedure Copy (From : Read_Write_Mapping;
     To : out Read_Write_Mapping;
     New_Source_Pos : Source_Positions.Source_Position :=
       Source_Positions.Null_Source_Position) is
   --  Copy mapping "From" to "To" leaving "From" as is.
   --  If New_Source_Pos is not null, then shift source positions
   --  of the references to the given source position (this is used
   --  for uplevel references associated with a given call).
   begin
      Initialize (To);
      if From /= null then
         To := new Read_Write_Mapping_Rec;
         for I in From.Tables'Range loop
            Copy_Table (From => From.Tables (I), To => To.Tables (I),
                        New_Source_Pos => New_Source_Pos);
         end loop;
      end if;
   end Copy;

   procedure Combine
     (Overall : in out Read_Write_Mapping;
      Addition : in out Read_Write_Mapping;
      How_Combined : Read_Write_Combination_Enum;
      Mode : Access_Mode_Enum := Read_Access) is
   --  Combine two read-write mappings.
   --  Destroy Addition as a side-effect.
   --  Complain if two mappings conflict and How_Combined is Parallel
   --  or if two references conflict and How_Combined is Within_Operation,
   --  and one of them can be updated (or more precisely, can have
   --  a subcomponent updated).
   --  If How_Combined is Uplevel_Refs, then the Addition is the set of
   --  accesses made via uplevel references.

      Show_Result : Boolean := False;

   begin
      --  Mode corresponds to parameter mode in a call.
      --  The "Read" and "Update" tables of the Addition are
      --  combined into the corresponding Overall tables,
      --  independent of the Mode.
      --  The "Ref" part of the Addition mapping is affected
      --  by the Mode.  The "Ref_Var" table of the Addition gets
      --  combined into the "Update" table of Overall
      --  if the Mode is "Ref_Var" or "Update".  The "Ref_Const" and
      --  "Ref_Var" tables of the Addition also get combined into
      --  the "Ref_Const" or "Ref_Var" table of Overall,
      --  if the Mode is "Ref", "Ref_Const," or "Ref_Var."

      if Debug_Object_Access and then How_Combined /= Not_Combined
        and then (How_Combined /= Sequential or else Mode /= Read_Access)
      then
         Put_Line (" Combining " &
           Read_Write_Combination_Enum'Image (How_Combined) &
           " of mode " & Access_Mode_Enum'Image (Mode));
      end if;

      if Addition = null or else How_Combined = Not_Combined then
         --  Nothing to do
         return;
      end if;

      if Overall = null then
         --  Initialize overall table
         Overall := new Read_Write_Mapping_Rec;
      end if;

      case How_Combined is
         when Not_Combined =>
            --  Each element is independent
            null;  --  Nothing to do

         when Uplevel_Refs =>
            --  Initialize RW mapping from up-level refs
            if Debug_Object_Access then
               Put_Line (" Initializing uplevel ref info so as to detect" &
                 " any conflicts with parameters");
               Put_Line (" Overall R/W mapping:");
               Dump_Read_Write_Mapping (Overall);
               Put_Line (" Uplevel R/W mapping:");
               Dump_Read_Write_Mapping (Addition);
            end if;

            --  This should only be called with Mode = Update_Access
            pragma Assert (Mode = Update_Access);

            --  Propagate all uplevel refs to appropriate ref tables
            Combine_Tree (Overall.Tables (Ref_Const_Access),
              Addition => Addition.Tables (Ref_Const_Access));
            Combine_Tree (Overall.Tables (Ref_Const_Access),
              Addition => Addition.Tables (Read_Access));
            Combine_Tree (Overall.Tables (Ref_Var_Access),
              Addition => Addition.Tables (Ref_Var_Access));
            Combine_Tree (Overall.Tables (Ref_Var_Access),
              Addition => Addition.Tables (Update_Access));

         when Within_Operation =>
            --  Complain about "ref" conflicts, and then
            --  union the "ref" effects.
            --  NOTE: Source position, etc. in individual access-tree nodes.
            --  TBD: Should ignore parameters that are "indivisible"

            if Debug_Object_Access then
               Put_Line (" Checking for conflicts within operation" &
                 " for param of mode " & Access_Mode_Enum'Image (Mode));
               Put_Line (" Overall R/W mapping:");
               Dump_Read_Write_Mapping (Overall);
               Put_Line (" Addition R/W mapping:");
               Dump_Read_Write_Mapping (Addition);
            end if;

            case Mode is
               when Ref_Access | Ref_Var_Access | Update_Access =>
                  --  Might be updating parameter
                  Check_For_Conflicts (Overall.Tables (Ref_Const_Access),
                    Addition.Tables (Ref_Var_Access),
                      "R/W");
                  Check_For_Conflicts (Overall.Tables (Ref_Var_Access),
                    Addition.Tables (Ref_Var_Access),
                      "W/W");
                  Check_For_Conflicts (Overall.Tables (Ref_Var_Access),
                    Addition.Tables (Ref_Const_Access),
                      "W/R");

                  --  Propagate refs to corresponding overall tables
                  Combine_Tree (Overall.Tables (Ref_Const_Access),
                    Addition => Addition.Tables (Ref_Const_Access));
                  Combine_Tree (Overall.Tables (Ref_Var_Access),
                    Addition => Addition.Tables (Ref_Var_Access));

               when Ref_Const_Access | Read_Access =>
                  --  Are reading only
                  Check_For_Conflicts (Overall.Tables (Ref_Var_Access),
                    Addition.Tables (Ref_Const_Access),
                      "W/R");
                  Check_For_Conflicts (Overall.Tables (Ref_Var_Access),
                    Addition.Tables (Ref_Var_Access),
                      "W/R");

                  --  Propagate refs to overall ref-const table
                  Combine_Tree (Overall.Tables (Ref_Const_Access),
                    Addition => Addition.Tables (Ref_Const_Access));
                  Combine_Tree (Overall.Tables (Ref_Const_Access),
                    Addition => Addition.Tables (Ref_Var_Access));

               when No_Access | Param_Computation =>
                  --  Not a meaningful combination
                  null;

            end case;

         when Sequential | Parallel =>
            --  Handle the other cases

            case Mode is
               when No_Access =>
                  --  Nothing to combine
                  null;

               when Param_Computation =>
                  --  Nothing to transform
                  null;

               when Update_Access =>
                  --  Turn any writable ref into an update.
                  if Num_Entries (Addition.Tables (Ref_Var_Access)) > 0 then
                     if Debug_Object_Access then
                        Put_Line
                          (" Converting Ref_Var_Access to Update_Access:");
                        Dump_Access_Table (Addition.Tables (Ref_Var_Access));
                     end if;

                     Combine_Tree (Addition.Tables (Update_Access),
                       Addition => Addition.Tables (Ref_Var_Access));

                     if Debug_Object_Access then
                        Put_Line (" New Update_Access Table:");
                        Dump_Access_Table (Addition.Tables (Update_Access));
                     end if;
                  end if;

               when Read_Access =>
                  --  Turn any ref into a Read access
                  if Debug_Object_Access then
                     if Num_Entries
                          (Addition.Tables (Ref_Const_Access)) > 0
                     then
                        Put_Line
                          (" Converting Ref_Const_Access to Read_Access:");
                        Dump_Access_Table (Addition.Tables (Ref_Const_Access));
                        Show_Result := True;
                     end if;
                     if Num_Entries (Addition.Tables (Ref_Var_Access)) > 0 then
                        Put_Line
                          (" Converting Ref_Var_Access to Read_Access:");
                        Dump_Access_Table (Addition.Tables (Ref_Var_Access));
                        Show_Result := True;
                     end if;
                  end if;

                  Combine_Tree (Addition.Tables (Read_Access),
                    Addition => Addition.Tables (Ref_Const_Access));

                  Combine_Tree (Addition.Tables (Read_Access),
                    Addition => Addition.Tables (Ref_Var_Access));

                  if Debug_Object_Access and then Show_Result then
                     Put_Line (" New Read_Access Table:");
                     Dump_Access_Table (Addition.Tables (Read_Access));
                  end if;

               when Ref_Const_Access =>
                  --  Propagate refs to overall ref-const table
                  Combine_Tree (Overall.Tables (Ref_Const_Access),
                    Addition => Addition.Tables (Ref_Const_Access));
                  Combine_Tree (Overall.Tables (Ref_Const_Access),
                    Addition => Addition.Tables (Ref_Var_Access));

               when Ref_Access | Ref_Var_Access =>
                  --  Propagate refs to corresponding overall tables
                  Combine_Tree (Overall.Tables (Ref_Const_Access),
                    Addition => Addition.Tables (Ref_Const_Access));
                  Combine_Tree (Overall.Tables (Ref_Var_Access),
                    Addition => Addition.Tables (Ref_Var_Access));

            end case;

            case How_Combined is
               when Within_Operation | Uplevel_Refs | Not_Combined =>
                  --  Handled above
                  pragma Assert (False); null;

               when Sequential =>
                  --  Fall through to union the effects
                  null;

               when Parallel =>
                  --  Complain about conflicts, and then fall through to
                  --  union the effects.
                  --  NOTE: Source position, etc. in individual
                  --        access-tree nodes.
                  Check_For_Conflicts (Overall.Tables (Read_Access),
                    Addition.Tables (Update_Access),
                      "R/W");
                  Check_For_Conflicts (Overall.Tables (Update_Access),
                    Addition.Tables (Update_Access),
                      "W/W");
                  Check_For_Conflicts (Overall.Tables (Update_Access),
                    Addition.Tables (Read_Access),
                      "W/R");
            end case;

            --  Propagate the read/write effects.
            Combine_Tree (Overall.Tables (Read_Access),
              Addition => Addition.Tables (Read_Access));
            Combine_Tree (Overall.Tables (Update_Access),
              Addition => Addition.Tables (Update_Access));

      end case;

   end Combine;

   procedure Check_Concurrent_Loop_Body
     (Loop_Region : Symbols.Region_Ptr; Loop_Body_RW : Read_Write_Mapping) is
   --  Check for conflicts within a concurrent loop body,
   --  given the R/W mapping for the loop body, and the region of the loop.
   --  TBD: Need another check to use for any exitable concurrent construct
   --       to be sure there are *no* updates of non-concurrent global objects.
      procedure Check_One_Obj (Obj : Access_Tree; Mode : Access_Mode_Enum) is
         --  Check one object to see if it is a global being updated
         --  inside a concurrent loop.

         function Kind_Of_Access return String is
         --  Return "updating" or "reading" depending on Mode
         begin
            if Mode = Update_Access then
               return "updating";
            else
               return "reading";
            end if;
         end Kind_Of_Access;

      begin  --  Check_One_Obj

         if not Obj_Enclosed_By (Obj, Loop_Region) then
            if not Indexed_By_Loop_Var (Obj, Loop_Region) then
               Trees.Semantics.Sem_Warning
                 ("Data Race in loop " & Kind_Of_Access & " " &
                    Access_Tree_Image (Obj),
                  Obj.Source_Pos);
            end if;
         end if;
      end Check_One_Obj;

      procedure Check_One_Mode is new Iterate_Leaves_Of_Mode (Check_One_Obj);

   begin  --  Check_Concurrent_Loop_Body
      --  Check for conflicts involving updates
      Check_One_Mode (Loop_Body_RW, Update_Access);
      --  TBD: Check for reading global array elements that are updated
      --       by other iterations.
   end Check_Concurrent_Loop_Body;

   function New_Object_Id (Sym : Symbols.Sym_Ptr) return Object_Id_Type is
   --  Assign a unique Id for an object.
   begin
      Num_Object_Ids := Num_Object_Ids + 1;
      return (Num => Num_Object_Ids, Sym => Sym);
   end New_Object_Id;

   procedure Refer_To_Object
     (Read_Write : in out Read_Write_Mapping;
      Obj : Object_Id_Type;
      Mode : Access_Mode_Enum;
      Source_Pos : Source_Positions.Source_Position) is
   --  Record an object reference of given Mode, at given source position.
      Tree_To_Insert : Access_Tree;
      Existing_Tree : Tree_Tables.Element_Ref;
   begin
      if Mode = No_Access then
         return;
      end if;

      if Debug_Object_Access then
         Put_Line (" Refer_To_Object: " &
           Source_Positions.Image (Source_Pos) & " " &
           Access_Mode_Enum'Image (Mode_For_Use (Mode)) & '/' &
           Access_Mode_Enum'Image (Mode_For_Ref (Mode)) & " to " &
           Symbols.Sym_Name (Obj.Sym) & " =" & Natural'Image (Obj.Num));
      end if;

      if Read_Write = null then
         --  Create the set of object tables
         Read_Write := new Read_Write_Mapping_Rec;
      end if;

      --  Create tree to insert into "use" table
      Tree_To_Insert := new Access_Tree_Node'(Kind => Whole_Object,
        First_Component => null, Source_Pos => Source_Pos, Id => Obj);

      if Mode_For_Ref (Mode) /= No_Access then
         declare
            --  Create tree to insert into "ref" table, by copying "use" tree
            Ref_Tree : constant Access_Tree :=
              new Access_Tree_Node'(Tree_To_Insert.all);
            Existing_Ref_Tree : Tree_Tables.Element_Ref;
         begin
            --  Try to add element into "ref" table.
            Tree_Tables.Enter_Element (Read_Write.Tables (Mode_For_Ref (Mode)),
              Key => Obj.Num, Elem => Ref_Tree,
              Existing_Elem => Existing_Ref_Tree);
            if Existing_Ref_Tree /= null then
               --  Object already in this tree
               --  We can wipe out the subcomponent tree, as this is a ref
               --  to the whole tree.
               if Debug_Object_Access
                 and then Existing_Ref_Tree.all.First_Component /= null
               then
                  Put_Line ("  Discarding subtree of ref'd components");
               end if;
               Existing_Ref_Tree.all.First_Component := null; --  TBD: Reclaim
            end if;
         end;
      end if;

      if Mode_For_Use (Mode) /= No_Access then
         --  Try to add element into "use" table.
         Tree_Tables.Enter_Element (Read_Write.Tables (Mode_For_Use (Mode)),
           Key => Obj.Num, Elem => Tree_To_Insert,
           Existing_Elem => Existing_Tree);

         if Existing_Tree /= null then
            --  Object already in this tree
            --  We can wipe out the subcomponent tree, as this is a use
            --  of the whole tree.
            if Debug_Object_Access
              and then Existing_Tree.all.First_Component /= null
            then
               Put_Line ("  Discarding subtree of used components");
            end if;
            Existing_Tree.all.First_Component := null; --  TBD: Reclaim
         end if;
      end if;

      --  TBD: Perhaps should check for conflicts, but we currently
      --       only use this for a single reference, so shouldn't
      --       be any other objects in the tree anyway.

   end Refer_To_Object;

   procedure Refer_To_Ref
     (Read_Write : in out Read_Write_Mapping;
      Saved_Mapping_Of_Ref : Read_Write_Mapping;
      Obj : Object_Id_Type;
      Mode : Access_Mode_Enum;
      Source_Pos : Source_Positions.Source_Position) is
   --  Record a reference to a "ref" of given Mode, at given source position.
      Copy_Of_Saved_Mapping : Read_Write_Mapping;
   begin
      null;  --  TBD
      if Mode = No_Access then
         return;
      end if;

      Copy (From => Saved_Mapping_Of_Ref, To => Copy_Of_Saved_Mapping,
        New_Source_Pos => Source_Pos);

      if Debug_Object_Access then
         Put_Line (" Refer_To_Ref: " &
           Source_Positions.Image (Source_Pos) & " " &
           Access_Mode_Enum'Image (Mode_For_Use (Mode)) & '/' &
           Access_Mode_Enum'Image (Mode_For_Ref (Mode)) & " to ref " &
           Symbols.Sym_Name (Obj.Sym) & " =" & Natural'Image (Obj.Num));
         Dump_Read_Write_Mapping (Saved_Mapping_Of_Ref);
      end if;

      if Read_Write = null then
         --  Create the set of object tables
         Read_Write := new Read_Write_Mapping_Rec;
      end if;

      --  Combine saved mapping
      Combine (Read_Write,
        Addition => Copy_Of_Saved_Mapping,
        How_Combined => Sequential,
        Mode => Mode);

      if Debug_Object_Access then
         Put_Line (" After Refer_To_Ref: ");
         Dump_Read_Write_Mapping (Read_Write);
      end if;

   end Refer_To_Ref;

   procedure Refer_To_Selected_Component
     (Read_Write : in out Read_Write_Mapping;
      Enclosing_Object : in out Read_Write_Mapping;
      Selector : Symbols.Sym_Ptr;
      Mode : Access_Mode_Enum;
      Source_Pos : Source_Positions.Source_Position) is
   --  Record a selected-component reference of given Mode,
   --  at given source position.

      procedure Add_Selected_Component
        (Old_Enclosing_Obj : Access_Tree; Mode : Access_Mode_Enum) is
         New_Enclosing_Obj : Access_Tree;
      begin
         --  Find/Create tree for enclosing object
         Get_Enclosing_Obj (Read_Write, Mode,
           Existing_Enclosing_Obj => Old_Enclosing_Obj,
           New_Enclosing_Obj => New_Enclosing_Obj);

         if New_Enclosing_Obj /= null then
            --  There is no ancestor in table that is
            --  already accessed (as a whole) in given mode.
            --  TBD: See whether there is a conflicting component.
            --  Hook new selected component onto head of list.
            New_Enclosing_Obj.First_Component :=
              new Access_Tree_Node'(Kind => Selected_Component,
                First_Component => null,
                Source_Pos => Source_Pos,
                Enclosing_Obj => New_Enclosing_Obj,
                Next_Component => New_Enclosing_Obj.First_Component,
                Selector => Selector);

            if Debug_Object_Access then
               Put_Line (" Adding selected component for " &
                 Access_Tree_Image (New_Enclosing_Obj.First_Component) &
                 " for mode " & Access_Mode_Enum'Image (Mode));
            end if;

         else
            if Debug_Object_Access then
               Put_Line (" NOT adding selected component because " &
                 Access_Tree_Image (Old_Enclosing_Obj) &
                 " already referenced in mode " &
                 Access_Mode_Enum'Image (Mode));
            end if;
         end if;

      end Add_Selected_Component;

      procedure Add_Selected_Refs is
        new Iterate_References (Add_Selected_Component);

   begin   --  Refer_To_Selected_Component

      if Mode = No_Access then
         return;
      end if;

      if Debug_Object_Access then
         Put_Line (" Refer_To_Selected_Component: " &
           Source_Positions.Image (Source_Pos) & " " &
           Access_Mode_Enum'Image (Mode_For_Use (Mode)) & '/' &
           Access_Mode_Enum'Image (Mode_For_Ref (Mode)) & " selection by " &
           Symbols.Sym_Name (Selector));
      end if;

      if Read_Write = null then
         --  Create the set of object tables
         Read_Write := new Read_Write_Mapping_Rec;
      end if;

      Add_Selected_Refs (Enclosing_Object);

      --  TBD: Perhaps should check for conflicts, but we currently
      --       only use this for a single reference, so shouldn't
      --       be any other objects in the tree anyway.

   end Refer_To_Selected_Component;

   procedure Refer_To_Indexed_Component
     (Read_Write : in out Read_Write_Mapping;
      Enclosing_Object : in out Read_Write_Mapping;
      Domain : Trees.Root_Sem_Ptr;
      Index : Trees.Root_Sem_Ptr;
      Mode : Access_Mode_Enum;
      Source_Pos : Source_Positions.Source_Position) is
   --  Record an indexed-component reference of given Mode,
   --  at given source position.

      procedure Add_Indexed_Component
        (Old_Enclosing_Obj : Access_Tree; Mode : Access_Mode_Enum) is
         New_Enclosing_Obj : Access_Tree;
      begin
         --  Find/Create tree for enclosing object
         Get_Enclosing_Obj (Read_Write, Mode,
           Existing_Enclosing_Obj => Old_Enclosing_Obj,
           New_Enclosing_Obj => New_Enclosing_Obj);

         if New_Enclosing_Obj /= null then
            --  There is no ancestor in table that is
            --  already accessed (as a whole) in given mode.
            --  TBD: See whether there is a conflicting component.
            --  Hook new Indexed component onto head of list.
            New_Enclosing_Obj.First_Component :=
              new Access_Tree_Node'(Kind => Indexed_Component,
                First_Component => null,
                Source_Pos => Source_Pos,
                Enclosing_Obj => New_Enclosing_Obj,
                Next_Component => New_Enclosing_Obj.First_Component,
                Domain => Domain,
                Index => Index);

            if Debug_Object_Access then
               Put_Line (" Adding indexed component for " &
                 Access_Tree_Image (New_Enclosing_Obj.First_Component) &
                 " for mode " & Access_Mode_Enum'Image (Mode));
            end if;

         else
            if Debug_Object_Access then
               Put_Line (" NOT adding indexed component because " &
                 Access_Tree_Image (Old_Enclosing_Obj) &
                 " already referenced in mode " &
                 Access_Mode_Enum'Image (Mode));
            end if;
         end if;

      end Add_Indexed_Component;

      procedure Add_Indexed_Refs is
        new Iterate_References (Add_Indexed_Component);

   begin   --  Refer_To_Indexed_Component

      if Mode = No_Access then
         return;
      end if;

      if Debug_Object_Access then
         Put_Line (" Refer_To_Indexed_Component: " &
           Source_Positions.Image (Source_Pos) & " " &
           Access_Mode_Enum'Image (Mode_For_Use (Mode)) & '/' &
           Access_Mode_Enum'Image (Mode_For_Ref (Mode)) & " indexed by " &
           Trees.Subtree_Image (Index.Definition) & ':' &
           Trees.Subtree_Image (Domain.Definition));
      end if;

      if Read_Write = null then
         --  Create the set of object tables
         Read_Write := new Read_Write_Mapping_Rec;
      end if;

      Add_Indexed_Refs (Enclosing_Object);

      --  TBD: Perhaps should check for conflicts, but we currently
      --       only use this for a single reference, so shouldn't
      --       be any other objects in the tree anyway.

   end Refer_To_Indexed_Component;

   procedure Save_Mapping_For_Ref
     (Mapping_Of_Ref : out Read_Write_Mapping;
      Initial_Value_Mapping : Read_Write_Mapping;
      Obj : Object_Id_Type;
      Source_Pos : Source_Positions.Source_Position) is
   --  Save r/w mapping from initial value for future use.
   begin
      --  TBD: Use Obj, Source_Pos for something other than debugging?
      if Debug_Object_Access then
         Put_Line (" Save_Mapping_For_Ref:" &
           Source_Positions.Image (Source_Pos) & " " &
           Symbols.Sym_Name (Obj.Sym) & " =" & Natural'Image (Obj.Num));
         Dump_Read_Write_Mapping (Initial_Value_Mapping);
      end if;
      Copy (From => Initial_Value_Mapping, To => Mapping_Of_Ref);
   end Save_Mapping_For_Ref;

   procedure Extract_Uplevel_Refs
     (Op_Region : Symbols.Region_Ptr; All_Refs : in out Read_Write_Mapping;
      Uplevel_Refs : out Read_Write_Mapping) is
   --  Copy any uplevel references from All_Refs into Uplevel_Refs, given
   --  the parameter region for the current operation.
   begin
      Initialize (Uplevel_Refs);
      if All_Refs /= null then
         --  Copy over the uplevel refs from All_Refs
         declare
            Uplevel_Tables : Table_Array;
            Count : Natural := 0;
         begin
            for I in All_Refs.Tables'Range loop
               Copy_Uplevel_Refs
                 (Op_Region => Op_Region,
                  From => All_Refs.Tables (I),
                  To => Uplevel_Tables (I));
               Count := Count + Num_Entries (Uplevel_Tables (I));
            end loop;

            if Count > 0 then
               --  We have at least one up-level reference
               Uplevel_Refs :=
                 new Read_Write_Mapping_Rec'(Tables => Uplevel_Tables);
            end if;
         end;
      end if;
   end Extract_Uplevel_Refs;

   function Is_Empty (Read_Write : Read_Write_Mapping) return Boolean is
   --  Return True if Read_Write has no objects in it.
   begin
      if Read_Write /= null then
         --  Scan tables for non-empty set of objects
         for I in Read_Write.Tables'Range loop
            if Num_Entries (Read_Write.Tables (I)) > 0 then
               --  Has at least one object
               return False;
            end if;
         end loop;
      end if;

      --  No objects found
      return True;
   end Is_Empty;

   function Num_Objects (Read_Write : Read_Write_Mapping) return Natural is
   --  Return count of number of objects in Read_Write
      Count : Natural := 0;
   begin
      if Read_Write /= null then
         --  Count number of entries in each table
         for I in Read_Write.Tables'Range loop
            Count := Count + Num_Entries (Read_Write.Tables (I));
         end loop;
      end if;

      --  Return overall count
      return Count;
   end Num_Objects;

   procedure Dump_Read_Write_Mapping (Read_Write : Read_Write_Mapping) is
   --  Dump tables in read/write mapping
      use Ada.Text_IO;
   begin
      if Read_Write = null then
         Put_Line ("null");
         return;
      end if;
      for I in Read_Write.Tables'Range loop
         Put_Line ("  " & Access_Mode_Enum'Image (I) & ':');
         Dump_Access_Table (Read_Write.Tables (I));
      end loop;
   end Dump_Read_Write_Mapping;

end PSC.Object_Access;
