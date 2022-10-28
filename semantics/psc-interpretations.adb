------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
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

with PSC.Symbols;
with PSC.Trees;
with PSC.Trees.Semantics;
with Ada.Text_IO;         use Ada.Text_IO;
package body PSC.Interpretations is

   Debug_Interpretations : constant Boolean := False;

   procedure Add_Interp
     (Interps : in out Interp_Tree;
      Associated_Type_Region : Type_Region_Ptr;
      Interp : Trees.Optional_Tree;
      Interp_Added : out Boolean) is
      --  Add an interpretation to the interpretation tree,
      --  given the region in which the identifier was looked up,
      --  and the expression tree (if any) representing the interpretation.
      --  If Interp is null, this implies that no possible interpretation
      --  was found in the given region.
      --  If Interp is null or equiv to something already there,
      --  no new interpretation is added and Interp_Added will
      --  be set False.
      Ptr : Interp_Tree := Interps;

      use type Type_Region_Ptr;
      use Trees;
   begin
      --  Init OUT param to default value.
      Interp_Added := False;

      while Ptr /= null loop
         --  Scan to see whether region already present in tree

         exit when Ptr.Associated_Type_Region = Associated_Type_Region;
         --  Region already present

         Ptr := Ptr.Next;
      end loop;

      if Ptr = null then
         --  Region not yet present, add it to the front
         Interps :=
           new Interpretations.Interp_Tree_Node'
           (Associated_Type_Region => Associated_Type_Region,
            List => null,
            Next => Interps);

         Ptr := Interps;
      end if;

      --  Add new interpretation (if any) to front of list for given region
      --  unless already there.
      --  TBD: This is quadratic in length of list.
      if Interp /= Null_Optional_Tree then
         declare
            Interp_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Interp).all;
            List : Interp_List := Ptr.List;
         begin
            while List /= null loop
               if Trees.Semantics.Equiv_Interps (List.Interp, Interp) then
                  --  Already there.
                  return;
               end if;
               List := List.Next;
            end loop;

            --  Not there yet
            Ptr.List :=
              new Interpretations.Interp_List_Node'
              (Interp => Interp,
               Next => Ptr.List);
            Interp_Added := True;
         end;
      end if;

   end Add_Interp;

   function Has_Type_Region
     (Interps : Interp_Tree;
      Type_Region : Type_Region_Ptr)
      return Boolean
   is
      --  Return True if interpretation tree already has
      --  a list of interpretations for given region.
      Ptr : Interp_Tree := Interps;

      use type Type_Region_Ptr;
      use Trees;
   begin
      while Ptr /= null loop
         --  Scan to see whether region already present in tree

         if Ptr.Associated_Type_Region = Type_Region then
            --  Region already present
            return True;
         end if;

         Ptr := Ptr.Next;
      end loop;

      --  Region not found
      return False;
   end Has_Type_Region;

   function Propagate_Interps (Interps : Interp_Tree) return Interp_Tree is
      --  Iterate over interpretations in Interps, and call Build_Interp
      --  with each non-null interp.  If Build_Interp returns a non-null
      --  result, add that into a new interp tree.
      --  Return the resulting tree.
      Result : Interp_Tree := null;
      Ptr : Interp_Tree := Interps;
      use Trees;
   begin
      --  Iterate over the set of regions
      while Ptr /= null loop
         declare
            Inner : Interp_List := Ptr.List;
            Interp_Added : Boolean;
         begin
            --  Start this region out with an empty list
            Add_Interp
              (Result,
               Ptr.Associated_Type_Region,
               Null_Optional_Tree,
               Interp_Added);

            --  Iterate over the set of interpretations in this region

            while Inner /= null loop
               if Inner.Interp /= Null_Optional_Tree then
                  declare
                     New_Interp : constant Optional_Tree :=
                       Build_Interp
                          (Ptr.Associated_Type_Region,
                           Inner.Interp);
                  begin
                     if New_Interp /= Null_Optional_Tree then
                        if Debug_Interpretations then
                           Put_Line
                             (" Propagate_Interps adding new interpretation: "
                              &
                              Subtree_Image (New_Interp));
                        end if;
                        Add_Interp
                          (Result,
                           Ptr.Associated_Type_Region,
                           New_Interp,
                           Interp_Added);
                     end if;
                  end;
               end if;
               Inner := Inner.Next;
            end loop;

            Ptr := Ptr.Next;
         end;
      end loop;

      return Result;
   end Propagate_Interps;

   procedure Iterate_Interps
     (Interps : Interp_Tree;
      Only_For_Type_Region : Type_Region_Ptr := null) is
      --  Call Action for each Interp in Interp tree.
      --  If Only_For_Region is non-null, restrict it to
      --  intepretations from the given region.
      Ptr : Interp_Tree := Interps;
      use Trees;
      use type Type_Region_Ptr;
   begin
      --  Iterate over the set of regions
      while Ptr /= null loop
         declare
            Inner : Interp_List := Ptr.List;
         begin
            --  Iterate over the set of interpretations in this region

            if Only_For_Type_Region = null
              or else Ptr.Associated_Type_Region = Only_For_Type_Region
            then
               while Inner /= null loop
                  if Inner.Interp /= Null_Optional_Tree then
                     Action (Ptr.Associated_Type_Region, Inner.Interp);
                  end if;
                  Inner := Inner.Next;
               end loop;
            end if;

            Ptr := Ptr.Next;
         end;
      end loop;
   end Iterate_Interps;

   function First_Interp (Interps : Interp_Tree) return Trees.Optional_Tree is
      --  Return First interp in tree, or Null_Optional_Tree if none
      Ptr : Interp_Tree := Interps;
      use Trees;
      use type Type_Region_Ptr;
   begin
      --  Iterate over the set of regions
      while Ptr /= null loop
         declare
            Inner : Interp_List := Ptr.List;
         begin
            --  Iterate over the set of interpretations in this region
            while Inner /= null loop
               if Inner.Interp /= Null_Optional_Tree then
                  --  Found a non-null interp
                  return Inner.Interp;
               end if;
               Inner := Inner.Next;
            end loop;

            Ptr := Ptr.Next;
         end;
      end loop;
      return Null_Optional_Tree;
   end First_Interp;

   function First_Interp_Region
     (Interps : Interp_Tree)
      return Type_Region_Ptr
   is
      --  Return region associated with First interp in tree,
      --  or null if none.
      Ptr : Interp_Tree := Interps;
      use Trees;
      use type Type_Region_Ptr;
   begin
      --  Iterate over the set of regions
      while Ptr /= null loop
         declare
            Inner : Interp_List := Ptr.List;
         begin
            --  Iterate over the set of interpretations in this region
            while Inner /= null loop
               if Inner.Interp /= Null_Optional_Tree then
                  --  Found first non-null interp; return its region
                  return Ptr.Associated_Type_Region;
               end if;
               Inner := Inner.Next;
            end loop;

            Ptr := Ptr.Next;
         end;
      end loop;
      return null;
   end First_Interp_Region;

   function Has_Interp (Interps : Interp_Tree) return Boolean is
      --  Return True if at least one Interp in Interp_Tree.
      use Trees;
   begin
      return First_Interp (Interps) /= Null_Optional_Tree;
   end Has_Interp;

   procedure Add_Interp_Tree
     (To_Interps : in out Interp_Tree;
      From_Interps : Interp_Tree) is
   --  Copy interps from From_Interps to To_Interps

      procedure Copy_One
        (Associated_Type_Region : Type_Region_Ptr;
         Interp : Trees.Optional_Tree) is
      --  Copy one interp to "To_Interps" tree
         Interp_Added : Boolean := False;
      begin
         Add_Interp (To_Interps, Associated_Type_Region, Interp, Interp_Added);
      end Copy_One;

      procedure Copy_All is new Iterate_Interps (Copy_One);

   begin  --  Add_Interp_Tree

      Copy_All (From_Interps);

   end Add_Interp_Tree;

   procedure Dump_Interp_Tree (Interps : Interp_Tree) is
   --  Display the contents of the interp tree

      Ptr : Interp_Tree := Interps;
      use Trees;
      use type Type_Region_Ptr;
   begin
      --  Iterate over the set of regions
      while Ptr /= null loop
         declare
            Inner : Interp_List := Ptr.List;
         begin
            Put_Line (" Interps for type-region " &
              Trees.Semantics.Type_Sem_Image
                (Ptr.Associated_Type_Region) & ":");

            --  Iterate over the set of interpretations in this region
            while Inner /= null loop
               if Inner.Interp /= Null_Optional_Tree then
                  --  Found non-null interp
                  Put_Line ("  Interp: " & Subtree_Image (Inner.Interp));
               end if;
               Inner := Inner.Next;
            end loop;

            Ptr := Ptr.Next;
         end;
      end loop;
   end Dump_Interp_Tree;

end PSC.Interpretations;
