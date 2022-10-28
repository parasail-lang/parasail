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

with PSC.Trees;
with PSC.Symbols;
package PSC.Interpretations is

   -------- Lists of interpretations used for overload resolution-----------

   --  TBD: Most of this should be moved into the private part of the package

   subtype Type_Region_Ptr is Trees.Root_Sem_Ptr;
   --  This sem info identifies the type and region

   type Interp_List_Node;
   type Interp_List is access Interp_List_Node;

   type Interp_List_Node is record
   --  a link on a chain of interpretations, all from the same module
      Interp : Trees.Optional_Tree;
      Next : Interp_List;
   end record;

   type Interp_Tree_Node;
   type Interp_Tree is access Interp_Tree_Node;

   type Interp_Tree_Node is record
   --  a link on a chain of type-regions where we already did lookups,
   --  each with its own interp list.
      Associated_Type_Region : Type_Region_Ptr;
      List : Interp_List;  --  Might be null
      Next : Interp_Tree;
   end record;

   procedure Add_Interp
     (Interps : in out Interp_Tree;
      Associated_Type_Region : Type_Region_Ptr;
      Interp : Trees.Optional_Tree;
      Interp_Added : out Boolean);
   --  Add an interpretation to the interpretation tree,
   --  given the type-region in which the identifier was looked up,
   --  and the expression tree (if any) representing the interpretation.
   --  If Interp is null, this implies that no possible interpretation
   --  was found in the given region.
   --  If Interp is null or equiv to something already there,
   --  no new interpretation is added and Interp_Added will
   --  be set False.

   function Has_Type_Region
     (Interps : Interp_Tree;
      Type_Region : Type_Region_Ptr)
      return Boolean;
   --  Return True if interpretation tree already has
   --  a list of interpretations for given region.

   function First_Interp (Interps : Interp_Tree) return Trees.Optional_Tree;
   --  Return First interp in tree, or Null_Optional_Tree if none

   function First_Interp_Region
     (Interps : Interp_Tree)
      return Type_Region_Ptr;
   --  Return region associated with First interp in tree,
   --  or null if none.

   function Has_Interp (Interps : Interp_Tree) return Boolean;
   --  Return True if at least one Interp in Interp_Tree.

   generic
      with function Build_Interp
        (Associated_Type_Region : Type_Region_Ptr;
         Operand_Interp : Trees.Optional_Tree)
         return Trees.Optional_Tree;
   function Propagate_Interps (Interps : Interp_Tree) return Interp_Tree;
   --  Iterate over interpretations in Interps, and call Build_Interp
   --  with each non-null interp.  If Build_Interp returns a non-null
   --  result, add that into a new interp tree.
   --  Return the resulting tree.

   generic
      with procedure Action
        (Associated_Type_Region : Type_Region_Ptr;
         Interp : Trees.Optional_Tree);
   procedure Iterate_Interps
     (Interps : Interp_Tree;
      Only_For_Type_Region : Type_Region_Ptr := null);
   --  Call Action for each Interp in Interp tree.
   --  If Only_For_Region is non-null, restrict it to
   --  intepretations from the given region.

   procedure Add_Interp_Tree
     (To_Interps : in out Interp_Tree;
      From_Interps : Interp_Tree);
   --  Copy interps from From_Interps to To_Interps

   procedure Dump_Interp_Tree (Interps : Interp_Tree);
   --  Display the contents of the interp tree

end PSC.Interpretations;
