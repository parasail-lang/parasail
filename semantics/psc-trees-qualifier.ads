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

package PSC.Trees.Qualifier is

   type Qualifier_Enum is (
     Is_Ref,
     Is_Abstract,
     Is_Optional,
     Is_Not_Null,
     Is_Mutable,
     Is_Concurrent,
     Is_Var,
     Is_Const,
     Is_Polymorphic);

   type Qualifier_Set is array (Qualifier_Enum) of Boolean;

   type Tree is new Trees.Tree with record
      Qualifiers : Qualifier_Set := (others => False);
      Operand : Optional_Tree;
   end record;

   function Qualify
     (Qualifiers : Qualifier_Set;
      Operand : Optional_Tree)
      return Optional_Tree;
   --  Create/Update a Qualifier tree.

   function Qualifiers (OT : Optional_Tree) return Qualifier_Set;
   --  If OT is a Qualifier Tree, return qualifier set.
   --  If not, return (others => False).

   function Unqualified_Tree (OT : Optional_Tree) return Optional_Tree;
   --  If OT is a Qualifier Tree, return OT.Operand.
   --  If not, return OT.

   function Num_Operands (T : Tree) return Natural;
   --  Return number of operands of given Tree

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree;
   --  Return Nth operand of given Tree

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree);
   --  Set Nth operand of given Tree

   function Kind (T : Tree) return Tree_Kind_Enum;
   -- Return tree type as enum

   procedure Display_Subtree
     (T : Tree;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False);
   --  Produce a human readable display of a subtree, at the given indent
   --  If Use_Short_Form is True, then elide some of the output for
   --  a module or an operation.

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class);
   --  Call appropriate RO *_Action procedure on Visitor

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class);
   --  Call appropriate RW *_Action procedure on Visitor

   function Find_Source_Pos
     (T : Tree)
      return Source_Positions.Source_Position;
   --  Walk into tree to try to find a meaningful source position

end PSC.Trees.Qualifier;
