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

with PSC.Trees.Qualified_Name;
with PSC.Trees.Lists;
package PSC.Trees.Module is
   --  Representation for an interface or class module

   use Lists;

   type Tree is new Trees.Tree with record
      Name : Optional_Tree; --  Qualified_Name or Identifier
      Import_Clauses : List;
      Add_On_Label : List;
      Is_Interface : Boolean := False;
      Is_Abstract : Boolean := False;
      Is_Private : Boolean := False;
      Is_Concurrent : Boolean := False;
      Is_Limited : Boolean := False;
      Has_Formals : Boolean := False;
      Treat_As_Type : Boolean := False;
      Module_Formals : List;
      Extends_Interface : Optional_Tree;
      Implements_Interfaces : List;
      Class_Locals : List;
      Module_Exports : List;
      Module_New_Exports : List;
      Module_Implements : List;
   end record;

   function Make
     (Name : Optional_Tree;
      Add_On_Label : List;
      Is_Interface : Boolean;
      Is_Abstract : Boolean;
      Is_Private : Boolean;
      Is_Concurrent : Boolean;
      Is_Limited : Boolean;
      Has_Formals : Boolean;
      Treat_As_Type : Boolean := False;
      Module_Formals : List;
      Extends_Interface : Optional_Tree;
      Implements_Interfaces : List;
      Class_Locals : List;
      Module_Exports : List;
      Module_New_Exports : List;
      Module_Implements : List)
      return Optional_Tree;
   --  Build up a module

   function Num_Operands (T : Tree) return Natural;
   --  Return number of operands of given Tree

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree;
   --  Return Nth operand of given Tree

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree);
   pragma No_Return (Set_Nth_Operand);
   --  Set Nth operand of given Tree

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

end PSC.Trees.Module;
