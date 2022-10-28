------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
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
package PSC.Trees.Param_Decl is
   --  Representation for a declaration of a parameter of an operation

   type Param_Kind is (
     Default_Param,
     Out_Param,
     Var_Param,
     Ref_Param,
     Ref_Const_Param,
     Ref_Out_Param,
     Ref_Var_Param,
     Global_Param,
     Global_Out_Param,
     Global_Var_Param);

   subtype Ref_Param_Kinds is Param_Kind range Ref_Param .. Ref_Var_Param;

   subtype Global_Param_Kinds is Param_Kind range
     Global_Param .. Global_Var_Param;

   Param_Is_Var : constant array (Param_Kind) of Boolean :=
     (Var_Param | Ref_Var_Param | Global_Var_Param => True,
      Out_Param | Ref_Out_Param | Global_Out_Param => True,
      Default_Param | Ref_Param | Ref_Const_Param | Global_Param => False);
   --  True if param-kind implies that actual parameter must be a variable.

   type Param_Locking is (Not_Locked, Locked_Param, Queued_Param);

   function Param_Kind_Image
     (Kind : Param_Kind; Locking : Param_Locking := Not_Locked) return String;
   --  Return Param_Kind/Locking as it would appear in the source as
   --  a prefix to the parameter name.

   use Lists;

   type Tree is new Trees.Tree with record
      Name : Optional_Tree;   --  Name is optional on param
      Kind : Param_Kind := Default_Param;
      Locking : Param_Locking := Not_Locked;
      Is_Optional : Boolean := False;     --  Param Value
                                          --  might be null
      In_Region : Optional_Tree;
      Param_Type : Optional_Tree;      --  Type is actually *not*
                                       --  optional
      Param_Default : Optional_Tree;
      Is_Implicit_Module_Param : Boolean := False;
   end record;

   function Make
     (Name : Optional_Tree;   --  Name is optional on param
      Kind : Param_Kind;
      Locking : Param_Locking;
      Is_Optional : Boolean;
      Param_Type : Optional_Tree;      --  Type is actually *not*
                                       --  optional
      Param_Default : Optional_Tree;
      In_Region : Optional_Tree := Null_Optional_Tree;
      Is_Implicit_Module_Param : Boolean := False)
      return Optional_Tree;
   --  Build up a parameter declaration

   function Num_Operands (T : Tree) return Natural;
   --  Return number of operands of given Tree

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree;
   --  Return Nth operand of given Tree

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree);
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

end PSC.Trees.Param_Decl;
