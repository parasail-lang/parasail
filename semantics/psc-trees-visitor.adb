------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

package body PSC.Trees.Visitor is

   --  Action procedures for read-only trees
   procedure Unary_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Unary.Tree) is
   begin
      null;
   end Unary_Action;

   procedure Binary_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Binary.Tree) is
   begin
      null;
   end Binary_Action;

   procedure Identifier_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Identifier.Tree) is
   begin
      null;
   end Identifier_Action;

   procedure Qualified_Name_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Qualified_Name.Tree) is
   begin
      null;
   end Qualified_Name_Action;

   procedure Module_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Module.Tree) is
   begin
      null;
   end Module_Action;

   procedure Implements_Element_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Implements_Element.Tree) is
   begin
      null;
   end Implements_Element_Action;

   procedure Operation_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Operation.Tree) is
   begin
      null;
   end Operation_Action;

   procedure Obj_Decl_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Obj_Decl.Tree) is
   begin
      null;
   end Obj_Decl_Action;

   procedure Param_Decl_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Param_Decl.Tree) is
   begin
      null;
   end Param_Decl_Action;

   procedure Assign_Stmt_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Assign_Stmt.Tree) is
   begin
      null;
   end Assign_Stmt_Action;

   procedure Invocation_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Invocation.Tree) is
   begin
      null;
   end Invocation_Action;

   procedure Reference_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Reference.Tree) is
   begin
      null;
   end Reference_Action;

   procedure Conditional_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Conditional.Tree) is
   begin
      null;
   end Conditional_Action;

   procedure Type_Decl_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Type_Decl.Tree) is
   begin
      null;
   end Type_Decl_Action;

   procedure Selection_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Selection.Tree) is
   begin
      null;
   end Selection_Action;

   procedure Control_Stmt_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Control_Stmt.Tree) is
   begin
      null;
   end Control_Stmt_Action;

   procedure Case_Construct_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Case_Construct.Tree) is
   begin
      null;
   end Case_Construct_Action;

   procedure Iterator_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Iterator.Tree) is
   begin
      null;
   end Iterator_Action;

   procedure While_Stmt_Action
     (Visitor : in out RO_Tree_Visitor;
      T : While_Stmt.Tree) is
   begin
      null;
   end While_Stmt_Action;

   procedure Block_Stmt_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Block_Stmt.Tree) is
   begin
      null;
   end Block_Stmt_Action;

   procedure For_Loop_Construct_Action
     (Visitor : in out RO_Tree_Visitor;
      T : For_Loop_Construct.Tree) is
   begin
      null;
   end For_Loop_Construct_Action;

   procedure Qualifier_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Qualifier.Tree) is
   begin
      null;
   end Qualifier_Action;

   procedure Annotation_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Annotation.Tree) is
   begin
      null;
   end Annotation_Action;

   procedure Property_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Property.Tree) is
   begin
      null;
   end Property_Action;

   -------- Action procedures for R/W trees. --------

   procedure Unary_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Unary.Tree) is
   begin
      null;
   end Unary_Action;

   procedure Binary_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Binary.Tree) is
   begin
      null;
   end Binary_Action;

   procedure Identifier_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Identifier.Tree) is
   begin
      null;
   end Identifier_Action;

   procedure Qualified_Name_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Qualified_Name.Tree) is
   begin
      null;
   end Qualified_Name_Action;

   procedure Module_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Module.Tree) is
   begin
      null;
   end Module_Action;

   procedure Implements_Element_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Implements_Element.Tree) is
   begin
      null;
   end Implements_Element_Action;

   procedure Operation_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Operation.Tree) is
   begin
      null;
   end Operation_Action;

   procedure Obj_Decl_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Obj_Decl.Tree) is
   begin
      null;
   end Obj_Decl_Action;

   procedure Param_Decl_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Param_Decl.Tree) is
   begin
      null;
   end Param_Decl_Action;

   procedure Assign_Stmt_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Assign_Stmt.Tree) is
   begin
      null;
   end Assign_Stmt_Action;

   procedure Invocation_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Invocation.Tree) is
   begin
      null;
   end Invocation_Action;

   procedure Reference_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Reference.Tree) is
   begin
      null;
   end Reference_Action;

   procedure Conditional_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Conditional.Tree) is
   begin
      null;
   end Conditional_Action;

   procedure Type_Decl_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Type_Decl.Tree) is
   begin
      null;
   end Type_Decl_Action;

   procedure Selection_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Selection.Tree) is
   begin
      null;
   end Selection_Action;

   procedure Control_Stmt_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Control_Stmt.Tree) is
   begin
      null;
   end Control_Stmt_Action;

   procedure Case_Construct_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Case_Construct.Tree) is
   begin
      null;
   end Case_Construct_Action;

   procedure Iterator_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Iterator.Tree) is
   begin
      null;
   end Iterator_Action;

   procedure While_Stmt_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out While_Stmt.Tree) is
   begin
      null;
   end While_Stmt_Action;

   procedure Block_Stmt_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Block_Stmt.Tree) is
   begin
      null;
   end Block_Stmt_Action;

   procedure For_Loop_Construct_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out For_Loop_Construct.Tree) is
   begin
      null;
   end For_Loop_Construct_Action;

   procedure Qualifier_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Qualifier.Tree) is
   begin
      null;
   end Qualifier_Action;

   procedure Annotation_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Annotation.Tree) is
   begin
      null;
   end Annotation_Action;

   procedure Property_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Property.Tree) is
   begin
      null;
   end Property_Action;

end PSC.Trees.Visitor;
