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

with PSC.Trees.Unary;
with PSC.Trees.Binary;
with PSC.Trees.Identifier;
with PSC.Trees.Qualified_Name;
with PSC.Trees.Module;
with PSC.Trees.Implements_Element;
with PSC.Trees.Operation;
with PSC.Trees.Obj_Decl;
with PSC.Trees.Param_Decl;
with PSC.Trees.Assign_Stmt;
with PSC.Trees.Invocation;
with PSC.Trees.Reference;
with PSC.Trees.Conditional;
with PSC.Trees.Type_Decl;
with PSC.Trees.Selection;
with PSC.Trees.Control_Stmt;
with PSC.Trees.Case_Construct;
with PSC.Trees.Iterator;
with PSC.Trees.While_Stmt;
with PSC.Trees.Block_Stmt;
with PSC.Trees.For_Loop_Construct;
with PSC.Trees.Qualifier;
with PSC.Trees.Annotation;
with PSC.Trees.Property;
package PSC.Trees.Visitor is

   type RO_Tree_Visitor is abstract new Root_RO_Tree_Visitor with null record;
   --  Extension of Root_RO_Tree_Visitor with "action" procedures

   --  Action procedures for R/O trees (default to no-op)
   procedure Unary_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Unary.Tree);

   procedure Binary_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Binary.Tree);

   procedure Identifier_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Identifier.Tree);

   procedure Qualified_Name_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Qualified_Name.Tree);

   procedure Module_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Module.Tree);

   procedure Implements_Element_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Implements_Element.Tree);

   procedure Operation_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Operation.Tree);

   procedure Obj_Decl_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Obj_Decl.Tree);

   procedure Param_Decl_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Param_Decl.Tree);

   procedure Assign_Stmt_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Assign_Stmt.Tree);

   procedure Invocation_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Invocation.Tree);

   procedure Reference_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Reference.Tree);

   procedure Conditional_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Conditional.Tree);

   procedure Type_Decl_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Type_Decl.Tree);

   procedure Selection_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Selection.Tree);

   procedure Control_Stmt_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Control_Stmt.Tree);

   procedure Case_Construct_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Case_Construct.Tree);

   procedure Iterator_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Iterator.Tree);

   procedure While_Stmt_Action
     (Visitor : in out RO_Tree_Visitor;
      T : While_Stmt.Tree);

   procedure Block_Stmt_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Block_Stmt.Tree);

   procedure For_Loop_Construct_Action
     (Visitor : in out RO_Tree_Visitor;
      T : For_Loop_Construct.Tree);

   procedure Qualifier_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Qualifier.Tree);

   procedure Annotation_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Annotation.Tree);

   procedure Property_Action
     (Visitor : in out RO_Tree_Visitor;
      T : Property.Tree);

   type RW_Tree_Visitor is abstract new Root_RW_Tree_Visitor with null record;
   --  Extension of Root_RW_Tree_Visitor with "action" procedures

   --  Action procedures for R/W trees (default to no-op)
   procedure Unary_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Unary.Tree);

   procedure Binary_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Binary.Tree);

   procedure Identifier_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Identifier.Tree);

   procedure Qualified_Name_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Qualified_Name.Tree);

   procedure Module_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Module.Tree);

   procedure Implements_Element_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Implements_Element.Tree);

   procedure Operation_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Operation.Tree);

   procedure Obj_Decl_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Obj_Decl.Tree);

   procedure Param_Decl_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Param_Decl.Tree);

   procedure Assign_Stmt_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Assign_Stmt.Tree);

   procedure Invocation_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Invocation.Tree);

   procedure Reference_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Reference.Tree);

   procedure Conditional_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Conditional.Tree);

   procedure Type_Decl_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Type_Decl.Tree);

   procedure Selection_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Selection.Tree);

   procedure Control_Stmt_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Control_Stmt.Tree);

   procedure Case_Construct_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Case_Construct.Tree);

   procedure Iterator_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Iterator.Tree);

   procedure While_Stmt_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out While_Stmt.Tree);

   procedure Block_Stmt_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Block_Stmt.Tree);

   procedure For_Loop_Construct_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out For_Loop_Construct.Tree);

   procedure Qualifier_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Qualifier.Tree);

   procedure Annotation_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Annotation.Tree);

   procedure Property_Action
     (Visitor : in out RW_Tree_Visitor;
      T : in out Property.Tree);

end PSC.Trees.Visitor;
