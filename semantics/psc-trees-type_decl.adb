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

with PSC.Languages;
with PSC.Stream_Output;
with PSC.Trees.Visitor;
package body PSC.Trees.Type_Decl is
   --  Representation for a declaration of an object

   function Make
     (Name : Optional_Tree;   --  Name is optional on param
      Is_New_Type : Boolean := False;
      Type_Definition : Optional_Tree)
      return Optional_Tree
   is
   --  Build up a type declaration
   begin
      if Is_New_Type then
         --  It doesn't make sense to create a "new" type
         --  and not give it a name!
         pragma Assert (Not_Null (Name));
         null;
      end if;
      return Optional
               (Tree'(Trees.Tree with
                      Name => Name,
                      Is_New_Type => Is_New_Type,
                      Type_Definition => Type_Definition));
   end Make;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 0;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
   --  Return Nth operand of given Tree
   begin
      pragma Assert (False);  --  not applicable to params
      return Null_Optional_Tree;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
   --  Set Nth operand of given Tree
   begin
      raise Program_Error;
   end Set_Nth_Operand;

   function Kind (T : Tree) return Tree_Kind_Enum is
   begin
      return Type_Decl_Kind;
   end Kind;

   procedure Display_Subtree
     (T : Tree;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is
      --  Produce a human readable display of a subtree, at the given indent
      --  If Use_Short_Form is True, then elide some of the output for
      --  a module or an operation.
      use PSC.Stream_Output;
      Indent_For_Def : Natural := Indent;
   begin
      --  Display parameter declaration at given indent
      Put_Indent (On, Indent);

      if Not_Null (T.Name) then
         if Indent > 0 then
            --  This is a stand-alone type declaration
            case Languages.Language is
               when Languages.Ada_Ish =>
                  if not T.Is_New_Type then
                     --  This is a "subtype" decl
                     Indent_For_Def := 0;
                     Put (On, "sub");
                  end if;
               when Languages.ParaSail | Languages.Parython |
                    Languages.Javallel =>
                  null;
            end case;
            Put (On, "type ");
         end if;
         Display_Subtree (T.Name, On);
         Put (On, " is ");
      end if;

      if T.Is_New_Type then
         case Languages.Language is
            when Languages.Ada_Ish =>
               null;  -- "new" is put out by Module display
            when Languages.ParaSail | Languages.Parython |
              Languages.Javallel =>
               Put (On, "new ");
         end case;
      end if;

      Display_Subtree (T.Type_Definition, On, Indent => Indent_For_Def,
        Use_Short_Form => Use_Short_Form);

   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Type_Decl_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Type_Decl_Action (RW_Tree_Visitor'Class (Visitor), T);
   end Visit;

   function Find_Source_Pos
     (T : Tree)
      return Source_Positions.Source_Position
   is
      --  Walk into tree to try to find a meaningful source position
      use Source_Positions;
      T_Source_Pos : Source_Position := T.Source_Pos;
   begin
      if T_Source_Pos = Null_Source_Position then
         T_Source_Pos := Find_Source_Pos (T.Name);
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Type_Definition);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Type_Decl;
