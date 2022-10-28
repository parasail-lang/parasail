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

with PSC.Strings;
with PSC.Stream_Output;
with PSC.Trees.Visitor;
package body PSC.Trees.Identifier is

   function Make
     (Str : Strings.U_String;
      Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position)
      return Optional_Tree
      --  Build up an identifier Tree
   is
      Result : constant Optional_Tree :=
        Optional (Tree'(Trees.Tree with Str => Str));
      use type Strings.U_String_Index;
   begin
      if Source_Pos.File /= Strings.Null_U_String_Index then
         --  Fill in the source position information
         Set_Source_Pos (Result, Source_Pos);
      end if;
      return Result;
   end Make;

   function Make
     (Id : String;
      Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position)
      return Optional_Tree
      --  Build up an identifier Tree
   is
   begin
      return Make (PSC.Strings.String_Lookup (Id), Source_Pos);
   end Make;

   procedure Display_Subtree
     (T : Tree;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is
      --  Produce a human readable display of a subtree, at the given indent
      --  If Use_Short_Form is True, then elide some of the output for
      --  a module or an operation.
      use PSC.Stream_Output;
      use type Strings.U_String;
   begin
      Put_Indent (On, Indent);
      if T.Str /= Strings.Null_U_String then
         Put (On, PSC.Strings.To_String (T.Str));
      end if;
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Identifier_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Identifier_Action (RW_Tree_Visitor'Class (Visitor), T);
   end Visit;

   function Find_Source_Pos
     (T : Tree)
      return Source_Positions.Source_Position
   is
   --  Walk into tree to try to find a meaningful source position
   begin
      return T.Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Identifier;
