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

with PSC.Stream_Output;
with PSC.Trees.Visitor;
with PSC.Messages;
with PSC.Languages;
package body PSC.Trees.Module is

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
      return Optional_Tree
   is
   --  Build up a module
      use type Languages.Language_Enum;
   begin
      if Is_Interface then
         --  No class locals in an interface
         pragma Assert (Length (Class_Locals) = 0);
         null;
      else
         --  No abstract or private classes allowed
         pragma Assert (not Is_Abstract);
         null;
         pragma Assert (not Is_Private);
         null;
      end if;

      if not Has_Formals then
         --  No formals if "Has_Formals" is False
         pragma Assert (Length (Module_Formals) = 0);
         null;
         --  If no formals, must either extend an interface or
         --  be an interface add-on
         --  or be a class, as of 10-Sep-2010 --> TBD: eliminate repeat
         --  of parameters, since have to look at interface for types
         --  and nested abstract interfaces declared there anyway?
         if Is_Interface then
            if not Is_Private
              and then Is_Null (Extends_Interface)
              and then Length (Add_On_Label) = 0
              and then Languages.Language = Languages.ParaSail
            then
               Messages.Parser_Error ("Interface requires parameters",
                 Find_Source_Pos (Name));
            end if;
         end if;
      else
         --  Has formals, so can't be an add-on to an interface
         --  (can be a class variant, such as "class Set[2] <blah>")
         if Is_Private then
            Messages.Parser_Error
              ("Private interface should not have parameters",
                 Find_Source_Pos (Name));
         end if;
         if Length (Add_On_Label) > 0 then
            if Is_Interface then
               Messages.Parser_Error
                 ("Interface add-on should not have parameters.",
                    Find_Source_Pos (Name));
            elsif Length (Add_On_Label) > 1 then
               Messages.Parser_Error
                 ("Class add-on should not have parameters.",
                    Find_Source_Pos (Name));
            end if;
         end if;
      end if;

      return Optional
               (Tree'(Trees.Tree with
                      Name => Name,
                      Add_On_Label => Add_On_Label,
                      Import_Clauses => Lists.Empty_List,
                      Is_Interface => Is_Interface,
                      Is_Abstract => Is_Abstract,
                      Is_Private => Is_Private,
                      Is_Concurrent => Is_Concurrent,
                      Is_Limited => Is_Limited,
                      Has_Formals => Has_Formals,
                      Treat_As_Type => Treat_As_Type,
                      Module_Formals => Module_Formals,
                      Extends_Interface => Extends_Interface,
                      Implements_Interfaces => Implements_Interfaces,
                      Class_Locals => Class_Locals,
                      Module_Exports => Module_Exports,
                      Module_New_Exports => Module_New_Exports,
                      Module_Implements => Module_Implements));
   end Make;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 0;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
   --  Return Nth operand of given Tree
   begin
      pragma Assert (False);  --  not applicable to modules
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

   procedure Display_Subtree
     (T : Tree;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is
      --  Produce a human readable display of a subtree, at the given indent
      --  If Use_Short_Form is True, then elide some of the output for
      --  a module or an operation.
      use PSC.Stream_Output;
      use type Languages.Language_Enum;
   begin
      --  Display import clauses, if any
      if not Use_Short_Form
        and then not Lists.Is_Empty (T.Import_Clauses)
      then
         case Languages.Language is
            when Languages.Parasail | Languages.Parython |
              Languages.Javallel =>
               Put_Line (On, "import", Indent => Indent);
            when Languages.Ada_Ish =>
               Put_Line (On, "with", Indent => Indent);
         end case;

         Display_List (T.Import_Clauses, On,
           Separator => ",",
           Terminator => ";",
           Indent => Indent + 2);
      end if;

      --  Display module at given indent
      if not T.Treat_As_Type then
         Put_Indent (On, Indent);
      end if;

      if T.Is_Abstract then
         Put (On, "abstract ");
      end if;
      if T.Is_Concurrent then
         Put (On, "concurrent ");
      end if;
      if T.Treat_As_Type then
         --  Finish display as a type

         if T.Is_Limited then
            Put (On, "limited ");
         end if;
         if Not_Null (T.Extends_Interface) then
            Put (On, "new ");
            Display_Subtree (T.Extends_Interface, On);
            if Length (T.Implements_Interfaces) > 0 then
               New_Line (On, Indent => Indent + 2);
               Put (On, " and ");
               Display_List
                 (T.Implements_Interfaces,
                  On,
                  Separator => " and ");
            end if;
            if not T.Is_Private
              and then Lists.Is_Empty (T.Module_Exports)
              and then Lists.Is_Empty (T.Class_Locals)
            then
               return;   --  return now
            end if;
            --  Display extension part
            Put (On, " with ");
         end if;
         if T.Is_Private then
            Put (On, "private");
            return;  --  return now
         elsif Lists.Is_Empty (T.Module_Exports)
           and then Lists.Is_Empty (T.Class_Locals)
         then
            Put (On, "null record");
            return;  --  return now
         else
            Put (On, "record ");
         end if;

      else
         --  Display as a module
         if T.Is_Private then
            Put (On, "private ");
         end if;

         case Languages.Language is
            when Languages.Parasail | Languages.Parython |
              Languages.Javallel =>
               if T.Is_Interface then
                  Put (On, "interface ");
               else
                  Put (On, "class ");
               end if;
            when Languages.Ada_Ish =>
               if T.Is_Interface then
                  Put (On, "package ");
               else
                  Put (On, "package body ");
               end if;
         end case;

         Display_Subtree (T.Name, On);

         if Length (T.Add_On_Label) > 0 then
            Put (On, "[");
            Display_List (T.Add_On_Label, On, Separator => ", ");
            Put (On, "]");
         end if;

         if T.Has_Formals then
            declare
               Indent_For_Formals : Natural := 0;
            begin
               if Lists.Length (T.Module_Formals) > 1 then
                  New_Line (On, Indent => Indent + 2);
                  Indent_For_Formals := Indent + 3;
               end if;
               Put (On, "<");
               Display_List
                 (T.Module_Formals,
                  On,
                  Separator => "; ",
                  Indent => Indent_For_Formals);
               Put (On, "> ");
            end;
         else
            Put (On, " ");
         end if;

         --  Put out module ancestry

         if Not_Null (T.Extends_Interface) then
            New_Line (On, Indent => Indent + 2);
            Put (On, "extends ");
            Display_Subtree (T.Extends_Interface, On);
            Put (On, " ");
         end if;

         if Length (T.Implements_Interfaces) > 0 then
            New_Line (On, Indent => Indent + 2);
            Put (On, "implements ");
            Display_List
              (T.Implements_Interfaces,
               On,
               Separator => ", ",
               Indent => Indent + 4);
            Put (On, " ");
         end if;

         Put (On, "is ");
      end if;

      if Use_Short_Form then
         Put (On, "...");

         return; ------ return now -------
      end if;

      New_Line (On);

      if not T.Is_Interface then
         Display_List
           (T.Class_Locals,
            On,
            Indent => Indent + 4,
            Separator => ";",
            Terminator => ";");
         if not T.Treat_As_Type then
            Put_Line (On, "exports", Indent + 2);
         end if;
      end if;

      Display_List
        (T.Module_Exports,
         On,
         Indent => Indent + 4,
         Separator => ";",
         Terminator => ";");

      if not Lists.Is_Empty (T.Module_New_Exports) then
         --  We have a "new" section
         New_Line (On);
         Put_Line (On, "new", Indent + 2);
         Display_List
           (T.Module_New_Exports,
            On,
            Indent => Indent + 4,
            Separator => ";",
            Terminator => ";");
      end if;
      if not Lists.Is_Empty (T.Module_Implements) then
         --  We have an "implements" section
         New_Line (On);
         Put_Line (On, "implements", Indent + 2);
         Display_List
           (T.Module_Implements,
            On,
            Indent => Indent + 2,
            Separator => "",
            Terminator => " ");
      end if;
      Put_Indent (On, Indent);
      Put (On, "end ");
      case Languages.Language is
         when Languages.ParaSail | Languages.Parython |
              Languages.Javallel =>
            if T.Is_Interface then
               Put (On, "interface ");
            else
               Put (On, "class ");
            end if;
         when Languages.Ada_Ish =>
            if T.Treat_As_Type then
               Put (On, "record ");
            else
               Put (On, "package ");
            end if;
      end case;
      Display_Subtree (T.Name, On);

      if Length (T.Add_On_Label) > 0 then
         Put (On, "[");
         Display_List (T.Add_On_Label, On, Separator => ", ");
         Put (On, "]");
      end if;

   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Module_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Module_Action (RW_Tree_Visitor'Class (Visitor), T);
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
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Module;
