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

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with PSC.Command_Input;
with PSC.Interpreter;
with PSC.Object_Access;
with PSC.Languages;
with PSC.Messages;
with PSC.Strings;       use type PSC.Strings.U_String;
with PSC.Symbols;       use PSC.Symbols;

with PSC.Trees.Lists;
pragma Elaborate (PSC.Trees.Lists);

with PSC.Trees.Identifier;
with PSC.Trees.Module;
with PSC.Trees.Operation;
with PSC.Trees.Property;
with PSC.Trees.Qualified_Name;
with PSC.Trees.Selection;

with PSC.Trees.Semantics.Debug;   use PSC.Trees.Semantics.Debug;
with PSC.Trees.Semantics.Info;
with PSC.Trees.Semantics.Static;
with PSC.Trees.Semantics.Dynamic;

with PSC.Versions;

package body PSC.Trees.Semantics is

   --  Semantics driver

   Library : Lists.List renames Symbols.Library_Region.Stmt_List;
   Library_Bodies : Lists.List;  --  This is appended to Library after
                                 --  parsing is complete.

   Debugger_Console : Boolean := False;
      --  If True, will stop in debugger console right away.

   --  Current list of module imports.
   Current_Import_Clauses : Lists.List := Lists.Empty_List;

   function Convert_Argument
      (Argument : String;
       Param_Type_Desc : Interpreter.Type_Descriptor_Ptr;
       Target_Stg_Rgn : Interpreter.Stg_Rgn_Ptr)
      return Interpreter.Word_Type is
   --  Convert argument according to parameter type.
      use Interpreter;
      Value : Word_Type;
   begin
      if not Dynamic.String_Is_Convertible
               (Argument,
                Param_Type_Desc)
      then
         --  Give a warning
         Put_Line
           ("Warning: '" &
            Argument &
            "' is not convertible to " &
            Strings.To_String
               (Param_Type_Desc.Name));
         if Param_Type_Desc.Is_Small then
            Value := Dynamic.Literal_Value (Argument);
            Put_Line
              (" passing in " & Hex_Image (Value));
         else
            Put_Line (" passing in null");
            Value := Interpreter.Null_For_Stg_Rgn (Target_Stg_Rgn);
         end if;
      else
         Value :=
            Dynamic.Convert_String
              (Argument,
               Param_Type_Desc,
               Target_Stg_Rgn);
      end if;
      return Value;
   end Convert_Argument;

   procedure Execute_Command (Words : Strings.U_String_Array;
                              Quit_Now : out Boolean) is
      use Strings;
      Op : constant Strings.U_String := Words (Words'First);
      Op_Name : constant String := To_String (Op);  --  Might be "A::B::C"
      Parts : constant Strings.U_String_Array :=
        Strings.Tokenize (Op_Name, ":.");  --  Break into A, B, C
      Op_Sym : Symbols.Sym_Ptr :=
         Symbols.Lookup_In_Region
           (Symbols.Library_Region, Parts (Parts'First));  -- Lookup "A" part
      use type Symbols.Sym_Ptr;
      Num_Parts_Found : Natural := 0;
   begin

      --  Set to True if user has requested the interpreter to shut down.
      Quit_Now := False;

      if Op_Sym = null and then Parts'Length = 1
        and then (Op_Name (Op_Name'First) in 'A' .. 'Z'
          or else
            Op_Name = "main"
          or else
            (Op_Name'Length >= 4
               and then Op_Name (Op_Name'First .. Op_Name'First + 3) = "test"))
      then
         --  Preface with "<std-lib-prefix>.Test"  (e.g. "PSL.Test")
         --  or "<std-lib-prefix>.test"  (e.g. "java.test")
         Op_Sym := Symbols.Lookup_In_Region (Symbols.Library_Region,
           Strings.String_Lookup (Languages.Standard_Library_Prefix));
         if Op_Sym /= null then
            declare
               Test_Module : Symbols.Sym_Ptr :=
                 Symbols.Lookup_In_Region (Op_Sym.Nested_Region,
                   Strings.String_Lookup ("Test"));
            begin
               if Test_Module = null then
                  Test_Module := Symbols.Lookup_In_Region
                    (Op_Sym.Nested_Region, Strings.String_Lookup ("test"));
               end if;
               if Test_Module /= null then
                  Op_Sym := Symbols.Lookup_In_Region
                    (Test_Module.Nested_Region, Parts (Parts'First));
               end if;
            end;
         end if;
      end if;

      if Debug_Second_Pass then
         if Parts'Length > 1 then
            Put (" Split into ");
            for I in Parts'Range loop
               if I > Parts'First then
                  Put (", ");
               end if;
               Put (To_String (Parts (I)));
            end loop;
            New_Line;
         end if;
      end if;

      --  Look up remainder of Op name
      for I in Parts'First + 1 .. Parts'Last loop
         exit when Op_Sym = null;
         if Op_Sym.Kind /= Module_Sym_Kind
           or else Op_Sym.Nested_Region = null
         then
            --  Not a module name
            Op_Sym := null;
            exit;
         end if;
         Num_Parts_Found := Num_Parts_Found + 1;
         --  Look up next part
         Op_Sym := Symbols.Lookup_In_Region (Op_Sym.Nested_Region, Parts (I));
      end loop;

      if Op_Sym = null then
         --  Name not in symbol table
         --  See whether is a built-in op
         if Num_Parts_Found > 0 then
            Put_Line (" " & To_String (Parts (Parts'First + Num_Parts_Found))
              & " not found.");
         elsif Op_Name = "console"
           or else Op_Name = "debugger"
           or else Op_Name = "psldb"
         then
            --  Turn on the debugger console
            Debugger_Console := True;
         elsif Op_Name = "debug" then
            --  Debug command
            if Words'Length = 1 then
               --  Default is to turn on all debugging
               Turn_On_Debugging;
            else
               declare
                  Operand : constant String :=
                    To_String (Words (Words'First + 1));
               begin
                  if Operand = "on" then
                     Turn_On_Debugging;
                  elsif Operand = "off" then
                     Turn_Off_Debugging;
                  elsif Operand = "console" then
                     Debugger_Console := True;
                  elsif Operand (Operand'First) in '0' .. '9'
                    or else Operand (Operand'First) = '+'
                  then
                     Turn_On_Debugging (Integer'Value (Operand));
                  elsif Operand (Operand'First) = '-' then
                     Turn_Off_Debugging (-Integer'Value (Operand));
                  else
                     --  Not understood,
                     Put_Line
                       ("Operand """ & Operand & """ not understood;" &
                          " should be ""on,"" ""off,""" &
                          " or an optionally signed digit.");
                  end if;
               exception
                  when others =>
                     --  Not understood,
                     Put_Line
                       ("Operand """ & Operand & """ not understood.");
               end;
            end if;
         elsif Op_Name = "checks" then
            if Words'Length = 1 then
               --  Default is to turn on checks
               Interpreter.Set_Run_Time_Checks (On => True);
            else
               declare
                  Operand : constant String :=
                    To_String (Words (Words'First + 1));
               begin
                  if Operand = "on" then
                     Interpreter.Set_Run_Time_Checks (On => True);
                  elsif Operand = "off" then
                     Interpreter.Set_Run_Time_Checks (On => False);
                  else
                     --  Not understood,
                     Put_Line
                       ("Operand """ & Operand & """ not understood;" &
                          " should be ""on"" or ""off.""");
                  end if;
               end;
            end if;
         elsif Op_Name = "stats" then
            if Words'Length = 1 then
               --  Default is to display stats and keep counting
               Interpreter.Show_Stats (Clear => False);
            else
               declare
                  Operand : constant String :=
                    To_String (Words (Words'First + 1));
               begin
                  if Operand = "clear" then
                     Interpreter.Show_Stats (Clear => True);
                  elsif Operand = "keep" then
                     Interpreter.Show_Stats (Clear => False);
                  else
                     --  Not understood,
                     Put_Line
                       ("Operand """ & Operand & """ not understood;" &
                          " should be ""clear"" or ""keep.""");
                  end if;
               end;
            end if;
         elsif Op_Name = "servers" then
            if Words'Length = 1 then
               --  Display some info about servers
               Interpreter.Show_Servers;
            else
               declare
                  Operand : constant String :=
                    To_String (Words (Words'First + 1));
                  Count : Integer := 0;
               begin
                  case Operand (Operand'First) is
                  when '0' .. '9' =>
                     Count := Integer'Value (Operand);
                     Interpreter.Set_Server_Count (Count);
                     Interpreter.Show_Servers;
                  when '+' | '-' =>
                     Count := Integer'Value (Operand);
                     Interpreter.Bump_Server_Count (Count);
                     Interpreter.Show_Servers;
                  when others =>
                     raise Program_Error;
                  end case;
               exception
                  when others =>
                     --  Not understood,
                     Put_Line
                       ("Operand """ & Operand & """ not understood;" &
                          " should be ""[+|-]<count>"" where number of" &
                          " servers will be set to, or bumped by <count>");
               end;
            end if;
         else
            Put_Line ("Routine """ & Op_Name & """ not defined.");
         end if;
         --  Try again ...
      else
         declare
            use type Symbols.Sym_Kind_Enum;
            use Semantics.Info;
         begin
            if Op_Sym.Kind /= Symbols.Operation_Sym_Kind then
               Put_Line (Op_Name & " not an operation.");
            else
               --  Execute operation
               declare
                  use Interpreter;
                  Op_Sem : Info.Operation_Semantic_Info renames
                    Info.Operation_Semantic_Info (Op_Sym.Sem_Info.all);
                  Op_Def : Operation.Tree renames Operation.Tree
                    (Tree_Ptr_Of (Op_Sym.Definition).all);
                  Num_Inputs : constant Natural :=
                    Lists.Length (Op_Def.Operation_Inputs);
                  Num_Outputs : constant Natural :=
                    Lists.Length (Op_Def.Operation_Outputs);
                  Last_Param : Info.Param_Sem_Ptr := null;
                  Last_Param_Is_Array : Boolean := False;
               begin
                  if Num_Inputs > 0 then
                     --  Check to see if last input is an array
                     declare
                        Last_Param_Tree : constant Optional_Tree :=
                          Lists.Nth_Element
                            (Op_Def.Operation_Inputs, Num_Inputs);
                     begin
                        Last_Param :=
                          Info.Param_Sem_Ptr (Sem_Info (Last_Param_Tree));
                        if Last_Param /= null
                          and then Last_Param.Resolved_Type /= null
                          and then Strings.To_String
                            (Last_Param.Resolved_Type.Associated_Module.
                               Associated_Symbol.Str) = "Basic_Array"
                        then
                           Last_Param_Is_Array := True;
                        end if;
                     end;
                  end if;
                  if Op_Sem.Routine = null then
                     Put_Line (Op_Name & " has not been compiled.");
                  elsif Num_Inputs /= Words'Length - 1
                    and then (not Last_Param_Is_Array
                      or else Words'Length - 1 < Num_Inputs - 1)
                  then
                     --  Too few arguments provided, or too many and
                     --  last param is not an array.
                     Put_Line
                       (Op_Name &
                        " expecting" &
                        Natural'Image (Num_Inputs) &
                        " inputs, found" &
                        Natural'Image (Words'Length - 1));
                  else
                     declare
                        Initial_Offset : constant Offset_Within_Area := 30;
                        New_Local_Area : constant Word_Ptr :=
                          Object_To_Physical_Address
                            ((Interpreter.Global_Stack_Chunk,
                              Offset => Initial_Offset));
                        Initial_Tcb : constant Word_Ptr :=
                          Interpreter.Initial_Tcb
                            (Target_Routine =>
                               Routine_Ptr (Op_Sem.Routine),
                             New_Tcb => Object_To_Physical_Address
                               ((Interpreter.Global_Stack_Chunk,
                                 Offset =>
                                   Initial_Offset -
                                    (Interpreter.Thread_Control_Block_Size +
                                     Interpreter.Thread_Master_Size))),
                             Server_Index => Main_Thread_Server_Index);
                        Initial_Stg_Rgn : constant Stg_Rgn_Ptr :=
                           Get_New_Local_Stg_Rgn
                             (New_Local_Area => New_Local_Area,
                              Server_Index => Main_Thread_Server_Index);
                        Context : Exec_Context :=
                          (Local_Null => Initial_Stg_Rgn.Null_Value,
                           Enclosing_Type => null,
                           Local_Stg_Rgn => Initial_Stg_Rgn,
                           Control_Area => Initial_Tcb,
                           Server_Index =>
                             Interpreter.Main_Thread_Server_Index,
                           Open_Master => null,
                           Params => Object_To_Physical_Address
                             ((Interpreter.Global_Stack_Chunk,
                               Offset =>
                                 Interpreter.Global_Stack_Chunk.Data'First)),
                           Local_Area => New_Local_Area,
                           Local_Area_Length =>
                             Interpreter.Global_Stack_Chunk.Chunk_Length -
                               Initial_Offset,
                           Start_Callee_Locals =>
                             Op_Sem.Routine.Start_Callee_Locals);

                        Thread_Was_Queued : Boolean;
                     begin
                        --  Initialize static link in new local area to null.
                        Init_Static_Link (New_Local_Area);

                        for I in 1 .. Num_Inputs loop
                           --  Store value for each (literal) operand
                           declare
                              Param_Index : Natural := Num_Outputs + I;
                              Matching_Input : Interpreter.Routine_Param_Info
                                 renames Op_Sem.Routine.Parameters
                                   (Param_Index);
                              Param_Type_Desc : constant Type_Descriptor_Ptr :=
                                Known_Type_Desc (Matching_Input.Type_Info);
                              Array_Elem_Type : Type_Descriptor_Ptr;
                              Value : Word_Type;
                           begin
                              if Last_Param_Is_Array
                                and then I = Num_Inputs
                              then
                                 --  Treat remaining words as elements of
                                 --  last-param array.
                                 Value := Interpreter.Create_Basic_Array_Obj
                                   (Param_Type_Desc,
                                    Array_Len     => Words'Length - I,
                                    Stg_Rgn       => Context.Local_Stg_Rgn,
                                    Server_Index  => Context.Server_Index);

                                 if Param_Type_Desc.Num_Parameters = 1 then
                                    --  ParaSail-style basic array type
                                    Array_Elem_Type :=
                                      Param_Type_Desc.Parameters
                                        (1).Data.Type_Desc;
                                 else
                                    --  Ada/SPARK-style basic array type
                                    Array_Elem_Type :=
                                      Param_Type_Desc.Enclosing_Type.Parameters
                                        (1).Data.Type_Desc;
                                 end if;

                                 for J in 1 .. Words'Length - I loop
                                    Interpreter.Store_Word
                                     (Value +
                                      Interpreter.Large_Obj_Header_Size +
                                      Offset_Within_Area (J),
                                      Convert_Argument
                                        (Strings.To_String (Words (J + I)),
                                         Array_Elem_Type,
                                         Context.Local_Stg_Rgn));
                                 end loop;
                              else
                                 --  Treat word as value of parameter.
                                 Value := Convert_Argument
                                   (Strings.To_String (Words (I + 1)),
                                    Param_Type_Desc,
                                    Context.Local_Stg_Rgn);
                              end if;

                              Store_Word
                                (Context,
                                 (Param_Area,
                                  Offset_Within_Area (Param_Index - 1),
                                  No_VM_Obj_Id),
                                 Value);
                           end;
                        end loop;

                        --  Execute compiled code
                        Execute
                          (Routine_Ptr (Op_Sem.Routine),
                           Start_Pc => 1,
                           Context => Context,
                           Thread_Was_Queued => Thread_Was_Queued,
                           Debugger_Console => Debugger_Console);

                        pragma Assert (not Thread_Was_Queued);

                        --  Free up the initial tcb/master
                        Free_Initial_Tcb (Initial_Tcb,
                          Server_Index => Main_Thread_Server_Index);

                        if Context.Server_Index /=
                             Main_Thread_Server_Index
                        then
                           --  Setting server_index to bad value
                           --  indicates operation was aborted by user.
                           --  TBD: We should have a better indicator.
                           Quit_Now := True;

                        elsif Num_Outputs > 0 then
                           Put ("Result : ");
                           for I in 1 .. Num_Outputs loop
                              declare
                                 Val : constant Word_Type :=
                                   Fetch_Word
                                      (Context,
                                       (Param_Area,
                                        Offset_Within_Area (I - 1),
                                        No_VM_Obj_Id));
                              begin
                                 if I > 1 then
                                    Put (", ");
                                 end if;
                                 if Val = Null_Value then
                                    Put ("null");
                                 else
                                    Put (Word_Type'Image (Val));
                                 end if;
                              end;
                           end loop;
                           New_Line;
                        end if;

                        --  Release local region
                        Release_Stg_Rgn (Context.Local_Stg_Rgn);
                     exception
                        when E : others =>
                           Sem_Error
                             (Op_Def,
                              Ada.Exceptions.Exception_Name (E) &
                              " raised in execution");
                     end;
                  end if;

               end;
            end if;
         end;
      end if;
   end Execute_Command;

   procedure Sem_Message_Helper
     (S : String;
      Src_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position;
      Message_Kind : String := "Error") is
      --  Produce an error message on the current error output.
      --  NOTE: This is called from Sem_Error twice, once to write out
      --       to Standard_Error, and once to the Error_File.
      use Ada.Text_IO;
      use type Strings.U_String;
      use Source_Positions;
   begin
      if Src_Pos /= Null_Source_Position then
         Put (Current_Error, Source_Positions.Image (Src_Pos) & ' ');
      end if;
      Put_Line (Current_Error, Message_Kind & ": " & S);
   end Sem_Message_Helper;

   ------- Visible Subprograms --------

   procedure Give_Copyright (File : Ada.Text_IO.File_Type) is
   --  Produce a copyright/license notice on the given File.
   begin
      Put_Line
        (File,
         Languages.Language_Name & " Interpreter and Virtual Machine" &
         PSC.Versions.Version_String);
      Put_Line (File, "Copyright (C) 2011-2021, AdaCore, New York NY, USA");
      Put_Line (File, "This program is provided ""as is"" with no warranty.");
      case Languages.Language is
         when Languages.ParaSail | Languages.Parython
            | Languages.Ada_Ish
            | Languages.Javallel =>
            Put_Line
              (File,
               "Report problems at " &
               "http://groups.google.com/group/" &
               "parasail-programming-language");
      end case;
      New_Line (File);
   end Give_Copyright;

   procedure Sem_Warning
     (S : String;
      Src_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position) is
      --  Produce a warning message on the standard error output.
   begin
      Messages.Put_Warning (S, Src_Pos);
   end Sem_Warning;

   procedure Sem_Error
     (S : String;
      Src_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position) is
      --  Produce an error message on the standard error output.
      use Ada.Text_IO;
   begin
      Messages.Put_Error (S, Src_Pos);
      Sem_Error_Count := Sem_Error_Count + 1;
   end Sem_Error;

   procedure Sem_Error (OT : Optional_Tree; S : String) is
      --  Produce an error message on the Current_Error
      use Ada.Text_IO;
   begin
      if Not_Null (OT) then
         Sem_Error (S & ':', Find_Source_Pos (OT));
         Messages.Put_Message
           ("  " & Subtree_Image (OT, Use_Short_Form => True),
            Find_Source_Pos (OT), Message_Kind => "Info");
      else
         Sem_Error (S);
      end if;
   end Sem_Error;

   procedure Sem_Error (T : Trees.Tree'Class; S : String) is
      --  Produce an error message on the Current_Error
      use Ada.Text_IO;
   begin
      Sem_Error (S & ':', Find_Source_Pos (T));
      Messages.Put_Message ("  " & Subtree_Image (T, Use_Short_Form => True),
        Find_Source_Pos (T), Message_Kind => "Info");
   end Sem_Error;

   function Type_Sem_Image
     (Type_Sem : Root_Sem_Ptr;
      Use_Short_Form : Boolean := False)
      return String
   is
   --  Return image of type, presuming Type_Sem designates a type
   begin
      if Type_Sem = null then
         return "null type";
      elsif Type_Sem.all not in Info.Type_Semantic_Info'Class then
         return "not a type";
      else
         return Info.Type_Image
                  (Info.Type_Sem_Ptr (Type_Sem),
                   Use_Short_Form);
      end if;
   end Type_Sem_Image;

   procedure Set_Language (Lang : Languages.Language_Enum) is
   --  This is called once at the beginning of processing to establish
   --  the language being parsed, within the ParaSail "family" of languages.
   begin
      --  Set global "Current_Language"
      Languages.Set_Language (Lang);

      --  Notify the static semantics phase that language has been set.
      Static.Init_Language_Specific_Info;
   end Set_Language;

   procedure Start_New_Source_File is
   --  This is called each time we start parsing a new source file
   begin
      --  Reset the set of import clauses, so they don't carry over
      --  from one source file to the next.
      Current_Import_Clauses := Lists.Empty_List;
   end Start_New_Source_File;

   procedure Add_Top_Level_Tree (OT : Optional_Tree; Imports : Lists.List) is
      --  Add top-level tree to library, with given Imports list
      Item : Trees.Tree'Class renames Tree_Ptr_Of (OT).all;
   begin
      if not Lists.Is_Empty (Imports) then
         --  Update global list of current imports.
         Current_Import_Clauses := Imports;
      end if;

      --  Fill in Import_Clauses on the tree from most recent specification.
      if Item in Module.Tree then
         Module.Tree (Item).Import_Clauses := Current_Import_Clauses;
      elsif Item in Operation.Tree then
         Operation.Tree (Item).Import_Clauses := Current_Import_Clauses;
      else
         --  Should not occur
         pragma Assert (False); null;
      end if;

      if (Item in Module.Tree and then Module.Tree (Item).Is_Interface)
        or else (Item in Operation.Tree
                and then not Operation.Tree (Item).Is_Def)
      then
         --  This is an interface-like thing
         Lists.Append (Library, OT);
      else
         --  This is a body-like thing
         Lists.Append (Library_Bodies, OT);
      end if;
   end Add_Top_Level_Tree;

   function Analyze_Library
     (Command_To_Execute : Strings.U_String_Array :=
        (1 .. 0 => Strings.Null_U_String)) return Natural is
      --  Analyze trees in library, starting servers when needed.
      --  If Command_To_Execute is non-null, invoke specified operation
      --  with given arguments.
      --  Return number of compile-time errors (0 if A-OK).
      --  Return Natural'Last if shut-down requested.

      Verbose_Semantics : Boolean :=
        Command_To_Execute'Length = 0
       or else Debug_First_Pass
       or else Debug_Second_Pass
       or else Debug_Pre_Cg
       or else Debug_Code_Gen;

      use type Interpreter.Type_Descriptor_Ptr;
      Read_Write : Object_Access.Read_Write_Mapping;

      Quit_Now : Boolean := False;

      Last_Parsed_Language : constant PSC.Languages.Language_Enum :=
        PSC.Languages.Language;
         --  Remember language we last parsed

   begin  --  Analyze_Library

      if not Verbose_Semantics then
         --  If there is a command, only show statistics if debug-threading
         --  is on.
         Interpreter.Debug_Statistics := Interpreter.Debug_Threading;
      end if;

      --  Create the stack chunk used for globals
      Interpreter.Initialize_Global_Stack_Chunk;

      if Verbose_Semantics then
         Ada.Text_IO.Put_Line
           (Integer'Image
               (Lists.Length (Library) + Lists.Length (Library_Bodies)) &
            " trees in library.");
      end if;

      Static.First_Pass_List (Symbols.Library_Region, Library);
      Static.First_Pass_List (Symbols.Library_Region, Library_Bodies);

      if Verbose_Semantics then
         Ada.Text_IO.Put_Line ("Done with First pass.");
      end if;

      if Sem_Error_Count > 0 then
         Put_Line
           (Current_Error,
            Integer'Image (Sem_Error_Count) & " semantic errors found");
         return Sem_Error_Count;
      end if;

      --  Process the decls of the interfaces first
      Static.Second_Pass_List
        (Symbols.Library_Region,
         Library,
         Context => Standalone_Item_Context,
         Mode => Static.Decls_Only);
      --  Now process the exprs in the interfaces
      Static.Second_Pass_List
        (Symbols.Library_Region,
         Library,
         Context => Standalone_Item_Context,
         Mode => Static.Exprs_Only);

      --  Now process both decls and exprs of the bodies
      Static.Second_Pass_List (Symbols.Library_Region, Library_Bodies,
        Context => Standalone_Item_Context);

      --  Now concatenate on the non-interface items
      Lists.Append (Library, Library_Bodies);

      --  Null out the bodies list.
      Library_Bodies := Lists.Empty_List;

      if Verbose_Semantics then
         Ada.Text_IO.Put_Line ("Done with Second pass.");
      end if;

      if Sem_Error_Count > 0 then
         Put_Line
           (Current_Error,
            Integer'Image (Sem_Error_Count) & " semantic errors found");
         return Sem_Error_Count;
      end if;

      Dynamic.Pre_Cg_List (Library, Read_Write, Object_Access.Not_Combined);

      if Verbose_Semantics then
         Ada.Text_IO.Put_Line ("Done with Pre codegen pass.");
      end if;

      if Sem_Error_Count > 0 then
         Put_Line
           (Current_Error,
            Integer'Image (Sem_Error_Count) & " pre-CG errors found");
         return Sem_Error_Count;
      end if;

      Dynamic.Code_Gen_List (Symbols.Library_Region, Library);

      if Verbose_Semantics then
         Ada.Text_IO.Put_Line ("Done with Code gen.");
      end if;

      if Sem_Error_Count > 0 then
         Put_Line
           (Current_Error,
            Integer'Image (Sem_Error_Count) & " codegen errors found");
         return Sem_Error_Count;
      end if;

      --  Fill in Cur_Inst_Param_Info for all type descriptors
      if Verbose_Semantics then
         Put_Line ("Filling in cur-inst-param info in op tables.");
      end if;

      Dynamic.Finish_Cur_Inst_Param_Info;

      if Verbose_Semantics then
         Put_Line ("Evaluating global constants.");
      end if;

      --  Now we need to actually start up thread servers
      --  (might be able to wait a *bit* more)
      if Command_To_Execute'Length = 0 then
         Put_Line ("Starting up thread servers");
      end if;
      Interpreter.Start_Up_Thread_Servers;

      Dynamic.Evaluate_Global_Constants;

      --  We might have some unfinished type descriptors
      if Verbose_Semantics then
         Put_Line ("Finishing type descriptors.");
      end if;

      Dynamic.Finish_All_Type_Descriptors;

      if Sem_Error_Count > 0 then
         Put_Line
           (Current_Error,
            Integer'Image (Sem_Error_Count) &
            " errors found while finishing type descriptors.");
         return Sem_Error_Count;
      end if;

      Dynamic.Finish_Global_Constants;
      --  Make any copies needed

      if Sem_Error_Count > 0 then
         Put_Line
           (Current_Error,
            Integer'Image (Sem_Error_Count) &
            " errors found while evaluating global constants.");
         return Sem_Error_Count;
      end if;

      Messages.Close_Error_File;  --  In case command wants to re-open it.

      --  Set back to the last language parsed
      Semantics.Set_Language (Last_Parsed_Language);

      if Command_To_Execute'Length > 0 then
      --  Command supplied already
         begin
            Execute_Command (Command_To_Execute, Quit_Now);
            if Quit_Now then
               return Natural'Last;
            else
               return 0;
            end if;
         end;
      end if;

      --  Loop, invoking interpreter on user-specified routine
      loop
         declare
            --  Get command
            Command : constant String :=
              PSC.Command_Input.Get_Line ("Command to execute: ");
         begin

            exit when Command = "";

            --  Break command up into words
            declare
               Words : constant Strings.U_String_Array :=
                 Strings.Tokenize (Command);
               Quit_Now : Boolean := False;
               use Strings;
            begin
               exit when Words'Length = 0
                        or else To_String (Words (Words'First)) = "quit"
                        or else To_String (Words (Words'First)) = "exit";
               Execute_Command (Words, Quit_Now);

               if Quit_Now then
                  --  Indicator of user-requested termination.
                  return Natural'Last;
               end if;
            exception
               when Interpreter.Propagating_Exception =>
                  Put_Line
                    (Current_Error,
                     "Exception propagated out of Interpeter");
               when E : others =>
                  Put_Line
                    (Current_Error,
                     "Unexpected exception propagated out of Interpreter: " &
                     Ada.Exceptions.Exception_Information (E));
            end;
         end;
      end loop;

      return 0;

   end Analyze_Library;

   function Analyze
     (Command_To_Execute : Strings.U_String_Array :=
        (1 .. 0 => Strings.Null_U_String)) return Natural is
      --  Analyze trees in library
      --  If Command_To_Execute is non-null, invoke specified operation
      --  with given arguments.
      --  Return number of compile-time errors (0 if A-OK).
      --  Return Natural'Last if command was terminated by the user.
      Num_Errors : Natural := 0;
   begin
      --  Do semantic analysis and execute the command(s)
      Num_Errors := Analyze_Library (Command_To_Execute);

      --  Shut down thread servers
      if Command_To_Execute'Length = 0 then
         Put_Line ("Shutting down thread servers");
      end if;

      if Num_Errors < Natural'Last then
         --  Already shut-down if Num_Errors = Natural'Last
         PSC.Interpreter.Shut_Down_Thread_Servers (Total_Errors => Num_Errors);
      end if;

      return Num_Errors;
   end Analyze;

   procedure Analyze_And_Interpret_All
     (Total_Errors : in out Natural;
      Command_Given : Boolean) is
   --  If no Command_Given, then
   --     Analyze all modules in the library
   --     If no errors, prompt for commands and execute them.
   --  If there was a Command_Given, then
   --    we will have already analyzed everything and executed the command
   --    during the Parse_All operation.
   --  Shut down the thread servers
   begin
      if Total_Errors = 0 then
         if not Command_Given then
            Put_Line ("---- Beginning semantic analysis ----");
            Total_Errors := PSC.Trees.Semantics.Analyze;
         end if;
      else
         --  In case of errors we need to stop the execution of the task
         --  Server_Creator (see psc-interpreter.adb) because otherwise, given
         --  that it is a library-level task, the interpreter cannot terminate
         --  its execution. In order to do it we first invoke Start_Up to allow
         --  the Server_Creator to pass to its next stage and then we shut down
         --  all the tasks of the runtime.

         Interpreter.Start_Up_Thread_Servers;
         PSC.Interpreter.Shut_Down_Thread_Servers (Total_Errors);

         Put_Line ("---- All done ----");
         New_Line;
      end if;

   exception
      when E : others =>
         Put_Line
           (Current_Error,
            "Internal error: Unhandled exception " &
            Ada.Exceptions.Exception_Information (E));
         New_Line;

         --  Start them if necessary
         Interpreter.Start_Up_Thread_Servers;

         --  Shut down thread servers
         if Total_Errors < Natural'Last then
            --  Already shut down if Total_Errors = Natural'Last.
            PSC.Interpreter.Shut_Down_Thread_Servers (Total_Errors + 1);
         end if;

         raise;

   end Analyze_And_Interpret_All;

   ------------------------------------

   procedure Turn_On_Debugging (Which : Debug_Index := 0) renames
     Semantics.Debug.Turn_On_Debugging;
   --  Turn on some or all debugging

   procedure Turn_Off_Debugging (Which : Debug_Index := 0) renames
     Semantics.Debug.Turn_Off_Debugging;
   --  Turn off some or all debugging

   procedure Finish_Type_Descriptor
     (Type_Desc : Interpreter.Type_Descriptor_Ptr;
      Return_On_Recursion : Boolean := False) renames
     Dynamic.Finish_Type_Descriptor;
   --  Finish up formal object parameters and nested constants
   --  If Return_On_Recursion is True, do not complain about recursion
   --  and simply return immediately.

   function Name_For_Object_Locator
     (Locator : Interpreter.Object_Locator;
      Enclosing_Type : Interpreter.Object_Locator :=
     Interpreter.Null_Object_Locator)
      return String renames Dynamic.Name_For_Object_Locator;
   --  Return a string representing the name associated with the
   --  given object locator.  Return "" if none available.
   --  NOTE: Currently only supports Const_Area, Zero_Base, and Type_Area.
   --       Type_Area requires Enclosing_Type to be provided,
   --       which must have a Zero_Base.

   function Equiv_Interps (Interp1, Interp2 : Optional_Tree) return Boolean
      renames Static.Equiv_Interps;
   --  Return True if Interp1 and Interp2 are equivalent as far
   --  as overload resolution.

   function Prefix (Name : Optional_Tree) return Optional_Tree is
   --  Return prefix of Qualified_Name, Selection, or Property
   --  Returns Null_Optional_Tree if given an Identifier
   --  Requires: Name is Qualified_Name, Selection, Property, or an Identifier
      Name_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Name).all;
   begin
      if Name_Tree in Qualified_Name.Tree then
         return Qualified_Name.Tree (Name_Tree).Prefix;
      elsif Name_Tree in Selection.Tree then
         return Selection.Tree (Name_Tree).Prefix;
      elsif Name_Tree in Property.Tree then
         return Property.Tree (Name_Tree).Operand;
      elsif Name_Tree in Identifier.Tree then
         return Null_Optional_Tree;
      else
         pragma Assert (False);
         return Null_Optional_Tree;
      end if;
   end Prefix;

   function Suffix (Name : Optional_Tree) return Optional_Tree is
   --  Return suffix of Qualified_Name, Selection (Id or Selector), or Property
   --  Return tree itself if given an Identifier
   --  Result should be an Identifier
   --  Requires: Name is Qualified_Name, Selection, Property, or an Identifier
      Name_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Name).all;
   begin
      if Name_Tree in Qualified_Name.Tree then
         return Qualified_Name.Tree (Name_Tree).Id;
      elsif Name_Tree in Selection.Tree then
         return Selection.Tree (Name_Tree).Selector;
      elsif Name_Tree in Property.Tree then
         return Property.Tree (Name_Tree).Property_Id;
      elsif Name_Tree in Identifier.Tree then
         return Name;
      else
         pragma Assert (False);
         return Null_Optional_Tree;
      end if;
   end Suffix;

end PSC.Trees.Semantics;
