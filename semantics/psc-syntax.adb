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

with Ada.Text_IO;
use  Ada.Text_IO;

with PSC.Command_Input;
with PSC.Languages;
with PSC.Messages;
with PSC.Strings;
with PSC.Trees.Semantics;
with PSC.Interpreter;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;

package body PSC.Syntax is

   Show_Tokens : constant Boolean := False;
      --  Set to True to separate tokens in listing

   function Cur_Source_Pos return Source_Positions.Source_Position is
   --  Return File/Lines/Char_Count as a source-position
      use Source_Positions;
      Result : Source_Position :=
        (File => Strings.Index (Cur_File), Line => Line_Number (lines),
         Col => Column_Number (Char_Count),
         End_Line => 0, End_Col => 0);
   begin
      --  TBD: "gcc" prefers column count rather than character count
      --       while "vim -q" prefers a character count.
      return Result;
   end Cur_Source_Pos;

   procedure Display_Linenum is
   --  Display current line number for listing
      Line_Number_String : constant String := Integer'Image (Lines + 1);
   begin
      Lines := Lines + 1;
      if Echo_Input then
         Put (Line_Number_String);
         for I in 1 .. 5 - Integer (Line_Number_String'Length) loop
            Ada.Text_IO.Put (" ");
         end loop;
      end if;
      Col_Count := 0;
      Char_Count := 0;

   end Display_Linenum;

   procedure Echo_Token (Token : String) is
   --  Update Char_Count/Col_Count, and display current token
   --  if Echo_Input.
   begin
      if Echo_Input and then Show_Tokens then
         --  Separate tokens in the input with a "$"
         Ada.Text_IO.Put ('$');
      end if;
      Char_Count := Char_Count + Token'Length;
      for I in Token'Range loop
         if Token (I) = ASCII.HT then
            --  Expand tab
            declare
               Next_Tab_Stop : constant Natural :=
                 Col_Count + Spaces_Per_Tab -
                   Col_Count mod Spaces_Per_Tab;
            begin
               if Echo_Input then
                  for J in Col_Count .. Next_Tab_Stop loop
                     Ada.Text_IO.Put (' ');
                  end loop;
               end if;
               Col_Count := Next_Tab_Stop;
            end;
         else
            if Echo_Input then
               Ada.Text_IO.Put (Token (I));
            end if;
            Col_Count := Col_Count + 1;
         end if;
      end loop;
   end Echo_Token;

   procedure Parse_All
     (Total_Errors : out Natural; Command_Given : out Boolean;
      Parsers : Parser_Array) is
   --  Handle command line, and/or interactive input of file names
   --  and parse them all.
   --  Recognize "-lang <language>" to switch language parser between
   --  the parsers included in Parsers array.
      Arg_Count : constant Natural := Argument_Count;
      Arg_Index : Positive := 1;
      Syntax_Errors : Natural := 0;

      Cur_Parser_Index : Positive := Parsers'First;

      procedure Parse_One (File_Name : String; Listing_Name : String := "") is
         Listing_File : Ada.Text_IO.File_Type;
         Echoing_Input : constant Boolean := Echo_Input;
         Parser : Parser_Operations renames Parsers (Cur_Parser_Index);
      begin
         PSC.Messages.Number_Of_Errors := 0;

         if not Command_Given then
            Ada.Text_IO.Put_Line ("Parsing " & File_Name);
         end if;

         begin
            Parser.Lexer_Open_Input (File_Name);
         exception
            when Ada.Text_IO.Name_Error =>
               Put_Line (Standard_Error, "Error: Cannot open " & File_Name);
               PSC.Trees.Semantics.Sem_Error ("Cannot open " & File_Name);
               Total_Errors := Total_Errors + 1;
               return;
         end;

         --  Remember current file name
         Cur_File := PSC.Strings.String_Lookup (File_Name);

         if Listing_Name /= "" then
         --  Redirect standard output to listing file
            begin
               Ada.Text_IO.Create
                 (Listing_File, Ada.Text_IO.Out_File, Listing_Name);
               PSC.Trees.Semantics.Give_Copyright (Listing_File);
               Ada.Text_IO.Set_Output (Listing_File);
            exception
               when others =>
                  Ada.Text_IO.Put_Line
                    (Standard_Error,
                     "Error: Cannot create " & Listing_Name);
            end;
         else
            --  Don't echo input if no listing file
            Echo_Input := False;
         end if;

         Lines := 0;

         Display_Linenum;

         Parser.Lexer_Init.all;

         PSC.Trees.Semantics.Start_New_Source_File;

         begin
            Parser.Yyparse.all;
         exception

            when E : others =>
               PSC.Messages.Put_Message
                 ("Exception raised: " &
                     Ada.Exceptions.Exception_Information (E),
                  Src_Pos => Cur_Source_Pos,
                  Message_Kind => "Info");
               PSC.Messages.Put_Message
                 ("skipping to end of file",
                  Src_Pos => Cur_Source_Pos,
                  Message_Kind => "Fatal syntax error");
               PSC.Messages.Number_Of_Errors :=
                 PSC.Messages.Number_Of_Errors + 1;
         end;

         Parser.Lexer_Close_Input.all;

         if Listing_Name /= "" then
            --  Close listing file and reset Current_Output to Standard_Output.
            Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
            Ada.Text_IO.Close (Listing_File);
         end if;

         if PSC.Messages.Number_Of_Errors > 0 then
            Put (Standard_Error,
                 Integer'Image (PSC.Messages.Number_Of_Errors));
            Put_Line (Standard_Error, " syntax errors found in " & File_Name);
         end if;
         Total_Errors := Total_Errors + PSC.Messages.Number_Of_Errors;

         --  Restore Echo_Input to its original state
         Echo_Input := Echoing_Input;

      end Parse_One;

   begin  --  Parse_All

      Total_Errors := 0;
      Command_Given := False;

      --  See whether a "-command" has been given.
      --  If so, eliminate extraneous output.
      for I in 1 .. Arg_Count loop
         if Argument (I) = "-command" then
            Command_Given := True;
            --  Change listing default to off
            Echo_Input := False;
            PSC.Trees.Semantics.List_Routine_Instructions := False;
            exit;
         end if;
      end loop;

      if not Command_Given then
         PSC.Trees.Semantics.Give_Copyright (Current_Output);
      end if;

      if Arg_Count = 0 then
         loop
            declare
               --  Get file names
               In_File_Name : constant String :=
                 PSC.Command_Input.Get_Line ("Enter input files: ");
               --  Break list up into words
               Files : constant PSC.Strings.U_String_Array :=
                 PSC.Strings.Tokenize (In_File_Name);
               use PSC.Strings;
            begin
               exit when Files'Length = 0
                        or else To_String (Files (Files'First)) = "quit"
                        or else To_String (Files (Files'First)) = "exit";

               for I in 1 .. Files'Length loop
                  if Echo_Input
                    or else PSC.Trees.Semantics.List_Routine_Instructions
                  then
                     --  Produce a listing
                     Parse_One
                       (To_String (Files (I)),
                        Listing_Name => To_String (Files (I)) & ".lst");
                  else
                     --  No listing desired
                     Parse_One (To_String (Files (I)));
                  end if;
               end loop;
            end;
         end loop;
      end if;

      while Arg_Index <= Arg_Count loop
         declare
            Command_Arg : String renames Argument (Arg_Index);
         begin
            if Command_Arg (Command_Arg'First) = '-' then
               --  The argument is some kind of flag
               if Command_Arg = "-debug" then
                  declare
                     Debug_Arg : String renames Argument (Arg_Index + 1);
                     use PSC.Trees.Semantics;
                  begin
                     Arg_Index := Arg_Index + 1;
                     if Debug_Arg = "on" then
                        Turn_On_Debugging;
                     elsif Debug_Arg = "off" then
                        Turn_Off_Debugging;
                     else
                        case Debug_Arg (Debug_Arg'First) is
                           when '-' | '+' | '0' .. '9' =>
                              declare
                                 Arg_Val : constant Integer :=
                                   Integer'Value (Debug_Arg);
                              begin
                                 if Arg_Val < 0 then
                                    Turn_Off_Debugging (-Arg_Val);
                                 else
                                    Turn_On_Debugging (+Arg_Val);
                                 end if;
                              end;
                           when others =>
                              Put_Line
                                (" -debug argument " &
                                 Debug_Arg &
                                 " not understood.");
                              Total_Errors := Total_Errors + 1;
                        end case;
                     end if;
                  end;
               elsif Command_Arg = "-listing" then
                  declare
                     Listing_Arg : String renames Argument (Arg_Index + 1);
                     use PSC.Trees.Semantics;
                  begin
                     Arg_Index := Arg_Index + 1;
                     if Listing_Arg = "on" then
                        Echo_Input                                    := True;
                        PSC.Trees.Semantics.List_Routine_Instructions := True;
                     elsif Listing_Arg = "off" then
                        Echo_Input                                    := False;
                        PSC.Trees.Semantics.List_Routine_Instructions := False;
                     else
                        Put_Line
                          (" -listing argument " &
                           Listing_Arg &
                           " not understood; must be ""on"" or ""off"".");
                        Total_Errors := Total_Errors + 1;
                     end if;
                  end;
               elsif Command_Arg = "-servers" then
                  declare
                     Servers_Arg : String renames Argument (Arg_Index + 1);
                     use PSC.Trees.Semantics;
                  begin
                     Arg_Index := Arg_Index + 1;
                     PSC.Interpreter.Set_Server_Count
                       (Integer'Value (Servers_Arg));
                  exception
                     when others =>
                        Put_Line
                          (" -servers argument " &
                           Servers_Arg &
                           " not understood; must be number of " &
                           "thread servers to allow");
                        Total_Errors := Total_Errors + 1;
                  end;
               elsif Command_Arg = "-parcalls" then
                  declare
                     Parcalls_Arg : String renames Argument (Arg_Index + 1);
                     use PSC.Trees.Semantics;
                  begin
                     Arg_Index := Arg_Index + 1;
                     if Parcalls_Arg = "on" then
                        PSC.Trees.Semantics.Insert_Implicit_Parallel_Calls :=
                          True;
                     elsif Parcalls_Arg = "off" then
                        PSC.Trees.Semantics.Insert_Implicit_Parallel_Calls :=
                          False;
                     else
                        Put_Line
                          (" -parcalls argument " &
                           Parcalls_Arg &
                           " not understood; must be ""on"" or ""off"".");
                        Total_Errors := Total_Errors + 1;
                     end if;
                  end;
               elsif Command_Arg = "-parloops" then
                  declare
                     Parloops_Arg : String renames Argument (Arg_Index + 1);
                     use PSC.Trees.Semantics;
                  begin
                     Arg_Index := Arg_Index + 1;
                     if Parloops_Arg = "on" then
                        PSC.Trees.Semantics.Insert_Implicit_Parallel_Loops :=
                          True;
                     elsif Parloops_Arg = "off" then
                        PSC.Trees.Semantics.Insert_Implicit_Parallel_Loops :=
                          False;
                     else
                        Put_Line
                          (" -parloops argument " &
                           Parloops_Arg &
                           " not understood; must be ""on"" or ""off"".");
                        Total_Errors := Total_Errors + 1;
                     end if;
                  end;
--             elsif Command_Arg = "-virt"
--               or else Command_Arg = "-virtual"
--             then
--                PSC.Interpreter.Virt_Is_Phys := False;
--                Put_Line ("Objects now using virtual addresses.");

--             elsif Command_Arg = "-phys"
--               or else Command_Arg = "-physical"
--             then
--                PSC.Interpreter.Virt_Is_Phys := True;
--                Put_Line ("Objects now using physical addresses.");

               elsif Command_Arg = "-lang"
                 or else Command_Arg = "-language"
               then
                  declare
                     Lang_Arg : String renames Argument (Arg_Index + 1);
                     use PSC.Trees.Semantics;
                     Parser_Found : Boolean := False;
                  begin
                     Arg_Index := Arg_Index + 1;

                     declare
                        Lang_Enum : PSC.Languages.Language_Enum;
                        use type PSC.Languages.Language_Enum;
                     begin
                        Lang_Enum :=
                          PSC.Languages.Language_Enum'Value (Lang_Arg);
                           --  raises Constraint_Error if not a valid
                           --  enumeral string.

                        for I in Parsers'Range loop
                           if Parsers (I).Language = Lang_Enum then
                              Cur_Parser_Index := I;
                              Parser_Found := True;
                              if Lang_Enum /= PSC.Languages.Language then
                                 PSC.Trees.Semantics.Set_Language (Lang_Enum);
                                 if not Command_Given then
                                    New_Line;
                                    Put_Line ("Language now " & Lang_Arg);
                                 end if;
                              end if;
                              exit;
                           end if;
                        end loop;
                     exception
                        when others =>
                           null;  -- Parser_Found still False
                     end;

                     if not Parser_Found then
                        Put_Line
                          ("-lang argument """ &
                           Lang_Arg &
                           """ not among languages supported:");
                        for I in Parsers'Range loop
                           Put_Line ("  " & PSC.Languages.Language_Enum'Image
                             (Parsers (I).Language));
                        end loop;
                        Total_Errors := Total_Errors + 1;

                        return;  ------  Quit now
                     end if;
                  end;

               elsif Command_Arg = "-command" then
                  --  Interpret rest of line as a command
                  declare
                     use PSC.Strings;
                     Command : U_String_Array (1 .. Arg_Count - Arg_Index);
                     use PSC.Trees.Semantics;
                  begin
                     for I in Command'Range loop
                        Command (I) :=
                          String_Lookup (Argument (I + Arg_Index));
                     end loop;

                     if Total_Errors = 0 then
                        --  Analyze and execute the command now
                        Total_Errors := PSC.Trees.Semantics.Analyze (Command);
                     end if;

                     --  We have used up all of the arguments
                     Arg_Index := Arg_Count;
                  end;
               else
                  Put_Line (Standard_Error,
                    "Fatal error: Flag " & Command_Arg & " not understood.");
                  Total_Errors := Total_Errors + 1;
                  return;  --  quit now  --
               end if;
            else
               if Echo_Input
                 or else PSC.Trees.Semantics.List_Routine_Instructions
               then
                  --  Produce a listing
                  Parse_One
                    (Command_Arg, Listing_Name => Command_Arg & ".lst");
               else
                  --  No listing desired
                  Parse_One (Command_Arg);
               end if;
               --  Remember if any syntax errors
               Syntax_Errors := Total_Errors;
            end if;

            Arg_Index := Arg_Index + 1;
         end;
      end loop;

      if Syntax_Errors > 0 then
         New_Line;
         Put (Standard_Error, Integer'Image (Syntax_Errors));
         Put_Line (Standard_Error, " total syntax errors found");
      end if;
   end Parse_All;

end PSC.Syntax;
