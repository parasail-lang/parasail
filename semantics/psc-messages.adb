------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation. See           --
-- documentation/COPYING3 and documentation/GCC_RUNTIME3_1 for details.     --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with     --
-- the ParaSail, Sparkel, Javallel, or Parython compiler, including any     --
-- required library run-time units written in Ada or in any of the above    --
-- languages, using any licensing terms  of your choosing.                  --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

with Generic_Lists;
pragma Elaborate (Generic_Lists);

with PSC.Hash_Tables;
pragma Elaborate (PSC.Hash_Tables);

with PSC.Strings;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
package body PSC.Messages is

   Error_File : Ada.Text_IO.File_Type;  --  errors written here
   Error_File_Name : constant String := "errors.err";
   --  This file name is special to "vim -q"
   Error_File_Failed : Boolean := False;
   --  If True, then we failed trying to create error file.
   Append_To_Error_File : Boolean := False;
   --  If True, then re-open the error file using Append rather than Create

   package Message_Lists is new Generic_Lists (Strings.U_String);
   --  List of messages associated with a given source position

   use type Source_Positions.Source_Position;

   package Message_Tables is new Hash_Tables
     (Element_Type => Message_Lists.List,
      Key_Type => Source_Positions.Source_Position,
      Hash_Type => Strings.Hash_Type,
      Hash => Source_Positions.Hash);

   Message_Table : Message_Tables.Hash_Table;
   --  Hash table of messages, indexed by source position

   procedure Message_Helper
     (S : String;
      Src_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position;
      Message_Kind : String := "Error") is
      --  Produce an error message on the current error output.
      --  NOTE: This is called from Put_Message twice, once to write out
      --       to Standard_Error, and once to the Error_File.
      use Ada.Text_IO;
      use Source_Positions;
   begin
      if Src_Pos /= Null_Source_Position then
         Put (Current_Error, Source_Positions.Image (Src_Pos) & ' ');
      end if;
      Put_Line (Current_Error, Message_Kind & ": " & S);
      Flush (Current_Error);
   end Message_Helper;

   --  Error file manager prevents race conditions on state of Error_File
   protected Error_File_Manager is
      entry Lock_Error_File;
      procedure Unlock_Error_File;
   private
      Is_Locked : Boolean := False;
   end Error_File_Manager;

   protected body Error_File_Manager is
      entry Lock_Error_File when not Is_Locked is
      begin
         Is_Locked := True;
      end Lock_Error_File;
      procedure Unlock_Error_File is
      begin
         Is_Locked := False;
      end Unlock_Error_File;
   end Error_File_Manager;

   procedure Open_Error_File is
      --  If Error_File not already open then:
      --     If Append_To_Error_File is True, open in Append mode;
      --     otherwise Create an empty file.
      --     Set Error_File_Failed if not successful.
   begin
      --  Error_File should be locked when this is called
      if not Error_File_Failed then
         begin
            if not Is_Open (Error_File) then
               --  Create or append to the file.
               if Append_To_Error_File then
                  --  Add to end of existing file
                  Open (Error_File, Append_File, Error_File_Name);
               else
                  Create (Error_File, Out_File, Error_File_Name);
               end if;
            end if;
         exception
            when E : others =>
               Error_File_Failed := True;  --  Don't try again
               Set_Error (Standard_Error);
               Put_Line
                 (Current_Error,
                  "Cannot create message file '" &
                  Error_File_Name &
                  "', exception " &
                  Ada.Exceptions.Exception_Name (E) &
                  " raised.");
               Flush (Standard_Error);
         end;
      end if;
   end Open_Error_File;

   ----------------  Visible Subprograms  -----------------

   procedure Put_Message
     (S : String;
      Src_Pos : Source_Positions.Source_Position;
      Message_Kind : String;
      Suppress_Duplicates : Boolean := True) is
      --  Produce a message on the standard error output
      --  and in the error file.
      --  Suppress duplicate messages by default.
      use Ada.Text_IO;
   begin
      Error_File_Manager.Lock_Error_File;  --  Lock during operation
      if Suppress_Duplicates
        and then Src_Pos /= Source_Positions.Null_Source_Position
      then
         --  Check whether we have already put out this message

         declare
            use Message_Tables;
            Message_U_Str : Strings.U_String :=
              Strings.String_Lookup (Message_Kind & ": " & S);
            Existing_Elem : Element_Ref :=
              Find_Element (Message_Table, Src_Pos);
            New_Elem : Message_Lists.List;

            function Matches
              (Old_Result : Boolean; Next_Str : Strings.U_String)
              return Boolean is
               --  Return True if Old_Result already True,
               --  or Next_Str matches the new message
               use type Strings.U_String;
            begin
               return Old_Result or else
                 Next_Str = Message_U_Str;
            end Matches;

            function Is_Present is new Message_Lists.Apply_Function
              (Result_Type => Boolean,
               New_Result => Matches,
               Initial_Result => False,
               Quit_On_Result => True);
            --  Scan list and return True if Message_U_Str already there

         begin
            if Existing_Elem = null then
               --  First message at this Src_Pos
               Enter_Element (Message_Table,
                              Key => Src_Pos,
                              Elem => Message_Lists.Make
                                        ((1 => Message_U_Str)),
                              Existing_Elem => Existing_Elem);
            elsif Is_Present (Existing_Elem.all) then
               Error_File_Manager.Unlock_Error_File;  --  Unlock on way out
               return;  --------  Message already present  --------
            else
               --  Add to end of list of messages for this Src_Pos
               --  Add message to end of list
               Message_Lists.Append (Existing_Elem.all, Message_U_Str);
            end if;
         end;
      end if;

      --  First put out to Standard_Error
      Set_Error (Standard_Error);
      Message_Helper (S, Src_Pos, Message_Kind);

      --  Now put out to error file
      Open_Error_File;
      if not Error_File_Failed then
         --  Now write out the message
         Set_Error (Error_File);
         Message_Helper (S, Src_Pos, Message_Kind);
         Set_Error (Standard_Error);
         if Append_To_Error_File then
            --  If we are appending, don't keep it open
            Close (Error_File);
         else
            Flush (Error_File);
         end if;
      end if;
      Error_File_Manager.Unlock_Error_File;  --  Unlock on way out
   exception
      when others =>
         Set_Error (Standard_Error);
         Error_File_Manager.Unlock_Error_File;  --  Unlock on way out
         raise;
   end Put_Message;

   procedure Parser_Error (S : String := "syntax error";
     Src_Pos : Source_Positions.Source_Position :=
       Source_Positions.Null_Source_Position) is
      Error_Col : constant Ada.Text_IO.Positive_Count := Col;
   begin
      Number_Of_Errors := Number_Of_Errors + 1;
      Put ("<<< *** error: ");
      Put_Line (S);
      Put_Message (S, Src_Pos => Src_Pos, Message_Kind => "Error");
      Set_Col (Error_Col);  -- Return to same column
   end Parser_Error;

   procedure Parser_Warning (S : String;
     Src_Pos : Source_Positions.Source_Position :=
       Source_Positions.Null_Source_Position) is
      Error_Col : constant Ada.Text_IO.Positive_Count := Col;
   begin
      if Number_Of_Errors = 0 then  --  Only give warnings if no errors
         Put ("<<< warning: ");
         Put_Line (S);
         Put_Message (S, Src_Pos => Src_Pos, Message_Kind => "Warning");
         Set_Col (Error_Col);  -- Return to same column
      end if;
   end Parser_Warning;

   function Is_Error_File (Name : String) return Boolean is
      --  Return True if Name matches name of file where errors are logged.
   begin
      --  Actually, we return true if end of Name string matches
      --  Error_File_Name, in case Name has a full pathname for current
      --  directory.
      return Name'Length >= Error_File_Name'Length
          and then
        Error_File_Name =
          Name (Name'Last - Error_File_Name'Length + 1 .. Name'Last);
   end Is_Error_File;

   procedure Close_Error_File is
      --  Close error-messages file if it is open
      --  so it can be re-opened by a separate package or by compiled code.
   begin
      Error_File_Manager.Lock_Error_File;  --  Lock to avoid race
      if Is_Open (Error_File) then
         Close (Error_File);
      elsif not Append_To_Error_File and then not Error_File_Failed then
         --  Create and then close error file now.
         Open_Error_File;
         if not Error_File_Failed then
            Close (Error_File);
         end if;
      end if;
      --  From now on, just open long enough to write out a message.
      Append_To_Error_File := True;
      Error_File_Manager.Unlock_Error_File;  --  Unlock on way out
   exception
      when others =>
         Error_File_Manager.Unlock_Error_File;  --  Unlock on way out
         raise;
   end Close_Error_File;

end PSC.Messages;
