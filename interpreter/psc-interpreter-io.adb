------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2022, AdaCore                     --
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

--  Package providing support for builtin ParaSail I/O-related operations

with PSC.Interpreter.Builtins; use PSC.Interpreter.Builtins;
with PSC.Messages;
with PSC.Strings;
with PSC.Univ_Strings;
with PSC.Vectors;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

pragma Elaborate (PSC.Interpreter.Builtins);
pragma Elaborate (PSC.Strings);
pragma Elaborate (Ada.Text_IO);

package body PSC.Interpreter.IO is

   --  Representation of a stream file
   type Stream_File_Info;
   type Stream_File_Ptr is access Stream_File_Info;

   package Stream_File_Vectors is new PSC.Vectors (Stream_File_Ptr);
   type Stream_File_Vector is new Stream_File_Vectors.Vector;
   Stream_File_Table : Stream_File_Vector;
   --  Global table of stream files

   subtype Stream_File_Index is Stream_File_Vectors.Elem_Index;
   use type Stream_File_Index;

   type Stream_File_Info is limited record
      File      : Ada.Streams.Stream_IO.File_Type;
      Index     : Stream_File_Index := 0;
      Next_Free : Stream_File_Ptr   := null;
   end record;

   Free_Stream_File : Stream_File_Ptr := null;  --  Free list

   type Output_Chan_Enum is (Stdout, Stderr);
      --  Used for Print_To_Standard_File

   ----------  Local Subprograms  -------------

   function New_Stream_File_Index
     return Stream_File_Index;
   --  Return the index of a stream file in the Stream_File_Table

   procedure Free_File
     (Info_Ptr : Stream_File_Ptr);
   --  Release the stream file

   ----------  Builtin Subprograms  -------------

   procedure Append_Output_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Create an Output_Stream which appends to a File
   --  Append(var IO; Name : Univ_String) -> optional File_Output_Stream
   pragma Export (Ada, Append_Output_File, "_psc_append_output_file");

   procedure Close_Output_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Close the Output_Stream
   --  Close(var optional File_Output_Stream)
   pragma Export (Ada, Close_Output_File, "_psc_close_file");

   procedure Delete_Output_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Close and Delete the File associated with the Output_Stream
   --  func Delete(var optional File_Output_Stream)
   pragma Export (Ada, Delete_Output_File, "_psc_delete_output");

   procedure Flush_Output_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Flush the Output_Stream
   --  Flush(var File_Output_Stream)
   pragma Export (Ada, Flush_Output_File, "_psc_flush_output_file");

   procedure Create_Output_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Create an Output_Stream connected to a File
   --  Create(var IO; Name : Univ_String) -> optional File_Output_Stream
   pragma Export (Ada, Create_Output_File, "_psc_create_output_file");

   procedure Open_Input_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Open an Input_Stream connected to a File
   --  Open(var IO; Name : Univ_String) -> optional File_Input_Stream
   pragma Export (Ada, Open_Input_File, "_psc_open_input_file");

   procedure Print_To_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Print string to file
   --  Print(var File_Output_Stream; Univ_String)
   pragma Export (Ada, Print_To_File, "_psc_print_to_file");

   procedure Print_To_Standard_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Print string to standard output/error file
   --  Print(var Standard_Output_Stream; Univ_String)
   pragma Export (Ada, Print_To_Standard_File, "_psc_print_to_standard_file");

   procedure Flush_Standard_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Flush the Standard_Output_Stream
   --  Flush(var Standard_Output_Stream)
   pragma Export (Ada, Flush_Standard_File, "_psc_flush_standard_file");

   procedure Read_From_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Read a line from a file as a string
   --  Readln(var Input_Stream) -> optional Univ_String
   pragma Export (Ada, Read_From_File, "_psc_read_from_file");

   procedure Set_Exit_Status
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Set_Exit_Status, "_psc_set_exit_status");

   ----------  Implementation of Local Subprograms  -------------

   ---------------------------
   -- New_Stream_File_Index --
   ---------------------------

   function New_Stream_File_Index return Stream_File_Index is
      New_File : Stream_File_Ptr := Free_Stream_File;
   begin
      if New_File /= null then
         --  There was a free stream file
         Free_Stream_File := New_File.Next_Free;
         New_File.Next_Free := null;
      else
         --  Need to create a new stream file and add to global table
         New_File := new Stream_File_Info;
         Add_Element (Stream_File_Table, New_File, New_File.Index);
      end if;

      return New_File.Index;
   end New_Stream_File_Index;

   ---------------
   -- Free_File --
   ---------------

   procedure Free_File (Info_Ptr : Stream_File_Ptr) is
   begin
      Info_Ptr.Next_Free := Free_Stream_File;
      Free_Stream_File := Info_Ptr;
   end Free_File;

   ----------------- Implementations of builtin subprograms -----------

   ------------------------
   -- Append_Output_File --
   ------------------------

   procedure Append_Output_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  Create an Output_Stream which appends to a File
      --  Append(var IO; Name : Univ_String) -> optional File_Output_Stream

      use Ada.Streams.Stream_IO;

      File_Index     : constant Stream_File_Index := New_Stream_File_Index;
      Info_Ptr       : constant Stream_File_Ptr :=
                         Nth_Element (Stream_File_Table, File_Index);
      Name_Word      : constant Word_Type :=
                         Fetch_Nonnull_Word (Params, 2);
      Name           : String renames Word_To_String (Name_Word);
      Target         : constant Word_Type :=
                         Fetch_Word (Params, 0);
      Target_Stg_Rgn : constant Stg_Rgn_Ptr := Stg_Rgn_Of_Large_Obj (Target);
      --  Type_Desc  : constant Non_Op_Map_Type_Ptr := Context.Enclosing_Type;
      New_Obj        : constant Word_Type :=
                         Allocate_From_Stg_Rgn
                           (Target_Stg_Rgn,
                            Large_Obj_Header_Size + 2,
                            Context.Server_Index);
      Name_Copy      : constant Word_Type :=
                           Copy_Object
                             (Context    => Context,
                              Type_Desc  =>
                                Univ_Strings.Get_Univ_String_Type_Desc,
                              Object     => Name_Word,
                              Stg_Rgn_Of => Target);
   begin
      if Messages.Is_Error_File (Name) then
         --  We are appending to the error file
         --  Close our own handle in case it is already open;
         --  create it now if never created
         Messages.Close_Error_File;
      end if;

      Ada.Streams.Stream_IO.Open
        (Info_Ptr.File, Mode => Append_File, Name => Name);

      --  Store filename and index
      Store_Word (New_Obj + Large_Obj_Header_Size, Name_Copy);
      Store_Word (New_Obj + (Large_Obj_Header_Size + 1),
        Word_Type (File_Index));

      --  Store type
      --  Set_Large_Obj_Type_Info (New_Obj, Type_Desc.Index);
      Set_Large_Obj_Type_Info (New_Obj, Static_Link.Index);

      --  Store file object
      Store_Word (Params, 0, New_Obj);
   exception
      when Name_Error | Use_Error | Device_Error =>
         Free_File (Info_Ptr);

         --  Store null file object
         Store_Word (Params, 0,
           Null_For_Stg_Rgn (Target_Stg_Rgn));
   end Append_Output_File;

   -----------------------
   -- Close_Output_File --
   -----------------------

   procedure Close_Output_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  Close the Output_Stream
      --  Close(var optional File_Output_Stream)

      File_Obj_Ref : constant Word_Ptr :=
                       Fetch_Word_Ptr (Params, 0);
      File_Obj     : constant Word_Type :=
                       Fetch_Word (File_Obj_Ref, 0);
   begin
      if not Is_Large_Null (File_Obj) then
         --  Close file and set to null
         declare
            File_Stg_Rgn : constant Stg_Rgn_Ptr :=
                             Stg_Rgn_Of_Large_Obj (File_Obj);
            File_Index   : constant Stream_File_Index :=
                             Stream_File_Index
                               (Fetch_Word
                                 (File_Obj + (Large_Obj_Header_Size + 1)));
            Info_Ptr     : constant Stream_File_Ptr :=
                             Nth_Element (Stream_File_Table, File_Index);
         begin
            Ada.Streams.Stream_IO.Close (Info_Ptr.File);

            --  Set file to null
            Store_Word (File_Obj_Ref, 0, Null_For_Stg_Rgn (File_Stg_Rgn));
         end;
      end if;
   end Close_Output_File;

   ------------------------
   -- Delete_Output_File --
   ------------------------

   procedure Delete_Output_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  Close and delete the file associated with the Output_Stream
      --  func Delete(var optional File_Output_Stream)

      File_Obj_Ref : constant Word_Ptr :=
                       Fetch_Word_Ptr (Params, 0);
      File_Obj     : constant Word_Type :=
                       Fetch_Word (File_Obj_Ref, 0);
   begin
      if not Is_Large_Null (File_Obj) then
         --  Close and delete file and set to null
         declare
            File_Stg_Rgn : constant Stg_Rgn_Ptr :=
                             Stg_Rgn_Of_Large_Obj (File_Obj);
            File_Index   : constant Stream_File_Index :=
                             Stream_File_Index
                               (Fetch_Word
                                 (File_Obj + (Large_Obj_Header_Size + 1)));
            Info_Ptr     : constant Stream_File_Ptr :=
                             Nth_Element (Stream_File_Table, File_Index);
         begin
            Ada.Streams.Stream_IO.Delete (Info_Ptr.File);

            --  Set file to null
            Store_Word (File_Obj_Ref, 0, Null_For_Stg_Rgn (File_Stg_Rgn));
         end;
      end if;
   end Delete_Output_File;

   -----------------------
   -- Flush_Output_File --
   -----------------------

   procedure Flush_Output_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  Flush the Output_Stream
      --  Flush(var File_Output_Stream)

      File_Obj_Ref : constant Word_Ptr :=
                       Fetch_Word_Ptr (Params, 0);
      File_Obj     : constant Word_Type :=
                       Fetch_Word (File_Obj_Ref, 0);
   begin
      if not Is_Large_Null (File_Obj) then
         --  Flush file
         declare
            File_Stg_Rgn : constant Stg_Rgn_Ptr :=
                             Stg_Rgn_Of_Large_Obj (File_Obj);
            File_Index   : constant Stream_File_Index :=
                             Stream_File_Index
                               (Fetch_Word
                                 (File_Obj + (Large_Obj_Header_Size + 1)));
            Info_Ptr     : constant Stream_File_Ptr :=
                             Nth_Element (Stream_File_Table, File_Index);
         begin
            Ada.Streams.Stream_IO.Flush (Info_Ptr.File);
         end;
      end if;
   end Flush_Output_File;

   -------------------------
   -- Flush_Standard_File --
   -------------------------

   procedure Flush_Standard_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  Flush the Standard Output_Stream
      --  Flush(var Standard_Output_Stream)

      File_Obj_Ref : constant Word_Ptr :=
                       Fetch_Word_Ptr (Params, 0);
      File_Chan    : constant Output_Chan_Enum :=
                       Output_Chan_Enum'Val (Fetch_Word (File_Obj_Ref, 0));

      use Ada.Text_IO;
   begin
      --  Flush the appropriate stream
      case File_Chan is
         when Stdout =>
            Flush (Standard_Output);
         when Stderr =>
            Flush (Standard_Error);
      end case;
   end Flush_Standard_File;

   ------------------------
   -- Create_Output_File --
   ------------------------

   procedure Create_Output_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  Create an Output_Stream connected to a File
      --  Create(var IO; Name : Univ_String) -> optional File_Output_Stream

      use Ada.Streams.Stream_IO;

      File_Index     : constant Stream_File_Index := New_Stream_File_Index;
      Info_Ptr       : constant Stream_File_Ptr :=
                         Nth_Element (Stream_File_Table, File_Index);
      Name_Word      : constant Word_Type :=
                         Fetch_Nonnull_Word (Params, 2);
      Name           : String renames Word_To_String (Name_Word);
      Target         : constant Word_Type :=
                         Fetch_Word (Params, 0);
      Target_Stg_Rgn : constant Stg_Rgn_Ptr := Stg_Rgn_Of_Large_Obj (Target);
      --  Type_Desc  : constant Non_Op_Map_Type_Ptr := Context.Enclosing_Type;

      New_Obj        : constant Word_Type :=
                         Allocate_From_Stg_Rgn
                           (Target_Stg_Rgn,
                            Large_Obj_Header_Size + 2,
                            Context.Server_Index);
      Name_Copy      : constant Word_Type :=
                           Copy_Object
                             (Context    => Context,
                              Type_Desc  =>
                                Univ_Strings.Get_Univ_String_Type_Desc,
                              Object     => Name_Word,
                              Stg_Rgn_Of => Target);
   begin
      if Messages.Is_Error_File (Name) then
         --  We are creating the error file
         --  Close it in case it is already open;
         --  create it now if never created
         Messages.Close_Error_File;

         --  We should append instead in case there are already
         --  some messages there.
         Ada.Streams.Stream_IO.Open
           (Info_Ptr.File, Mode => Append_File, Name => Name);
      else
         --  Create the file
         Ada.Streams.Stream_IO.Create (Info_Ptr.File, Name => Name,
           Form => "(WCEM=8)");
      end if;

      --  Store filename and index
      Store_Word (New_Obj + Large_Obj_Header_Size, Name_Copy);
      Store_Word (New_Obj + (Large_Obj_Header_Size + 1),
        Word_Type (File_Index));

      --  Store type
      Set_Large_Obj_Type_Info (New_Obj, Static_Link.Index);

      --  Store file object
      Store_Word (Params, 0, New_Obj);
   exception
      when Name_Error | Use_Error | Device_Error =>
         Free_File (Info_Ptr);

         --  Store null file object
         Store_Word (Params, 0,
           Null_For_Stg_Rgn (Target_Stg_Rgn));
   end Create_Output_File;

   ---------------------
   -- Open_Input_File --
   ---------------------

   procedure Open_Input_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  Open an Input_Stream connected to a File
      --  Open(var IO; Name : Univ_String) -> optional File_Input_Stream

      use Ada.Streams.Stream_IO;

      File_Index : constant Stream_File_Index := New_Stream_File_Index;
      Info_Ptr   : constant Stream_File_Ptr :=
                     Nth_Element (Stream_File_Table, File_Index);
      Name_Word  : constant Word_Type := Fetch_Nonnull_Word (Params, 2);
      Name       : String renames Word_To_String (Name_Word);
      Target     : constant Word_Type := Fetch_Word (Params, 0);
      Target_Stg_Rgn : constant Stg_Rgn_Ptr := Stg_Rgn_Of_Large_Obj (Target);
      --  Type_Desc  : constant Non_Op_Map_Type_Ptr := Context.Enclosing_Type;
      New_Obj    : constant Word_Type :=
                     Allocate_From_Stg_Rgn
                       (Target_Stg_Rgn,
                        Large_Obj_Header_Size + 2,
                        Context.Server_Index);
      Name_Copy      : constant Word_Type :=
                           Copy_Object
                             (Context    => Context,
                              Type_Desc  =>
                                Univ_Strings.Get_Univ_String_Type_Desc,
                              Object     => Name_Word,
                              Stg_Rgn_Of => Target);
   begin
      Ada.Streams.Stream_IO.Open
        (Info_Ptr.File, Mode => In_File, Name => Name);

      --  Store filename and index
      Store_Word (New_Obj + Large_Obj_Header_Size, Name_Copy);
      Store_Word (New_Obj + (Large_Obj_Header_Size + 1),
                  Word_Type (File_Index));

      --  Store type
      Set_Large_Obj_Type_Info (New_Obj, Static_Link.Index);

      --  Store file object
      Store_Word (Params, 0, New_Obj);
   exception
      when Name_Error | Use_Error | Device_Error =>
         Free_File (Info_Ptr);

         --  Store null file object
         Store_Word (Params, 0,
           Null_For_Stg_Rgn (Target_Stg_Rgn));
   end Open_Input_File;

   -------------------
   -- Print_To_File --
   -------------------

   procedure Print_To_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  Print string to file
      --  Print(var File_Output_Stream; Univ_String)

      use Ada.Streams.Stream_IO;

      File_Obj_Ref : constant Word_Ptr :=
                       Fetch_Word_Ptr (Params, 0);
      File_Obj     : constant Word_Type :=
                       Fetch_Word (File_Obj_Ref, 0);
      pragma Assert (not Is_Large_Null (File_Obj));

      File_Index   : constant Stream_File_Index :=
                       Stream_File_Index
                         (Fetch_Word (File_Obj + (Large_Obj_Header_Size + 1)));
      Info_Ptr     : constant Stream_File_Ptr :=
                       Nth_Element (Stream_File_Table, File_Index);
      Str_Word     : constant Word_Type :=
                       Fetch_Word (Params, 1);
      Str          : Wide_Wide_String
                       renames Word_To_Wide_Wide_String (Str_Word);

      use Ada.Strings.UTF_Encoding;
      Utf_8_Str    : UTF_8_String
                       renames Wide_Wide_Strings.Encode (Str);
   begin
      UTF_8_String'Write (Stream (Info_Ptr.File), Utf_8_Str);
   end Print_To_File;

   ----------------------------
   -- Print_To_Standard_File --
   ----------------------------

   procedure Print_To_Standard_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Print string to standard output/error file
   --  Print(var Standard_Output_Stream; Univ_String)
      use Ada.Streams.Stream_IO;

      File_Obj_Ref : constant Word_Ptr :=
                       Fetch_Word_Ptr (Params, 0);
      File_Chan    : constant Output_Chan_Enum :=
                       Output_Chan_Enum'Val (Fetch_Word (File_Obj_Ref, 0));
      Str_Word     : constant Word_Type :=
                       Fetch_Word (Params, 1);
      Str          : Wide_Wide_String
                       renames Word_To_Wide_Wide_String (Str_Word);

      use Ada.Strings.UTF_Encoding;
      Utf_8_Str    : UTF_8_String
                       renames Wide_Wide_Strings.Encode (Str);
      use Ada.Text_IO;
      File_Stream  : Text_Streams.Stream_Access;
   begin
      --  Get the stream to use
      case File_Chan is
         when Stdout =>
            File_Stream := Text_Streams.Stream (Standard_Output);
         when Stderr =>
            File_Stream := Text_Streams.Stream (Standard_Error);
      end case;

      --  Write the string out to the stream
      UTF_8_String'Write (File_Stream, Utf_8_Str);
   end Print_To_Standard_File;

   --------------------
   -- Read_From_File --
   --------------------

   procedure Read_From_File
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  Read a line from a file as a string
      --  Readln(var Input_Stream) -> optional Univ_String

      use Ada.Streams.Stream_IO;

      Target       : constant Word_Type := Fetch_Word (Params, 0);

      File_Obj_Ref : constant Word_Ptr :=
                       Fetch_Word_Ptr (Params, 1);
      File_Obj     : constant Word_Type :=
                       Fetch_Word (File_Obj_Ref, 0);
      pragma Assert (not Is_Large_Null (File_Obj));

      File_Index   : constant Stream_File_Index :=
                       Stream_File_Index
                         (Fetch_Word
                           (File_Obj + (Large_Obj_Header_Size + 1)));
      Info_Ptr     : constant Stream_File_Ptr :=
                       Nth_Element (Stream_File_Table, File_Index);
      Max_Line     : constant := 4096;  -- Max length of a line

      Line : String (1 .. Max_Line);
      Char : Character;
      Last : Natural := 0;
   begin
      --  Read until get LF or CR/LF
      while Last < Line'Last loop
         Character'Read (Stream (Info_Ptr.File), Char);
         case Char is
            when ASCII.CR =>
               declare
                  Next_Char : Character;
               begin
                  Character'Read (Stream (Info_Ptr.File), Next_Char);
                  --  Treat CR/LF as a single line terminator
                  exit when Next_Char = ASCII.LF;
                  Last := Last + 2;
                  Line (Last - 1) := Char;
                  Line (Last) := Next_Char;
               end;
            when ASCII.LF =>
               --  End of line
               exit;
            when others =>
               --  Add char to line
               Last := Last + 1;
               Line (Last) := Char;
         end case;
      end loop;
      --  End of line or 4096 chars, whichever came first
      --  Store result
      Store_Word (Params, 0, String_To_Word (Line (1 .. Last),
                                             Target,
                                             Context.Server_Index));
   exception
      when End_Error =>
         if Last > 0 then
            --  Last line
            Store_Word (Params, 0,
              String_To_Word (Line (1 .. Last),
                              Target,
                              Context.Server_Index));
         else
            --  End of file
            --  Return a "null" of type Univ_String
            Store_Word
              (Params, 0,
                 Univ_Strings.Null_Of_Same_Rgn
                   (Univ_Strings.From_Word_Type (Target)));
         end if;
   end Read_From_File;

   procedure Set_Exit_Status
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Exit_Code : constant Word_Type := Fetch_Word (Params, 1);
   begin
      Ada.Command_Line.Set_Exit_Status
         (Ada.Command_Line.Exit_Status (Exit_Code));
   end Set_Exit_Status;

   use Strings;
begin
   Register_Builtin
     (String_Lookup ("#create_output_file"),
      Create_Output_File'Access);

   Register_Builtin
     (String_Lookup ("#append_output_file"),
      Append_Output_File'Access);

   Register_Builtin
     (String_Lookup ("#print_to_standard_file"),
      Print_To_Standard_File'Access);

   Register_Builtin
     (String_Lookup ("#flush_standard_file"),
      Flush_Standard_File'Access);

   Register_Builtin
     (String_Lookup ("#close_output_file"),
      Close_Output_File'Access);

   Register_Builtin
     (String_Lookup ("#delete_output_file"),
      Delete_Output_File'Access);

   Register_Builtin
     (String_Lookup ("#flush_output_file"),
      Flush_Output_File'Access);

   Register_Builtin
     (String_Lookup ("#print_to_file"),
      Print_To_File'Access);

   Register_Builtin
     (String_Lookup ("#open_input_file"),
      Open_Input_File'Access);

   Register_Builtin
     (String_Lookup ("#close_input_file"),
      Close_Output_File'Access);  --  NOTE: OK to reuse

   Register_Builtin
     (String_Lookup ("#read_from_file"),
      Read_From_File'Access);

   Register_Builtin
     (String_Lookup ("#set_exit_status"),
      Set_Exit_Status'Access);

end PSC.Interpreter.IO;
