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

with System.Storage_Elements;

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
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Create an Output_Stream which appends to a File
   --  Append(var IO; Name : Univ_String) -> optional File_Output_Stream
   pragma Export (Ada, Append_Output_File, "_psc_append_output_file");

   procedure Close_Output_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Close the Output_Stream
   --  Close(var optional File_Output_Stream)
   pragma Export (Ada, Close_Output_File, "_psc_close_file");

   procedure Delete_Output_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Close and Delete the File associated with the Output_Stream
   --  func Delete(var optional File_Output_Stream)
   pragma Export (Ada, Delete_Output_File, "_psc_delete_output");

   procedure Flush_Output_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Flush the Output_Stream
   --  Flush(var File_Output_Stream)
   pragma Export (Ada, Flush_Output_File, "_psc_flush_output_file");

   procedure Create_Output_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Create an Output_Stream connected to a File
   --  Create(var IO; Name : Univ_String) -> optional File_Output_Stream
   pragma Export (Ada, Create_Output_File, "_psc_create_output_file");

   procedure Open_Input_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Open an Input_Stream connected to a File
   --  Open(var IO; Name : Univ_String) -> optional File_Input_Stream
   pragma Export (Ada, Open_Input_File, "_psc_open_input_file");

   procedure Print_To_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Print string to file
   --  Print(var File_Output_Stream; Univ_String)
   pragma Export (Ada, Print_To_File, "_psc_print_to_file");

   procedure Print_To_Standard_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Print string to standard output/error file
   --  Print(var Standard_Output_Stream; Univ_String)
   pragma Export (Ada, Print_To_Standard_File, "_psc_print_to_standard_file");

   procedure Flush_Standard_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Flush the Standard_Output_Stream
   --  Flush(var Standard_Output_Stream)
   pragma Export (Ada, Flush_Standard_File, "_psc_flush_standard_file");

   procedure Read_From_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Read a line from a file as a string
   --  Readln(var Input_Stream) -> optional Univ_String
   pragma Export (Ada, Read_From_File, "_psc_read_from_file");

   procedure Read_Bytes_From_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Read an array of bytes from file stream.
   --  Read(var Input_Stream; var Stream_Element_Array)
   --   -> Bytes_Read : Stream_Count;
   pragma Export (Ada, Read_Bytes_From_File, "_psc_read_bytes_from_file");

   procedure Write_Bytes_To_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Write an array of bytes to file stream.
   --  Write(var Output_Stream; Stream_Element_Array)
   --   -> Bytes_Written : Stream_Count;
   pragma Export (Ada, Write_Bytes_To_File, "_psc_write_bytes_to_file");

   procedure Write_Obj
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Write_Obj (var Output_Object_Stream; Obj : Obj_Type is Assignable<>)
   pragma Export (Ada, Write_Obj, "_psc_write_obj");
   procedure Write_Optional_Obj
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Write_Optional_Obj
   --    (var Output_Object_Stream; Obj : optional Obj_Type is Assignable<>)
   pragma Export (Ada, Write_Optional_Obj, "_psc_write_optional_obj");

   procedure Write_Default
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Write_Default
   --    (var Output_Object_Stream; Obj : Obj_Type is Assignable<>)
   pragma Export (Ada, Write_Default, "_psc_write_default");
   procedure Write_Optional_Default
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Write_Optional_Default
   --    (var Output_Object_Stream; Obj : optional Obj_Type is Assignable<>)
   pragma Export (Ada, Write_Optional_Default, "_psc_write_optional_default");

   procedure Read_Obj
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Read_Obj (var Input_Object_Stream; var Obj : Obj_Type is Assignable<>)
   pragma Export (Ada, Read_Obj, "_psc_read_obj");
   procedure Read_Optional_Obj
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Read_Optional_Obj
   --    (var Input_Object_Stream; var Obj : optional Obj_Type is Assignable<>)
   pragma Export (Ada, Read_Optional_Obj, "_psc_read_optional_obj");

   procedure Read_Default
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Read_Default
   --    (var Input_Object_Stream; var Obj : Obj_Type is Assignable<>)
   pragma Export (Ada, Read_Default, "_psc_read_default");
   procedure Read_Optional_Default
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Read_Optional_Default
   --    (var Input_Object_Stream; var Obj : optional Obj_Type is Assignable<>)
   pragma Export (Ada, Read_Optional_Default, "_psc_read_optional_default");

   procedure Set_Exit_Status
     (Context : in out Exec_Context;
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
     (Context : in out Exec_Context;
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
     (Context : in out Exec_Context;
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
     (Context : in out Exec_Context;
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
     (Context : in out Exec_Context;
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
     (Context : in out Exec_Context;
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
     (Context : in out Exec_Context;
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
     (Context : in out Exec_Context;
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
     (Context : in out Exec_Context;
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
     (Context : in out Exec_Context;
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
     (Context : in out Exec_Context;
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

   --------------------------
   -- Read_Bytes_From_File --
   --------------------------

   procedure Read_Bytes_From_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Read an array of bytes from file stream.
   --  Read(var Input_Stream; var Stream_Element_Array)
   --   -> Bytes_Read : Stream_Count;
      use Ada.Streams.Stream_IO;

      File_Obj_Ref : constant Word_Ptr :=
                       Fetch_Word_Ptr (Params, 1);
      File_Obj     : constant Word_Type :=
                       Fetch_Nonnull_Word (File_Obj_Ref, 0);

      File_Index   : constant Stream_File_Index :=
                       Stream_File_Index
                         (Fetch_Word
                           (File_Obj + (Large_Obj_Header_Size + 1)));
      Info_Ptr     : constant Stream_File_Ptr :=
                       Nth_Element (Stream_File_Table, File_Index);

      Arr_Obj_Ref  : constant Word_Ptr :=
                       Fetch_Word_Ptr (Params, 2);
      Arr_Obj      : constant Word_Ptr :=
                       Fetch_Word_Ptr (Arr_Obj_Ref, 0);

      Arr_Bounds   : constant Word_Ptr :=
                       Fetch_Word_Ptr
                         (Arr_Obj, Large_Obj_Header_Size + 0);

      use Ada.Streams;

      Arr_First    : constant Stream_Element_Count := Stream_Element_Count
                       (Fetch_Word (Arr_Bounds, Large_Obj_Header_Size + 0));
      Arr_Last     : constant Stream_Element_Offset := Stream_Element_Offset
                       (Fetch_Word (Arr_Bounds, Large_Obj_Header_Size + 1));
      Arr_Basic    : constant Word_Type :=
                       Fetch_Nonnull_Word (Arr_Obj, Large_Obj_Header_Size + 1);

      use System.Storage_Elements;

      Arr_Start    : constant System.Address := To_Address (Integer_Address
                       (Word_Type'(Arr_Basic + (Large_Obj_Header_Size + 1))));

      Stream_Elem_Arr : Stream_Element_Array (Arr_First .. Arr_Last)
        with Address => Arr_Start;

      Last : Stream_Element_Offset := 0;
   begin
      --  Read in the bytes
      Stream (Info_Ptr.File).Read (Stream_Elem_Arr, Last);

      --  Store the count of bytes read from the file
      Store_Word (Params, 0, Word_Type (Last - Arr_First + 1));
   end Read_Bytes_From_File;

   -------------------------
   -- Write_Bytes_To_File --
   -------------------------

   procedure Write_Bytes_To_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Write an array of bytes to file stream.
   --  Write(var Output_Stream; Stream_Element_Array)
   --   -> Bytes_Written : Stream_Count;
      use Ada.Streams.Stream_IO;

      File_Obj_Ref : constant Word_Ptr :=
                       Fetch_Word_Ptr (Params, 1);
      File_Obj     : constant Word_Ptr :=
                       Fetch_Word_Ptr (File_Obj_Ref, 0);

      File_Index   : constant Stream_File_Index :=
                       Stream_File_Index
                         (Fetch_Word
                           (File_Obj, Large_Obj_Header_Size + 1));
      Info_Ptr     : constant Stream_File_Ptr :=
                       Nth_Element (Stream_File_Table, File_Index);

      Arr_Obj      : constant Word_Ptr :=
                       Fetch_Word_Ptr (Params, 2);

      Arr_Bounds   : constant Word_Ptr :=
                       Fetch_Word_Ptr (Arr_Obj, Large_Obj_Header_Size + 0);

      use Ada.Streams;

      Arr_First    : constant Stream_Element_Count := Stream_Element_Count
                       (Fetch_Word (Arr_Bounds, Large_Obj_Header_Size + 0));
      Arr_Last     : constant Stream_Element_Offset := Stream_Element_Offset
                       (Fetch_Word (Arr_Bounds, Large_Obj_Header_Size + 1));
      Arr_Basic    : constant Word_Type :=
                       Fetch_Nonnull_Word (Arr_Obj, Large_Obj_Header_Size + 1);

      use System.Storage_Elements;

      Arr_Start    : constant System.Address := To_Address (Integer_Address
                       (Word_Type'(Arr_Basic + (Large_Obj_Header_Size + 1))));

      Stream_Elem_Arr : Stream_Element_Array (Arr_First .. Arr_Last)
        with Address => Arr_Start;

      Last : Stream_Element_Offset := 0;
   begin
      --  Write out the bytes
      Stream (Info_Ptr.File).Write (Stream_Elem_Arr);

      --  Store the count of bytes written (all of them)
      Store_Word (Params, 0, Word_Type (Arr_Last - Arr_First + 1));
   end Write_Bytes_To_File;

   -----------------------------------------
   -- Write_[Optional_]{Obj,Default} --
   -----------------------------------------

   NYI : exception;

   procedure Write_Obj_To_Stream
     (Context : in out Exec_Context;
      Obj_Stream : Word_Type;
      Obj : Word_Type;
      Obj_Type : Type_Descriptor_Ptr;
      Is_Optional, Use_Default : Boolean);
   --  Write Obj of type Obj_Type to Obj_Stream.
   --  If Is_Optional is True, then Obj is allowed to be null.
   --  Is Use_Default is True, then any definition of the "write"
   --  operation for Obj_Type should be ignored, and the default
   --  implementation should be used.

   procedure Write_Obj_To_Stream
     (Context : in out Exec_Context;
      Obj_Stream : Word_Type;
      Obj : Word_Type;
      Obj_Type : Type_Descriptor_Ptr;
      Is_Optional, Use_Default : Boolean) is

      use Type_Descriptor_Ops;

      Non_Map_Type_Desc : constant Non_Op_Map_Type_Ptr :=
                           Skip_Over_Op_Map (Obj_Type);

      Debug : constant Boolean := False;
      use Ada.Text_IO;

   begin  --  Write_Obj_To_Stream

      if not Use_Default then
         --  TBD: Check if type implements "write" of Custom_Streaming
         --       interface
         null;
      end if;

      if Non_Map_Type_Desc.Is_Wrapper then
         --  Is a wrapper, recurse with only component
         Write_Obj_To_Stream
           (Context,
            Obj_Stream,
            Obj,
            Non_Map_Type_Desc.Components (1).Type_Desc,
            Is_Optional, Use_Default);
         return;  --  All done  --
      end if;

      declare
         --  (Polymorphic) Value stream is only component of Obj_Stream
         Val_Stream_Poly_Obj : constant Word_Type :=
           Fetch_Word (Obj_Stream, Large_Obj_Header_Size);

         --  Get poly type from val stream obj
         Val_Stream_Poly_Type : constant Type_Descriptor_Ptr :=
           Large_Obj_Type_Desc (Val_Stream_Poly_Obj);
         pragma Assert (Val_Stream_Poly_Type.Is_Polymorphic);

         --  Get non-poly type for val stream (might be op-map)
         Val_Stream_Type : constant Type_Descriptor_Ptr :=
           Val_Stream_Poly_Type.Components (1).Type_Desc;
         pragma Assert (not Val_Stream_Type.Is_Polymorphic);

         --  Get underlying non-poly stream obj
         Val_Stream_Obj : constant Word_Type :=
           Fetch_Word (Val_Stream_Poly_Obj, Large_Obj_Header_Size);

         --  TBD: We could use non-optional ops if Is_Optional => False.

         Write_Int_Op_Index : constant Operation_Index := 4;
            --  func Write_Optional_Int
            --   (var Output_Value_Stream;
            --    Val : optional Univ_Integer;
            --    Low, High : Univ_Integer);

         Write_Float_Op_Index : constant Operation_Index := 6;
            --  func Write_Optional_Float
            --   (var Output_Value_Stream; Val : optional Univ_Float;
            --   Digits : Univ_Integer);

         Write_Str_Op_Index : constant Operation_Index := 8;
            --  func Write_Optional_String
            --   (var Output_Value_Stream; Val : optional Univ_String;
            --    Max_Char : Univ_Character;
            --    Min_Len, Max_Len : Univ_Integer);

         Begin_Seq_Op_Index : constant Operation_Index := 10;
         --  func Write_Begin_Optional_Seq
         --        (var Output_Value_Stream;
         --         Min_Len, Max_Len : Univ_Integer;
         --         Actual_Len : optional Univ_Integer);

         End_Seq_Op_Index : constant Operation_Index := 11;
         --  func Write_End_Seq (var Output_Value_Stream);

         Begin_Obj_Op_Index : constant Operation_Index := 19;
         --  func Write_Begin_Optional_Obj
         --         (var Output_Value_Stream;
         --          Is_Null : Boolean);
         End_Obj_Op_Index : constant Operation_Index := 20;
         --  func Write_End_Obj (var Output_Value_Stream);

         Param_Arr : array (0 .. 4) of aliased Word_Type;

         Op_Index : Operation_Index := 0;

      begin

         --  Initialize val-stream parameter
         Param_Arr (0) := Val_Stream_Obj;

         if Debug then
            Put ("Write_Obj: ");
         end if;

         if Non_Map_Type_Desc.Type_Kind = Univ_String_Kind then
            --  write a Univ_String
            if Debug then
               Put_Line ('"' &
                 Univ_Strings.To_String (Univ_Strings.From_Word_Type (Obj))
                 & '"');
            end if;

            --  Init rest of params for Write_Optional_String
            Param_Arr (1) := Obj;
            Param_Arr (2) := 2**23 - 1;  --  Max_Char TBD
            Param_Arr (3) := 0;
            Param_Arr (4) := 2**16 - 1;  --  Max_Len TBD
            Op_Index := Write_Str_Op_Index;

         elsif Is_Small (Non_Map_Type_Desc) then
            if Is_Null_Value (Obj, Non_Map_Type_Desc) then
               if Debug then
                  Put_Line ("null");
               end if;
            else
               if Debug then
                  Put_Line (Hex_Image (Obj));
               end if;
            end if;

            --  Init "obj" parameter
            Param_Arr (1) := Obj;

            --  Do type-kind-specific processing
            case Non_Map_Type_Desc.Type_Kind is
            when Basic_Array_Kind
               | Univ_String_Kind
               | Aliased_Object_Kind =>
               --  These are "large" types
               raise Program_Error;

            when Univ_Integer_Kind
               | Univ_Char_Kind  --  TBD
               | Unsigned_64_Kind  --  TBD
               | Normal_Kind  --  TBD
               | Integer_64_Kind =>  -- TBD
               --  These are all "integerish" types
               --  TBD -- need to convert to a real Univ_Integer

               --  Init rest of params for Write_Optional_Integer
               --  TBD: Low and High could be much larger!
               Param_Arr (2) := Word_Type'First + Word_Type'(1);
               Param_Arr (3) := Word_Type'Last;
               Op_Index := Write_Int_Op_Index;

            when Univ_Real_Kind =>
               --  Init rest of params for Write_Optional_Float
               Param_Arr (2) := 15;  --  Digits
               Op_Index := Write_Float_Op_Index;

            when Univ_Enum_Kind =>
               raise NYI;

            end case;

         elsif Is_Large_Null (Obj) then

            pragma Assert (Is_Optional);  --  TBD: check earlier as well

            if Non_Map_Type_Desc.Type_Kind = Basic_Array_Kind then
               if Debug then
                  Put_Line ("null basic-array");
               end if;

               Param_Arr (1) := 0;  --  Min_Len
               Param_Arr (2) := 2**31 - 1;  --  Max_Len
               Param_Arr (3) := Null_Value;  --  Actual_Len
               Op_Index := Begin_Seq_Op_Index;
            else
               if Debug then
                  Put_Line ("null large obj");
               end if;

               Param_Arr (1) := 1;  -- Is_Null => #true
               Op_Index := Begin_Obj_Op_Index;
            end if;

         else
            --  Non-null large object
            --  Write each component
            declare
               Lock_Obj : constant Lock_Obj_Index :=
                            Large_Obj_Lock_Obj (Obj);
            begin
               if Debug then
                  Put_Line
                    ("Large obj at " &
                     Hex_Image (Obj) &
                     ", size = " &
                     Hex_Image (Word_Type (Large_Obj_Size (Obj))));
                  Put_Line
                    (" region =" &
                     Stg_Rgn_Index'Image (Large_Obj_Stg_Rgn_Index (Obj)));
                  Put_Line
                    (" type = " &
                     Strings.To_String (Non_Map_Type_Desc.Name));

                  if Lock_Obj /= 0 then
                     Put_Line
                       (" lock =" & Lock_Obj_Index'Image (Lock_Obj));
                  end if;
               end if;

               if Non_Map_Type_Desc.Type_Kind = Basic_Array_Kind then
                  --  write "Basic_Array" components
                  declare
                     Comp_Type : constant Type_Descriptor_Ptr :=
                       Basic_Array_Comp_Type (Non_Map_Type_Desc);
                     Len : constant Word_Type :=
                       Content_Of_Virtual_Address
                          (Obj + Large_Obj_Header_Size);

                  begin
                     Param_Arr (1) := 0;  --  Min_Len
                     Param_Arr (2) := 2**31 - 1;  --  Max_Len
                     Param_Arr (3) := Len;  --  Actual_Len
                     Execute_Compiled_Nth_Op_Of_Type
                       (Context => Context,
                        Params => Param_Arr (0)'Unchecked_Access,
                        Static_Link => Val_Stream_Type,
                        Target_Base => Type_Area,
                        Op_Index => Begin_Seq_Op_Index);

                     if Debug then
                        Put_Line (" Begin_Optional_Seq: Len = " & Len'Image);
                     end if;

                     for I in 1 .. Offset_Within_Area (Len) loop
                        declare
                           Offset : constant Offset_Within_Area :=
                             Large_Obj_Header_Size + I;
                           Comp_Value : constant Word_Type :=
                             Content_Of_Virtual_Address (Obj + Offset);
                        begin
                           --  Write each component
                           Write_Obj_To_Stream
                             (Context,
                              Obj_Stream,
                              Comp_Value,
                              Comp_Type,
                              Is_Optional => True,  --  TBD
                              Use_Default => False);
                        end;
                     end loop;

                     Op_Index := End_Seq_Op_Index;

                  end;
               else
                  --  Not a "Basic_Array" so each component of a different type
                  Param_Arr (1) := 0;  --  Is_Null => #false
                  Execute_Compiled_Nth_Op_Of_Type
                    (Context => Context,
                     Params => Param_Arr (0)'Unchecked_Access,
                     Static_Link => Val_Stream_Type,
                     Target_Base => Type_Area,
                     Op_Index => Begin_Obj_Op_Index);

                  for I in 1 .. Non_Map_Type_Desc.Num_Components loop
                     declare
                        Comp_Type : constant Non_Op_Map_Type_Ptr :=
                          Skip_Over_Op_Map
                             (Non_Map_Type_Desc.Components (I).Type_Desc);
                        Comp_Value : constant Word_Type :=
                          Content_Of_Virtual_Address
                             (Obj +
                              Large_Obj_Header_Size +
                              Offset_Within_Area (I - 1));
                     begin
                        if Non_Map_Type_Desc.Components (I).Is_By_Ref then
                           --  TBD -- by-ref component
                           Put_Line
                             ("  Ref: " & Hex_Image (Comp_Value));
                        else
                           Write_Obj_To_Stream
                             (Context,
                              Obj_Stream,
                              Comp_Value,
                              Comp_Type,
                              Is_Optional => True,  --  TBD
                              Use_Default => False);
                        end if;
                     end;
                  end loop;

                  Op_Index := End_Obj_Op_Index;

               end if;  --  Whether is Basic_Array
            end;
         end if;  --  Whether is string, small, or large non-string

         if Debug then
            Put_Line ("  Op_Index = " & Op_Index'Image);
         end if;

         --  Emit the (final) call to the appropriate Value_Stream operation
         Execute_Compiled_Nth_Op_Of_Type
           (Context => Context,
            Params => Param_Arr (0)'Unchecked_Access,
            Static_Link => Val_Stream_Type,
            Target_Base => Type_Area,
            Op_Index => Op_Index);

      end;
   end Write_Obj_To_Stream;

   procedure Write_Obj
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Write_Obj (var Output_Object_Stream; Obj : Obj_Type is Assignable<>)
      Enclosing_Type : constant Non_Op_Map_Type_Ptr := Static_Link;
         --  Implicit module instance with one formal param

      Obj_Type : constant Type_Descriptor_Ptr :=
        Enclosing_Type.Parameters (1).Data.Type_Desc;
         --  Type of object

      Obj_Stream : constant Word_Type := Fetch_Word (Params, 0);
      Obj : constant Word_Type := Fetch_Word (Params, 1);

   begin  --  Write_Obj
      --  Pass the buck to recursive version
      Write_Obj_To_Stream (Context, Obj_Stream, Obj, Obj_Type,
        Is_Optional => False, Use_Default => False);
   end Write_Obj;

   procedure Write_Optional_Obj
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Write_Optional_Obj
   --    (var Output_Object_Stream; Obj : optional Obj_Type is Assignable<>)

      Enclosing_Type : constant Non_Op_Map_Type_Ptr := Static_Link;
         --  Implicit module instance with one formal param

      Obj_Type : constant Type_Descriptor_Ptr :=
        Enclosing_Type.Parameters (1).Data.Type_Desc;
         --  Type of object

      Obj_Stream : constant Word_Type := Fetch_Word (Params, 0);
      Obj : constant Word_Type := Fetch_Word (Params, 1);

   begin  --  Write_Optional_Obj
      --  Pass the buck to recursive version
      Write_Obj_To_Stream (Context, Obj_Stream, Obj, Obj_Type,
        Is_Optional => True, Use_Default => False);
   end Write_Optional_Obj;

   procedure Write_Default
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Write_Default
   --    (var Output_Object_Stream; Obj : Obj_Type is Assignable<>)
      Enclosing_Type : constant Non_Op_Map_Type_Ptr := Static_Link;
         --  Implicit module instance with one formal param

      Obj_Type : constant Type_Descriptor_Ptr :=
        Enclosing_Type.Parameters (1).Data.Type_Desc;
         --  Type of object

      Obj_Stream : constant Word_Type := Fetch_Word (Params, 0);
      Obj : constant Word_Type := Fetch_Word (Params, 1);

   begin  --  Write_Default
      --  Pass the buck to recursive version
      Write_Obj_To_Stream (Context, Obj_Stream, Obj, Obj_Type,
        Is_Optional => False, Use_Default => True);
   end Write_Default;

   procedure Write_Optional_Default
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Write_Optional_Default
   --    (var Output_Object_Stream; Obj : optional Obj_Type is Assignable<>)
      Enclosing_Type : constant Non_Op_Map_Type_Ptr := Static_Link;
         --  Implicit module instance with one formal param

      Obj_Type : constant Type_Descriptor_Ptr :=
        Enclosing_Type.Parameters (1).Data.Type_Desc;
         --  Type of object

      Obj_Stream : constant Word_Type := Fetch_Word (Params, 0);
      Obj : constant Word_Type := Fetch_Word (Params, 1);

   begin  --  Write_Optional_Default
      --  Pass the buck to recursive version
      Write_Obj_To_Stream (Context, Obj_Stream, Obj, Obj_Type,
        Is_Optional => True, Use_Default => True);
   end Write_Optional_Default;

   ----------------------------------------
   -- Read_[Optional_]{Obj,Default} --
   ----------------------------------------

   procedure Read_Obj_From_Stream
     (Context : in out Exec_Context;
      Obj_Stream : Word_Type;
      Obj_Ptr : Word_Ptr;
      Obj_Type : Type_Descriptor_Ptr;
      Is_Optional, Use_Default : Boolean);
   --  Read Obj of type Obj_Type from Obj_Stream into Obj_Ptr.all.
   --  If Obj_Type implies a large object, then Obj_Ptr must be
   --  initialized to point to a large "null" of the appropriate region.
   --  If Is_Optional is True, then Obj is allowed to be null.
   --  Is Use_Default is True, then any definition of the "read"
   --  operation for Obj_Type should be ignored, and the default
   --  implementation should be used.

   procedure Read_Obj_From_Stream
     (Context : in out Exec_Context;
      Obj_Stream : Word_Type;
      Obj_Ptr : Word_Ptr;
      Obj_Type : Type_Descriptor_Ptr;
      Is_Optional, Use_Default : Boolean) is

      use Type_Descriptor_Ops;

      Non_Map_Type_Desc : constant Non_Op_Map_Type_Ptr :=
                           Skip_Over_Op_Map (Obj_Type);

      Debug : constant Boolean := False;
      use Ada.Text_IO;

      package Vec_Of_Words is new PSC.Vectors (Word_Type);

   begin  --  Read_Obj_From_Stream

      if not Use_Default then
         --  TBD: Check if type implements "read" of Custom_Streaming
         --       interface
         null;
      end if;

      if Non_Map_Type_Desc.Is_Wrapper then
         --  Is a wrapper, recurse with only component
         if Debug then
            Put_Line
              ("  [wrapper: " & Strings.To_String (Non_Map_Type_Desc.Name) &
                  "]");
         end if;

         Read_Obj_From_Stream
           (Context,
            Obj_Stream,
            Obj_Ptr,
            Non_Map_Type_Desc.Components (1).Type_Desc,
            Is_Optional, Use_Default);
         return;  --  All done  --
      end if;

      if Debug then
         Put ("Read_Obj: ");
      end if;
      declare
         --  (Polymorphic) Value stream is only component of Obj_Stream
         Val_Stream_Poly_Obj : constant Word_Type :=
           Fetch_Word (Obj_Stream, Large_Obj_Header_Size);

         --  Get poly type from val stream obj
         Val_Stream_Poly_Type : constant Type_Descriptor_Ptr :=
           Large_Obj_Type_Desc (Val_Stream_Poly_Obj);
         pragma Assert (Val_Stream_Poly_Type.Is_Polymorphic);

         --  Get non-poly type for val stream (might be op-map)
         Val_Stream_Type : constant Type_Descriptor_Ptr :=
           Val_Stream_Poly_Type.Components (1).Type_Desc;
         pragma Assert (not Val_Stream_Type.Is_Polymorphic);

         --  Get underlying non-poly stream obj
         Val_Stream_Obj : constant Word_Type :=
           Fetch_Word (Val_Stream_Poly_Obj, Large_Obj_Header_Size);

         Null_For_Obj : constant Word_Type :=
           Null_Of_Same_Stg_Rgn (Obj_Type, Obj_Ptr.all);
            --  Null to use for output slot

         Result : Word_Type := Null_For_Obj;

         --  Define operation indices for Value_Stream operations.

         Read_Int_Op_Index : constant Operation_Index := 4;
            --  func Read_Optional_Int
            --   (var Input_Value_Stream;
            --    Low, High : Univ_Integer) -> optional Univ_Integer;

         Read_Float_Op_Index : constant Operation_Index := 6;
            --  func Read_Optional_Float
            --   (var Input_Value_Stream;
            --   Digits : Univ_Integer) -> optional Univ_Real;

         Read_Str_Op_Index : constant Operation_Index := 8;
            --  func Read_Optional_String
            --   (var Input_Value_Stream;
            --    Max_Char : Univ_Character;
            --    Min_Len, Max_Len : Univ_Integer) -> optional Univ_String;

         Begin_Seq_Op_Index : constant Operation_Index := 10;
         --  func Read_Begin_Optional_Seq
         --        (var Input_Value_Stream;
         --         Min_Len, Max_Len : Univ_Integer)
         --         -> Actual_Len : optional Univ_Integer;

         More_Seq_Elements_Op_Index : constant Operation_Index := 11;
         --  func More_Seq_Elements (var Input_Value_Stream) -> Boolean;

         End_Seq_Op_Index : constant Operation_Index := 12;
         --  func Read_End_Seq (var Input_Value_Stream);

         Begin_Obj_Op_Index : constant Operation_Index := 22;
         --  func Read_Begin_Optional_Obj
         --         (var Input_Value_Stream) -> Boolean;
         End_Obj_Op_Index : constant Operation_Index := 23;
         --  func Read_End_Obj (var Input_Value_Stream);

         Param_Arr : array (0 .. 4) of aliased Word_Type;

         Op_Index : Operation_Index := 0;

      begin

         --  Initialize output slot and val-stream parameter
         Param_Arr (0) := Null_For_Obj;
         Param_Arr (1) := Val_Stream_Obj;

         if Obj_Type.Type_Kind = Univ_String_Kind then
            --  read a Univ_String
            --  Init rest of params for Read_Optional_String
            Param_Arr (2) := 2**23 - 1;  --  Max_Char TBD
            Param_Arr (3) := 0;
            Param_Arr (4) := 2**16 - 1;  --  Max_Len TBD
            Op_Index := Read_Str_Op_Index;

         elsif Is_Small (Non_Map_Type_Desc) then
            --  Init output slot
            case Obj_Type.Type_Kind is
            when Basic_Array_Kind
               | Univ_String_Kind
               | Aliased_Object_Kind =>
               --  These are "large" types
               raise Program_Error;

            when Univ_Integer_Kind
               | Univ_Char_Kind  --  TBD
               | Unsigned_64_Kind  --  TBD
               | Normal_Kind  --  TBD
               | Integer_64_Kind =>  -- TBD
               --  These are all "integerish" types
               --  TBD -- need to convert to a real Univ_Integer

               --  Init rest of params for Read_Optional_Integer
               --  TBD: Low and High could be much larger!
               --       These should ideally come from parameters to
               --       the module instance defining the type, which
               --       is probably a "wrapper", so we would need to
               --       pass through more information from the outer
               --       wrapper's type-descriptor.
               Param_Arr (2) := Word_Type'First + Word_Type'(1);
               Param_Arr (3) := Word_Type'Last;
               Op_Index := Read_Int_Op_Index;

            when Univ_Real_Kind =>
               --  TBD: Univ_Float_Kind once we make Univ_Real a rational type
               --  Init rest of params for Read_Optional_Float
               Param_Arr (2) := 15;  --  Digits
               --  TBD: As above, this should ideally come from a parameter
               --       to the module instance defining the type, which
               --       is probably a "wrapper", so we would need to
               --       pass through more information from the outer
               --       wrapper's type-descriptor.
               Op_Index := Read_Float_Op_Index;

            when Univ_Enum_Kind =>
               raise NYI;

            end case;

            if Debug then
               Put ("op_index = " & Op_Index'Image);
            end if;
            Execute_Compiled_Nth_Op_Of_Type
              (Context => Context,
               Params => Param_Arr (0)'Unchecked_Access,
               Static_Link => Val_Stream_Type,
               Target_Base => Type_Area,
               Op_Index => Op_Index);

            --  Get result from output slot.
            Result := Param_Arr (0);

            if Debug then
               Put_Line(", value = 0x" & Hex_Image (Result));
            end if;

         else
            --  Read large object
            --  Read each component
            declare
               Existing_Lock_Obj : constant Lock_Obj_Index :=
                            (if Is_Large_Null (Obj_Ptr.all)
                             then 0 else Large_Obj_Lock_Obj (Obj_Ptr.all));
            begin
               if Debug then
                  Put_Line
                    ("Read Large obj, target = " &
                     Hex_Image (Obj_Ptr.all));
                  Put_Line
                    (" region =" &
                     Stg_Rgn_Index'Image
                       (Large_Obj_Stg_Rgn_Index (Obj_Ptr.all)));
                  Put_Line
                    (" type = " &
                     Strings.To_String (Non_Map_Type_Desc.Name));

                  if Existing_Lock_Obj /= 0 then
                     Put_Line
                       (" lock =" & Lock_Obj_Index'Image (Existing_Lock_Obj));
                  end if;
               end if;

               if Non_Map_Type_Desc.Type_Kind = Basic_Array_Kind then
                  --  read Length and then "Basic_Array" components
                  declare
                     Comp_Type : constant Type_Descriptor_Ptr :=
                       Basic_Array_Comp_Type (Non_Map_Type_Desc);
                     Len : Word_Type := 0;

                     Saved_Elements : Vec_Of_Words.Vector;
                        --  Elements saved up if length unknown in advance
                     use Vec_Of_Words;

                     Elems_Were_Saved : Boolean := False;

                     Max_Len : constant := Offset_Within_Area'Last;
                  begin

                     Param_Arr (0) := 0;  --  Output slot
                     Param_Arr (2) := 0;  --  Min_Len
                     Param_Arr (3) := Max_Len;
                     Execute_Compiled_Nth_Op_Of_Type
                       (Context => Context,
                        Params => Param_Arr (0)'Unchecked_Access,
                        Static_Link => Val_Stream_Type,
                        Target_Base => Type_Area,
                        Op_Index => Begin_Seq_Op_Index);

                     Len := Param_Arr (0);  --  Get Actual_Len

                     if Debug then
                        Put_Line (" Begin_Optional_Seq, Len = " &
                          (if Len = Null_Value then "null" else Len'Image));
                     end if;

                     if Len = Null_Value then
                        --  Result is null
                        Result := Null_For_Obj;
                        return;  --  All done  --
                     end if;

                     if Len < 0 then
                        --  Variable length.
                        --  Use More_Seq to determine when we are done
                        Len := 0;  --  Reset Len to zero
                        --  Now read until "More" returns #false.
                        for I in 1 .. Offset_Within_Area (Max_Len) loop
                           declare
                              Comp : aliased Word_Type := Null_For_Obj;
                              Comp_Value_Ptr : constant Word_Ptr :=
                                Comp'Unchecked_Access;
                              Comp_Index : Vec_Of_Words.Elem_Index;
                           begin

                              --  Invoke More_Seq_Elements
                              Param_Arr (0) := 0;
                                 --  output slot tells if more
                              pragma Assert (Param_Arr (1) = Val_Stream_Obj);

                              Execute_Compiled_Nth_Op_Of_Type
                                (Context => Context,
                                 Params => Param_Arr (0)'Unchecked_Access,
                                 Static_Link => Val_Stream_Type,
                                 Target_Base => Type_Area,
                                 Op_Index => More_Seq_Elements_Op_Index);

                              exit when Param_Arr (0) = 0;  --  exit at end

                              --  Read the component
                              Read_Obj_From_Stream
                                (Context,
                                 Obj_Stream,
                                 Comp_Value_Ptr,
                                 Comp_Type,
                                 Is_Optional => True,  --  TBD
                                 Use_Default => False);

                              --  Save element in the vector
                              Add_Element (Saved_Elements, Comp, Comp_Index);

                              Len := Len + Word_Type'(1);
                              pragma Assert (Len = Word_Type (Comp_Index));
                           end;
                        end loop;
                        Elems_Were_Saved := True;
                     end if;
                     Result := Create_Basic_Array_Obj
                       (Array_Type_Desc => Non_Map_Type_Desc,
                        Array_Len => Integer (Len),
                        Stg_Rgn => Stg_Rgn_Of_Large_Obj (Obj_Ptr.all),
                        Server_Index => Context.Server_Index);

                     --  Now read in the elements
                     for I in 1 .. Offset_Within_Area (Len) loop
                        declare
                           Offset : constant Offset_Within_Area :=
                             Large_Obj_Header_Size + I;
                           Comp_Value_Ptr : constant Word_Ptr :=
                             Word_To_Word_Ptr (Result + Offset);
                        begin
                           --  Read each component
                           if Elems_Were_Saved then
                              --  Get Ith saved element
                              Comp_Value_Ptr.all :=
                                Nth_Element
                                  (Saved_Elements,
                                   Vec_Of_Words.Elem_Index (I));
                           else
                              --  Read in Ith element
                              Read_Obj_From_Stream
                                (Context,
                                 Obj_Stream,
                                 Comp_Value_Ptr,
                                 Comp_Type,
                                 Is_Optional => True,  --  TBD
                                 Use_Default => False);
                           end if;
                        end;
                     end loop;

                     if Elems_Were_Saved then
                        --  Recover storage used for saving elements
                        Set_Empty (Saved_Elements);
                     end if;

                     --  Final operation is End_Seq
                     Param_Arr (0) := Val_Stream_Obj;
                     Op_Index := End_Seq_Op_Index;

                  end;
               else
                  --  Not a "Basic_Array" so each component of a different type
                  Param_Arr (0) := 0;  --  output slot tells if null
                  Execute_Compiled_Nth_Op_Of_Type
                    (Context => Context,
                     Params => Param_Arr (0)'Unchecked_Access,
                     Static_Link => Val_Stream_Type,
                     Target_Base => Type_Area,
                     Op_Index => Begin_Obj_Op_Index);

                  if Debug then
                     Put_Line (" Begin_Optional_Obj, Is_Null = " &
                       Param_Arr (0)'Image);
                  end if;

                  if Param_Arr (0) = 1 then
                     --  Is_Null == #true
                     Result := Null_For_Obj;
                     return;  --  All done  --
                  end if;

                  --  Create the object
                  Result := Create_Large_Obj
                    (Type_Desc => Obj_Type,
                     Stg_Rgn => Stg_Rgn_Of_Large_Obj (Obj_Ptr.all),
                     Server_Index => Context.Server_Index);

                  --  Read each component with a recursive call.
                  for I in 1 .. Non_Map_Type_Desc.Num_Components loop
                     declare
                        Comp_Type : constant Non_Op_Map_Type_Ptr :=
                          Skip_Over_Op_Map
                             (Non_Map_Type_Desc.Components (I).Type_Desc);
                        Comp_Value_Ptr : constant Word_Ptr :=
                          Word_To_Word_Ptr
                            (Result + Large_Obj_Header_Size +
                                   Offset_Within_Area (I - 1));
                     begin
                        if Non_Map_Type_Desc.Components (I).Is_By_Ref then
                           --  TBD -- by-ref component
                           Put_Line
                             ("  Ref: " & Hex_Image (Comp_Value_Ptr.all));
                        else
                           --  Recursive call to read in component.
                           Read_Obj_From_Stream
                             (Context,
                              Obj_Stream,
                              Comp_Value_Ptr,
                              Comp_Type,
                              Is_Optional => True,  --  TBD
                              Use_Default => False);
                        end if;
                     end;
                  end loop;

                  Op_Index := End_Obj_Op_Index;
                  --  No other parameters
                  Param_Arr (0) := Val_Stream_Obj;

               end if;  --  Whether is Basic_Array

               if Debug then
                  Put_Line
                    ("  end of large obj, op_index = " & Op_Index'Image);
               end if;
               --  Emit the (final) call to the appropriate
               --  "End" Value_Stream operation
               Execute_Compiled_Nth_Op_Of_Type
                 (Context => Context,
                  Params => Param_Arr (0)'Unchecked_Access,
                  Static_Link => Val_Stream_Type,
                  Target_Base => Type_Area,
                  Op_Index => Op_Index);

            end;
         end if;  --  Whether is string, small, or large non-string

         Obj_Ptr.all := Result;

      end;

   end Read_Obj_From_Stream;

   procedure Read_Obj
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Read_Obj (var Input_Object_Stream; var Obj : Obj_Type is Assignable<>)
      Enclosing_Type : constant Non_Op_Map_Type_Ptr := Static_Link;
         --  Implicit module instance with one formal param

      Obj_Type : constant Type_Descriptor_Ptr :=
        Enclosing_Type.Parameters (1).Data.Type_Desc;
         --  Type of object

      Obj_Stream : constant Word_Type := Fetch_Word (Params, 0);
      Obj_Ptr : constant Word_Ptr := Fetch_Word_Ptr (Params, 1);

   begin  --  Read_Obj
      --  Pass the buck to recursive version
      Read_Obj_From_Stream (Context, Obj_Stream, Obj_Ptr, Obj_Type,
        Is_Optional => False, Use_Default => False);
   end Read_Obj;

   procedure Read_Optional_Obj
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Read_Optional_Obj
   --    (var Input_Object_Stream; var Obj : optional Obj_Type is Assignable<>)
      Enclosing_Type : constant Non_Op_Map_Type_Ptr := Static_Link;
         --  Implicit module instance with one formal param

      Obj_Type : constant Type_Descriptor_Ptr :=
        Enclosing_Type.Parameters (1).Data.Type_Desc;
         --  Type of object

      Obj_Stream : constant Word_Type := Fetch_Word (Params, 0);
      Obj_Ptr : constant Word_Ptr := Fetch_Word_Ptr (Params, 1);

   begin  --  Read_Optional_Obj
      --  Pass the buck to recursive version
      Read_Obj_From_Stream (Context, Obj_Stream, Obj_Ptr, Obj_Type,
        Is_Optional => True, Use_Default => False);
   end Read_Optional_Obj;

   procedure Read_Default
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Read_Default
   --    (var Input_Object_Stream; var Obj : Obj_Type is Assignable<>)
      Enclosing_Type : constant Non_Op_Map_Type_Ptr := Static_Link;
         --  Implicit module instance with one formal param

      Obj_Type : constant Type_Descriptor_Ptr :=
        Enclosing_Type.Parameters (1).Data.Type_Desc;
         --  Type of object

      Obj_Stream : constant Word_Type := Fetch_Word (Params, 0);
      Obj_Ptr : constant Word_Ptr := Fetch_Word_Ptr (Params, 1);

   begin  --  Read_Default
      --  Pass the buck to recursive version
      Read_Obj_From_Stream (Context, Obj_Stream, Obj_Ptr, Obj_Type,
        Is_Optional => False, Use_Default => True);
   end Read_Default;

   procedure Read_Optional_Default
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Read_Optional_Default
   --    (var Input_Object_Stream; var Obj : optional Obj_Type is Assignable<>)
      Enclosing_Type : constant Non_Op_Map_Type_Ptr := Static_Link;
         --  Implicit module instance with one formal param

      Obj_Type : constant Type_Descriptor_Ptr :=
        Enclosing_Type.Parameters (1).Data.Type_Desc;
         --  Type of object

      Obj_Stream : constant Word_Type := Fetch_Word (Params, 0);
      Obj_Ptr : constant Word_Ptr := Fetch_Word_Ptr (Params, 1);

   begin  --  Read_Default
      --  Pass the buck to recursive version
      Read_Obj_From_Stream (Context, Obj_Stream, Obj_Ptr, Obj_Type,
        Is_Optional => True, Use_Default => True);
   end Read_Optional_Default;

   ----------------------
   --  Set_Exit_Status --
   ----------------------

   procedure Set_Exit_Status
     (Context : in out Exec_Context;
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
     (String_Lookup ("#write_bytes_to_file"),
      Write_Bytes_To_File'Access);

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
     (String_Lookup ("#read_bytes_from_file"),
      Read_Bytes_From_File'Access);

   Register_Builtin
     (String_Lookup ("#write_obj"),
      Write_Obj'Access);

   Register_Builtin
     (String_Lookup ("#write_optional_obj"),
      Write_Optional_Obj'Access);

   Register_Builtin
     (String_Lookup ("#write_default"),
      Write_Default'Access);

   Register_Builtin
     (String_Lookup ("#write_optional_default"),
      Write_Optional_Default'Access);

   Register_Builtin
     (String_Lookup ("#read_obj"),
      Read_Obj'Access);

   Register_Builtin
     (String_Lookup ("#read_optional_obj"),
      Read_Optional_Obj'Access);

   Register_Builtin
     (String_Lookup ("#read_default"),
      Read_Default'Access);

   Register_Builtin
     (String_Lookup ("#read_optional_default"),
      Read_Optional_Default'Access);

   Register_Builtin
     (String_Lookup ("#set_exit_status"),
      Set_Exit_Status'Access);

end PSC.Interpreter.IO;
