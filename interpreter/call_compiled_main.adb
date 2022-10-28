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

--  Driver to call a compiled ParaSail program and provide runtime support,
--  without pulling in the entire compiler.

with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Command_Line;

with GNAT.Exception_Traces;
with System.Storage_Elements;

with PSC.Interpreter;
with PSC.Interpreter.Builtins;
with PSC.Interpreter.IO;
with PSC.Interpreter.Math;

with PSC.Link_Names;

with PSC.Strings;
use PSC.Strings;

with PSC.Messages;
with PSC.Source_Positions;
use PSC;
procedure Call_Compiled_Main is
   use Interpreter;

   procedure parasail_main_routine (Context : in out Exec_Context;
     Params : Word_Ptr; Static_Link : Type_Descriptor_Ptr);

   pragma Import (Ada, parasail_main_routine, "_parasail_main_routine");
   --  llvm generator uses this link-name for suitable main ParaSail routine

   procedure Test_Operation_Descriptor (Context : in out Exec_Context;
     Params : Word_Ptr; Static_Link : Type_Descriptor_Ptr);
   pragma Import (Ada, Test_Operation_Descriptor,
     Link_Name => Link_Names.Link_Name_Prefix &
       "PSL" & Link_Names.Dot & "Test" & Link_Names.Dot &
      "Test_Operation_Descriptor");
   --  Routine whose presence ensures that the operation-descriptor type
   --  is reconstructed.
   --  NOTE: This is really only needed if somewhere there is a use
   --        of an operation descriptor, but it is small so we might
   --        as well include it in all compiled ParaSail programs.

   Context : constant Exec_Context_RW_Ptr := Initial_Context;
   Null_Args : aliased Word_Type := Null_For_Stg_Rgn (Context.Local_Stg_Rgn);

begin
   --  Show unhandled exceptions
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

   --  Turn on/off verbose statistics
   Interpreter.Debug_Statistics := False;

   --  Turn on/off type-desc debugging
   Interpreter.Debug_Type_Descs := False;

   --  Interpreter.Debug_Stg_Rgns := True;
   --  Interpreter.Debug_Threading := True;
   --  Interpreter.Debug_Calls := True;

   --  Interpreter.Set_Server_Count (4);  --  Limit parallel processing

   --  Start the threads going
   Interpreter.Start_Up_Thread_Servers;

   --  Call the per-file global constructors
   Interpreter.Invoke_Per_File_Initializers;

   --  Build argument array
   declare
      Argc : constant Natural := Ada.Command_Line.Argument_Count;
      Type_Desc : constant Type_Descriptor_Ptr :=
         --  This will return null if Basic_Array<Univ_String> is never used.
         Type_Descriptor_Ops.Get_Type_Desc_By_Name
            (String_Lookup
               ("PSL::Containers::Basic_Array<PSL::Core::Univ_String>"));
      Op_Desc_Type : constant Type_Descriptor_Ptr :=
         --  This should never return null
         Type_Descriptor_Ops.Get_Type_Desc_By_Name
            (String_Lookup
               ("PSL::Core::Operation_Descriptor"));
   begin
      if Type_Desc = null then
         --  User program doesn't use command line arguments
         --  Just pass null.

         --  Call user's main routine
         parasail_main_routine (Context.all, Null_Args'Unchecked_Access, null);
      else
         --  User's program does use command line arguments
         --  Convert them from Ada's Command_Line package
         --  to a ParaSail Basic_Array
         declare
            Args : aliased Word_Type := Create_Basic_Array_Obj
               (Type_Desc, Argc, Context.Local_Stg_Rgn, Context.Server_Index);
            Args_Ptr : constant Word_Ptr := Virtual_To_Physical_Address (Args);
         begin
            for I in 1 .. Argc loop
               Store_Word (Args_Ptr,
                  Large_Obj_Header_Size + Offset_Within_Area (I),
                  (String_To_Word
                    (Ada.Command_Line.Argument (I),
                     Interpreter.Null_For_Stg_Rgn (Context.Local_Stg_Rgn),
                     Context.Server_Index)));
            end loop;
            --  Call user's main routine
            parasail_main_routine (Context.all, Args'Unchecked_Access, null);
         end;
      end if;
      if Op_Desc_Type = null then
         --  This should never be invoked, but we include the reference
         --  so we will get a link-time error if the operation descriptor
         --  type descriptor is not included.
         null;  --  tbd:stt Test_Operation_Descriptor
         --  (Context.all, Null_Args'Unchecked_Access, null);
      end if;
   end;

   Interpreter.Shut_Down_Thread_Servers (Total_Errors => 0);
exception
   when E : others =>
      Messages.Put_Error (Ada.Exceptions.Exception_Information (E),
        Src_Pos => Source_Positions.Null_Source_Position);
      Interpreter.Shut_Down_Thread_Servers (Total_Errors => 1);
end Call_Compiled_Main;
