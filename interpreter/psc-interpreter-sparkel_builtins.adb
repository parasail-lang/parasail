------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2015, AdaCore                     --
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

with PSC.Interpreter.Builtins;
with PSC.Messages;
with PSC.Strings;
with Ada.Text_IO;             use Ada.Text_IO;
pragma Elaborate (PSC.Interpreter.Builtins);
pragma Elaborate (PSC.Strings);
pragma Elaborate (Ada.Text_IO);
package body PSC.Interpreter.Sparkel_Builtins is
   --  Package providing support for builtin Sparkel operations

   True_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#True"));

   False_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#False"));

   procedure Bool_From_Univ
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  "from_univ"(Univ_Enumeration) -> Boolean
      Index : constant Strings.U_String_Index :=
        Strings.Index (To_U_String (Fetch_Word (Params, 1)));
      Result : Boolean := False;
      use type Strings.U_String_Index;
   begin
      --  Determine boolean value from index of enumeration literal
      if Index = True_Index then
         Result := True;
      elsif Index = False_Index then
         Result := False;
      else
         Messages.Put_Error
           ("Boolean literal must be #True or #False" &
            ", value = " &
            Strings.To_String (Strings.To_U_String (Index)),
            Src_Pos => Execution_Source_Pos);
         pragma Assert (False);
         null;
      end if;

      Store_Word (Params, 0, Boolean'Pos (Result));
   end Bool_From_Univ;

   procedure Bool_To_Univ
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  "to_univ"(Boolean) -> Univ_Enumeration
      Bool_Pos : constant Word_Type := Fetch_Word (Params, 1);
      Val : constant Boolean := Boolean'Val (Bool_Pos);
      Result : Strings.U_String_Index;
   begin
      --  Determine univ-enum value given boolean value
      if Val then
         Result := True_Index;
      else
         Result := False_Index;
      end if;
      Store_Word (Params, 0, Word_Type (Result));
   end Bool_To_Univ;

begin

   Builtins.Register_Builtin
     (Strings.String_Lookup ("#sparkel_bool_from_univ"),
      Bool_From_Univ'Access);
   Builtins.Register_Builtin
     (Strings.String_Lookup ("#sparkel_bool_to_univ"), Bool_To_Univ'Access);

end PSC.Interpreter.Sparkel_Builtins;
