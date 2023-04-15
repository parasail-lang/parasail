------------------------------------------------------------------------------
--                               S P A R K E L                              --
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
------------------------------------------------------------------------------

with Sparkel_Parser, Sparkel_Lex_IO, Sparkel_Lex, Sparkel_Tokens,
  Text_IO;
use  Sparkel_Parser, Text_IO;
with PSC.Languages;
with PSC.Messages;
with PSC.Strings;
with PSC.Syntax;
with PSC.Trees.Semantics;
with PSC.Interpreter;
with PSC.Interpreter.Sparkel_Builtins;  --  Pull in Sparkel builtins
with PSC.Interpreter.IO;  --  Pull in I/O builtins
with PSC.Trees.Semantics.Translator;  --  So will have translator primitives

with RL_C_Interface;  --  Pull in readline interface

with Sparkel_Lex_DFA;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;

procedure Sparkel_Main is
--  Main procedure for Sparkel.
   Total_Errors  : Natural := 0;
   Command_Given : Boolean := False;

begin  --  Sparkel_Main

   PSC.Trees.Semantics.Set_Language (PSC.Languages.Sparkel);

   PSC.Syntax.Parse_All (Total_Errors, Command_Given,
     (1 =>
       (PSC.Languages.Sparkel,
        Sparkel_Lex.Init'Access,
        Sparkel_Lex_IO.Open_Input'Access,
        Sparkel_Lex_IO.Close_Input'Access,
        Sparkel_Parser.YYParse'Access,
        Sparkel_Tokens.Syntax_Error'Identity)));

   PSC.Trees.Semantics.Analyze_And_Interpret_All (Total_Errors, Command_Given);

end Sparkel_Main;
