------------------------------------------------------------------------------
--                               A D A 2 0 2 X                              --
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

with Ada202x_Parser, Ada202x_Lex_IO, Ada202x_Lex,
  Text_IO;
use  Ada202x_Parser, Text_IO;
with Ada202x_Lex_DFA;

with ParaSail_Parser, ParaSail_Lex_IO, ParaSail_Lex;
with ParaSail_Lex_DFA;

with PSC.Languages;
with PSC.Messages;
with PSC.Strings;
with PSC.Syntax;
with PSC.Trees.Semantics;
with PSC.Interpreter;
with PSC.Interpreter.Ada202x_Builtins;  --  Pull in Ada202x builtins
with PSC.Interpreter.C_Builtins;        --  Pull in builtins based on C library
with PSC.Interpreter.Math;
with PSC.Interpreter.IO;  --  Pull in I/O builtins
with PSC.Trees.Semantics.Translator;  --  So will have translator primitives

with RL_C_Interface;  --  Pull in readline interface

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;

procedure Ada202x_Main is
--  Main procedure for Ada202x.
   Total_Errors  : Natural := 0;
   Command_Given : Boolean := False;

begin  --  Ada202x_Main

   PSC.Trees.Semantics.Set_Language (PSC.Languages.Ada202x);

   PSC.Syntax.Parse_All (Total_Errors, Command_Given,
     Parsers =>
       ((PSC.Languages.Ada202x,
         Ada202x_Lex.Init'Access,
         Ada202x_Lex_IO.Open_Input'Access,
         Ada202x_Lex_IO.Close_Input'Access,
         Ada202x_Parser.YYParse'Access),
        (PSC.Languages.ParaSail,
         ParaSail_Lex.Init'Access,
         ParaSail_Lex_IO.Open_Input'Access,
         ParaSail_Lex_IO.Close_Input'Access,
         ParaSail_Parser.YYParse'Access)));

   PSC.Trees.Semantics.Analyze_And_Interpret_All (Total_Errors, Command_Given);

end Ada202x_Main;
