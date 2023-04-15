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

with ParaSail_Parser, ParaSail_Lex_IO, ParaSail_Lex, ParaSail_Tokens;
with PSC.Languages;
with PSC.Syntax;
with PSC.Trees.Semantics;

pragma Warnings (Off);
with PSC.Interpreter.IO;  --  Pull in I/O builtins
with PSC.Interpreter.Math;  --  Pull in Math builtins
with PSC.Trees.Semantics.Translator;  --  So will have translator primitives
with RL_C_Interface;  --  Pull in readline/add_history interface
pragma Warnings (On);

procedure ParaSail_Main is
--  Main procedure for ParaSail.
--  See ParaSail_Main_GTK for version that pulls in builtins for GTK graphics.
   Total_Errors : Natural := 0;
   Command_Given : Boolean := False;

begin  --  Parasail_Main

   PSC.Trees.Semantics.Set_Language (PSC.Languages.ParaSail);

   PSC.Syntax.Parse_All (Total_Errors, Command_Given,
     (1 =>
       (PSC.Languages.ParaSail,
        ParaSail_Lex.Init'Access,
        ParaSail_Lex_IO.Open_Input'Access,
        ParaSail_Lex_IO.Close_Input'Access,
        ParaSail_Parser.YYParse'Access,
        ParaSail_Tokens.Syntax_Error'Identity)));

   PSC.Trees.Semantics.Analyze_And_Interpret_All (Total_Errors, Command_Given);

end ParaSail_Main;
