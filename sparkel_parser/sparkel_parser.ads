------------------------------------------------------------------------------
--                              S P A R K E L                               --
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

with Sparkel_Tokens;
package Sparkel_Parser is

   procedure Yyparse;

   procedure Yyerror (S : String := "syntax error";
     At_Token : Sparkel_Tokens.YYSType := (Sparkel_Tokens.Optional,
       Is_Present => False));

   procedure Parser_Warning (S : String;
     At_Token : Sparkel_Tokens.YYSType := (Sparkel_Tokens.Optional,
       Is_Present => False));

end Sparkel_Parser;
