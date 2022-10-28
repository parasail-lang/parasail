------------------------------------------------------------------------------
--                              A D A 2 0 2 X                               --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with Ada202x_Tokens;
package Ada202x_Parser is

   procedure Yyparse;

   procedure Yyerror (S : String := "syntax error";
     At_Token : Ada202x_Tokens.YYSType := (Ada202x_Tokens.Optional,
       Is_Present => False));

   procedure Parser_Warning (S : String;
     At_Token : Ada202x_Tokens.YYSType := (Ada202x_Tokens.Optional,
       Is_Present => False));

end Ada202x_Parser;
