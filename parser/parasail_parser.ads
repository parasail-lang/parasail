------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
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

with ParaSail_Tokens;
package ParaSail_Parser is

   procedure Yyparse;

   procedure Yyerror (S : String := "syntax error";
     At_Token : ParaSail_Tokens.YYSType := (ParaSail_Tokens.Optional,
       Is_Present => False));

   procedure Parser_Warning (S : String;
     At_Token : ParaSail_Tokens.YYSType := (ParaSail_Tokens.Optional,
       Is_Present => False));

end ParaSail_Parser;
