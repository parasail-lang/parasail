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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with System;

package body Cairo_C_Interface is

   procedure C_pango_layout_set_text
     (Layout : PangoLayout; Text : chars_ptr; Length : GTK_C_Interface.gint);
   pragma Import (C, C_pango_layout_set_text, "pango_layout_set_text");

   function To_chars_ptr is new
     Ada.Unchecked_Conversion (System.Address, chars_ptr);

   procedure pango_layout_set_text (Layout : PangoLayout; Text : String) is
   begin
      C_pango_layout_set_text
        (Layout, To_chars_ptr (Text'Address), Text'Length);
   end pango_layout_set_text;

end Cairo_C_Interface;
