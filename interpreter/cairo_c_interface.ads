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

with Interfaces.C; use Interfaces.C;
with GTK_C_Interface;
with System;

package Cairo_C_Interface is

   type CairoContext is private;

   function gdk_cairo_create
     (drawable : GTK_C_Interface.GdkWindow) return CairoContext;
   pragma Import (C, gdk_cairo_create, "gdk_cairo_create");

   procedure cairo_arc
     (Cr : CairoContext; Xc, Yc, Radius, Angle1, Angle2 : double);
   pragma Import (C, cairo_arc, "cairo_arc");

   procedure cairo_arc_negative
     (Cr : CairoContext; Xc, Yc, Radius, Angle1, Angle2 : double);
   pragma Import (C, cairo_arc_negative, "cairo_arc_negative");

   procedure cairo_close_path (Cr : CairoContext);
   pragma Import (C, cairo_close_path, "cairo_close_path");

   procedure cairo_fill (Cr : CairoContext);
   pragma Import (C, cairo_fill, "cairo_fill");

   procedure cairo_fill_preserve (Cr : CairoContext);
   pragma Import (C, cairo_fill_preserve, "cairo_fill_preserve");

   procedure cairo_line_to (Cr : CairoContext; X : double; Y : double);
   pragma Import (C, cairo_line_to, "cairo_line_to");

   procedure cairo_move_to (Cr : CairoContext; X : double; Y : double);
   pragma Import (C, cairo_move_to, "cairo_move_to");

   procedure cairo_rectangle
       (Cr : CairoContext; X, Y, Width, Height : double);
   pragma Import (C, cairo_rectangle, "cairo_rectangle");

   procedure cairo_set_source_rgb
       (Cr : CairoContext; Red, Green, Blue : double);
   pragma Import (C, cairo_set_source_rgb, "cairo_set_source_rgb");

   procedure cairo_set_source_rgba
       (Cr : CairoContext; Red, Green, Blue, Alpha : double);
   pragma Import (C, cairo_set_source_rgba, "cairo_set_source_rgba");

   procedure cairo_stroke (Cr : CairoContext);
   pragma Import (C, cairo_stroke, "cairo_stroke");

   procedure cairo_stroke_preserve (Cr : CairoContext);
   pragma Import (C, cairo_stroke_preserve, "cairo_stroke_preserve");

   procedure cairo_set_dash (Cr : CairoContext;
     Dashes : System.Address; Num_Dashes : GTK_C_Interface.gint;
     Offset : double);
   pragma Import (C, cairo_set_dash, "cairo_set_dash");
   --  Dashes: an array of positive values, length of alternate on/off parts
   --  Num_Dashes: length of Dashes array
   --  Offset: an offset into dash pattern at which the stroke should start

   type PangoLayout is private;

   function pango_cairo_create_layout (Cr : CairoContext) return PangoLayout;
   pragma Import (C, pango_cairo_create_layout, "pango_cairo_create_layout");

   procedure pango_cairo_show_layout (Cr : CairoContext; Layout : PangoLayout);
   pragma Import (C, pango_cairo_show_layout, "pango_cairo_show_layout");

   procedure pango_layout_set_text (Layout : PangoLayout; Text : String);

   type int_ptr is access all int;
   procedure pango_layout_get_pixel_size (Layout : PangoLayout;
     width : int_ptr; height : int_ptr);
   pragma Import
     (C, pango_layout_get_pixel_size, "pango_layout_get_pixel_size");

private
   type CairoContext is access all unsigned_long;
   type PangoLayout is access all unsigned_long;

   pragma No_Strict_Aliasing (CairoContext);
   pragma No_Strict_Aliasing (PangoLayout);

end Cairo_C_Interface;
