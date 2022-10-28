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

with PSC.Interpreter.Builtins;         use PSC.Interpreter.Builtins;
with PSC.Interpreter.Param_Signatures; use PSC.Interpreter.Param_Signatures;
with PSC.Interpreter.GTK;
with PSC.Strings;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with GTK_C_Interface;
with Cairo_C_Interface;
with System.Storage_Elements;
with Ada.Unchecked_Conversion;
pragma Elaborate (PSC.Interpreter.Builtins);
pragma Elaborate (PSC.Interpreter.Param_Signatures);
pragma Elaborate (PSC.Strings);
pragma Elaborate (Ada.Text_IO);

package body PSC.Interpreter.Cairo is
   --  Package providing support for builtin ParaSail Cairo-related operations

   use GTK_C_Interface;
   use Cairo_C_Interface;

   function To_Widget is new Ada.Unchecked_Conversion (gpointer, GTKWidget);
   function To_gpointer is new Ada.Unchecked_Conversion (GTKWidget, gpointer);

   function To_GTKWidget is new Ada.Unchecked_Conversion
     (System.Storage_Elements.Integer_Address, GTKWidget);

   function From_GTKWidget is new Ada.Unchecked_Conversion
     (GTKWidget, System.Storage_Elements.Integer_Address);

   function To_CairoContext is new Ada.Unchecked_Conversion
     (Word_Type, Cairo_C_Interface.CairoContext);

   function From_CairoContext is new Ada.Unchecked_Conversion
     (Cairo_C_Interface.CairoContext, Word_Type);

   --   func Create(var GTK::Widget+) -> Context
   --      is import(#cairo_context_create)
   function Cairo_Context_Create (Poly_Widget : Word_Type) return Word_Type is
      --  NOTE: Polymorphic types are not passed by reference
      Drawable_Widget : constant GTK_C_Interface.GtkWidget :=
        Interpreter.GTK.Poly_To_GtkWidget (Poly_Widget);
      Drawable_Window : constant GTK_C_Interface.GdkWindow :=
        GTK_C_Interface.gtk_widget_get_window (Drawable_Widget);
   begin
      return From_CairoContext
        (Cairo_C_Interface.gdk_cairo_create (Drawable_Window));
   end Cairo_Context_Create;

   package Cairo_Context_Create_Op is new Unary_Builtin
     (Name => "#cairo_context_create",
      Operand => Word_Param,
      Result => Word_Param,
      Op => Cairo_Context_Create,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Arc(var Context; XC, YC, Radius, Angle1, Angle2 : Float)
   --      is import(#cairo_arc)//*
   procedure Arc
     (Context : Word_Ptr; XC, YC, Radius, Angle1, Angle2 : Univ_Real) is
   begin
      Cairo_C_Interface.cairo_arc
        (To_CairoContext (Fetch_Word (Context, 0)), double (XC), double (YC),
         double (Radius), double (Angle1), double (Angle2));
   end Arc;

   package Arc_Op is new Six_Input_Builtin
     (Name => "#cairo_arc",
      First => Word_Ptr_Param, Second => Real_Param,
      Third => Real_Param, Fourth => Real_Param,
      Fifth => Real_Param, Sixth => Real_Param,
      Op => Arc,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Arc_Negative(var Context; XC, YC, Radius, Angle1, Angle2 : Float)
   --      is import(#cairo_arc_negative)//*
   procedure Arc_Negative
     (Context : Word_Ptr; XC, YC, Radius, Angle1, Angle2 : Univ_Real) is
   begin
      Cairo_C_Interface.cairo_arc_negative
        (To_CairoContext (Fetch_Word (Context, 0)), double (XC), double (YC),
         double (Radius), double (Angle1), double (Angle2));
   end Arc_Negative;

   package Arc_Negative_Op is new Six_Input_Builtin
     (Name => "#cairo_arc_negative",
      First => Word_Ptr_Param, Second => Real_Param,
      Third => Real_Param, Fourth => Real_Param,
      Fifth => Real_Param, Sixth => Real_Param,
      Op => Arc_Negative,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Close_Path(var Context)
   --      is import(#cairo_close_path)//*
   procedure Close_Path (Context : Word_Ptr) is
   begin
      cairo_close_path (To_CairoContext (Fetch_Word (Context, 0)));
   end Close_Path;

   package Close_Path_Op is new One_Input_Builtin
     (Name => "#cairo_close_path",
      Operand => Word_Ptr_Param,
      Op => Close_Path,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Fill(var Context)
   --      is import(#cairo_fill)//*
   procedure Fill (Context : Word_Ptr) is
   begin
      cairo_Fill (To_CairoContext (Fetch_Word (Context, 0)));
   end Fill;

   package Fill_Op is new One_Input_Builtin
     (Name => "#cairo_fill",
      Operand => Word_Ptr_Param,
      Op => Fill,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Fill_Preserve(var Context)
   --      is import(#cairo_fill_preserve)//*
   procedure Fill_Preserve (Context : Word_Ptr) is
   begin
      cairo_fill_preserve (To_CairoContext (Fetch_Word (Context, 0)));
   end Fill_Preserve;

   package Fill_Preserve_Op is new One_Input_Builtin
     (Name => "#cairo_fill_preserve",
      Operand => Word_Ptr_Param,
      Op => Fill_Preserve,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Line_To(var Context; X, Y : Float)
   --      is import(#cairo_line_to)//#
   procedure Line_To (Context : Word_Ptr; X, Y : Univ_Real) is
   begin
      cairo_line_to
        (To_CairoContext (Fetch_Word (Context, 0)), double (X), double (Y));
   end Line_To;

   package Line_To_Op is new Three_Input_Builtin
     (Name => "#cairo_line_to",
      First => Word_Ptr_Param,
      Second => Real_Param,
      Third => Real_Param,
      Op => Line_To,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Move_To(var Context; X, Y : Float)
   --      is import(#cairo_move_to)//#
   procedure Move_To (Context : Word_Ptr; X, Y : Univ_Real) is
   begin
      cairo_move_to
        (To_CairoContext (Fetch_Word (Context, 0)), double (X), double (Y));
   end Move_To;

   package Move_To_Op is new Three_Input_Builtin
     (Name => "#cairo_move_to",
      First => Word_Ptr_Param,
      Second => Real_Param,
      Third => Real_Param,
      Op => Move_To,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Rectangle(var Context; X, Y, Width, Height : Float)
   --      is import(#cairo_rectangle)//*
   procedure Rectangle (Context : Word_Ptr; X, Y, Width, Height : Univ_Real)
   is
   begin
      cairo_rectangle (To_CairoContext (Fetch_Word (Context, 0)),
        double (X), double (Y), double (Width), double (Height));
   end Rectangle;

   package Rectangle_Op is new Five_Input_Builtin
     (Name => "#cairo_rectangle",
      First => Word_Ptr_Param, Second => Real_Param,
      Third => Real_Param, Fourth => Real_Param,
      Fifth => Real_Param,
      Op => Rectangle,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Set_Dash(var Context; Basic_Array<Float>)
   --      is import(#cairo_set_dash)
   procedure Set_Dash (Context : Word_Ptr; Dashes : Word_Type;
     Offset : Univ_Real) is
   begin
      cairo_set_dash (To_CairoContext (Fetch_Word (Context, 0)),
        Dashes => Virtual_To_Physical_Address
          (Dashes + (Large_Obj_Header_Size + 1)).all'Address,
        Num_Dashes => int (Fetch_Word (Dashes + Large_Obj_Header_Size)),
        Offset => gdouble (Offset));
   end Set_Dash;

   package Set_Dash_Op is new Three_Input_Builtin
     (Name => "#cairo_set_dash",
      First => Word_Ptr_Param, Second => Word_Param, Third => Real_Param,
      Op => Set_Dash,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Set_Source_RGB(var Context; Red, Green, Blue : Float)
   --      is import(#cairo_set_source_rgb)//*
   procedure Set_Source_RGB (Context : Word_Ptr; Red, Green, Blue : Univ_Real)
   is
   begin
      cairo_set_source_rgb (To_CairoContext (Fetch_Word (Context, 0)),
        double (Red), double (Green), double (Blue));
   end Set_Source_RGB;

   package Set_Source_RGB_Op is new Four_Input_Builtin
     (Name => "#cairo_set_source_rgb",
      First => Word_Ptr_Param, Second => Real_Param,
      Third => Real_Param, Fourth => Real_Param,
      Op => Set_Source_RGB,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Set_Source_RGB_Alpha(var Context; Red, Green, Blue, Alpha : Float)
   --      is import(#cairo_set_source_rgb_alpha)//*
   procedure Set_Source_RGB_Alpha
     (Context : Word_Ptr; Red, Green, Blue, Alpha : Univ_Real) is
   begin
      cairo_set_source_rgba (To_CairoContext (Fetch_Word (Context, 0)),
        double (Red), double (Green), double (Blue), double (Alpha));
   end Set_Source_RGB_Alpha;

   package Set_Source_RGB_Alpha_Op is new Five_Input_Builtin
     (Name => "#cairo_set_source_rgb_alpha",
      First => Word_Ptr_Param, Second => Real_Param,
      Third => Real_Param, Fourth => Real_Param,
      Fifth => Real_Param,
      Op => Set_Source_RGB_Alpha,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Stroke(var Context)
   --      is import(#cairo_stroke)//#
   procedure Stroke (Context : Word_Ptr) is
   begin
      cairo_stroke (To_CairoContext (Fetch_Word (Context, 0)));
   end Stroke;

   package Stroke_Op is new One_Input_Builtin
     (Name => "#cairo_stroke",
      Operand => Word_Ptr_Param,
      Op => Stroke,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Stroke_Preserve(var Context)
   --      is import(#cairo_stroke_preserve)//*
   procedure Stroke_Preserve (Context : Word_Ptr) is
   begin
      cairo_stroke_preserve (To_CairoContext (Fetch_Word (Context, 0)));
   end Stroke_Preserve;

   package Stroke_Preserve_Op is new One_Input_Builtin
     (Name => "#cairo_stroke_preserve",
      Operand => Word_Ptr_Param,
      Op => Stroke_Preserve,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   function To_PangoLayout is new Ada.Unchecked_Conversion
     (Word_Type, Cairo_C_Interface.PangoLayout);

   function PangoLayout_To_Word is new Ada.Unchecked_Conversion
     (Cairo_C_Interface.PangoLayout, Word_Type);

   function Pango_Layout_New (Context : Word_Ptr) return Word_Type is
   --   func New(var Context) -> Layout
      CCtxt : constant CairoContext :=
        To_CairoContext (Fetch_Word (Context, 0));
   begin
      return PangoLayout_To_Word
        (Cairo_C_Interface.pango_cairo_create_layout (CCtxt));
   end Pango_Layout_New;

   package Pango_Layout_New_Op is new Unary_Builtin
     (Name => "#pango_layout_new",
      Operand => Word_Ptr_Param,
      Result => Word_Param,
      Op => Pango_Layout_New,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --   func Show_Layout(var Context; Pango::Layout)
   --      is import(#cairo_show_layout)//#
   procedure Show_Layout (Context : Word_Ptr; Layout : Word_Type) is
   begin
      pango_cairo_show_layout (To_CairoContext (Fetch_Word (Context, 0)),
        To_PangoLayout (Layout));
   end Show_Layout;

   package Show_Layout_Op is new Two_Input_Builtin
     (Name => "#cairo_show_layout",
      First => Word_Ptr_Param, Second => Word_Param,
      Op => Show_Layout,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Pango_Layout_Set_Text (Layout : Word_Ptr; Text : Word_Type) is
   --   func Set_Text(var Layout; Univ_String)
   begin
      pango_layout_set_text (To_PangoLayout (Fetch_Word (Layout, 0)),
        Word_To_String (Text));
   end Pango_Layout_Set_Text;

   package Pango_Layout_Set_Text_Op is new Two_Input_Builtin
     (Name => "#pango_layout_set_text",
      First => Word_Ptr_Param,
      Second => Word_Param,
      Op => Pango_Layout_Set_Text,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   function Pango_Layout_Get_Pixel_Height (Layout : Word_Type)
     return Word_Type is
   --  func Get_Pixel_Height(Layout) -> Univ_Integer
      Pixel_Height : aliased int;
   begin
      pango_layout_get_pixel_size (To_PangoLayout (Layout),
        width => null,
        height => Pixel_Height'Unchecked_Access);
      return Word_Type (Pixel_Height);
   end Pango_Layout_Get_Pixel_Height;

   package Pango_Layout_Get_Pixel_Height_Op is new Unary_Builtin
     (Name => "#pango_layout_get_pixel_height",
      Operand => Word_Param,
      Result => Word_Param,
      Op => Pango_Layout_Get_Pixel_Height,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   function Pango_Layout_Get_Pixel_Width (Layout : Word_Type)
     return Word_Type is
   --  func Get_Pixel_Width(Layout) -> Univ_Integer
      Pixel_Width : aliased int;
   begin
      pango_layout_get_pixel_size (To_PangoLayout (Layout),
        width => Pixel_Width'Unchecked_Access,
        height => null);
      return Word_Type (Pixel_Width);
   end Pango_Layout_Get_Pixel_Width;

   package Pango_Layout_Get_Pixel_Width_Op is new Unary_Builtin
     (Name => "#pango_layout_get_pixel_width",
      Operand => Word_Param,
      Result => Word_Param,
      Op => Pango_Layout_Get_Pixel_Width,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

end PSC.Interpreter.Cairo;
