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

--  Package providing support for builtin ParaSail GTK-related operations

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package body GTK_C_Interface is

   pragma Linker_Options ("@gtk_libs");

   type GdkWindowRecord is record
      Object_Type : GType;
   end record;

   type GdkDeviceRecord is record
      Object_Type : GType;
   end record;

   type GdkRectangle is record
      X, Y, Width, Height : gint;
   end record;

   subtype GtkAllocation is GdkRectangle;

   type GtkRequisition is record
      Width, Height : gint;
   end record;

   type GtkWidgetRecord is record
      Object_Type        : GType;
      private_flags      : unsigned_short;
      state, saved_state : unsigned_char;
      name               : gpointer;
      style              : gpointer;
      requisition        : GtkRequisition;
      allocation         : GtkAllocation;
      window             : GdkWindow;
      parent             : GtkWidget;
   end record;

   type gdouble_ptr is access all gdouble;

   type GdkEventRecord (Event_Type : GdkEventType := GDK_NOTHING) is record
      Window     : GdkWindow;
      Send_Event : signed_char;
      Time       : Interfaces.Integer_32;
      case Event_Type is
         when GDK_EVENT_KEY =>
            State  : unsigned;
            Keyval : unsigned;  --  This matches GdkEventKey, hopefully
         when GDK_EVENT_BUTTON =>
            X, Y   : gdouble;
            Axes   : gdouble_ptr;
            Button_State : guint;
            Button       : guint;
         when others =>
            null;
      end case;
   end record;

   type GClosure is record
      Closure_Flags : Interfaces.Unsigned_32;
      Marshal       : System.Address;
      Data          : gpointer;
      Notifiers     : System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, GClosure);

   type GClosureNotify is access procedure
      (Data : gpointer; Closure : GClosure);
   pragma Convention (C, GClosureNotify);

   type GConnectFlags is (G_CONNECT_AFTER, G_CONNECT_SWAPPED);
   for GConnectFlags use (G_CONNECT_AFTER => 1, G_CONNECT_SWAPPED => 2);

   procedure gdk_threads_init;
   pragma Import (C, gdk_threads_init, "gdk_threads_init");

   procedure g_thread_init (vtable : gpointer);  --  Not needed on newer GTKs
   pragma Import (C, g_thread_init, "g_thread_init");

   procedure C_gtk_init (Argc, Argv : System.Address);
   pragma Import (C, C_gtk_init, "gtk_init");

   function C_gtk_main_iteration_do (Blocking : gboolean) return gboolean;
   pragma Import (C, C_gtk_main_iteration_do, "gtk_main_iteration_do");

   procedure C_gtk_widget_destroy
     (Widget : GtkWidget);
   pragma Import (C, C_gtk_widget_destroy, "gtk_widget_destroy");

   procedure gtk_widget_destroy (Widget : GtkWidget; Data : gpointer) is
   begin
      C_gtk_widget_destroy (Widget);
   end gtk_widget_destroy;

   function C_gtk_widget_get_window (Widget : GtkWidget) return GdkWindow;
   pragma Import (C, C_gtk_widget_get_window, "gtk_widget_get_window");

   --  function C_gtk_widget_get_window (Widget : GtkWidget) return GdkWindow
   --  is   --  Not present in old version of GTK
   --  begin
   --     return Widget.window;
   --  end C_gtk_widget_get_window;

   function gtk_widget_get_window (Widget : GtkWidget) return GdkWindow is
   begin
      return C_gtk_widget_get_window (Widget);
   end gtk_widget_get_window;

   procedure C_gtk_widget_get_allocation
      (Widget : GtkWidget; Alloc : out GtkAllocation);
   pragma Import (C, C_gtk_widget_get_allocation,
     "gtk_widget_get_allocation");

   --  procedure C_gtk_widget_get_allocation
   --     (Widget : GtkWidget; Alloc : out GtkAllocation) is
   --  Not present in old version of GTK
   --  begin
   --      Alloc := Widget.allocation;
   --  end C_gtk_widget_get_allocation;

   function gtk_widget_get_allocated_width (Widget : GtkWidget) return int is
      Alloc : GtkAllocation;
   begin
      C_gtk_widget_get_allocation (Widget, Alloc);
      return int (Alloc.Width);
   end gtk_widget_get_allocated_width;

   function gtk_widget_get_allocated_height (Widget : GtkWidget) return int is
      Alloc : GtkAllocation;
   begin
      C_gtk_widget_get_allocation (Widget, Alloc);
      return int (Alloc.Height);
   end gtk_widget_get_allocated_height;

   function C_gtk_button_new_with_label
     (Label : char_array) return GtkWidget;
   pragma Import (C, C_gtk_button_new_with_label,
     "gtk_button_new_with_label");

   function C_g_signal_connect_data (Widget : GtkWidget; Signal : char_array;
     Handler : System.Address; Extra : gpointer;
     Destroy_Data : GClosureNotify; Connect_Flags : GConnectFlags)
   return unsigned_long;
   pragma Import (C, C_g_signal_connect_data, "g_signal_connect_data");

   GTK_Is_Inited : Boolean := False;
   pragma Atomic (GTK_Is_Inited);

   procedure gtk_init_if_needed is
   --  Initialize GTK, unless already done
      use Interfaces.C.Strings;
      Args : chars_ptr_array
        (0 .. size_t (Ada.Command_Line.Argument_Count) + 1) :=
          (others => Null_Ptr);
      Argc : aliased int;
      Argv : aliased System.Address;
   begin
      if not GTK_Is_Inited then
         GTK_Is_Inited := True;

         --  Make a C-compatible array of pointers to arguments
         Args (0) := New_String (Ada.Command_Line.Command_Name);
         for I in 1 .. Ada.Command_Line.Argument_Count loop
            Args (size_t (I)) := New_String (Ada.Command_Line.Argument (I));
         end loop;

         --  Init GDK threading

         g_thread_init (System.Null_Address);  --  Not needed on newer GTKs
         gdk_threads_init;

         --  Init GTK
         Argc := int (Args'Last) - 1;
         Argv := Args'Address;
         C_gtk_init (Argc'Address, Argv'Address);
      end if;
   end gtk_init_if_needed;

   procedure C_g_print (Str : char_array);
   pragma Import (C, C_g_print, "g_print");

   procedure g_print (Str : String) is
   begin
      C_g_print (To_C (Str));
   end g_print;

   function gtk_button_new_with_label (Label : String) return GtkWidget is
      Result : GtkWidget;
   begin
      Result := C_gtk_button_new_with_label (Interfaces.C.To_C (Label));
      return Result;
   end gtk_button_new_with_label;

   procedure g_signal_connect (Widget : GtkWidget; Signal : String;
     Handler : Callback_1; Extra : gpointer) is
      Ignore : unsigned_long;
   begin
      Ignore := C_g_signal_connect_data (Widget, To_C (Signal),
        Handler.all'Address, Extra, null, G_CONNECT_AFTER);
   end g_signal_connect;

   procedure g_signal_connect (Widget : GtkWidget; Signal : String;
     Handler : Callback_2; Extra : gpointer) is
      Ignore : unsigned_long;
      function Handler_Func
        (Widget : GtkWidget;
         Event : GdkEvent;
         Data : gpointer)
         return Boolean renames Handler.all; --  TBD: Workaround GNAT bug
   begin
      Ignore := C_g_signal_connect_data (Widget, To_C (Signal),
        Handler_Func'Address, Extra, null, G_CONNECT_AFTER);
   end g_signal_connect;

   procedure g_signal_connect_swapped (Widget : GtkWidget; Signal : String;
     Handler : Callback_1; First_Param : gpointer) is
      Ignore : unsigned_long;
   begin
      Ignore := C_g_signal_connect_data (Widget, To_C (Signal),
        Handler.all'Address, First_Param, null, G_CONNECT_SWAPPED);
   end g_signal_connect_swapped;

   --  GQuark operations

   function C_g_quark_from_string (gchar : Interfaces.C.Strings.chars_ptr)
     return GQuark;
   pragma Import (C, C_g_quark_from_string, "g_quark_from_string");

   function g_quark_from_string (gchar : String) return GQuark is
      use Interfaces.C.Strings;
   begin
      return C_g_quark_from_string (New_String (gchar));
   end g_quark_from_string;

   function C_g_quark_to_string (quark : GQuark)
     return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, C_g_quark_to_string, "g_quark_to_string");

   function g_quark_to_string (quark : GQuark) return String is
      use Interfaces.C.Strings;
   begin
      return Value (C_g_quark_to_string (quark));
   end g_quark_to_string;

   --  GType operations  --
   function C_g_type_from_name (name : Interfaces.C.Strings.chars_ptr)
     return GType;
   pragma Import (C, C_g_type_from_name, "g_type_from_name");

   function g_type_from_name (name : String) return GType is
   begin
      return C_g_type_from_name (Interfaces.C.Strings.New_String (name));
   end g_type_from_name;

   function C_g_type_name (itype : GType)
     return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, C_g_type_name, "g_type_name");

   function g_type_name (itype : GType) return String is
   begin
      return Interfaces.C.Strings.Value (C_g_type_name (itype));
   end g_type_name;

   function gpointer_To_GtkWidget is new Ada.Unchecked_Conversion
     (gpointer, GtkWidget);

   function GValue_To_GtkWidget (Val : GValue) return GtkWidget is
   --  Extract GtkWidget from a GValue
   begin
      return gpointer_To_GtkWidget (Val.data (1).v_pointer);
   end GValue_To_GtkWidget;

   --  Signal_Id parser  --

   function C_g_signal_parse_name
     (detailed_signal : Interfaces.C.Strings.chars_ptr;
      itype : GType;
      signal_id : gpointer;
      detail : gpointer;
      force_detail_quark : gboolean) return gboolean;
   pragma Import (C, C_g_signal_parse_name, "g_signal_parse_name");

   function g_signal_parse_name
     (detailed_signal : String;
      itype : GType;
      signal_id : access guint;
      detail : access GQuark;
      force_detail_quark : Boolean) return Boolean is
   --  Parse signal into signal_id and detail
      Result : constant gboolean := C_g_signal_parse_name
        (Interfaces.C.Strings.New_String (detailed_signal), itype,
         signal_id.all'Address, detail.all'Address,
         Boolean'Pos (force_detail_quark));
   begin
      return Boolean'Val (Result);
   end g_signal_parse_name;

   function gdk_event_get_event_type (Event : GdkEvent) return GdkEventType is
   begin
      return Event.Event_Type;
   end gdk_event_get_event_type;

   function gdk_event_get_coords (Event : GdkEvent;
     x_win : access gdouble; y_win : access gdouble) return gboolean;
   pragma Import (C, gdk_event_get_coords, "gdk_event_get_coords");

   function gdk_event_get_x (Event : GdkEvent) return gdouble is
      x_win, y_win : aliased gdouble;
   begin
      if gdk_event_get_coords (Event, x_win'Access, y_win'Access) /= 0 then
         return x_win;
      else
         return 0.0;
      end if;
   end gdk_event_get_x;

   function gdk_event_get_y (Event : GdkEvent) return gdouble is
      x_win, y_win : aliased gdouble;
   begin
      if gdk_event_get_coords (Event, x_win'Access, y_win'Access) /= 0 then
         return y_win;
      else
         return 0.0;
      end if;
   end gdk_event_get_y;

   function gdk_event_get_keyval (Event : GdkEvent) return guint is
   begin
      if Event.Event_Type in GDK_EVENT_KEY then
         return Event.Keyval;
      else
         return 0;
      end if;
   end gdk_event_get_keyval;

   function gdk_event_get_button (Event : GdkEvent) return guint is
   begin
      if Event.Event_Type in GDK_EVENT_BUTTON then
         return Event.Button;
      else
         return 0;
      end if;
   end gdk_event_get_button;

end GTK_C_Interface;
