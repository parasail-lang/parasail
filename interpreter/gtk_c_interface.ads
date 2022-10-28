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

with Interfaces.C; use Interfaces.C;
with System;

package GTK_C_Interface is
   --  These declarations are based on www.gtk.org/api/...

   subtype gpointer is System.Address;

   subtype gboolean is int;
   gFalse : constant gboolean := 0;
   gTrue  : constant gboolean := 1;

   subtype gint    is int;
   subtype guint   is unsigned;
   subtype guint32 is Interfaces.Unsigned_32;
   subtype glong   is long;
   subtype gulong  is unsigned_long;
   subtype gint64  is Interfaces.Integer_64;
   subtype guint64 is Interfaces.Unsigned_64;
   subtype gfloat  is Float;
   subtype gdouble is double;

   --  GValue and GType operations
   subtype GType is gulong;

   function g_type_from_name (name : String) return GType;
   function g_type_name (itype : GType) return String;

   type data_kind is
     (v_int, v_uint, v_long, v_ulong, v_int64, v_uint64,
      v_float, v_double, v_pointer);

   type GValue_union (D : data_kind := v_int) is record
      case D is
         when v_int     => v_int     : gint;
         when v_uint    => v_uint    : guint;
         when v_long    => v_long    : glong;
         when v_ulong   => v_ulong   : gulong;
         when v_int64   => v_int64   : gint64;
         when v_uint64  => v_uint64  : guint64;
         when v_float   => v_float   : gfloat;
         when v_double  => v_double  : gdouble;
         when v_pointer => v_pointer : gpointer;
      end case;
   end record;
   pragma Unchecked_Union (GValue_union);

   type GValue_data_array is array (Positive range <>) of GValue_union;

   type GValue is record
      g_type : GType;
      data   : GValue_data_array (1 .. 2);
   end record;

   Max_GValues : constant := 100;

   type GValue_Array is array (1 .. Max_GValues) of GValue;
   --  A constrained array so it can be pointed-to with a simple address
   --  It might *not* actually have all of these elements.

   type GValue_ptr is access all GValue_Array;
   --  Ptr[1] is equivalent to ".all" if this had been a simple "access GValue"
   pragma No_Strict_Aliasing (GValue_Ptr);

   --  GQuark operations
   subtype GQuark is guint32;

   function g_quark_from_string (gchar : String) return GQuark;

   function g_quark_from_static_string (gchar : gpointer) return GQuark;
   pragma Import (C, g_quark_from_static_string, "g_quark_from_static_string");

   function g_quark_to_string (quark : GQuark) return String;

   type GtkWidget is private;

   function GValue_To_GtkWidget (Val : GValue) return GtkWidget;
   --  Extract GtkWidget from a GValue

   procedure gtk_init_if_needed;

   procedure gtk_main;
   pragma Import (C, gtk_main, "gtk_main");

   procedure gtk_main_quit;
   pragma Import (C, gtk_main_quit, "gtk_main_quit");

   procedure g_print (Str : String);

   type GdkEventType is
     (GDK_NOTHING,           --  -1
      GDK_DELETE,            --  0
      GDK_DESTROY,           --  1
      GDK_EXPOSE,            --  2
      GDK_MOTION_NOTIFY,     --  3
      GDK_BUTTON_PRESS,      --  4
      GDK_2BUTTON_PRESS,     --  5
      GDK_3BUTTON_PRESS,     --  6
      GDK_BUTTON_RELEASE,    --  7
      GDK_KEY_PRESS,         --  8
      GDK_KEY_RELEASE,       --  9
      GDK_ENTER_NOTIFY,      --  10
      GDK_LEAVE_NOTIFY,      --  11
      GDK_FOCUS_CHANGE,      --  12
      GDK_CONFIGURE,         --  13
      GDK_MAP,               --  14
      GDK_UNMAP,             --  15
      GDK_PROPERTY_NOTIFY,   --  16
      GDK_SELECTION_CLEAR,   --  17
      GDK_SELECTION_REQUEST, --  18
      GDK_SELECTION_NOTIFY,  --  19
      GDK_PROXIMITY_IN,      --  20
      GDK_PROXIMITY_OUT,     --  21
      GDK_DRAG_ENTER,        --  22
      GDK_DRAG_LEAVE,        --  23
      GDK_DRAG_MOTION,       --  24
      GDK_DRAG_STATUS,       --  25
      GDK_DROP_START,        --  26
      GDK_DROP_FINISHED,     --  27
      GDK_CLIENT_EVENT,      --  28
      GDK_VISIBILITY_NOTIFY, --  29
      GDK_NO_EXPOSE,         --  30
      GDK_SCROLL,            --  31
      GDK_WINDOW_STATE,      --  32
      GDK_SETTING,           --  33
      GDK_OWNER_CHANGE,      --  34
      GDK_GRAB_BROKEN,       --  35
      GDK_DAMAGE,            --  36
      GDK_EVENT_LAST);       --  helper variable for decls

   for GdkEventType use
     (GDK_NOTHING           => -1,
      GDK_DELETE            => 0,
      GDK_DESTROY           => 1,
      GDK_EXPOSE            => 2,
      GDK_MOTION_NOTIFY     => 3,
      GDK_BUTTON_PRESS      => 4,
      GDK_2BUTTON_PRESS     => 5,
      GDK_3BUTTON_PRESS     => 6,
      GDK_BUTTON_RELEASE    => 7,
      GDK_KEY_PRESS         => 8,
      GDK_KEY_RELEASE       => 9,
      GDK_ENTER_NOTIFY      => 10,
      GDK_LEAVE_NOTIFY      => 11,
      GDK_FOCUS_CHANGE      => 12,
      GDK_CONFIGURE         => 13,
      GDK_MAP               => 14,
      GDK_UNMAP             => 15,
      GDK_PROPERTY_NOTIFY   => 16,
      GDK_SELECTION_CLEAR   => 17,
      GDK_SELECTION_REQUEST => 18,
      GDK_SELECTION_NOTIFY  => 19,
      GDK_PROXIMITY_IN      => 20,
      GDK_PROXIMITY_OUT     => 21,
      GDK_DRAG_ENTER        => 22,
      GDK_DRAG_LEAVE        => 23,
      GDK_DRAG_MOTION       => 24,
      GDK_DRAG_STATUS       => 25,
      GDK_DROP_START        => 26,
      GDK_DROP_FINISHED     => 27,
      GDK_CLIENT_EVENT      => 28,
      GDK_VISIBILITY_NOTIFY => 29,
      GDK_NO_EXPOSE         => 30,
      GDK_SCROLL            => 31,
      GDK_WINDOW_STATE      => 32,
      GDK_SETTING           => 33,
      GDK_OWNER_CHANGE      => 34,
      GDK_GRAB_BROKEN       => 35,
      GDK_DAMAGE            => 36,
      GDK_EVENT_LAST        => 37);

   subtype GDK_EVENT_KEY is GdkEventType
     range GDK_KEY_PRESS .. GDK_KEY_RELEASE;

   subtype GDK_EVENT_BUTTON is GdkEventType
     range GDK_BUTTON_PRESS .. GDK_BUTTON_RELEASE;

   type GdkWindow is private;
   type GdkDevice is private;

   type GdkEvent is private;

   function gdk_event_get_event_type (Event : GdkEvent) return GdkEventType;
   function gdk_event_get_x (Event : GdkEvent) return gdouble;
   function gdk_event_get_y (Event : GdkEvent) return gdouble;
   function gdk_event_get_keyval (Event : GdkEvent) return guint;
   function gdk_event_get_button (Event : GdkEvent) return guint;
   function gdk_event_get_state (Event : GdkEvent) return gint;
   pragma Import (C, gdk_event_get_state, "gdk_event_get_state");

   procedure gtk_widget_show (Widget : GtkWidget);
   pragma Import (C, gtk_widget_show, "gtk_widget_show");

   procedure gtk_widget_show_all (Widget : GtkWidget);
   pragma Import (C, gtk_widget_show_all, "gtk_widget_show_all");

   procedure gtk_widget_queue_draw (Widget : GtkWidget);
   pragma Import (C, gtk_widget_queue_draw, "gtk_widget_queue_draw");

   procedure gtk_widget_set_size_request
     (Widget : GtkWidget; Width, Height : gint);
   pragma Import
     (C, gtk_widget_set_size_request, "gtk_widget_set_size_request");

   function gtk_widget_get_allocated_width (Widget : GtkWidget) return int;
   function gtk_widget_get_allocated_height (Widget : GtkWidget) return int;

   procedure gtk_widget_add_events (Widget : GtkWidget; Mask : gint);
   pragma Import (C, gtk_widget_add_events, "gtk_widget_add_events");

   procedure gtk_widget_set_events (Widget : GtkWidget; Mask : gint);
   pragma Import (C, gtk_widget_set_events, "gtk_widget_set_events");

   function gtk_widget_get_window (Widget : GtkWidget) return GdkWindow;

   procedure gtk_container_set_border_width
     (Window : GtkWidget; Width : Natural);
   pragma Import (C, gtk_container_set_border_width,
     "gtk_container_set_border_width");

   procedure gtk_container_add (Container : GtkWidget; Element : GtkWidget);
   pragma Import (C, gtk_container_add, "gtk_container_add");

   function gtk_table_new (Rows, Columns : guint; Homogeneous : gboolean)
     return GtkWidget;
   pragma Import (C, gtk_table_new, "gtk_table_new");

   procedure gtk_table_attach_defaults
     (Table : GtkWidget; Widget : GtkWidget;
      Left_Attach, Right_Attach, Top_Attach, Bottom_Attach : guint);
   pragma Import (C, gtk_table_attach_defaults, "gtk_table_attach_defaults");

   type GtkWindowType is (GTK_WINDOW_TOPLEVEL, GTK_WINDOW_POPUP);

   function gtk_window_new (Window_Type : GtkWindowType) return GtkWidget;
   pragma Import (C, gtk_window_new, "gtk_window_new");

   procedure gtk_window_resize (Window : GtkWidget; Width, Height : gint);
   pragma Import (C, gtk_window_resize, "gtk_window_resize");

   procedure gtk_window_set_default_size
     (Window : GtkWidget; Width, Height : gint);
   pragma Import
     (C, gtk_window_set_default_size, "gtk_window_set_default_size");

   type GtkWindowPosition is
     (GTK_WIN_POS_NONE,
      GTK_WIN_POS_CENTER,
      GTK_WIN_POS_MOUSE,
      GTK_WIN_POS_CENTER_ALWAYS,
      GTK_WIN_POS_CENTER_ON_PARENT);

   procedure gtk_window_set_position
     (Window : GtkWidget; Position : GtkWindowPosition);
   pragma Import (C, gtk_window_set_position, "gtk_window_set_position");

   procedure gtk_window_set_title
     (Window : GtkWidget; Title : char_array);
   pragma Import (C, gtk_window_set_title, "gtk_window_set_title");

   function gtk_button_new_with_label (Label : String) return GtkWidget;

   function gtk_drawing_area_new return GtkWidget;
   pragma Import (C, gtk_drawing_area_new, "gtk_drawing_area_new");

   --  calls on gdk_threads_enter/leave must surround all calls into GTK.

   --  gdk_threads_leave/enter must be called before making
   --  a call into the GTK from a *call back*, because otherwise
   --  the call may deadlock because the call back into GTK will presumably
   --  call gdk_threads_enter, which will deadlock if we haven't first called
   --  gdk_threads_leave.

   procedure gdk_threads_enter;
   pragma Import (C, gdk_threads_enter, "gdk_threads_enter");

   procedure gdk_threads_leave;
   pragma Import (C, gdk_threads_leave, "gdk_threads_leave");

   type Callback_1 is access procedure (Widget : GtkWidget; Data : gpointer);
   pragma Convention (C, Callback_1);

   type Callback_2 is access function
     (Widget : GtkWidget;
      Event  : GdkEvent;
      Data   : gpointer)
      return Boolean;
   pragma Convention (C, Callback_2);

   type GDestroyNotify is access procedure (data : gpointer);
   pragma Convention (C, GDestroyNotify);

   procedure gtk_widget_destroy (Widget : GtkWidget; Data : gpointer);
   pragma Convention (C, gtk_widget_destroy);

   procedure g_signal_connect (Widget : GtkWidget; Signal : String;
     Handler : Callback_1; Extra : gpointer);

   procedure g_signal_connect (Widget : GtkWidget; Signal : String;
     Handler : Callback_2; Extra : gpointer);

   procedure g_signal_connect_swapped (Widget : GtkWidget; Signal : String;
     Handler : Callback_1; First_Param : gpointer);

   function gdk_keyval_to_unicode (keyval : guint) return guint32;
   pragma Import (C, gdk_keyval_to_unicode, "gdk_keyval_to_unicode");

   function gdk_unicode_to_keyval (unicode : guint32) return guint;
   pragma Import (C, gdk_unicode_to_keyval, "gdk_unicode_to_keyval");

   type GSignalFlags is new int;
   G_SIGNAL_RUN_FIRST   : constant GSignalFlags := 2**0;
   G_SIGNAL_RUN_LAST    : constant GSignalFlags := 2**1;
   G_SIGNAL_RUN_CLEANUP : constant GSignalFlags := 2**2;
   G_SIGNAL_NO_RECURSE  : constant GSignalFlags := 2**3;
   G_SIGNAL_DETAILED    : constant GSignalFlags := 2**4;
   G_SIGNAL_ACTION      : constant GSignalFlags := 2**5;
   G_SIGNAL_NO_HOOKS    : constant GSignalFlags := 2**6;

   function g_signal_parse_name
     (detailed_signal    : String;
      itype              : GType;
      signal_id          : access guint;
      detail             : access GQuark;
      force_detail_quark : Boolean) return Boolean;

   type GSignalInvocationHint is record
      signal_id : guint;
      detail    : GQuark;
      run_type  : GSignalFlags;
   end record;

   type GSignalEmissionHook is access function
     (ihint          : GSignalInvocationHint;
      n_param_values : guint;
      param_values   : GValue_ptr;
      data           : gpointer) return gboolean;
   pragma Convention (C, GSignalEmissionHook);

   function g_signal_add_emission_hook
     (signal_id    : guint;
      detail       : GQuark;
      hook_func    : GSignalEmissionHook;
      hook_data    : gpointer;
      data_destroy : GDestroyNotify) return gulong;
   pragma Import (C, g_signal_add_emission_hook, "g_signal_add_emission_hook");

   procedure g_signal_remove_emission_hook
     (signal_id : guint;
      hook_id   : gulong);
   pragma Import
     (C, g_signal_remove_emission_hook, "g_signal_remove_emission_hook");

private

   type GdkEventRecord;
   type GdkEvent is access all GdkEventRecord;

   type GdkWindowRecord;
   type GdkWindow is access all GdkWindowRecord;

   type GdkDeviceRecord;
   type GdkDevice is access all GdkDeviceRecord;

   type GtkWidgetRecord;
   type GtkWidget is access all GtkWidgetRecord;

   pragma Assert (GtkWidget'Size = System.Address'Size);
   pragma Assert (GdkWindow'Size = System.Address'Size);
   pragma Assert (GdkDevice'Size = System.Address'Size);
   pragma Assert (GdkEvent'Size = System.Address'Size);

end GTK_C_Interface;
