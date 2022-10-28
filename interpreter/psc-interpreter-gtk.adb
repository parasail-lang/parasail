------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2015, AdaCore                     --
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
with PSC.Strings;
with Ada.Text_IO;
with GTK_C_Interface;
with Interfaces.C;
with System.Storage_Elements;
with Ada.Unchecked_Conversion;
pragma Elaborate (PSC.Interpreter.Builtins);
pragma Elaborate (PSC.Interpreter.Param_Signatures);
pragma Elaborate (PSC.Strings);
pragma Elaborate (Ada.Text_IO);

package body PSC.Interpreter.GTK is
   --  Package providing support for builtin ParaSail GTK-related operations

   use GTK_C_Interface;

   procedure Hello (Widget : GtkWidget; Data : gpointer);
   pragma Convention (C, Hello);

   function To_Widget is new Ada.Unchecked_Conversion (gpointer, GtkWidget);
   function To_gpointer is new Ada.Unchecked_Conversion (GtkWidget, gpointer);

   procedure Hello (Widget : GtkWidget; Data : gpointer) is
   begin
      Ada.Text_IO.Put_Line ("Inside ""Hello"" signal handler");
      g_print ("Hello World" & ASCII.LF);
      --  g_print ("About to try to destroy enclosing Window" & ASCII.LF);
      --  gtk_widget_destroy (To_Widget (Data), To_gpointer (Widget));
   end Hello;

   function Delete_Event
     (Widget : GtkWidget;
      Event : GdkEvent;
      Data : gpointer)
      return Boolean;
   pragma Convention (C, Delete_Event);

   function Delete_Event
     (Widget : GtkWidget;
      Event : GdkEvent;
      Data : gpointer)
      return Boolean
   is
   begin
      Ada.Text_IO.Put_Line ("Inside ""Delete-Event"" signal handler");
      g_print ("delete event occurred" & ASCII.LF);

      return True;
   end Delete_Event;

   Top_Level_Window : GtkWidget;

   function To_GtkWidget is new Ada.Unchecked_Conversion
     (System.Storage_Elements.Integer_Address, GtkWidget);

   function From_GtkWidget is new Ada.Unchecked_Conversion
     (GtkWidget, System.Storage_Elements.Integer_Address);

   function Word_To_GtkWidget (Val : Word_Type) return GtkWidget is
   --  Convert a word representing a Widget or extension thereof
   --  in ParaSail (but *not* a polymorphic Widget+), to a GtkWidget
   begin
      return To_GtkWidget (System.Storage_Elements.Integer_Address (Val));
   end Word_To_GtkWidget;

   function Poly_To_GtkWidget (Poly : Object_Virtual_Address) return GtkWidget
   is
   --  Convert a word representing a ParaSail polymorphic Widget+
   --  to a GtkWidget
      Poly_Param_Type : constant Non_Op_Map_Type_Ptr :=
        Large_Obj_Type_Desc (Poly);
      Underlying_Type : constant Type_Descriptor_Ptr :=
        Poly_Param_Type.Components (1).Type_Desc;
      Addr_Of_Underlying_Value : constant Object_Virtual_Address :=
        Poly + Large_Obj_Header_Size;
      Underlying_Value : constant Word_Type :=
        Fetch_Word (Addr_Of_Underlying_Value);
   begin
      if Is_Small (Underlying_Type) then
         --  This object is just a widget id
         return Word_To_GtkWidget (Underlying_Value);
      else
         --  This is an extension of a widget, so we need
         --  to fetch the first component to get the widget id.
         return Word_To_GtkWidget (Fetch_Word
           (Underlying_Value + Large_Obj_Header_Size));
      end if;
   end Poly_To_GtkWidget;

   function GtkWidget_To_Word (Widget : GtkWidget) return Word_Type is
   --  Convert a GtkWidget (or derivative thereof) to a word
   --  representing a Widget (or extension thereof) in ParaSail
   --  (but *not* a polymorphic Widget+)
   begin
      return Word_Type (From_GtkWidget (Widget));
   end GtkWidget_To_Word;

   procedure Destroy (Widget : GtkWidget; Data : gpointer);
   pragma Convention (C, Destroy);

   Destroy_Handler_Func : Word_Type := 0;
   Destroy_Handler_Ref_Poly_Controller : Word_Ptr := null;
   Destroy_Handler_Hooks_Id : Word_Type := 0;

   procedure Destroy (Widget : GtkWidget; Data : gpointer) is
      Outputs : Word_Array (1 .. 1) := (1 => Null_Value);
   begin
      Ada.Text_IO.Put_Line ("Inside ""Destroy"" signal handler");
      if Destroy_Handler_Func /= 0 then
         gdk_threads_leave;  --  Release the lock while in the callback
         Call_Through_Operation_Desc (Destroy_Handler_Func,
           Inputs =>
             (Word_Ptr_To_Word (Destroy_Handler_Ref_Poly_Controller),
              GtkWidget_To_Word (Widget), 0),
           Outputs => Outputs);
         gdk_threads_enter;  --  Reacquire the lock
      else
         gtk_main_quit;
      end if;
   end Destroy;

   function G_Value_To_Event (G_Value : Word_Type) return Word_Type is
   --   func To_Event(G_Value) -> GTK::Event
   begin
      return G_Value;  --  This is an identity now
   end G_Value_To_Event;

   package G_Value_To_Event_Op is new Unary_Builtin
     (Name => "#g_value_to_event",
      Operand => Word_Param,
      Result => Word_Param,
      Op => G_Value_To_Event);

   function G_Value_To_Cairo_Context (G_Value : Word_Type) return Word_Type is
   --   func To_Cairo_Context(G_Value) -> Cairo::Context
   begin
      return G_Value;
   end G_Value_To_Cairo_Context;

   package G_Value_To_Cairo_Context_Op is new Unary_Builtin
     (Name => "#g_value_to_cairo_context",
      Operand => Word_Param,
      Result => Word_Param,
      Op => G_Value_To_Cairo_Context);

   --  This needs to be initialized at some point!
   Poly_Widget_Type_Desc : Non_Op_Map_Type_Ptr := null;

   procedure Gtk_Init (IO : Word_Type; Args : Word_Type) is
   --  Initialize GTK
   begin
      gtk_init_if_needed;
   end Gtk_Init;

   package Gtk_Init_Op is new Two_Input_Builtin
     (Name => "#gtk_init",
      First => Word_Param,
      Second => Word_Param,
      Op => Gtk_Init);

   procedure Gtk_Main_Loop (Gtk : Word_Type; Controller : Word_Type) is
   --  Presume the Controller has been set up as the universal signal handler,
   --  and enter the GTK main loop.
   begin
      --  NOTE: Presuming caller has connected Controller
      GTK_C_Interface.gtk_main;
   end Gtk_Main_Loop;

   package Gtk_Main_Loop_Op is new Two_Input_Builtin
     (Name => "#gtk_main_loop",
      First => Word_Param,
      Second => Word_Param,
      Op => Gtk_Main_Loop,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Main_Quit (Gtk : Word_Type) is
   --  Cause the main loop to exit
   begin
      GTK_C_Interface.gtk_main_quit;
   end Gtk_Main_Quit;

   package Gtk_Main_Quit_Op is new One_Input_Builtin
     (Name => "#gtk_main_quit",
      Operand => Word_Param,
      Op => Gtk_Main_Quit,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   function To_GValue_Ptr is new Ada.Unchecked_Conversion
     (System.Address, GValue_Ptr);

   function G_Value_Array_Indexing
     (G_Value_Array : Word_Type; Index : Word_Type) return Word_Type is
   --  op "indexing"(G_Value_Array; Index : Univ_Integer) -> G_Value
   begin
      if G_Value_Array = Null_Value then
         return Null_Value;
      else
         --  Convert word back to a GValue_Ptr, and get Index + 1'th element.
         declare
            Ptr : constant GValue_Ptr := To_GValue_Ptr
              (System.Storage_Elements.To_Address
                (System.Storage_Elements.Integer_Address (G_Value_Array)));
            Addr : constant gpointer :=
              Ptr (Integer (Index) + 1).data (1).v_pointer;
            use System;
         begin
            if Addr = Null_Address then
               return Null_Value;
            else
               return Word_Type (System.Storage_Elements.To_Integer (Addr));
            end if;
         end;
      end if;
   end G_Value_Array_Indexing;

   package G_Value_Array_Indexing_Op is new Binary_Builtin
     (Name => "#g_value_array_indexing",
      Left => Word_Param,
      Right => Word_Param,
      Result => Word_Param,
      Op => G_Value_Array_Indexing);

   function G_Value_Array_To_ParaSail_Array
     (n_param_values : guint; param_values : GValue_Ptr)
     return Word_Type is
   --  Return a ParaSail array whih can be passed to G_Value_Array_Indexing
   --  TBD: We ignore n_param_values for now
   begin
      if param_values = null then
         return Null_Value;
      else
         return Word_Type
           (System.Storage_Elements.To_Integer (param_values.all'Address));
      end if;
   end G_Value_Array_To_ParaSail_Array;

   function G_Signal_Emission_Handler
     (ihint : GSignalInvocationHint;
      n_param_values : guint;
      param_values : GValue_ptr;
      data : gpointer) return gboolean;
   pragma Convention (C, G_Signal_Emission_Handler);

   Max_Hooks : constant := 100;
   Num_Hooks : Natural := 0;
   Last_Hooks_Id : Word_Type := 0;

   type Handler_Info is record
      Handler_Id : gulong := 0;
      Handler_Func : Word_Type := 0;
      Signal_Id : aliased guint := 0;
      Detail : aliased GQuark := 0;
      Object_Type : GType;
      Ref_Poly_Controller : Word_Ptr := null;
      Hooks_Id : Word_Type := 0;
   end record;

   type Handler_Info_Ptr is access all Handler_Info;
   function To_Handler_Info is new Ada.Unchecked_Conversion
     (gpointer, Handler_Info_Ptr);

   function Init_Hooks_Id (Ref_Hooks_Id : Word_Ptr) return Word_Type is
   --  If Ref_Hooks_Id refers to a zero variable, assign it a unique ID
   --  Return the assigned unique Id
      Hooks_Id : Word_Type := Fetch_Word (Ref_Hooks_Id, 0);  --  Passed by ref
   begin
      if Hooks_Id = 0 then
         --  Assign unique Id
         Last_Hooks_Id := Last_Hooks_Id + Word_Type'(1);
         Hooks_Id := Last_Hooks_Id;
         Store_Word (Ref_Hooks_Id, 0, Value => Hooks_Id);
         Ada.Text_IO.Put_Line
           (" Assigning Hooks_Id =" & Word_Type'Image (Hooks_Id));
      end if;
      --  Return (pre-)assigned unique Id
      return Hooks_Id;
   end Init_Hooks_Id;

   function G_Signal_Emission_Handler
     (ihint : GSignalInvocationHint;
      n_param_values : guint;
      param_values : GValue_ptr;
      data : gpointer) return gboolean is
   --  Invoke ParaSail Handler presuming "data" points at Handler_Info record
      Info : constant Handler_Info_Ptr := To_Handler_Info (data);
      Outputs : Word_Array (1 .. 1) := (1 => Null_Value);
   begin
      Ada.Text_IO.Put_Line (" In G_Signal_Emission_Handler for signal" &
        guint'Image (Info.Signal_Id) & " of type " &
        g_type_name (Info.Object_Type));
      gdk_threads_leave;  --  Release the lock while in the callback
      Call_Through_Operation_Desc (Info.Handler_Func,
        Inputs => (Word_Ptr_To_Word (Info.Ref_Poly_Controller),
                   GtkWidget_To_Word (GValue_To_GtkWidget (param_values (1))),
                   G_Value_Array_To_ParaSail_Array
                     (n_param_values, param_values)),
        Outputs => Outputs);
      gdk_threads_enter;  --  Reacquire the lock
      return gboolean (Outputs (1));
   end G_Signal_Emission_Handler;

   Handler_Infos : array (1 .. Max_Hooks) of aliased Handler_Info;

   --  ParaSail declaration of Emission hook is as follows:
   --   type G_Signal_Emission_Hook is
   --     func(var Controller+;
   --          Widget_Id;
   --          Param_Values : G_Value_Array) -> Boolean;

   procedure G_Signal_Add_Emission_Hook
     (Handler : Word_Type;
      Signal : Word_Type;
      Object_Type : Word_Type;
      Ref_Poly_Controller : Word_Ptr;
      Ref_Hooks_Id : Word_Ptr) is
   --  func G_Signal_Add_Emission_Hook
   --    (Handler : G_Signal_Emission_Hook;
   --     Signal : Signal_Id;
   --     Object_Type : G_Type;
   --     var Controller+;
   --     var Hooks_Id : Univ_Integer)
      itype : constant GType :=
        g_type_from_name (Word_To_String (Object_Type));
      Result : Boolean;
      Hooks_Id : constant Word_Type := Init_Hooks_Id (Ref_Hooks_Id);
      use type GType;
   begin
      if itype = 0 then
         --  Cannot create hook if associated type never used
         Ada.Text_IO.Put_Line (" Did NOT add emission hook for signal " &
           Word_To_String (Signal) & " because " &
           Word_To_String (Object_Type) & " never instantiated.");
         return;
      end if;

      Num_Hooks := Num_Hooks + 1;
      Handler_Infos (Num_Hooks).Handler_Func := Handler;
      Handler_Infos (Num_Hooks).Ref_Poly_Controller := Ref_Poly_Controller;
      Handler_Infos (Num_Hooks).Object_Type := itype;
      Handler_Infos (Num_Hooks).Hooks_Id := Hooks_Id;

      Result := GTK_C_Interface.g_signal_parse_name
        (detailed_signal => Word_To_String (Signal),
         itype => itype,
         signal_id => Handler_Infos (Num_Hooks).signal_id'Access,
         detail => Handler_Infos (Num_Hooks).detail'Access,
         force_detail_quark => False);

      Handler_Infos (Num_Hooks).Handler_Id :=
        GTK_C_Interface.g_signal_add_emission_hook
          (signal_id => Handler_Infos (Num_Hooks).signal_id,
           detail => Handler_Infos (Num_Hooks).detail,
           hook_func => G_Signal_Emission_Handler'Access,
           hook_data => Handler_Infos (Num_Hooks)'Address,
           data_destroy => null);

      Ada.Text_IO.Put_Line (" Added emission hook" & Natural'Image (Num_Hooks)
        & " of Hooks_Id" & Word_Type'Image (Hooks_Id)
        & " for signal " & Word_To_String (Signal) & " = " &
          guint'Image (Handler_Infos (Num_Hooks).signal_id));
   end G_Signal_Add_Emission_Hook;

   package G_Signal_Add_Emission_Hook_Op is new Five_Input_Builtin
     (Name => "#g_signal_add_emission_hook",
      First => Word_Param, Second => Word_Param,
      Third => Word_Param, Fourth => Word_Ptr_Param,
      Fifth => Word_Ptr_Param,
      Op => G_Signal_Add_Emission_Hook,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure G_Signal_Remove_Emission_Hooks
     (Hooks_Id : Word_Type) is
   --  func G_Signal_Remove_Emission_Hooks (Hooks_Id : Univ_Integer)
   --    Remove all emission hooks associated with given Hooks_Id
   begin
      if Hooks_Id = 0 then
         Ada.Text_IO.Put_Line (" No emission hooks to remove");
         return;
      end if;

      if Destroy_Handler_Hooks_Id = Hooks_Id then
         Ada.Text_IO.Put_Line (" Remove handler for Destroy signal");
         Destroy_Handler_Func := 0;
         Destroy_Handler_Ref_Poly_Controller := null;
         Destroy_Handler_Hooks_Id := 0;
      end if;

      for I in 1 .. Num_Hooks loop
         if Hooks_Id = Handler_Infos (I).Hooks_Id then
            --  Found a hook with this Hooks_Id; deactivate it
            GTK_C_Interface.g_signal_remove_emission_hook
              (signal_id => Handler_Infos (I).signal_id,
               hook_id => Handler_Infos (I).Handler_Id);

            Ada.Text_IO.Put_Line (" Removed emission hook" & Natural'Image (I)
              & " for signal id " &
              guint'Image (Handler_Infos (I).signal_id));

            Handler_Infos (I).Hooks_Id := 0;
            Handler_Infos (I).Ref_Poly_Controller := null;
            Handler_Infos (I).Handler_Func := 0;
         end if;
      end loop;

      --  Back up "Num_Hooks" as far as possible.
      for I in reverse 1 .. Num_Hooks loop
         exit when
           Handler_Infos (I).Hooks_Id /= 0;

         Num_Hooks := I - 1;
      end loop;

   end G_Signal_Remove_Emission_Hooks;

   package G_Signal_Remove_Emission_Hooks_Op is new One_Input_Builtin
     (Name => "#g_signal_remove_emission_hooks",
      Operand => Word_Param,
      Op => G_Signal_Remove_Emission_Hooks,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Add_Window_Destroy_Hook
     (Handler : Word_Type;
      Ref_Poly_Controller : Word_Ptr;
      Ref_Hooks_Id : Word_Ptr) is
   --  func Add_Window_Destroy_Hook
   --    (Handler : G_Signal_Emission_Hook;
   --     var Controller+;
   --     var Hooks_Id : Univ_Integer)
      Hooks_Id : constant Word_Type := Init_Hooks_Id (Ref_Hooks_Id);
   begin
      Destroy_Handler_Func := Handler;
      Destroy_Handler_Ref_Poly_Controller := Ref_Poly_Controller;
      Destroy_Handler_Hooks_Id := Hooks_Id;

      Ada.Text_IO.Put_Line (" Added window-destroy hook");
   end Add_Window_Destroy_Hook;

   package Add_Window_Destroy_Hook_Op is new Three_Input_Builtin
     (Name => "#gtk_add_window_destroy_hook",
      First => Word_Param, Second => Word_Ptr_Param,
      Third => Word_Ptr_Param,
      Op => Add_Window_Destroy_Hook);

   --  Event-related operations  --

   function To_Event is new Ada.Unchecked_Conversion
     (System.Address, GdkEvent);

   pragma Warnings (Off);
   function To_Word is new Ada.Unchecked_Conversion
     (GdkEventType, Word_Type);
   pragma Warnings (On);

   function Gdk_Get_Event_Type (Event : System.Address) return Word_Type is
   --  Get event type from event
   begin
      return To_Word (GTK_C_Interface.gdk_event_get_event_type
        (To_Event (Event)));
   end Gdk_Get_Event_Type;

   package Gdk_Get_Event_Type_Op is new Unary_Builtin
     (Name => "#gdk_get_event_type",
      Operand => Addr_Param,
      Result => Word_Param,
      Op => Gdk_Get_Event_Type);

   function Gdk_Unicode_To_Keyval (Univ : Word_Type) return Word_Type is
   --  Convert Univ_Character (unicode) to a keyval
   begin
      return Word_Type
        (GTK_C_Interface.gdk_unicode_to_keyval (guint32 (Univ)));
   end Gdk_Unicode_To_Keyval;

   package Gtk_Unicode_To_Keyval_Op is new Unary_Builtin
     (Name => "#gdk_unicode_to_keyval",
      Operand => Word_Param,
      Result => Word_Param,
      Op => Gdk_Unicode_To_Keyval);

   function Gdk_Keyval_To_Unicode (Keyval : Word_Type) return Word_Type is
   --  Convert keyval to a Univ_Character (unicode)
   begin
      return Word_Type
        (GTK_C_Interface.gdk_keyval_to_unicode (guint (Keyval)));
   end Gdk_Keyval_To_Unicode;

   package Gtk_Keyval_To_Unicode_Op is new Unary_Builtin
     (Name => "#gdk_keyval_to_unicode",
      Operand => Word_Param,
      Result => Word_Param,
      Op => Gdk_Keyval_To_Unicode);

   --  To_String: gdk_keyval_name().
   --  From_String: gdk_keyval_from_name().
   --  Is_Upper/Is_Lower: gdk_keyval_is_upper() and gdk_keyval_is_lower().
   --  To_Upper/To_Lower: gdk_keyval_to_upper() and gdk_keyval_to_lower().

   function Get_Keyval (Event_Key : System.Address) return Word_Type is
   --      func Get_Keyval(Event_Key) -> Keyval
   --        is import(#gdk_event_key_get_keyval)
   begin
      return Word_Type (GTK_C_Interface.gdk_event_get_keyval
        (To_Event (Event_Key)));
   end Get_Keyval;

   package Get_Keyval_Op is new Unary_Builtin
     (Name => "#gdk_event_key_get_keyval",
      Operand => Addr_Param,
      Result => Word_Param,
      Op => Get_Keyval,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   function Get_State (Event_Key : System.Address) return Word_Type is
   --      func Get_State(Event_Key) -> Modifier_Type
   --        is import(#gdk_event_get_state)
   --      func Get_State(Event_Button) -> Modifier_Type
   --        is import(#gdk_event_get_state)
   begin
      return Word_Type (GTK_C_Interface.gdk_event_get_state
        (To_Event (Event_Key)));
   end Get_State;

   package Get_State_Op is new Unary_Builtin
     (Name => "#gdk_event_get_state",
      Operand => Addr_Param,
      Result => Word_Param,
      Op => Get_State,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   function Get_Button (Event_Button : System.Address) return Word_Type is
   --      type Mouse_Button is Enum<[#back, #forward, #left, #middle, #right]>
   --
   --      func Get_Button(Event_Button) -> Mouse_Button
   --        is import(#gdk_event_button_get_button)
   begin
      return Word_Type (GTK_C_Interface.gdk_event_get_button
        (To_Event (Event_Button)));
   end Get_Button;

   package Get_Button_Op is new Unary_Builtin
     (Name => "#gdk_event_button_get_button",
      Operand => Addr_Param,
      Result => Word_Param,
      Op => Get_State,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   function Button_Get_X (Event_Button : System.Address) return Univ_Real is
   --      func Get_X(Event_Button) -> Location
   --        is import(#gdk_event_button_get_x)
   begin
      return Univ_Real (GTK_C_Interface.gdk_event_get_x
        (To_Event (Event_Button)));
   end Button_Get_X;

   package Button_Get_X_Op is new Unary_Builtin
     (Name => "#gdk_event_button_get_x",
      Operand => Addr_Param,
      Result => Real_Param,
      Op => Button_Get_X,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   function Button_Get_Y (Event_Button : System.Address) return Univ_Real is
   --      func Get_Y(Event_Button) -> Location
   --        is import(#gdk_event_button_get_y)
   begin
      return Univ_Real (GTK_C_Interface.gdk_event_get_y
        (To_Event (Event_Button)));
   end Button_Get_Y;

   package Button_Get_Y_Op is new Unary_Builtin
     (Name => "#gdk_event_button_get_y",
      Operand => Addr_Param,
      Result => Real_Param,
      Op => Button_Get_Y,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Widget_Show (Widget : Word_Ptr) is
      --  func Show(var Widget)
   begin
      GTK_C_Interface.gtk_widget_show
        (Word_To_GtkWidget (Fetch_Word (Widget, 0)));  --  passed by ref
   end Gtk_Widget_Show;

   package Gtk_Widget_Show_Op is new One_Input_Builtin
     (Name => "#gtk_widget_show",
      Operand => Word_Ptr_Param,
      Op => Gtk_Widget_Show,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Widget_Show_All (Widget : Word_Ptr) is
      --  func Show_All(var Widget)
   begin
      GTK_C_Interface.gtk_widget_show_all
        (Word_To_GtkWidget (Fetch_Word (Widget, 0)));  --  passed by ref
   end Gtk_Widget_Show_All;

   package Gtk_Widget_Show_All_Op is new One_Input_Builtin
     (Name => "#gtk_widget_show_all",
      Operand => Word_Ptr_Param,
      Op => Gtk_Widget_Show_All,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Widget_Queue_Draw (Widget : Word_Ptr) is
      --  func Queue_Draw(var Widget)
   begin
      GTK_C_Interface.gtk_widget_queue_draw
        (Word_To_GtkWidget (Fetch_Word (Widget, 0)));  --  passed by ref
   end Gtk_Widget_Queue_Draw;

   package Gtk_Widget_Queue_Draw_Op is new One_Input_Builtin
     (Name => "#gtk_widget_queue_draw",
      Operand => Word_Ptr_Param,
      Op => Gtk_Widget_Queue_Draw,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Widget_Add_Events (Widget : Word_Ptr; Mask : Word_Type) is
   --      func Add_Events (var Widget; Event_Mask)
   begin
      GTK_C_Interface.gtk_widget_add_events
        (Word_To_GtkWidget (Fetch_Word (Widget, 0)), gint (Mask));
   end Gtk_Widget_Add_Events;

   package Gtk_Widget_Add_Events_Op is new Two_Input_Builtin
     (Name => "#gtk_widget_add_events",
      First => Word_Ptr_Param, Second => Word_Param,
      Op => Gtk_Widget_Add_Events,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Widget_Set_Events (Widget : Word_Ptr; Mask : Word_Type) is
   --      func Set_Events (var Widget; Event_Mask)
   begin
      GTK_C_Interface.gtk_widget_set_events
        (Word_To_GtkWidget (Fetch_Word (Widget, 0)), gint (Mask));
   end Gtk_Widget_Set_Events;

   package Gtk_Widget_Set_Events_Op is new Two_Input_Builtin
     (Name => "#gtk_widget_set_events",
      First => Word_Ptr_Param, Second => Word_Param,
      Op => Gtk_Widget_Set_Events,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Widget_Set_Size_Request (Widget : Word_Ptr;
     Width, Height : Word_Type) is
   --      func Set_Size_Request (var Widget; Width, Height : Univ_Integer)
   begin
      GTK_C_Interface.gtk_widget_set_size_request
        (Word_To_GtkWidget (Fetch_Word (Widget, 0)),
         gint (Width), gint (Height));
   end Gtk_Widget_Set_Size_Request;

   package Gtk_Widget_Set_Size_Request_Op is new Three_Input_Builtin
     (Name => "#gtk_widget_set_size_request",
      First => Word_Ptr_Param, Second => Word_Param, Third => Word_Param,
      Op => Gtk_Widget_Set_Size_Request,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   function Gtk_Widget_Get_Allocated_Width
     (Widget : Word_Type) return Word_Type is
   --      func Get_Allocated_Width (Widget) -> Univ_Integer
   begin
      return Word_Type (GTK_C_Interface.gtk_widget_get_allocated_width
        (Word_To_GtkWidget (Widget)));
   end Gtk_Widget_Get_Allocated_Width;

   package Gtk_Widget_Get_Allocated_Width_Op is new Unary_Builtin
     (Name => "#gtk_widget_get_allocated_width",
      Operand => Word_Param,
      Result => Word_Param,
      Op => Gtk_Widget_Get_Allocated_Width,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   function Gtk_Widget_Get_Allocated_Height
     (Widget : Word_Type) return Word_Type is
   --      func Get_Allocated_Height (Widget) -> Univ_Integer
   begin
      return Word_Type (GTK_C_Interface.gtk_widget_get_allocated_height
        (Word_To_GtkWidget (Widget)));
   end Gtk_Widget_Get_Allocated_Height;

   package Gtk_Widget_Get_Allocated_Height_Op is new Unary_Builtin
     (Name => "#gtk_widget_get_allocated_height",
      Operand => Word_Param,
      Result => Word_Param,
      Op => Gtk_Widget_Get_Allocated_Height,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Widget_Destroy (Widget : Word_Ptr) is
   --      func Destroy (var Widget)
   begin
      GTK_C_Interface.gtk_widget_destroy
        (Word_To_GtkWidget (Fetch_Word (Widget, 0)),
         Data => System.Null_Address);
   end Gtk_Widget_Destroy;

   package Gtk_Widget_Destroy_Op is new One_Input_Builtin
     (Name => "#gtk_widget_destroy",
      Operand => Word_Ptr_Param,
      Op => Gtk_Widget_Destroy,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --  interface GTK::Widget<> is
   --    // The root of the GTK Widget hierarchy
   --
   --      func Activate (var Widget)
   --        is import("null") --(#gtk_widget_activate)
   --
   --      func Get_Parent (Widget) -> Container+
   --        is import("null") --(#gtk_widget_get_parent)
   --      func Get_Toplevel (Widget) -> Widget+
   --        is import("null") --(#gtk_widget_get_toplevel)
   --
   --  end interface GTK::Widget
   --
   procedure Gtk_Container_Add (Container : Word_Ptr; Element : Word_Type) is
   --      func Add (var Container; Element : Widget+)
   begin
      GTK_C_Interface.gtk_container_add
        (Word_To_GtkWidget (Fetch_Word (Container, 0)),  --  var passed by ref
         Poly_To_GtkWidget (Element));
   end Gtk_Container_Add;

   package Gtk_Container_Add_Op is new Two_Input_Builtin
     (Name => "#gtk_container_add",
      First => Word_Ptr_Param,
      Second => Word_Param,
      Op => Gtk_Container_Add,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Container_Set_Border_Width
     (Container : Word_Ptr; Width : Word_Type) is
   --   func Set_Border_Width (var Container; Width : Univ_Integer)
   begin
      GTK_C_Interface.gtk_container_set_border_width
        (Word_To_GtkWidget (Fetch_Word (Container, 0)),  --  passed by ref
         Natural (Width));
   end Gtk_Container_Set_Border_Width;

   package Gtk_Container_Set_Border_Width_Op is new Two_Input_Builtin
     (Name => "#gtk_container_set_border_width",
      First => Word_Ptr_Param,
      Second => Word_Param,
      Op => Gtk_Container_Set_Border_Width,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --  interface GTK::Container<> extends GTK::Widget is

   --      func Remove (var Container; Element : Widget+)
   --        is import("null") --(#gtk_container_remove)
   --
   --      func Get_Children (Container) -> Vector<Widget+>
   --        is import("null") --(#gtk_container_get_children)
   --  end interface GTK::Container

   function Gtk_Table_New (GTK : Word_Ptr; Rows, Columns : Word_Type;
     Homogeneous : Word_Type)
     return Word_Type is
   --      func New (var GTK; Rows, Column : Univ_Integer;
   --        Homogeneous : Boolean) -> Table
   begin
      return GtkWidget_To_Word (GTK_C_Interface.gtk_table_new
        (guint (Rows), guint (Columns), gboolean (Homogeneous)));
   end Gtk_Table_New;

   package Gtk_Table_New_Op is new Quarternary_Builtin
     (Name => "#gtk_table_new",
      First => Word_Ptr_Param, Second => Word_Param,
      Third => Word_Param, Fourth => Word_Param,
      Result => Word_Param,
      Op => Gtk_Table_New,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Table_Attach (Table : Word_Ptr; Child : Word_Type;
     Left_Attach, Right_Attach, Top_Attach, Bottom_Attach : Word_Type) is
   --      func Attach (var Table; Child : Widget+;
   --        Left_Attach, Right_Attach,
   --          Top_Attach, Bottom_Attach : Univ_Integer;
   --        X_Options, Y_Options : optional Attach_Options := null;
   --        X_Padding, Y_Padding : Univ_Integer := 0)

   --  TBD: We are ignoring X/Y_Options and X/Y_Padding parameters for now.
   begin
      GTK_C_Interface.gtk_table_attach_defaults
        (Word_To_GtkWidget (Fetch_Word (Table, 0)),
         Word_To_GtkWidget (Fetch_Word (Child + Offset_Within_Area'(1))),
         guint (Left_Attach), guint (Right_Attach),
         guint (Top_Attach), guint (Bottom_Attach));
   end Gtk_Table_Attach;

   package Gtk_Table_Attach_Op is new Six_Input_Builtin
     (Name => "#gtk_table_attach",
      First => Word_Ptr_Param, Second => Word_Param, Third => Word_Param,
      Fourth => Word_Param, Fifth => Word_Param, Sixth => Word_Param,
      Op => Gtk_Table_Attach,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --  interface GTK::Table<> extends GTK::Container is
   --      type Attach_Options is Enum<[#expand, #fill, #shrink]>
   --
   --      func New (var GTK; Rows, Column : Univ_Integer;
   --        Homogeneous : Boolean) -> Table
   --        is import("null") --(#gtk_table_new)
   --
   --      func Attach (var Table; Child : Widget+;
   --        Left_Attach, Right_Attach,
   --          Top_Attach, Bottom_Attach : Univ_Integer;
   --        X_Options, Y_Options : optional Attach_Options := null;
   --        X_Padding, Y_Padding : Univ_Integer := 0)
   --        is import("null") --(#gtk_table_attach)
   --
   --      func Resize (var Table; Rows, Columns : Univ_Integer)
   --        is import("null") --(#gtk_table_resize)
   --
   --      func Set_Column_Spacing (var Table;
   --        Column : optional Univ_Integer := null; Spacing : Univ_Integer)
   --        is import("null") --(#gtk_table_set_column_spacing)
   --
   --      func Set_Row_Spacing (var Table;
   --        Row : optional Univ_Integer := null; Spacing : Univ_Integer)
   --        is import("null") --(#gtk_table_set_row_spacing)
   --
   --  end interface GTK::Table
   --
   --  interface GTK::Bin<> extends GTK::Container is
   --    // A container that only holds a single child widget
   --      func New (var GTK) -> Bin
   --        is import("null") --(#gtk_bin_new)
   --
   --      func Get_Child (GTK) -> Widget+
   --        is import("null") --(#gtk_bin_get_child)
   --  end interface GTK::Bin
   --
   function Gtk_Window_New (GTK : Word_Ptr; Window_Type : Word_Type)
     return Word_Type is
   --      func New (var GTK; Type : Window_Type := #toplevel) -> Window
      Window : constant GtkWidget :=
        GTK_C_Interface.gtk_window_new (GtkWindowType'Val (Window_Type));
   begin
      if Window_Type = 0 then
         Top_Level_Window := Window;
         g_signal_connect (Window, "delete-event",
           Delete_Event'Access, System.Null_Address);

         g_signal_connect (Window, "destroy",
           Destroy'Access, System.Null_Address);
      end if;
      return GtkWidget_To_Word (Window);
   end Gtk_Window_New;

   package Gtk_Window_New_Op is new Binary_Builtin
     (Name => "#gtk_window_new",
      Left => Word_Ptr_Param,
      Right => Word_Param,
      Result => Word_Param,
      Op => Gtk_Window_New,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Window_Resize (Window : Word_Ptr;
     Width, Height : Word_Type) is
   --      func Resize (var Window; Width, Height : Univ_Integer)
   begin
      GTK_C_Interface.gtk_window_resize
        (Word_To_GtkWidget (Fetch_Word (Window, 0)),
         gint (Width), gint (Height));
   end Gtk_Window_Resize;

   package Gtk_Resize_Op is new Three_Input_Builtin
     (Name => "#gtk_window_resize",
      First => Word_Ptr_Param, Second => Word_Param, Third => Word_Param,
      Op => Gtk_Window_Resize,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Window_Set_Default_Size (Window : Word_Ptr;
     Width, Height : Word_Type) is
   --      func Set_Default_Size (var Window; Width, Height : Univ_Integer)
   begin
      GTK_C_Interface.gtk_window_set_default_size
        (Word_To_GtkWidget (Fetch_Word (Window, 0)),
         gint (Width), gint (Height));
   end Gtk_Window_Set_Default_Size;

   package Gtk_Set_Default_Size_Op is new Three_Input_Builtin
     (Name => "#gtk_window_set_default_size",
      First => Word_Ptr_Param, Second => Word_Param, Third => Word_Param,
      Op => Gtk_Window_Set_Default_Size,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Window_Set_Title (Window : Word_Ptr; Title : Word_Type) is
   --      func Set_Title (var Window; Title : Univ_String)
   begin
      GTK_C_Interface.gtk_window_set_title
        (Word_To_GtkWidget (Fetch_Word (Window, 0)),
         Interfaces.C.To_C (Word_To_String (Title)));
   end Gtk_Window_Set_Title;

   package Gtk_Window_Set_Title_Op is new Two_Input_Builtin
     (Name => "#gtk_window_set_title",
      First => Word_Ptr_Param, Second => Word_Param,
      Op => Gtk_Window_Set_Title,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   procedure Gtk_Window_Set_Position
     (Window : Word_Ptr; Position : Word_Type) is
   --      func Set_Position (var Window; Position : Window_Position)
   begin
      GTK_C_Interface.gtk_window_set_position
        (Word_To_GtkWidget (Fetch_Word (Window, 0)),
         GtkWindowPosition'Val (Position));
   end Gtk_Window_Set_Position;

   package Gtk_Window_Set_Position_Op is new Two_Input_Builtin
     (Name => "#gtk_window_set_position",
      First => Word_Ptr_Param, Second => Word_Param,
      Op => Gtk_Window_Set_Position,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --  interface GTK::Window<> extends GTK::Bin is
   --    // A GTK Window
   --      type Window_Type is Enum<[#popup, #toplevel]>
   --
   --      func New (var GTK; Type : Window_Type := #toplevel) -> Window
   --        is import("null") --(#gtk_window_new)
   --
   --      func Get_Width (Window) -> Univ_Integer
   --        is import("null") --(#gtk_window_get_width)
   --      func Get_Height (Window) -> Univ_Integer
   --        is import("null") --(#gtk_window_get_height)
   --
   --      func Get_Maximized (Window) -> Boolean
   --        is import("null") --(#gtk_get_maximized)
   --
   --      func Set_Modal (var Window; Modal : Boolean)
   --        is import("null") --(#gtk_window_set_modal)
   --
   --      func Present (var Window)
   --        is import("null") --(#gtk_window_present)
   --
   --  end interface GTK::Window
   --
   function Gtk_Button_New_With_Label (GTK : Word_Ptr; Label : Word_Type)
     return Word_Type is
   --      func New (var GTK; Label : Univ_String) -> Button
      Button : constant GtkWidget :=
        GTK_C_Interface.gtk_button_new_with_label (Word_To_String (Label));
   begin
      return GtkWidget_To_Word (Button);
   end Gtk_Button_New_With_Label;

   package Gtk_Button_New_With_Label_Op is new Binary_Builtin
     (Name => "#gtk_button_new_with_label",
      Left => Word_Ptr_Param,
      Right => Word_Param,
      Result => Word_Param,
      Op => Gtk_Button_New_With_Label,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --  interface GTK::Button<> extends GTK::Bin is
   --      func New (var GTK) -> Button
   --        is import("null") --(#gtk_button_new)
   --
   --      func Set_Label (var Button; Label : Univ_String)
   --        is import("null") --(#gtk_button_set_label)
   --      func Get_Label (Button) -> Univ_String
   --        is import("null") --(#gtk_button_get_label)
   --
   --      func Emit_Clicked (var Button)
   --        is import("null") --(#gtk_button_emit_clicked)
   --  end interface GTK::Button
   --
   --  interface GTK::Event_Box<> extends GTK::Bin is
   --      func New (var GTK) -> Event_Box
   --        is import("null") --(#gtk_event_box_new)
   --      func Set_Above_Child (var Event_Box; Setting : Boolean)
   --        is import("null") --(#gtk_event_box_set_above_child)
   --      func Set_Visible_Window (var Event_Box; Setting : Boolean)
   --        is import("null") --(#gtk_event_box_set_visible_window)
   --  end interface GTK::Event_Box

   function Gtk_Drawing_Area_New (GTK : Word_Ptr) return Word_Type is
   --      func New (var GTK) -> Drawing_Area
   begin
      return GtkWidget_To_Word (GTK_C_Interface.gtk_drawing_area_new);
   end Gtk_Drawing_Area_New;

   package Gtk_Drawing_Area_New_Op is new Unary_Builtin
     (Name => "#gtk_drawing_area_new",
      Operand => Word_Ptr_Param,
      Result => Word_Param,
      Op => Gtk_Drawing_Area_New,
      Invoke_Before_Call => gdk_threads_enter,
      Invoke_After_Call => gdk_threads_leave);

   --  Example of use of parts of GTK_C_Interface  --

   procedure Display_Button (Str : Word_Type) is
   --  Create a window and a button using GTK
      Window : GtkWidget;
      Button : GtkWidget;
   begin
      gtk_init_if_needed;
      gdk_threads_enter;
      Window := gtk_window_new (GTK_WINDOW_TOPLEVEL);

      g_signal_connect (Window, "delete-event",
        Delete_Event'Access, System.Null_Address);

      g_signal_connect (Window, "destroy",
        Destroy'Access, System.Null_Address);

      gtk_container_set_border_width (Window, 10);

      Button := gtk_button_new_with_label (Word_To_String (Str));
      g_signal_connect (Button, "clicked", Hello'Access, System.Null_Address);

      g_signal_connect_swapped (Button, "clicked",
        gtk_widget_destroy'Access, To_gpointer (Window));

      gtk_container_add (Window, Button);

      gtk_widget_show (Button);
      gtk_widget_show (Window);

      --  Start main loop
      gtk_main;
      gdk_threads_leave;
   end Display_Button;

   package Display_Button_Op is new One_Input_Builtin
     (Name => "#display_button",
      Operand => Word_Param,
      Op => Display_Button);

end PSC.Interpreter.GTK;
