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
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation. See           --
-- documentation/COPYING3 and documentation/GCC_RUNTIME3_1 for details.     --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with     --
-- the ParaSail, Sparkel, Javallel, or Parython compiler, including any     --
-- required library run-time units written in Ada or in any of the above    --
-- languages, using any licensing terms  of your choosing.                  --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

with System;
with Interfaces.C;            use Interfaces;

with PSC.Interpreter.Builtins;
with PSC.Messages;
with PSC.Strings;
with PSC.Univ_Strings;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
pragma Elaborate (PSC.Interpreter.Builtins);
pragma Elaborate (PSC.Strings);
pragma Elaborate (Ada.Text_IO);
package body PSC.Interpreter.Ada202x_Builtins is
   --  Package providing support for builtin Ada202x operations

   Debug_Access_Types : constant Boolean := True;

   procedure Bool_From_Univ
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
      --  "from_univ"(Univ_Enumeration) -> Boolean
   pragma Export (Ada, Bool_From_Univ, "_psc_ada202x_bool_from_univ");

   procedure Bool_To_Univ
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
      --  "to_univ"(Boolean) -> Univ_Enumeration
   pragma Export (Ada, Bool_To_Univ, "_psc_ada202x_bool_to_univ");

   --------

   procedure Raise_Exception_Occurrence
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Raise_Exception_Occurrence,
     "_psc_raise_exception_occurrence");
      --  func Raise_Exception_Occurrence(Exception_Type'Class)

   procedure Exception_Name
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Exception_Name,
     "_psc_exception_name");
      --  func Exception_Name(Exception_Type'Class) return String

   --------

   procedure Allocate_From_Pool
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
      --  func Allocate
      --    (Pool : in out Storage_Pool'Class;
      --     Val : Desig_Type)
      --    return Access_Type is ...
   pragma Export (Ada, Allocate_From_Pool, "_psc_allocate_from_pool");

   procedure Tick_Access
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
      --  func Tick_Access (Ali : in out Aliased_Obj) return Acc
   pragma Export (Ada, Tick_Access, "_psc_tick_access");

   procedure Deref_Var
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
      --  func Deref (Acc)
      --    return aliased in out Aliased_Obj is ...
   pragma Export (Ada, Deref_Var, "_psc_deref_var");

   procedure Deref_Const
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
      --  func Deref_Const (Acc)
      --    return Aliased_Obj is ...
   pragma Export (Ada, Deref_Const, "_psc_deref_const");

   ------------------------

   procedure Atomic_Set_Value
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Atomic_Set_Value, "_psc_atomic_set_value");

   procedure Atomic_Value
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Atomic_Value, "_psc_atomic_value");

   procedure Atomic_Exchange
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Atomic_Exchange,
     "_psc_atomic_exchange");

   procedure Atomic_Compare_And_Exchange
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Atomic_Compare_And_Exchange,
     "_psc_atomic_compare_and_exchange");

   procedure Atomic_Fetch_And_Add
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Atomic_Fetch_And_Add, "_psc_atomic_fetch_and_add");

   ------------------------

   True_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#True"));

   False_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#False"));

   procedure Bool_From_Univ
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  "from_univ"(Univ_Enumeration) -> Boolean
      Index : constant Strings.U_String_Index :=
        Strings.Index (To_U_String (Fetch_Word (Params, 1)));
      Result : Boolean := False;
      use type Strings.U_String_Index;
   begin
      --  Determine boolean value from index of enumeration literal
      if Index = True_Index then
         Result := True;
      elsif Index = False_Index then
         Result := False;
      else
         Messages.Put_Error
           ("Boolean literal must be #True or #False" &
            ", value = " &
            Strings.To_String (Strings.To_U_String (Index)),
            Src_Pos => Execution_Source_Pos);
         pragma Assert (False);
         null;
      end if;

      Store_Word (Params, 0, Boolean'Pos (Result));
   end Bool_From_Univ;

   procedure Bool_To_Univ
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  "to_univ"(Boolean) -> Univ_Enumeration
      Bool_Pos : constant Word_Type := Fetch_Word (Params, 1);
      Val : constant Boolean := Boolean'Val (Bool_Pos);
      Result : Strings.U_String_Index;
   begin
      --  Determine univ-enum value given boolean value
      if Val then
         Result := True_Index;
      else
         Result := False_Index;
      end if;
      Store_Word (Params, 0, Word_Type (Result));
   end Bool_To_Univ;

   ------------------------

   procedure Raise_Exception_Occurrence
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Raise_Exception_Occurrence(Exception_Type'Class)
      --  TBD: Perhaps should be "var" parameter because it is moved to
      --       handler's stg rgn

      --  Make a copy so can be set to null after moved to handler's stg rgn.
      Excep_Obj : Word_Type := Fetch_Word (Params, 0);
   begin
      --  Just call the routine in the main interpreter
      Interpreter.Raise_Exception_Occurrence
        (Context, Excep_Obj);
   end Raise_Exception_Occurrence;

   procedure Exception_Name
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Exception_Name(Exception_Type'Class) return String
      Excep_Obj : aliased Word_Type := Fetch_Word (Params, 1);
      Excep_Type_Desc : Type_Descriptor_Ptr :=
        Large_Obj_Type_Desc (Excep_Obj);
   begin
      if Excep_Type_Desc.Is_Polymorphic then
         Excep_Type_Desc := Excep_Type_Desc.Root_Type_Desc;
      end if;
      Store_Word (Params, 0,
        Univ_Strings.To_Word_Type (Univ_Strings.From_U_String
          (Excep_Type_Desc.Name, Fetch_Word (Params, 0))));
   end Exception_Name;

   ------------------------

   --  Start with 1024 proxies in the region.
   --  This is doubled each time we run out of proxies.
   Initial_Proxy_Table_Last : constant Proxy_Index := 2 ** 10 - 1;

   function To_Proxy_Tab_Ptr is new Ada.Unchecked_Conversion
     (Word_Type, Proxy_Table_Ptr);

   function From_Proxy_Tab_Ptr is new Ada.Unchecked_Conversion
     (Proxy_Table_Ptr, Word_Type);

   function To_Proxy_Self_Access is new Ada.Unchecked_Conversion
     (Proxy_Self_Rec, Word_Type);

   function To_Proxy_Self_Rec is new Ada.Unchecked_Conversion
     (Word_Type, Proxy_Self_Rec);

   function Extract_Proxy_Index (Acc_Val : Word_Type) return Proxy_Index;
   --  Extract proxy index from an access value

   function Extract_Proxy_Index (Acc_Val : Word_Type) return Proxy_Index is
   begin
      return To_Proxy_Self_Rec (Acc_Val).Proxy_Ix;
   end Extract_Proxy_Index;

   function Extract_Stg_Rgn_Index (Acc_Val : Word_Type) return Stg_Rgn_Index;
   --  Extract stg rgn index from an access value

   function Extract_Stg_Rgn_Index (Acc_Val : Word_Type) return Stg_Rgn_Index is
   begin
      return To_Proxy_Self_Rec (Acc_Val).Stg_Rgn_Ix;
   end Extract_Stg_Rgn_Index;

   procedure Allocate_Proxy_Table
     (Shared_Rgn : Stg_Rgn_Ptr;
      Table_Last : Proxy_Index;
      Server_Index : Thread_Server_Index);
   --  Allocate proxy table from given region.
   --  Update both Shared_Rgn.Proxy_Table and <unshared_rgn>.Proxy_Table.
   --  Caller should release prior Proxy_Table, if any, after return.
   --  Caller should acquire lock on Shared_Rgn.

   procedure Allocate_Proxy_Table
     (Shared_Rgn : Stg_Rgn_Ptr;
      Table_Last : Proxy_Index;
      Server_Index : Thread_Server_Index) is

      subtype Constrained_Proxy_Tab is Proxy_Table_Rec (Table_Last);

      Proxy_Tab_Size_In_Words : constant Offset_Within_Chunk :=
        To_Size_In_Words (Constrained_Proxy_Tab'Size);

      Proxy_Tab_Addr : Object_Virtual_Address;
   begin
      if Debug_Access_Types then
         Put_Line ("Allocate_Proxy_Table of size " &
           Proxy_Tab_Size_In_Words'Img);
      end if;
      if Proxy_Tab_Size_In_Words <= Offset_Within_Area'Last then
         --  Allocate from region itself
         Allocate_From_Unshared_Stg_Rgn
           (Stg_Rgn => Shared_Rgn,
            Size_In_Words => Proxy_Tab_Size_In_Words,
            Obj_Addr => Proxy_Tab_Addr,
            Server_Index => Server_Index);

      else
         --  If Proxy table too big, use system allocator.
         --  TBF: Should we free the table when storage region is re-used,
         --       or put on a separate free list of such tables?
         Proxy_Tab_Addr :=
           From_Proxy_Tab_Ptr (Proxy_Table_Ptr'(new Constrained_Proxy_Tab));
      end if;

      declare
         --  Default-initialize the allocated space
         Const_Addr : constant Word_Ptr :=
                        Virtual_To_Physical_Address (Proxy_Tab_Addr);
         pragma Warnings (Off); --  default init wanted
         Allocated_Info : aliased Constrained_Proxy_Tab;
         for Allocated_Info'Address use Const_Addr.all'Address;
         pragma Warnings (On);
      begin
         --  Store new proxy table in shared part of region
         Shared_Rgn.Proxy_Table := To_Proxy_Tab_Ptr (Proxy_Tab_Addr);
         --  and in unshared part as well.
         Stg_Rgn_Table (Shared_Rgn.Index).Proxy_Table :=
           Shared_Rgn.Proxy_Table;
      end;
   end Allocate_Proxy_Table;

   procedure Free_Proxy_Table is
     new Ada.Unchecked_Deallocation (Proxy_Table_Rec, Proxy_Table_Ptr);

   procedure Expand_Proxy_Table
     (Shared_Rgn : Stg_Rgn_Ptr; Server_Index : Thread_Server_Index);
   --  Make sure the Proxy_Table has at least one available proxy id in it.
   --  Requires that caller has already acquired a lock on the region.

   procedure Expand_Proxy_Table
     (Shared_Rgn : Stg_Rgn_Ptr; Server_Index : Thread_Server_Index) is
   begin
      if Shared_Rgn.Proxy_Table = null then
         --  Very first proxy; allocate table.
         Allocate_Proxy_Table
           (Shared_Rgn => Shared_Rgn,
            Table_Last => Initial_Proxy_Table_Last,
            Server_Index => Server_Index);
         --  Initialize to empty.
         Shared_Rgn.Proxy_Table.Proxies := (others => Null_Virtual_Address);
      elsif Shared_Rgn.Proxy_Table.Next_Proxy >
        Shared_Rgn.Proxy_Table.Table_Last
      then
         --  Need to expand proxy table; double its size.
         declare
            Old_Table : Proxy_Table_Ptr := Shared_Rgn.Proxy_Table;
            New_Last : Proxy_Index := Old_Table.Table_Last * 2 + 1;
            Old_Table_As_Obj : constant Word_Type :=
              From_Proxy_Tab_Ptr (Old_Table);
         begin
            Allocate_Proxy_Table
              (Shared_Rgn => Shared_Rgn,
               Table_Last => New_Last,
               Server_Index => Server_Index);

            --  Initialize to copy of old table, followed by zeroes
            Shared_Rgn.Proxy_Table.Proxies (0 .. Old_Table.Table_Last) :=
              Old_Table.Proxies;
            Shared_Rgn.Proxy_Table.Proxies
              (Old_Table.Table_Last + 1 ..  New_Last) :=
                (others => Null_Virtual_Address);

            --  Free up old table, after filling in size
            if Old_Table.all'Size < Word_Type'Size * Offset_Within_Area'Last
            then
               Set_Large_Obj_Size
                 (Large_Obj_Addr => Old_Table_As_Obj,
                  Size => To_Size_In_Words (Old_Table.all'Size));

               Deallocate_From_Unshared_Stg_Rgn
                 (Shared_Rgn, Old_Table_As_Obj, Server_Index);
            else
               --  If size > max chunk, then use unchecked deallocation.
               Free_Proxy_Table (Old_Table);
            end if;
         end;
      end if;
   end Expand_Proxy_Table;

   type Get_Proxy_Id_Action is new Stg_Rgn_Action_Type with record
      Result : Word_Type := 0;
   end record;

   procedure Do_Stg_Rgn_Action
     (Act : in out Get_Proxy_Id_Action;
      Shared_Rgn : Stg_Rgn_Ptr;
      Server_Index : Thread_Server_Index);
   --  Define the action to be performed to get an available proxy ID

   procedure Do_Stg_Rgn_Action
     (Act : in out Get_Proxy_Id_Action;
      Shared_Rgn : Stg_Rgn_Ptr;
      Server_Index : Thread_Server_Index) is
      --  Algorithm
      --  Look for a proxy on the free list.
      --  If found, copy its proxy id, and deallocate it.
      --  If not found, bump the "last proxy" index, and if
      --  necessary, double the size of the proxy table.
      --  TBF: Allocate some of the proxy range to the unshared table.

      Proxy_Obj : constant Word_Type := Shared_Rgn.Free_Proxies;
   begin
      if Proxy_Obj /= 0 then
         --  Follow the chain of free proxies
         Shared_Rgn.Free_Proxies :=
           Fetch_Word (Proxy_Obj + Aliased_Obj_Payload_Offset);

         --  Get the "self" proxy index
         Act.Result := Fetch_Word (Proxy_Obj + Aliased_Obj_Self_Offset);

         --  Null out the slot in the table
         Shared_Rgn.Proxy_Table.Proxies
           (Extract_Proxy_Index (Act.Result)) := Null_Virtual_Address;

         --  Free the proxy object, since we don't need it
         --  as the caller already has an aliased object.
         Deallocate_From_Unshared_Stg_Rgn
           (Shared_Rgn, Proxy_Obj, Server_Index);

         return;
      end if;

      Expand_Proxy_Table (Shared_Rgn, Server_Index);

      declare
         Proxy_Ix : constant Proxy_Index := Shared_Rgn.Proxy_Table.Next_Proxy;
      begin
         --  Return self access based on next proxy index, and bump it.
         Act.Result :=
           To_Proxy_Self_Access
             ((Proxy_Ix => Proxy_Ix,
               Proxy_Gen_Num => 1,
               Stg_Rgn_Ix => Shared_Rgn.Index,
               Stg_Rgn_Gen_Num => Shared_Rgn.Gen_Num));

         Shared_Rgn.Proxy_Table.Next_Proxy := Proxy_Ix + 1;
      end;

   end Do_Stg_Rgn_Action;

   function Get_Available_Proxy_Id
     (Context : Exec_Context;
      Aliased_Obj_Stg_Rgn : Stg_Rgn_Ptr)
     return Word_Type;
   --  Get an available proxy index for specified region.

   function Get_Available_Proxy_Id
     (Context : Exec_Context;
      Aliased_Obj_Stg_Rgn : Stg_Rgn_Ptr)
     return Word_Type is
   begin
      if Aliased_Obj_Stg_Rgn.Owning_Server = Context.Server_Index then
         --  Look for a free proxy in the unshared part of stg rgn
         declare
            Proxy_Obj : constant Word_Type :=
              Aliased_Obj_Stg_Rgn.Free_Proxies;
            Result : Word_Type;
         begin
            if Proxy_Obj /= 0 then
               --  Follow the chain of free proxies
               Aliased_Obj_Stg_Rgn.Free_Proxies :=
                 Fetch_Word (Proxy_Obj + Aliased_Obj_Payload_Offset);

               --  Get the "self" proxy index
               Result := Fetch_Word (Proxy_Obj + Aliased_Obj_Self_Offset);

               --  Set it to zero so it will be freed normally.
               Store_Word (Proxy_Obj + Aliased_Obj_Self_Offset, 0);

               --  Null out the slot in the table
               Aliased_Obj_Stg_Rgn.Proxy_Table.Proxies
                 (Extract_Proxy_Index (Result)) := Null_Virtual_Address;

               --  Free the proxy object, since we don't need it
               --  as the caller already has an aliased object.
               Deallocate_From_Stg_Rgn
                 (Aliased_Obj_Stg_Rgn, Proxy_Obj, Context.Server_Index);

               return Result;
            end if;
         end;
      end if;

      --  Look in the shared part, and if necessary, allocate
      --  a completely new proxy id.

      declare
         Action : Get_Proxy_Id_Action;
      begin
         --  Invoke the action on the shared stg rgn
         Invoke_Shared_Stg_Rgn_Action
           (Aliased_Obj_Stg_Rgn.Shared_Part, Action, Context.Server_Index);

         --  Return the resulting proxy index
         return Action.Result;
      end;
   end Get_Available_Proxy_Id;

   type Get_Aliased_Obj_Action is new Stg_Rgn_Action_Type with record
      Result : Word_Type := 0;
   end record;

   procedure Do_Stg_Rgn_Action
     (Act : in out Get_Aliased_Obj_Action;
      Shared_Rgn : Stg_Rgn_Ptr;
      Server_Index : Thread_Server_Index);
   --  Define the action to be performed to get an aliased object

   procedure Do_Stg_Rgn_Action
     (Act : in out Get_Aliased_Obj_Action;
      Shared_Rgn : Stg_Rgn_Ptr;
      Server_Index : Thread_Server_Index) is

      Result : Word_Type := Shared_Rgn.Free_Proxies;
   begin
      --  Algorithm
      --  Look for a proxy on the free list.
      --  If found, return it.
      --  If not found, bump the "last proxy" index, and if
      --  necessary, double the size of the proxy table.
      --  TBF: Allocate some of the proxy range to the unshared table.
      --  Allocate an object and fill in the header
      if Result /= 0 then
         --  Follow the chain of free proxies
         Shared_Rgn.Free_Proxies :=
           Fetch_Word (Result + Aliased_Obj_Payload_Offset);

         Act.Result := Result;
         return;
      end if;

      --  Make sure we have at least one available proxy id
      Expand_Proxy_Table (Shared_Rgn, Server_Index);

      --  Allocate an aliased obj out of the Shared_Rgn
      Allocate_From_Unshared_Stg_Rgn
        (Stg_Rgn => Shared_Rgn,
         Size_In_Words => Aliased_Obj_Size_In_Words,
         Obj_Addr => Result,
         Server_Index => Server_Index);

      declare
         --  Get the new proxy index
         Proxy_Ix : constant Proxy_Index := Shared_Rgn.Proxy_Table.Next_Proxy;
      begin
         --  Fill in the proxy self access
         Store_Word (Result + Aliased_Obj_Self_Offset,
           To_Proxy_Self_Access
             ((Proxy_Ix => Proxy_Ix,
               Proxy_Gen_Num => 1,
               Stg_Rgn_Ix => Shared_Rgn.Index,
               Stg_Rgn_Gen_Num => Shared_Rgn.Gen_Num)));

         --  Fill in the proxy table
         Shared_Rgn.Proxy_Table.Proxies (Proxy_Ix) := Result;

         --  Bump the proxy id
         Shared_Rgn.Proxy_Table.Next_Proxy := Proxy_Ix + 1;

         --  Return the result in the Action object.
         Act.Result := Result;
      end;
   end Do_Stg_Rgn_Action;

   function Get_New_Aliased_Obj
     (Context : Exec_Context;
      Pool_Stg_Rgn : Stg_Rgn_Ptr;
      Aliased_Obj_Type : Type_Descriptor_Ptr)
     return Word_Type;
   --    Find/Create an aliased object in the same stg rgn as the Pool,
   --    initialize its self pointer to its index in the proxy table,
   --    initialize its type to the given Aliased_Obj_Type, and the
   --    Self field to the proxy index.

   function Get_New_Aliased_Obj
     (Context : Exec_Context;
      Pool_Stg_Rgn : Stg_Rgn_Ptr;
      Aliased_Obj_Type : Type_Descriptor_Ptr)
     return Word_Type is
      Result : Word_Type := 0;
   begin
      if Pool_Stg_Rgn.Owning_Server = Context.Server_Index then
         --  Look for a free proxy in the unshared part of stg rgn
         Result := Pool_Stg_Rgn.Free_Proxies;
      end if;

      if Result /= 0 then
         --  Follow the chain of free proxies
         Pool_Stg_Rgn.Free_Proxies :=
           Fetch_Word (Result + Aliased_Obj_Payload_Offset);

      else
         --  Look in the shared part, and if necessary, allocate
         --  a completely new aliased object.

         declare
            Action : Get_Aliased_Obj_Action;
         begin
            --  Invoke the action on the shared stg rgn
            Invoke_Shared_Stg_Rgn_Action
              (Pool_Stg_Rgn.Shared_Part, Action, Context.Server_Index);

            --  Get the returned object
            Result := Action.Result;
         end;
      end if;

      --  Fill in the type.
      Set_Large_Obj_Type_Info (Result, Aliased_Obj_Type.Index);

      --  Zero out the payload.
      Store_Word
        (Result + Aliased_Obj_Payload_Offset, Null_Virtual_Address);

      return Result;
   end Get_New_Aliased_Obj;

   function Get_Aliased_Obj_Ref
     (Context : Exec_Context;
      Acc_Val : Word_Type)
     return Word_Ptr;
   --    If access value is zero or Null_Value
   --    then raise a null-deref exception.
   --    Otherwise, identify aliased object given region-index/proxy-index.
   --    Check that its "self" part matches, if not, raise a dangling-ref
   --    exception.
   --    If everything checks out, return a "ref" to the aliased object.

   function Get_Aliased_Obj_Ref
     (Context : Exec_Context;
      Acc_Val : Word_Type)
     return Word_Ptr is
   begin
      if Debug_Access_Types then
         Put_Line ("Get_Aliased_Obj_Ref, Acc_Val = " & Hex_Image (Acc_Val));
      end if;

      if Acc_Val = 0 or else Acc_Val = Null_Value then
         raise Constraint_Error;
      end if;

      declare
         Obj_Stg_Rgn : constant Stg_Rgn_Ptr :=
           Stg_Rgn_Table (Extract_Stg_Rgn_Index (Acc_Val));
         Obj_Proxy_Id : constant Proxy_Index := Extract_Proxy_Index (Acc_Val);
         Obj_Proxy_Ref : constant Word_Ptr :=
           Obj_Stg_Rgn.Proxy_Table.Proxies (Obj_Proxy_Id)'Access;
      begin
         --  Verify that all of the "check" digits match.
         pragma Assert
           (Fetch_Word (Obj_Proxy_Ref.all + Aliased_Obj_Self_Offset) =
              Acc_Val);
         return Obj_Proxy_Ref;
      end;
   end Get_Aliased_Obj_Ref;

   ------------------------

   procedure Allocate_From_Pool
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  func Allocate
      --    (Pool : in out Storage_Pool'Class;
      --     Val : Desig_Type)
      --    return Access_Type is ...

      --  Algorithm:
      --    Find/Create an aliased object in the same stg rgn as the Pool,
      --    initialize its self pointer to its index in the proxy table,
      --    initialize its payload to a copy of the Val object.
      --    TBD: Might want to be a "move" at some point, though only
      --         useful if it is frequently already in the correct stg rgn.
      --    Return an access value that identifies its region and its
      --    proxy index within region.  The "check" digits should already
      --    be set properly if the aliased/proxy object already exists.
      --    Otherwise, initialize them to the region, region-gen-#,
      --    (new) proxy index, and gen# = 1.

      Pool_Obj : constant Word_Type := Fetch_Word (Params, 1);
      --  NOTE: Not passed by ref because Pool param is polymorphic.

      Pool_Stg_Rgn : constant Stg_Rgn_Ptr :=
        Stg_Rgn_Of_Large_Obj (Pool_Obj);
      Val : constant Word_Type := Fetch_Word (Params, 2);
      Acc_Type_Desc : constant Non_Op_Map_Type_Ptr := Static_Link;
      Desig_Type : constant Non_Op_Map_Type_Ptr :=
                       Acc_Type_Desc.Parameters (1).Data.Type_Desc;
      Desig_Obj : constant Word_Type :=
        Get_New_Aliased_Obj (Context, Pool_Stg_Rgn, Desig_Type);
      Copy_Of_Val : constant Word_Type :=
         Copy_Object
           (Context    => Context,
            Type_Desc  => Desig_Type,
            Object     => Val,
            Stg_Rgn_Of => Pool_Obj);
      Result : constant Word_Type :=
        Fetch_Word (Desig_Obj + Aliased_Obj_Self_Offset);
   begin
      Store_Word (Desig_Obj + Aliased_Obj_Payload_Offset, Copy_Of_Val);
      Store_Word (Params, 0, Result);
      if Debug_Access_Types then
         Put_Line ("Allocate_From_Pool, Result = " & Hex_Image (Result));
         Put_Line ("Dump of aliased obj: ");
         Dump_Obj_With_Type (Desig_Obj, Desig_Type);
      end if;
   end Allocate_From_Pool;

   procedure Tick_Access
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Tick_Access (Ali : in out Aliased_Obj) return Acc

      --  Algorithm:
      --    Check if aliased object "self" pointer already non-zero.
      --    if so, simply return that.  If is zero, then assign it
      --    an available proxy index, moving the prior aliased object
      --    with that index to the "usual" free list of available
      --    objects with two components (size = 3).
      --    If no "availble" proxy index, then assign a new one for
      --    the region of the aliased object.
      --  NOTE: We need to recognize aliased objects so when freeing them
      --        we do the "right" thing as far as bumping their proxy index
      --        and adding them to the aliased-obj free list, if their
      --        index is non-zero.  If zero, can just do the usual thing
      --        with the object.
      --        We could set a flag, as a side-effect of Tick_Access,
      --        inside the object's header, or change
      --        its type descriptor somehow, to make it recognizable.
      --        It may be that its module is already recognizable,
      --        meaning we don't really need to do anything.
      --        Conceivably a weird "size" could be used (e.g. 2 but non-poly).
      --  NOTE 2: Aliased obj *not* passed by ref, because not assignable.

      Aliased_Obj : constant Word_Type := Fetch_Word (Params, 1);
      Result : Word_Type;
   begin
      if Debug_Access_Types then
         Put_Line ("Tick_Access, Aliased_Obj = " & Hex_Image (Aliased_Obj));
         Dump_Obj_With_Type (Aliased_Obj, Large_Obj_Type_Desc (Aliased_Obj));
      end if;

      Result := Fetch_Word (Aliased_Obj + Aliased_Obj_Self_Offset);
      if Debug_Access_Types then
         Put_Line ("Tick_Access, Self_Access = " & Hex_Image (Result));
      end if;
      if Result = 0 or else Result = Null_Value then
         --  Not assigned a proxy index yet.
         --  Get an available proxy index in this region.
         declare
            Proxy_Stg_Rgn : constant Stg_Rgn_Ptr :=
              Stg_Rgn_Of_Large_Obj (Aliased_Obj);
         begin
            if Debug_Access_Types then
               Put_Line
                 ("Tick_Access, Calling Get_Available_Proxy_Id, region =" &
                  Proxy_Stg_Rgn.Index'Img);
            end if;
            Result := Get_Available_Proxy_Id (Context, Proxy_Stg_Rgn);
            if Debug_Access_Types then
               Put_Line
                 ("Tick_Access, New proxy self acc =" & Hex_Image (Result));
            end if;

            --  Save it in aliased obj
            Store_Word (Aliased_Obj + Aliased_Obj_Self_Offset, Result);
            --  Fill in slot in proxy table
            Proxy_Stg_Rgn.Proxy_Table.Proxies (Extract_Proxy_Index (Result)) :=
              Aliased_Obj;
         end;
      end if;
      --  Return proxy index (which is used as the access value).
      Store_Word (Params, 0, Result);
   end Tick_Access;

   procedure Deref_Var
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  func Deref (Acc)
      --    return aliased Aliased_Obj is ...

      --  Algorithm:
      --    If access value is zero or Null_Value
      --    then raise a null-deref exception.
      --    Otherwise, identify aliased object given region-index/proxy-index.
      --    Check that its "self" part matches, if not, raise a dangling-ref
      --    exception.
      --    If everything checks out, return a "ref" to the aliased object.
      Acc_Val : constant Word_Type := Fetch_Word (Params, 1);
      pragma Assert (Acc_Val /= 0 and then Acc_Val /= Null_Value);
      Result : constant Word_Ptr := Get_Aliased_Obj_Ref (Context, Acc_Val);
   begin
      Store_Word (Params, 0, Result.all);  --  *Not* returned by reference
   end Deref_Var;

   procedure Deref_Const
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  func Deref_Const (Acc)
      --    return aliased constant Aliased_Obj is ...
      --  Algorithm:
      --    Basically same as Deref_Var, except we return a constant
      --    ref rather than a "var" ref to the aliased obj.
   begin
      --  Identical algorithm!
      Deref_Var (Context, Params, Static_Link);
   end Deref_Const;

   -------------------

   type Large_Atomic_Obj is record
      Header : Word_Type;
      Value : Word_Type;
      pragma Atomic (Value);
   end record;

   type Large_Atom_Ptr is access all Large_Atomic_Obj;
   for Large_Atom_Ptr'Storage_Size use 0;
   pragma No_Strict_Aliasing (Large_Atom_Ptr);

   function To_Atom_Ptr is new Ada.Unchecked_Conversion
     (Word_Type, Large_Atom_Ptr);

   use type C.size_t;
   type uint64 is mod 2**64;
   for uint64'Size use 64;

   --  Memory models from C11

   Relaxed : constant := 0;
   Consume : constant := 1;
   Acquire : constant := 2;
   Release : constant := 3;
   Acq_Rel : constant := 4;
   Seq_Cst : constant := 5;
   Last    : constant := 6;

   subtype Mem_Model is Integer range Relaxed .. Last;

   function C_Atomic_Exchange_8
     (M_Ptr : System.Address;
      Val : Word_Type;
      S_Model : Mem_Model) return Word_Type;
   pragma Import (C, C_Atomic_Exchange_8, "__atomic_exchange_8");
   --  "8-byte" version of atomic exchange.

   pragma Assert (Word_Type'Size = 8 * 8);

   function Builtin_Atomic_Exchange
     (Item_Address : System.Address; Value : Word_Type) return Word_Type;

   function Builtin_Atomic_Exchange
     (Item_Address : System.Address; Value : Word_Type) return Word_Type is
   begin
      return C_Atomic_Exchange_8
        (M_Ptr => Item_Address,
         Val => Value,
         S_Model => Seq_Cst);
   end Builtin_Atomic_Exchange;

   function C_Atomic_Compare_Exchange_8
     (M_Ptr, E_Ptr : System.Address; Desired : Word_Type;
      S_Model : Mem_Model; F_Model : Mem_Model) return C.unsigned_char;
   pragma Import
     (C, C_Atomic_Compare_Exchange_8, "__atomic_compare_exchange_8");
   --  "8-byte" version of atomic compare exchange

   function Builtin_Atomic_Compare_Exchange
       (Item_Address : System.Address;
        Prior_Address : System.Address;
        Desired : Word_Type) return Boolean;

   function Builtin_Atomic_Compare_Exchange
       (Item_Address : System.Address;
        Prior_Address : System.Address;
        Desired : Word_Type) return Boolean
   is
      Result : C.unsigned_char;
      use type C.unsigned_char;
   begin
      Result := C_Atomic_Compare_Exchange_8
        (M_Ptr => Item_Address,
         E_Ptr => Prior_Address,
         Desired => Desired,
         S_Model => Seq_Cst,
         F_Model => Seq_Cst);
      return C.unsigned_char'Pos (Result) /= 0;
   end Builtin_Atomic_Compare_Exchange;

   procedure Atomic_Set_Value
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  procedure Set_Value(A : aliased in out Atomic; Val : Content_Type)
      --   with Import, External_Name => #atomic_set_value;
      --  --  Atomically set the new value
      Atom_Addr : constant Word_Ptr := Fetch_Word_Ptr (Params, 0);
      pragma Assert (Large_Obj_Size (Atom_Addr.all) = 2);

      Val : constant Word_Type := Fetch_Word (Params, 1);
   begin
      To_Atom_Ptr (Atom_Addr.all).Value := Val;
   end Atomic_Set_Value;

   procedure Atomic_Value
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  function Value(A : aliased Atomic) return Content_Type
      --    with Import, External_Name => #atomic_value;
      --  -- Return the current value
      Atom_Addr : constant Word_Ptr := Fetch_Word_Ptr (Params, 1);
      pragma Assert (Large_Obj_Size (Atom_Addr.all) = 2);
   begin
      Store_Word (Params, 0, To_Atom_Ptr (Atom_Addr.all).Value);
   end Atomic_Value;

   procedure Atomic_Exchange
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  function Exchange(A : aliased in out Atomic;
      --    New_Val : Content_Type) return Content_Type
      --    with Import, External_Name => #atomic_exchange;
      --  --  Result := Value(A); Set_Value(A, New_Val)
      Atom_Addr : constant Word_Ptr := Fetch_Word_Ptr (Params, 1);
      pragma Assert (Large_Obj_Size (Atom_Addr.all) = 2);

      New_Val : constant Word_Type := Fetch_Word (Params, 2);
      Result : Word_Type;
   begin
      Result := Builtin_Atomic_Exchange
                  (Item_Address => To_Atom_Ptr (Atom_Addr.all).Value'Address,
                   Value => New_Val);
      Store_Word (Params, 0, Result);
   end Atomic_Exchange;

   procedure Atomic_Compare_And_Exchange
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  function Compare_And_Exchange(A : aliased in out Atomic;
      --    Expected_Val, New_Val : Content_Type) return Content_Type
      --    with Import, External_Name => #atomic_compare_and_exchange;
      --  --  If Value(A) = Expected_Val, then set Value(A) to New_Val.
      --  --  In any case, return the prior value.
      Atom_Addr : constant Word_Ptr := Fetch_Word_Ptr (Params, 1);
      pragma Assert (Large_Obj_Size (Atom_Addr.all) = 2);

      Expected_Val : Word_Type := Fetch_Word (Params, 2);
      pragma Atomic (Expected_Val);

      New_Val : constant Word_Type := Fetch_Word (Params, 3);

      Was_Exchanged : constant Boolean :=
        Builtin_Atomic_Compare_Exchange
         (Item_Address => To_Atom_Ptr (Atom_Addr.all).Value'Address,
          Prior_Address => Expected_Val'Address,
          Desired => New_Val);
      pragma Unreferenced (Was_Exchanged);
   begin
      Store_Word (Params, 0, Expected_Val);
   end Atomic_Compare_And_Exchange;

   procedure Atomic_Fetch_And_Add
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  function Fetch_And_Add(A : aliased in out Atomic;
      --    Value : Content_Type) return Content_Type
      --    with Import, External_Name => #atomic_fetch_and_add;
      --  -- Result := Value (A); Set_Value(A) = Result + Value
      Atom_Addr : constant Word_Ptr := Fetch_Word_Ptr (Params, 1);
      pragma Assert (Large_Obj_Size (Atom_Addr.all) = 2);

      Value_To_Add : constant Word_Type := Fetch_Word (Params, 2);

      Old_Value : aliased Word_Type := To_Atom_Ptr (Atom_Addr.all).Value;
      pragma Atomic (Old_Value);

      New_Value : Word_Type := Old_Value + Value_To_Add;
   begin
      --  Keep iterating until the exchange succeeds
      while not Builtin_Atomic_Compare_Exchange
                  (Item_Address => To_Atom_Ptr (Atom_Addr.all).Value'Address,
                   Prior_Address => Old_Value'Address,
                   Desired => New_Value)
      loop
         New_Value := Old_Value + Value_To_Add;
      end loop;

      --  OK, we successfully set the value, atomically, to
      --  Old_Value + Value_To_Add
      Store_Word (Params, 0, Old_Value);
   end Atomic_Fetch_And_Add;

   use Strings;
begin

   Builtins.Register_Builtin
     (String_Lookup ("#ada202x_bool_from_univ"),
      Bool_From_Univ'Access);
   Builtins.Register_Builtin
     (String_Lookup ("#ada202x_bool_to_univ"), Bool_To_Univ'Access);

   Register_Builtin
     (Strings.String_Lookup ("#raise_exception_occurrence"),
        Raise_Exception_Occurrence'Access);
   Register_Builtin
     (Strings.String_Lookup ("#exception_name"),
        Exception_Name'Access);

   Register_Builtin
     (String_Lookup ("#allocate_from_pool"), Allocate_From_Pool'Access);
   Register_Builtin
     (String_Lookup ("#tick_access"), Tick_Access'Access);
   Register_Builtin
     (String_Lookup ("#deref_var"), Deref_Var'Access);
   Register_Builtin
     (String_Lookup ("#deref_const"), Deref_Const'Access);

   Register_Builtin
     (Strings.String_Lookup ("#atomic_set_value"),
        Atomic_Set_Value'Access);
   Register_Builtin
     (Strings.String_Lookup ("#atomic_value"),
        Atomic_Value'Access);
   Register_Builtin
     (Strings.String_Lookup ("#atomic_exchange"),
        Atomic_Exchange'Access);
   Register_Builtin
     (Strings.String_Lookup ("#atomic_compare_and_exchange"),
        Atomic_Compare_And_Exchange'Access);
   Register_Builtin
     (Strings.String_Lookup ("#atomic_fetch_and_add"),
        Atomic_Fetch_And_Add'Access);

end PSC.Interpreter.Ada202x_Builtins;
