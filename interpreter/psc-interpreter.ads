------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2022, AdaCore                     --
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

--  Package defining the virtual machine instructions for ParaSail, and an
--  interpreter for them.

pragma Style_Checks (All_Checks);
--  Turn off subprogram ordering, not used for this unit

with Ada.Calendar;
with Ada.Streams;
with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

with PSC.Hash_Tables;
with PSC.Languages;
with PSC.Per_File_Strings;
with PSC.Strings;
with PSC.String_Streams;
with PSC.Source_Positions;
with PSC.Trees;                --  For debugging
with PSC.Vectors;

with System;

pragma Elaborate (PSC.Hash_Tables);
pragma Elaborate (PSC.Strings);
pragma Elaborate (PSC.Vectors);

package PSC.Interpreter is

   --  Whether run-time checks are to be performed
   Doing_Run_Time_Checks : Boolean := True;

   Virt_Is_Phys : constant Boolean := True;
   --  Set to True to make virtual addrs = physical addrs

   type Opcode_Enum is (
     Skip_Op,
     Call_Op,
     Indirect_Call_Op,
     Return_Op,
     Copy_Word_Op,
     Copy_Address_Op,
     Store_Address_Op,
     Create_Obj_Op,
     Assign_Word_Op,
     Swap_Obj_Op,
     Move_Obj_Op,
     Make_Copy_In_Stg_Rgn_Op,
     Store_Local_Null_Op,
     Store_Large_Local_Null_Op,
     Store_Null_Of_Same_Stg_Rgn_Op,
     Is_Null_Op,
     Not_Null_Op,
     Store_Int_Lit_Op,
     Store_Real_Lit_Op,
     Store_Char_Lit_Op,
     Store_Enum_Lit_Op,
     Store_Str_Lit_Op,
     Store_Operation_Desc_Op,
     Store_Type_Related_Obj_Op,
     Store_Type_Related_Addr_Op,
     Start_Parallel_Op,
     Start_Handled_Op,
     Add_Parallel_Op,
     Prepare_To_Exit_Parallel_Op,
     Wait_For_Parallel_Op,
     Start_Parallel_Call_Op,
     Add_Parallel_Call_Op,
     Create_Lock_For_Obj_Op,
     Create_Tcb_Op,
     Create_Polymorphic_Obj_Op,
     Unwrap_Polymorphic_Obj_Op,
     Select_Polymorphic_Ancestor_Part_Op,
     Select_Ancestor_Part_Op,
     If_Op,
     Call_Nested_Block_Op,
     Check_Nested_Block_Op,
     Exit_Op,
     Begin_Nested_Block_Op,
     Check_Not_Null_Op,
     Case_Op, Loop_Op,  --  TBD: These two are not used yet
     Declare_Obj_Op);
      --  The various op codes in the instruction set

   type Area_Base_Indicator is
     range 0 .. 2**15 - 1;
   --  Indicator of which base register to use

   Zero_Base  : constant Area_Base_Indicator := 0;
   Local_Area : constant Area_Base_Indicator := 1;
   --  Relative to current local area
   Param_Area : constant Area_Base_Indicator := 2;
   --  Relative to param area of current operation
   Type_Area  : constant Area_Base_Indicator := 3;
   --  Relative to enclosing type for current operation;
   --  This is equivalent to the outermost enclosing local area.
   Const_Area : constant Area_Base_Indicator := 4;
   --  This is a compile-time-known constant.
   --  The offset is an index into a Const_Info_Array.

   subtype Base_Registers is Area_Base_Indicator
     range 10_000 .. 19_999;
   --  Relative to some temp base register kept in the current Local_Area
   --  NOTE: These generally hold the "value" of a large object, and
   --        word offsets are used relative to such a base register to
   --        fetch a component of the large object.

   subtype Phys_Base_Registers is Area_Base_Indicator
     range 20_000 .. 29_999;
   --  Relative to some temp base register kept in the current Local_Area,
   --  which contains a physical, rather than a virtual, address.
   --  NOTE: These generally hold an address of an object, and the offset
   --        used with them is almost always 0, as a way of achieving indirect
   --        access to an object, which might be large or small.
   --        The only time we use a non-zero offset with a "physical" base
   --        register is when accessing the parameters that are
   --        appended to a (pico) thread control block (TCB), when spawning
   --        a parameterized out-of-line operation to run in parallel.

   subtype Any_Base_Register is Area_Base_Indicator
     range Base_Registers'First .. Phys_Base_Registers'Last;
   --  A union of both kinds of base registers.

   subtype Enclosing_Param_Areas is Area_Base_Indicator range 16 .. 31;
   --  Relative to a param area of an enclosing operation. The second word of
   --  a local area points at its param area if it has nested operations. This
   --  walks up through the static links in the local areas, and then when it
   --  reaches the right local area, it gets the param area from there.
   subtype Enclosing_Local_Areas is Area_Base_Indicator range 32 .. 47;
   --  Relative to a local area of an enclosing block or operation. The first
   --  word of a local area points at its enclosing block, or at the outermost
   --  level, the enclosing type area. The type area has a null in its first
   --  word.
   subtype Enclosing_Type_Areas is Area_Base_Indicator range 48 .. 63;
   --  Relative to a type area for an enclosing module. Type areas are linked
   --  together via the Enclosing_Type link.

   --  Make sure that we can use "mod" to extract the up-level count, from both
   --  Enclosing_Param_Areas and Enclosing_Local_Areas
   Max_Up_Levels : constant Area_Base_Indicator :=
                     Enclosing_Param_Areas'Last
                       - Enclosing_Param_Areas'First + 1;
   pragma Assert
     (Max_Up_Levels =
      Enclosing_Local_Areas'Last - Enclosing_Local_Areas'First + 1);
   pragma Assert (Enclosing_Param_Areas'First mod Max_Up_Levels = 0);
   pragma Assert (Enclosing_Local_Areas'First mod Max_Up_Levels = 0);
   pragma Assert (Enclosing_Type_Areas'First mod Max_Up_Levels = 0);

   type Code_Nesting_Level is range 0 .. Max_Up_Levels - 1;

   type Word_Type is
     range -2 ** 63 .. +2 ** 63 - 1;
   --  Memory is organized as an array of 64-bit "words"

   Word_Size    : constant := Word_Type'Size;  --  Size in bits

   Word_SU_Size : constant := Word_Size / System.Storage_Unit;
   --  Size in storage units

   Null_Value : constant Word_Type := Word_Type'First;
   --  Most negative value used to represent "null" int

   subtype Non_Null_Value is
     Word_Type range Word_Type'First + 1 .. Word_Type'Last;

   Null_Unsigned_64 : constant Word_Type := 16#BAD1_FEED_DEAD_BEEF# - 2**64;
   --  We use an artificial null value, which can be returned to indicate
   --  an end of iteration, but otherwise doesn't cause failures.

   Null_Float_Value : constant Word_Type := -1;  --  all ones is an IEEE NaN

   type Real_Word is digits 15;
   for Real_Word'Size use Word_Size;  --  This should be "confirming"

   subtype Univ_Real is Real_Word;  --  TBD: May be ratio at some point

   function To_Univ_Real is
      new Ada.Unchecked_Conversion (Word_Type, Univ_Real);
   function From_Univ_Real is
     new Ada.Unchecked_Conversion (Univ_Real, Word_Type);

   Null_Real_Value : constant Univ_Real := To_Univ_Real (Null_Float_Value);
   --  IEEE NaN

   type Unsigned_Word_Type is
     mod 2 ** Word_Type'Size;  --  Unsigned type, same size as Word_Type
   function To_Unsigned_Word is
     new Ada.Unchecked_Conversion (Word_Type, Unsigned_Word_Type);
   function From_Unsigned_Word is
     new Ada.Unchecked_Conversion (Unsigned_Word_Type, Word_Type);

   subtype Object_Virtual_Address is Word_Type;
   --  Virtual addresses are 64-bits, with high 32-bits identifying the chunk,
   --  and low 32-bits identifying the offset within a chunk. Actually, to
   --  avoid signed-ness issues, we presume (normal) virtual addresses are
   --  non-negative, as are chunk indices and chunk offsets , so we only
   --  allow for 2**31 chunks and 2**31 (64-bit) words/chunk.
   --  Negative virtual addresses are presumed to represent something special,
   --  including types, null values, and optionally-small values.

   Null_Virtual_Address : constant Object_Virtual_Address := 0;
   --  Note that a (large or small) "null" value need not be equal to
   --  Null_Virtual_Address (and generally is not).

   type Chunk_Index is range 0 .. 1_000_000;
   --  Index of chunk in global region-chunk table.

   Chunk_Divisor : constant := 2 ** 32;
   pragma Assert
     ((Chunk_Index'Pos (Chunk_Index'Last) + 1) * Chunk_Divisor <=
      Word_Type'Pos (Word_Type'Last) + 1);
   --  We divide by the chunk divisor to convert a virtual address into
   --  a chunk index. We use Chunk_Divisor as a modulus to extract offset
   --  within chunk from a virtual address. Or equivalently, we subtract
   --  Chunk.Starting_Virtual_Address (see below) from the virtual address
   --  to produce the offset.

   type Offset_Within_Chunk is range 0 .. 2 ** 31 - 1;
   pragma Assert (Offset_Within_Chunk'Last < Chunk_Divisor);
   --  We are limited to the overall number of bits given to the chunk.

   subtype Offset_Within_Area is Offset_Within_Chunk range 0 .. 2 ** 16 - 1;
   --  Offset within area identified by base register

   Abs_Large_Null_Chunk_Index : constant := 2 ** 25;
   --  This is negated to represent the chunk number used for a large null.

   Large_Null_Chunk : constant Object_Virtual_Address :=
                                      -Abs_Large_Null_Chunk_Index;
   --  Chunk index used in virtual addresses to represent "large" nulls.
   --  NOTE: We purposely make this more negative than the maximum
   --        type index to avoid collisions with type-relative addressing.
   --  NOTE2: This needs to be consistent with what is used for Univ_Strings.

   Chunk_Mask : constant Unsigned_Word_Type :=
     Unsigned_Word_Type'(-Chunk_Divisor);
   --  Mask to isolate the chunk index, as an unsigned value

   Large_Null_Base_Value : constant Unsigned_Word_Type :=
      Unsigned_Word_Type'(-(Abs_Large_Null_Chunk_Index * Chunk_Divisor)) + 1;
   --  Null value after removing region index, as an unsigned value

   Local_Area_Local_Data_Offset : constant Offset_Within_Area := 3;
   --  Offset to where local data starts in local area

   Max_Offset_For_Base_Register : constant Offset_Within_Area :=
     Offset_Within_Area (Base_Registers'Last - Base_Registers'First);
   --  Limit on local-area offset that can be turned into a base register

   type Word_Array is
     array (Offset_Within_Chunk range <>) of aliased Word_Type;
   --  Memory is an array of "Word"s

   type Word_Ptr is access all Word_Type;
   pragma No_Strict_Aliasing (Word_Ptr);

   function Word_To_Word_Ptr (Word : Word_Type) return Word_Ptr;
      --  Convert a Word to a Word_Ptr, for a "by-ref" parameter.

   function Word_Ptr_To_Word (Addr : Word_Ptr) return Word_Type;
      --  Convert a Word_Ptr to a Word, for a "by-ref" parameter.

   Type_Indicator : constant Word_Type;
      --  This is a special value stored in the first word of every
      --  type descriptor.

   type VM_Obj_Kind is
     (No_VM_Obj_Kind, Local_Kind, Param_Kind, Component_Kind, Temp_Kind);
      --  Kind of VM Object.  Locals, Params, and Components are user-declared
      --  objects/components.  Temps are created by compiler.

   Max_VM_Obj : constant := 2**31 - 1;

   type VM_Obj_Unique_Num is range 0 .. Max_VM_Obj;
      --  Unique number assigned to each VM object

   Indir_Count_Max : constant := 3;

   type VM_Obj_Indir_Count is range -1 .. Indir_Count_Max;
      --  -1 means asking for address of object
      --  0 means asking for content of object
      --  +1 means asking for deref of object
      --  +2 means asking for deref of deref of object (ever used?).

   type VM_Obj_Id_Type (Kind : VM_Obj_Kind := No_VM_Obj_Kind) is record
      --  Type used to identify an object within the PSVM instructions
      Is_Var : Boolean := False;
         --  Indicates whether object can be updated after initialization
      Level  : Code_Nesting_Level := 0;
         --  Level of (base) of object
      Indir  : VM_Obj_Indir_Count := 0;
         --  Indicate whether getting address, or dereference, or just value
      Num    : VM_Obj_Unique_Num := 0;
         --  VM register for object value (Indir=0) or address (Indir=1)
      case Kind is
         when Param_Kind | Component_Kind =>
            Offset : Offset_Within_Area := 0;
            --  Offset within parameter area or composite object
         when Local_Kind =>
            First_Call_Param_Num : VM_Obj_Unique_Num := 0;
            --  Only used on a call, indicates first VM register for params.
         when others =>
            null;
      end case;
   end record;
   pragma Pack (VM_Obj_Id_Type);

   No_VM_Obj_Id : constant VM_Obj_Id_Type := (No_VM_Obj_Kind, False, 0, 0, 0);

   type Object_Locator is record
   --  Combination of base register identifier and offset, this kind of locator
   --  can appear within an instruction. These are used both for identifying
   --  code and for identifying data. When used for identifying code, the
   --  locator is pointing at something which contains the "address" of the
   --  code.
      Base   : Area_Base_Indicator := Zero_Base;
      Offset : Offset_Within_Area  := 0;
      VM_Obj_Id : VM_Obj_Id_Type := No_VM_Obj_Id;
   end record;

   procedure Obj_Locator_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : in Object_Locator);
   for Object_Locator'Write use Obj_Locator_Write;
   --  Write the Base and Offset, ignore VM_Obj_Id

   procedure Obj_Locator_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Object_Locator);
   for Object_Locator'Read use Obj_Locator_Read;
   --  Read the Base and Offset, fill in No_VM_Obj_Id for VM_Obj_Id.

   Null_Object_Locator : constant Object_Locator :=
                                    (Zero_Base, 0, No_VM_Obj_Id);

   function Is_Null_Obj_Locator (Locator : Object_Locator) return Boolean;
   --  Return True if Locator is a null object locator

   Unknown_Func_Type_Obj_Locator : constant Object_Locator :=
     (Zero_Base, Offset_Within_Area'Last, No_VM_Obj_Id);
         --  Special locator of func-type

   type Obj_Locator_Array is array (Positive range <>) of Object_Locator;

   function Base_Register
     (Local_Area_Offset : Offset_Within_Area) return Area_Base_Indicator;
   --  Return an Area_Base_Indicator for the base register represented by the
   --  local at the given offset within the local area.

   function Phys_Base_Register
     (Local_Area_Offset : Offset_Within_Area) return Area_Base_Indicator;
   --  Return an Area_Base_Indicator for the phys-address base register
   --  represented by the local at the given offset within the local area.

   function Area_Base_Image (Area : Area_Base_Indicator) return String;
   --  Return string representation for area base

   function Obj_Locator_Image (Locator : Object_Locator) return String;
   --  Return string representation for object locator

   type Stg_Rgn_Index is range 0 .. 2 ** 16 - 1;
   --  Every region is assigned a unique region index.

   type Stg_Rgn_Generation is mod 2 ** 16;
   --  Every time a storage region is re-used this gen number is bumped.

   type Type_Index is range 0 .. 2 ** 16 - 1;
   --  Every type descriptor is assigned a unique index.

   type Lock_Obj_Index is range 0 .. 2 ** 15 - 1;
   --  Concurrent objects have an index of a lock object

   Large_Obj_Header_Size : constant Offset_Within_Area := 1;
   --  Size of "header" of a large object, and also the offset of first
   --  component within a "large" object. Header components are the region
   --  ptr, size, type info, and lock.

   --  Sizes of control blocks used in Parallel_Ops
   Thread_Master_Size : constant Offset_Within_Area := 3;
   Thread_Control_Block_Size : constant Offset_Within_Area := 11;

   --  Unique indices for heavyweight server threads
   Max_Thread_Servers : constant := 100;

   type Thread_Server_Index_Base is range 0 .. 2 * Max_Thread_Servers;
   subtype Thread_Server_Index is Thread_Server_Index_Base
     range 1 .. Max_Thread_Servers;
   --  Unique index to identify a thread server. Each thread server has its own
   --  "local" thread queue but can steal from other thread queues.
   --  NOTE: We need to be able to represent 2 * Max_Thread_Servers because we
   --        use a wrap-around computation when deciding which server to
   --        steal from.

   Delay_Queue_Server_Index : constant Thread_Server_Index :=
     Thread_Server_Index'First;
   --  This server index is used by the delay server, which just finishes
   --  the picothread spawned to do a delay. It doesn't really do any "real"
   --  execution.

   Main_Thread_Server_Index : constant Thread_Server_Index :=
     Delay_Queue_Server_Index + 1;
   --  This server index is used by the "main" thread, invoked from the command
   --  line.

   type Stg_Rgn;
   type Stg_Rgn_Ptr is access all Stg_Rgn;

   type Stg_Rgn_Chunk;
   type Stg_Rgn_Chunk_Ptr is access all Stg_Rgn_Chunk;

   type Reclamation_Info_Ptr is private;

   type Object_Address is record
   --  This identifies a chunk and an offset within it.
   --  This is the kind of value that a base register holds.
   --  This is the kind of "address" we use at run-time.
   --  These don't appear in instructions, since we don't have any
   --  statically-allocated chunks to point at.
      Enclosing_Chunk : Stg_Rgn_Chunk_Ptr;
      Offset : Offset_Within_Chunk := 0;
   end record;

   Null_Object_Address : constant Object_Address :=
     (Enclosing_Chunk => null,
      Offset => 0);

   type Stg_Rgn_Manager_Ptr is private;
   --  This is a pointer to a protected type which manages
   --  allocation/deallocation from its associated storage region.

   subtype Proxy_Index is Offset_Within_Chunk range 0 .. 2 ** 24 - 1;
   --  Identifies element of proxy table.
   --  There is also an 8-bit proxy "generation" number
   --  which must match to verify the access value is not a dangling
   --  reference.

   type Proxy_Generation is mod 2 ** 8;
   --  This is bumped each time a proxy is re-used.

   type Proxy_Table_Rec (Table_Last : Proxy_Index) is record
      Next_Proxy : Proxy_Index := 0;
      Proxies : Word_Array (0 .. Table_Last);
      --  A proxy is a 3-word object.  It is allocated out of the
      --  storage region, like any other such object.
      --  But there is a separate free list of such objects,
      --  and they have a "self" field that includes the region
      --  index, the proxy index, a 16-bit generation # for the region,
      --  and an 8-bit generation # for the proxy index.
      --  When a proxy is freed, the gen number is bumped.
      --  Similarly, when a region is freed, its gen number is bumped.
   end record;

   type Proxy_Table_Ptr is access all Proxy_Table_Rec;

   type Proxy_Self_Rec is record
      --  This represents the layout of the proxy-self-address word
      --  which is also used for access values.
      Proxy_Ix : Proxy_Index := 0;
      Proxy_Gen_Num : Proxy_Generation := 0;
      Stg_Rgn_Ix : Stg_Rgn_Index := 0;
      Stg_Rgn_Gen_Num : Stg_Rgn_Generation := 0;
   end record;

   pragma Pack (Proxy_Self_Rec);
   for Proxy_Self_Rec'Size use Word_Type'Size;

   type Proxy_Self_Ptr is access all Proxy_Self_Rec;

   function To_Proxy_Self_Ptr is new Ada.Unchecked_Conversion
     (Word_Ptr, Proxy_Self_Ptr);

   --  Aliased obj consists of a self proxy-index, and a payload
   --  NOTE: Aliased_Obj is declared as a *limited* type,
   --        meaning assignment is not defined, meaning that
   --        (as of 3-Jul-2020) we do *not* pass by reference.
   Aliased_Obj_Self_Offset : constant Offset_Within_Area :=
     Large_Obj_Header_Size;
   Aliased_Obj_Payload_Offset : constant Offset_Within_Area :=
     Aliased_Obj_Self_Offset + 1;
   Aliased_Obj_Size_In_Words : constant Offset_Within_Area :=
     Aliased_Obj_Payload_Offset + 1;

   type Stg_Rgn is record
   --  A storage region is made up of a set of region "chunks" each of which
   --  is a contiguous chunk of storage. When a region is reclaimed, all of
   --  the chunks get reclaimed as well.
   --  Most chunks are a standard size (e.g. 4096 bytes, 512 64-bit words).
   --  Larger chunks are allocated if a given object needs to have a larger
   --  contiguous piece.
   --  Allocation within a chunk is generally single-threaded.
   --  Multiple threads may allocate from a single region, but generally not
   --  from a single chunk (at least not at the same time).
   --  Multiple threads can be referencing parameter lists that reside in
   --  a single chunk. It is the allocation which we generally want to be
   --  single-threaded within a chunk.

      Null_Value            : Word_Type := Null_Virtual_Address;
      --  Null for this region

      Index                 : Stg_Rgn_Index := 0;
      --  Index into global table of regions

      Gen_Num               : Stg_Rgn_Generation := 0;
      --  This is bumped each time a region is re-used.

      Owning_Server         : Thread_Server_Index := Thread_Server_Index'Last;
      --  Thread that can use the unshared part of the region without
      --  any synchronization

      Is_Constant_Stg_Rgn   : Boolean := False;
      --  True if this region is used for large constants that are
      --  never reclaimed.

      Enclosing_Stg_Rgn     : Stg_Rgn_Ptr;
      --  Also used to point to next free region if on Free_Stg_Rgns chain.

      Saved_Open_Master     : Word_Ptr := null;
      --  This is the value of Context.Open_Master on entry to the routine
      --  or nested block.

      First_Chunk           : Stg_Rgn_Chunk_Ptr;
      --  First chunk in circularly-linked list, or null if list empty.

      Reclamation           : Reclamation_Info_Ptr;
      --  Table of storage from deallocated objects, available for re-use

      Associated_Local_Area : Word_Ptr;
      --  Local area associated with region.
      --  NOTE: This is primarily for debugging

      Proxy_Table           : Proxy_Table_Ptr;
      --  Pointer to a table of "proxies" used to implement
      --  access values.  This is the same in the shared and unshared parts.

      Free_Proxies          : Word_Type := 0;
      --  First free proxy object, if any.
      --  The Unshared and Shared parts have separate free lists.

      Shared_Part           : Stg_Rgn_Ptr;
      --  Pointer to a matching record used for allocation/deallocation
      --  for this region when performed by non-owners.

      Manager               : Stg_Rgn_Manager_Ptr;
      --  Protected object which handles allocation/release of objects
      --  and storage-region chunks.
      --  This is null in the "unshared" part of storage region.
   end record;

   type Stg_Rgn_Chunk (Chunk_Length : Offset_Within_Chunk) is record
   --  We pass around references to region-chunks and starting offsets within
   --  them. This is instead of passing around "bare" addresses. A "base"
   --  register contains a reference to a chunk and an offset. We then add
   --  in the offset appearing in the base/offset pair, to produce the total
   --  offset within the chunk.
   --  The Starting_Virtual_Address is the virtual address of the first word
   --  of the chunk. This is added into the offset to produce a virtual address
   --  of any word within the chunk. Virtual addresses are what we store in
   --  "memory". Chunk pointers only exist in "registers."
      Associated_Stg_Rgn       : Stg_Rgn_Ptr;
      Prev_Chunk               : Stg_Rgn_Chunk_Ptr;
      Next_Chunk               : Stg_Rgn_Chunk_Ptr;
      Starting_Virtual_Address : Object_Virtual_Address;

      Index                    : Chunk_Index := 0;
      --  Index into chunk table

      Last_In_Use              : Offset_Within_Chunk := 0;
      --  Last item of chunk now in use

      Space_Left               : Offset_Within_Chunk := 0;
      --  Defined to equal Chunk_Length - Last_In_Use, to speed up searches.

      Mark                     : Offset_Within_Chunk := 0;
      --  Value of "Last_In_Use" when Associated_Stg_Rgn started using chunk.
      --  At Mark is the old value for Mark.
      --  At Mark-1 is the old value for "Depth_Of_Mark"

      Depth_Of_Mark            : Natural := 0;
      --  The number of levels the mark remained the same as chunk was
      --  "borrowed" from enclosing regions.
      --  On a release back to enclosing region:
      --   if Depth_Of_Mark > 0, then we merely decrement
      --   Depth of Mark and reset Last_In_Use to Mark.
      --   If Depth_Of_Mark = 0, then we restore Last_In_Use to Mark-2,
      --   and restore Mark and Depth_Of_Mark from Mark/Mark-1.
      --  When "borrowing" a chunk from an enclosing region:
      --   if Last_In_Use matches Mark, then just increment Depth_Of_Mark
      --   and change Associated_Stg_Rgn.
      --   if Last_In_Use > Mark, then save Depth_Of_Mark and Mark at
      --   Last_In_Use+1,+2 and set Mark:=Last_In_Use:=Last_In_Use+2,
      --   and set Depth_Of_Mark to zero.
      Data : Word_Array (1 .. Chunk_Length);
   end record;

   procedure Install_Chunk
     (Chunk : Stg_Rgn_Chunk_Ptr;
      Index : Chunk_Index);
   --  Install chunk at given index in global table.
   --  Requires: Chunk with given Index didn't already exist

   function "+"
     (Base   : Object_Address;
      Offset : Offset_Within_Area) return Object_Address;
   --  Add Offset to Base

   function "+"
     (Base   : Object_Virtual_Address;
      Offset : Offset_Within_Area) return Object_Virtual_Address;
   --  Add Offset to Base virtual address

   pragma Inline ("+");

   function Hex_Image
     (Addr              : Object_Virtual_Address;
      Underscores_Every : Natural := 4) return String;
   --  Produce hex image of given address, with underscores inserted
   --  periodically

   function Hex_Image
     (Addr              : Word_Ptr;
      Underscores_Every : Natural := 4) return String;
   --  Produce hex image of given address, with underscores inserted
   --  periodically

   function Integer_Value (Img : String) return Word_Type;
   --  Convert image to a value.
   --  Allow 0xFF, 0b01, 16#ff#, etc.
   --  Recognize "null" and return Null_Value

   function Unsigned_Image (Val : Unsigned_Word_Type) return String;
   --  Return Val as an image, removing leading blank from Ada 'image.

   function Unsigned_Value (Img : String) return Unsigned_Word_Type;
   --  Convert image to an unsigned value.
   --  Allow 0xFF, 0b01, 16#ff#, etc.

   function Real_Image (Val : Univ_Real) return String;
   --  Return Val as an image, removing leading blank from Ada 'image.
   --  Recognize null value and return "null"

   function Real_Value (Img : String) return Univ_Real;
   --  Return Img as a Univ_Real value.
   --  Recognize "null" and return Null_Real_Value

   function Obj_Address_Image (Addr : Object_Address) return String;
   --  Return image of given object-address as a pair

   --  Types and operations for reconstructing type descriptors,
   --  values, and strings from their stream representation

   use type Ada.Streams.Stream_Element_Offset;

   subtype Stream_Rep_Type is
     Ada.Streams.Stream_Element_Array (0 .. 2**16 - 1);

   type Stream_Rep_Ptr is access all Stream_Rep_Type;

   type Stream_Local_Count is new Per_File_Strings.Local_String_Count;
   subtype Stream_Local_Index is
     Stream_Local_Count range 1 .. Stream_Local_Count'Last;

   type Stream_Rep_Table_Type is
     array (Stream_Local_Index) of Stream_Rep_Ptr;

   subtype Stream_Element_Array_Ptr is
     String_Streams.Stream_Element_Array_Ptr;

   --  Run-time Per-file string table
   subtype String_Table_Type is Per_File_Strings.Local_String_Map_Type;

   type String_Table_Ptr is access String_Table_Type;

   procedure Reconstruct_Strings
     (Num_Entries : Stream_Local_Count;
      Stream_Rep_Table : Stream_Rep_Table_Type;
      String_Table  : out String_Table_Type);
   --  Reconstruct Univ_String values from their stream representations
   pragma Export (Ada, Reconstruct_Strings, "_psc_reconstruct_strings");

   function Reconstruct_Value (Stream_Rep : Stream_Rep_Ptr;
     String_Table : String_Table_Ptr) return Word_Type;
   --  Reconstruct a ParaSail value given a stream representation
   --  and a string table.
   pragma Export (Ada, Reconstruct_Value, "_psc_reconstruct_value");

   type Type_Descriptor (Has_Op_Map : Boolean);
   type Type_Descriptor_Ptr is access all Type_Descriptor;

   subtype Non_Op_Map_Type_Ptr is Type_Descriptor_Ptr (Has_Op_Map => False);
   subtype Op_Map_Type_Ptr is Type_Descriptor_Ptr (Has_Op_Map => True);

   procedure Type_Desc_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Type_Descriptor_Ptr);
   for Type_Descriptor_Ptr'Write use Type_Desc_Ptr_Write;
   --  Write out the type-desc index and a has_op_map flag
   --  rather than the type desc ptr itself

   procedure Type_Desc_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Type_Descriptor_Ptr);
   for Type_Descriptor_Ptr'Read use Type_Desc_Ptr_Read;
   --  Read the type-desc index and has_op_map flag and return
   --  a pointer to a type desc that might not be fully filled in yet.

   ------------------
   -- Exec_Context --
   ------------------

   type Exec_Context is tagged record
   --  This encapsulates the information needed to convert an Object_Locator
   --  into an Object_Virtual_Address, by providing the "base registers,"
   --  directly or indirectly.
   --  NOTE: This is tagged so we can take 'Access for debugging purposes
      Local_Null          : Word_Type := Null_Virtual_Address;
      --  Large Null Value for local region

      Enclosing_Type      : Non_Op_Map_Type_Ptr;
      Local_Stg_Rgn       : Stg_Rgn_Ptr;  --  Local region

      --  Control block for current pico-thread executing with this context
      Control_Area        : Word_Ptr := null;

      --  Index of server executing the current pico-thread
      Server_Index        : Thread_Server_Index := Thread_Server_Index'Last;

      --  This is non-null when a master has been started but not yet awaited.
      --  We must not return from a routine or exit a nested block when this is
      --  non-null.
      Open_Master         : Word_Ptr := null;

      --  The following fields are not used by compiled code:
      Params              : Word_Ptr;
      Local_Area          : Word_Ptr;
      Local_Area_Length   : Offset_Within_Area := 0;
      Start_Callee_Locals : Offset_Within_Area := 0;
   end record;

   type Exec_Context_Ptr is access constant Exec_Context;
   type Exec_Context_RW_Ptr is access all Exec_Context;

   function Get_New_Local_Stg_Rgn
     (New_Local_Area    : Word_Ptr;
      Server_Index      : Thread_Server_Index) return Stg_Rgn_Ptr;
   --  Return a region that can be used as a local region.
   --  Check the free list, then allocate a new region if nothing free.

   function Get_New_Local_Stg_Rgn
     (New_Local_Area    : Object_Address;
      Server_Index      : Thread_Server_Index) return Stg_Rgn_Ptr;
   --  Return a region that can be used as a local region.
   --  Check the free list, then allocate a new region if nothing free.
   --  TBD: This verion is obsolescent

   procedure Release_Stg_Rgn
     (Stg_Rgn : in out Stg_Rgn_Ptr);
   --  Release all chunks of region back to enclosing region, or back to master
   --  list of free chunks.
   --  Reset Last_In_Use, Mark, and Depth_Of_Mark as appropriate.
   --  Put region on free list, and null out Stg_Rgn parameter.

   procedure Initialize_Stg_Rgn
     (Context : in out Exec_Context;
      Local_Area : Word_Ptr);
   --  Allocate a new storage region associated with given Local_Area
   --  Resulting stg_rgn pointer is stored in the Context.
   --  Save the current value of Open_Master and set it to null.
   --  NOTE: This should be called at the beginning of a compiled
   --        ParaSail routine or nested block if Code.Uses_Stg_Rgn is true.
   pragma Export (Ada, Initialize_Stg_Rgn, "_psc_initialize_stg_rgn");

   procedure Finalize_Stg_Rgn
     (Context : in out Exec_Context;
      Local_Area : Word_Ptr);
   --  Finalize the storage region associated with the given local area.
   --  Context is updated to point to enclosing region.
   --  Restore the saved value of Open_Master.
   --  NOTE: This should be called at the end of a compiled
   --        ParaSail routine or nested block if Code.Uses_Stg_Rgn is true.
   pragma Export (Ada, Finalize_Stg_Rgn, "_psc_finalize_stg_rgn");

   function Allocate_Stg_Rgn_Chunk
     (Min_Size          : Offset_Within_Area;
      Server_Index      : Thread_Server_Index;
      Enclosing_Stg_Rgn : Stg_Rgn_Ptr := null) return Stg_Rgn_Chunk_Ptr;
   --  Allocate a region chunk having at least the given amount of space.
   --  Chunk.Last_In_Use+1 gives the index of the first available element
   --  of Chunk.Data.

   procedure Release_Stg_Rgn_Chunk
     (Chunk             : in out Stg_Rgn_Chunk_Ptr;
      Server_Index      : Thread_Server_Index;
      Enclosing_Stg_Rgn : Stg_Rgn_Ptr := null);
   --  Release chunk back to the enclosing region or free list.

   Global_Data_Stg_Rgn : Stg_Rgn_Ptr;
   --  Stg_Rgn where compile-time computations are stored.

   Global_Const_Stg_Rgn : Stg_Rgn_Ptr;
   --  Stg_Rgn where long-lived constants are stored and never reclaimed.
   --  These still need to be copied on assignment since otherwise we would
   --  lose information about the stg rgn associated with a variable.
   --  TBF: This argues for making the "special" univ-int values similar
   --       to "special" univ-string values, so they can carry a region
   --       within their value.

   Global_Stack_Chunk  : Stg_Rgn_Chunk_Ptr;
   --  Stg_Rgn chunk used for primary stack

   procedure Initialize_Global_Stack_Chunk;
   --  Initialize the global stack chunk, if not yet initialized.

   procedure Initialize_Global_Data_Stg_Rgn;
   --  Initialize Global_Data_Stg_Rgn and Global_Const_Stg_Rgn if not yet done

   function Add (Base : Word_Ptr; Offset : Offset_Within_Area) return Word_Ptr;
   --  Do a word-pointer add of base + offset
   pragma Inline (Add);

   function To_Virtual_Address
     (Address : Object_Address) return Object_Virtual_Address;
   --  Create a virtual address from a chunk/offset style of address

   function Content_Of_Virtual_Address
     (Virtual : Object_Virtual_Address) return Word_Type;
   --  Return word at given virtual address
   pragma Export
     (Ada, Content_Of_Virtual_Address, "content_of_virtual_address");

   procedure Delay_Tcb_Until
     (Tcb_Addr    : Word_Ptr;
      Delay_Until : Ada.Calendar.Time);
   --  Add TCB to the delay queue, to wake up when the wall clock reaches
   --  Delay_Until.

   function Fetch_Word
     (Context      : Exec_Context;
      Word_Locator : Object_Locator) return Word_Type;
   --  Fetch word at given locator
   pragma Export (Ada, Fetch_Word, "fetch_word");

   function Fetch_Word
     (Virtual : Object_Virtual_Address) return Word_Type
     renames Content_Of_Virtual_Address;
   --  A convenient renaming...

   function Fetch_Word
     (Base : Word_Ptr; Offset : Offset_Within_Area) return Word_Type;
   --  Fetch word at Base + Offset

   function Fetch_Word
     (Base : Object_Virtual_Address; Offset : Offset_Within_Area)
     return Word_Type;
   --  Fetch word at Base + Offset

   function Content_Of_Physical_Address
     (Physical : Word_Ptr; Offset : Offset_Within_Area := 0) return Word_Type
     renames Fetch_Word;
   --  Return word at given physical address
   pragma Inline (Content_Of_Physical_Address);

   function Fetch_Nonnull_Word
     (Context      : Exec_Context;
      Word_Locator : Object_Locator) return Word_Type;
   --  Fetch word at given locator.
   --  Complain if it is null.

   function Fetch_Nonnull_Word
     (Virtual : Object_Virtual_Address) return Word_Type;
   --  Return word at given virtual address, but complain if null.

   function Fetch_Nonnull_Word
     (Base : Word_Ptr; Offset : Offset_Within_Area) return Word_Type;
   --  Fetch word at Base + Offset; complain if null.

   function Fetch_Word_Ptr
     (Context      : Exec_Context;
      Word_Locator : Object_Locator) return Word_Ptr;
   --  Fetch word_ptr at given location

   function Fetch_Word_Ptr
     (Base : Word_Ptr; Offset : Offset_Within_Area) return Word_Ptr;
   --  Fetch word_ptr at Base + Offset

   function Fetch_Real
     (Context      : Exec_Context;
      Real_Locator : Object_Locator) return Univ_Real;
   --  Fetch Real at given locator

   function Fetch_Real
     (Virtual : Object_Virtual_Address) return Univ_Real;
   --  Return Real at given virtual address

   function Fetch_Real
     (Base : Word_Ptr; Offset : Offset_Within_Area) return Univ_Real;
   --  Fetch real at Base + Offset

   function Fetch_Nonnull_Real
     (Context      : Exec_Context;
      Real_Locator : Object_Locator) return Univ_Real;
   --  Fetch Real at given locator.
   --  Complain if it is null.

   function Fetch_Nonnull_Real
     (Virtual : Object_Virtual_Address) return Univ_Real;
   --  Fetch Real at given virtual address.
   --  Complain if it is null.

   function Fetch_Nonnull_Real
     (Base : Word_Ptr; Offset : Offset_Within_Area) return Univ_Real;
   --  Fetch real at Base + Offset; complain if null.

   procedure Init_Static_Link
     (New_Local_Area : Word_Ptr;
      Static_Link    : Word_Ptr := null);
   --  Initialize static link in a new local area.
   --  By default initialize to null, signifying end of static chain, with no
   --  enclosing type descriptor.

   function Object_To_Physical_Address
     (Addr : Object_Address) return Word_Ptr;
   --  Convert object address to a physical address
   pragma Export (Ada, Object_To_Physical_Address, "_psc_object_to_physical");
   --  Give this an external name for use in the debugger.

   procedure Set_Large_Obj_Type_Info
     (Large_Obj_Addr : Object_Virtual_Address;
      Type_Id        : Type_Index);
   --  Set type associated with large object

   procedure Set_Large_Obj_Lock_Obj
     (Large_Obj_Addr : Object_Virtual_Address;
      Lock_Obj       : Lock_Obj_Index);
   --  Set lock obj associated with large object

   procedure Set_Tcb_Was_Queued
     (Tcb_Addr   : Word_Ptr;
      Was_Queued : Boolean);
   --  Set "Thread_Was_Queued" flag of TCB

   procedure Store_Real
     (Context      : Exec_Context;
      Real_Locator : Object_Locator;
      Value        : Univ_Real);
   --  Store Real at given locator

   procedure Store_Real
     (Address : Object_Address;
      Value   : Univ_Real);
   --  Store value at given object address

   procedure Store_Real
     (Address : Object_Virtual_Address;
      Value   : Univ_Real);
   --  Store value at given virtual address

   procedure Store_Real
     (Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Univ_Real);
   --  Store value into Base[Offset]

   procedure Store_Word
     (Context      : Exec_Context;
      Word_Locator : Object_Locator;
      Value        : Word_Type);
   --  Store word at given locator

   procedure Store_Word
     (Address : Object_Address;
      Value   : Word_Type);
   --  Store value at given object address

   procedure Store_Word
     (Address : Object_Virtual_Address;
      Value   : Word_Type);
   --  Store value at given virtual address

   procedure Store_Word
     (Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Word_Type);
   --  Store value into Base[Offset]

   procedure Store_Word_Ptr
     (Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Word_Ptr);
   --  Store word_ptr into Base[Offset]

   function String_To_Word (Str : String;
                            Null_For_Rgn : Word_Type;
                            Server_Index : Thread_Server_Index)
     return Word_Type;
   --  Convert String to a Univ_String

   function To_Univ_String_Null (Obj_In_Rgn : Word_Type) return Word_Type;
   --  Return "null" for a Univ_String, given an object in the right region.

   function To_Univ_String_Word
     (U_Str        : Strings.U_String;
      Null_For_Rgn : Word_Type) return Word_Type;
   --  Return a word representing a Univ_String, given a U_String

   function To_Univ_String_Word
     (Index        : Strings.U_String_Index;
      Null_For_Rgn : Word_Type) return Word_Type;
   --  Return a word representing a Univ_String, given a U_String_Index

   function Tcb_Call_Can_Be_Queued
     (Tcb_Addr : Word_Ptr) return Boolean;
   --  Return "Call_Can_Be_Queued" flag of TCB

   function Tcb_Was_Queued
     (Tcb_Addr : Word_Ptr) return Boolean;
   --  Return "Thread_Was_Queued" flag of TCB

   function Virtual_To_Object_Address
     (Virtual : Object_Virtual_Address) return Object_Address;
   pragma Export (Ada, Virtual_To_Object_Address,
      "_psc_virtual_to_object_address");
   --  Convert Object_Virtual_Address to Object_Address

   function Locator_To_Physical_Address
     (Context : Exec_Context;
      Locator : Object_Locator) return Word_Ptr;
   --  Convert Object_Locator to Word_Ptr

   function Locator_To_Physical_Address_Exported  --  Avoid overloaded name
     (Context : Exec_Context;
      Base : Area_Base_Indicator;
      Offset : Offset_Within_Area) return Word_Ptr;
   --  Convert Base/Offset of an Object_Locator to Word_Ptr
   pragma Export (Ada, Locator_To_Physical_Address_Exported,
      "_psc_locator_to_physical_address");

   function To_Word_Type (Str : Strings.U_String) return Word_Type;
   --  Convert Index(Str) to Word_Type

   function To_U_String (Val : Word_Type) return Strings.U_String;
   --  Convert U_String_Index as a word to corresponding U_String

   function Virtual_To_Physical_Address
     (Addr : Object_Virtual_Address) return Word_Ptr;
   pragma Export (Ada, Virtual_To_Physical_Address, "_psc_large_obj_addr");
   --  Given a large object, get the "physical" address where it
   --  resides in memory.

   function Word_To_String (Word : Word_Type) return String;
   --  Convert ParaSail Univ_String to Ada String.

   function Word_To_Wide_Wide_String (Word : Word_Type)
     return Wide_Wide_String;
   --  Convert ParaSail Univ_String to Ada Wide_Wide_String.

   function Enum_Word_To_String (Word : Word_Type) return String;
   --  Convert ParaSail Univ_Enumeration to Ada String.

   type Code_Offset is range -2 ** 15 .. +2 ** 15 - 1;

   type Nested_Block_Outcome is record
      Level : Integer range -2 ** 15 .. 2 ** 15 - 1 := 0;
      Skip : Code_Offset := 0;
   end record;
   --  Level is high 16 bits, Skip is low 16 bits
   for Nested_Block_Outcome use record
      Skip at 0 range 0 .. 15;
      Level at 2 range 0 .. 15;
   end record;
   type Nested_Block_Outcome_As_Int is new Interfaces.Unsigned_32;

   subtype Code_Length_Type is Code_Offset range 0 .. Code_Offset'Last;
   subtype Code_Index is Code_Offset range 1 .. Code_Offset'Last;

   type Code_Offset_Array is array (Positive range <>) of Code_Offset;
   type Code_Offset_Array_Ptr is access Code_Offset_Array;
   --  Used for recording what skip counts can occur after a wait-for-parallel

   type Code_Block_Descriptor is record
      Pc_Offset           : Code_Offset := 0;
      --  This is an offset relative to the instruction with the reference,
      --  or relative to the start of the routine for boundary conditions.

      Uses_Queuing        : Boolean := False;
      --  This indicates whether code block has any calls on
      --  operations that might use queuing.

      Uses_Stg_Rgn        : Boolean := False;
      --  This indicates whether code block includes any operations
      --  that need a local stg rgn.

      Local_Area_Length   : Offset_Within_Area := 0;
      --  How big a local area to pre-allocate

      Start_Callee_Locals : Offset_Within_Area := 0;
      --  How much of local area is devoted to current procedure,
      --  and hence at what offset callee locals begin.

      Nesting_Level       : Code_Nesting_Level := 0; --  > 0 in a nested block
      --  The nesting level of the code relative to the module level.
      --  This is 0 for top-level operations, and > 0 for nested routines and
      --  nested blocks.
      --  NOTE: The static link refers to a type descriptor if this is 0,
      --        and refers to an enclosing local area if > 0.
   end record;

   Null_Code_Block_Descriptor : aliased constant Code_Block_Descriptor :=
     (Pc_Offset           => 0,
      Uses_Queuing        => False,
      Uses_Stg_Rgn        => False,
      Local_Area_Length   => 0,
      Start_Callee_Locals => 0,
      Nesting_Level       => 1);

   type Code_Block_RW_Ptr is access all Code_Block_Descriptor;
   pragma No_Strict_Aliasing (Code_Block_RW_Ptr);

   type Code_Block_Ptr is access constant Code_Block_Descriptor;

   type Ordering is (Less, Equal, Greater, Unordered);
   --  Result of Compare operation ("=?")

   subtype Ordered is Ordering range Less .. Greater;

   type Condition_Bit_Mask is mod 2 ** 16;
   --  Type used in conditionals to indicate when comparison is "True"

   --  If condition returns a value of type Ordering
   Compare_Less      : constant Condition_Bit_Mask := 2 ** 0;
   Compare_Equal     : constant Condition_Bit_Mask := 2 ** 1;
   Compare_Greater   : constant Condition_Bit_Mask := 2 ** 2;
   Compare_Unordered : constant Condition_Bit_Mask := 2 ** 3;

   Compare_Not_Equal : constant Condition_Bit_Mask :=
     Compare_Less or Compare_Greater or Compare_Unordered;

   Ordering_Mask : constant array (Ordering) of Condition_Bit_Mask :=
     (Less      => Compare_Less,
      Equal     => Compare_Equal,
      Greater   => Compare_Greater,
      Unordered => Compare_Unordered);

   --  If condition returns a value of type Boolean
   Boolean_Is_False : constant Condition_Bit_Mask := 2 ** 0;
   Boolean_Is_True  : constant Condition_Bit_Mask := 2 ** 1;

   --  Mapping from normal Boolean to Condition_Bit_Mask
   If_Condition_Table : constant array (Boolean) of Condition_Bit_Mask :=
     (False => Boolean_Is_False, True => Boolean_Is_True);

   type Direction is (
     Unordered_Dir,
     Forward_Dir,
     Reverse_Dir,
     Concurrent_Dir);
   --  Used to indicate direction of iteration

   function Direction_Image (Dir : Direction) return String;
   --  Return "unordered," "forward," "reverse," or "concurrent."

   type Basic_Routine_Index is range 0 .. 2 ** 16 - 1;
   --  This is the "basic" version of Routine_Index which has the default
   --  'Read/'Write.

   type Routine_Index is new Basic_Routine_Index;
   --  Every routine is assigned a unique index.

   procedure Routine_Index_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Routine_Index'Base);
   for Routine_Index'Write use Routine_Index_Write;
   --  Write out a Routine_Index as a pair (module-id, operation-id)

   procedure Routine_Index_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Routine_Index'Base);
   for Routine_Index'Read use Routine_Index_Read;
   --  Read in (module-id, operation-id) and look up in Id->Routine map

   type Block_Region_Index is  --  Same as Symbols.Region_Index
     range 0 .. 2**31 - 1;
   No_Block_Region_Index : Block_Region_Index := 0;

   Max_Params : constant := 31;

   subtype Param_Index_Subtype is Natural range 0 .. Max_Params;

   --  Info on the locked parameter, if any, in a call
   type Locked_Param_Info_Type is record
      Param_Index    : Param_Index_Subtype := 0;
      Is_By_Ref      : Boolean := False;
      Is_Var         : Boolean := False;
      Is_Queued_Call : Boolean := False;
   end record;
   pragma Pack (Locked_Param_Info_Type);

   Null_Locked_Param_Info : constant Locked_Param_Info_Type :=
     (0, others => False);

   type Locked_Param_Info_As_Byte_Type is mod 2 ** 8;

   pragma Assert (Locked_Param_Info_Type'Size =
     Locked_Param_Info_As_Byte_Type'Size);

   function Locked_Param_Info_As_Byte
     (Locked_Param_Info : Locked_Param_Info_Type)
       return Locked_Param_Info_As_Byte_Type;
   --  Convert locked-param-info record into a byte

   type Instruction (Op : Opcode_Enum := Skip_Op) is record
   --  A single (virtual) machine instruction
      Source_Pos : Source_Positions.Source_Position :=
                     Source_Positions.Null_Source_Position;
      case Op is
         when Skip_Op | Exit_Op =>
            Skip_Count : Code_Offset := 0;
            --  Number of instructions to skip.  Is a No_Op if 0.
            case Op is
               when Exit_Op =>
                  Level_Diff : Natural := 0;
               --  This indicates how many nested block levels the exit
               --  statement is exiting from.
               when others =>
                  null;
            end case;

         when Call_Op | Indirect_Call_Op |
              Call_Nested_Block_Op | Check_Nested_Block_Op =>
            Params      : Object_Locator;
            Static_Link : Object_Locator;
            --  Points to enclosing local area or type area
            case Op is
               when Call_Op | Indirect_Call_Op =>
                  Call_Target        : Object_Locator;
                  Locked_Param_Info  : Locked_Param_Info_Type;
               --  If Locked_Param_Info.Param_Index /= zero,
               --  indicates which parameter is locked
               --  during the execution of the call, and indicates
               --  whether parameter is a variable, and/or is passed by ref.
               --  If zero, then there is no lock acquired, though it might be
               --  a lock-free operation on a concurrent object.
                  Precond_Proved     : Boolean := False;
                  --  Set to True when preconditions have been proved
                  Output_Inited_Null : Boolean := False;
                  --  If True, then output was inited to null
                  case Op is
                     when Call_Op =>
                        Target_Index       : Routine_Index;
                           --  Index of called routine, which might be abstract
                     when Indirect_Call_Op =>
                        Indirect_Num_In_Params  : Natural;
                           --  Number of input parameters in the indir call
                        Indirect_Num_Out_Params : Natural;
                           --  Number of output parameters in the indir call
                     when others =>
                        null;
                  end case;

               when Call_Nested_Block_Op | Check_Nested_Block_Op =>
                  Code_Block : aliased Code_Block_Descriptor;
                  --  This describes the nested block of code
                  case Op is
                     when Check_Nested_Block_Op =>
                        Assertion_Str : Strings.U_String_Index :=
                          Strings.Null_U_String_Index;
                        --  A string representation of assertion being checked
                        Assertion_Proved : Boolean := False;
                        --  Set to True if static analysis proved assertion
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;

         when Return_Op =>
            Postcond_Proved : Boolean := False;
            --  Set to True when postcondition proved

         when Store_Int_Lit_Op                    |
              Store_Real_Lit_Op                   |
              Store_Str_Lit_Op                    |
              Store_Char_Lit_Op                   |
              Store_Enum_Lit_Op                   |
              Store_Operation_Desc_Op             |
              Copy_Word_Op                        |
              Copy_Address_Op                     |
              Store_Address_Op                    |
              Assign_Word_Op                      |
              Swap_Obj_Op                         |
              Store_Local_Null_Op                 |
              Store_Large_Local_Null_Op           |
              Store_Null_Of_Same_Stg_Rgn_Op       |
              Create_Obj_Op                       |
              Create_Lock_For_Obj_Op              |
              Create_Polymorphic_Obj_Op           |
              Unwrap_Polymorphic_Obj_Op           |
              Store_Type_Related_Obj_Op           |
              Store_Type_Related_Addr_Op          |
              Select_Polymorphic_Ancestor_Part_Op |
              Select_Ancestor_Part_Op             |
              Make_Copy_In_Stg_Rgn_Op             |
              Move_Obj_Op                         |
              Is_Null_Op                          |
              Not_Null_Op                         |
              Check_Not_Null_Op                   |
              Declare_Obj_Op                      =>

            Destination : Object_Locator;
            --  For Assign_Word_Op, existing object is freed if Type_Info
            --  implies is large
            --  For Swap_Obj_Op, existing object is copied and then freed
            --  if Type_Info implies is large and objects not in same region.
            --  For Create_Polymorphic_Obj_Op, identifies object to be
            --  replaced with a polymorphic object which wraps it.
            --  For Check_Not_Null_Op, is object being tested.
            Dest_Name   : Strings.U_String_Index :=
                            Strings.Null_U_String_Index;
            --  If non-null, identifies object being initialized/tested
            case Op is
               when Store_Local_Null_Op | Check_Not_Null_Op =>
                  --  Type of object determines whether or not object is small,
                  --  and specific kind of null if small.
                  --  Stg_Rgn is the current local region.
                  Null_Type_Info : Object_Locator;
                  case Op is
                     when Check_Not_Null_Op =>
                        Not_Null_Proved : Boolean := False;
                        --  Set to True if static analysis proves not null
                     when others =>
                        null;
                  end case;
               when Store_Large_Local_Null_Op =>
                  --  Type unknown, but known to be large
                  --  Use region associated with given local area
                  Local_Addr : Object_Locator;
               when Store_Int_Lit_Op =>
                  Int_Value  : Word_Type;
               when Store_Char_Lit_Op =>
                  Char_Value : Word_Type;
               when Store_Real_Lit_Op =>
                  Real_Value : Univ_Real;
               when Store_Str_Lit_Op =>
                  Str_Value  : Strings.U_String_Index;
                  Existing_Str_In_Stg_Rgn : Object_Locator;
                     --  Determines storage region for literal
               when Store_Enum_Lit_Op =>
                  Enum_Value : Strings.U_String_Index;
               when Declare_Obj_Op =>
                  --  May eventually have more information for declared object
                  Is_By_Ref         : Boolean := False;
                  Is_Var            : Boolean := False;
                  Declare_Type_Info : Object_Locator;

               when Store_Operation_Desc_Op             |
                    Copy_Word_Op                        |
                    Copy_Address_Op                     |
                    Store_Address_Op                    |
                    Assign_Word_Op                      |
                    Swap_Obj_Op                         |
                    Store_Null_Of_Same_Stg_Rgn_Op       |
                    Create_Obj_Op                       |
                    Create_Polymorphic_Obj_Op           |
                    Unwrap_Polymorphic_Obj_Op           |
                    Store_Type_Related_Obj_Op           |
                    Store_Type_Related_Addr_Op          |
                    Select_Polymorphic_Ancestor_Part_Op |
                    Select_Ancestor_Part_Op             |
                    Make_Copy_In_Stg_Rgn_Op             |
                    Move_Obj_Op                         |
                    Is_Null_Op                          |
                    Not_Null_Op                         =>

                  Source : Object_Locator;
                  --  For Store_Null, Create_Obj, and Store_Operation_Desc
                  --  determines region if non-null
                  --  For Assign and Make_Copy, is object to be copied.
                  --  For Select_*Ancestor_Part_Op, is object whose ancestor
                  --  part is to be extracted.

                  Might_Be_Null : Boolean;
                  --  Indicates whether source might contain a null value

                  case Op is
                     when Store_Operation_Desc_Op =>
                        --  Operation_Desc includes both static link and
                        --  routine locator.
                        --  A "null" operation is represented by a null
                        --  static link and a null locator.
                        Operation_Static_Link : Object_Locator;
                        Operation_Locator     : Object_Locator;

                     when Assign_Word_Op                      |
                          Swap_Obj_Op                         |
                          Move_Obj_Op                         |
                          Store_Null_Of_Same_Stg_Rgn_Op       |
                          Create_Obj_Op                       |
                          Create_Polymorphic_Obj_Op           |
                          Unwrap_Polymorphic_Obj_Op           |
                          Store_Type_Related_Obj_Op           |
                          Store_Type_Related_Addr_Op          |
                          Select_Polymorphic_Ancestor_Part_Op |
                          Select_Ancestor_Part_Op             |
                          Make_Copy_In_Stg_Rgn_Op             |
                          Is_Null_Op                          |
                          Not_Null_Op                         =>

                        Type_Info : Object_Locator;
                        --  Determines whether small or large.
                        --  For Store_Null, determines kind of null if small.
                        --  For Create_Obj_Op determines type of obj to create
                        --   if large, and kind of null if small.
                        --  For Select_*Ancestor_Part_Op, determines type
                        --  of ancestor part to be extracted. This becomes
                        --  a simple Copy_Word_Op if the component-extension
                        --  levels are the same between the object's type-id
                        --  and the ancestor type.
                        --  For Unwrap_Polymorphic_Obj_Op, this is the
                        --  non-polymorphic type for which we are testing.
                        case Op is
                           when Make_Copy_In_Stg_Rgn_Op =>
                              Existing_Obj_In_Stg_Rgn : Object_Locator;
                           --  Determines region for Make_Copy
                           when Select_Ancestor_Part_Op |
                             Unwrap_Polymorphic_Obj_Op  |
                             Store_Type_Related_Obj_Op  |
                             Store_Type_Related_Addr_Op =>
                              Source_Type_Info : Object_Locator;
                           --  Determines type of Source object
                              case Op is
                                 when Select_Ancestor_Part_Op =>
                                    Ancestor_Lvalue : Boolean;
                                 when others =>
                                    null;
                              end case;
                           when Select_Polymorphic_Ancestor_Part_Op =>
                              Polymorphic_Ancestor_Lvalue : Boolean;
                           when others =>
                              null;
                        end case;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;

         when If_Op =>
            If_Source     : Object_Locator;
            If_Condition  : Condition_Bit_Mask;
            Skip_If_False : Code_Offset;  --  Number of instructions to skip
            --  if condition not satisfied.
            --  Negative for loop back.

         when Start_Parallel_Op      |
              Start_Handled_Op       |
              Add_Parallel_Op        |
              Wait_For_Parallel_Op   |
              Start_Parallel_Call_Op |
              Add_Parallel_Call_Op   |
              Create_Tcb_Op          |
              Prepare_To_Exit_Parallel_Op =>

            Parallel_Master : Object_Locator;
            --  This is where the count/list of subthreads hangs

            case Op is
               when Start_Parallel_Op      |
                    Start_Handled_Op       |
                    Add_Parallel_Op        |
                    Start_Parallel_Call_Op |
                    Add_Parallel_Call_Op   |
                    Create_Tcb_Op          =>

                  Parallel_Control     : Object_Locator;
                  --  This is where the control block is stored, followed by
                  --  the parameters, if any.
                  Parallel_Static_Link : Object_Locator;
                  --  Location of enclosing local area/type area

                  Num_In_Params  : Natural;
                  --  Number of input parameters in the parallel call

                  Num_Out_Params : Natural;
                  --  Number of output parameters in the parallel call

                  case Op is
                     when Start_Parallel_Op |
                          Start_Handled_Op  |
                          Add_Parallel_Op   =>

                        Parallel_Code_Block : aliased Code_Block_Descriptor;
                        --  This describes the nested block of code

                     when Start_Parallel_Call_Op | Add_Parallel_Call_Op =>

                        Parallel_Call_Target        : Object_Locator;
                        --  This identifies the operation to be called
                        Parallel_Target_Index       : Routine_Index;
                        --  Index of called routine, which might be abstract.
                        Parallel_Locked_Param_Info  : Locked_Param_Info_Type;
                        --  Indicates which param, if any, is locked during the
                        --  execution of the call, and whether is var/by-ref
                        Parallel_Precond_Proved     : Boolean := False;
                        --  Set to True if precondition proved
                        Parallel_Output_Inited_Null : Boolean := False;
                        --  If True, then output was inited to null

                     when Create_Tcb_Op =>
                        null;

                     when others =>
                        null;
                  end case;

               when Wait_For_Parallel_Op =>
                  Skip_Counts : Code_Offset_Array_Ptr := null;
                  --  Array of skip counts which can occur on exit

               when others =>
                  null;
            end case;

         when Case_Op =>
            Case_Selector     : Object_Locator;
            Case_First        : Non_Null_Value;
            --  Skips number of instructions determined by
            --    Case_Selector - Case_First
            --      if Case_Selector in Case_First .. Case_Last.
            Case_Last         : Non_Null_Value;
            Case_Default_Skip : Code_Offset;
            --  Number of instructions to skip
            --  if Case_Selector not in Case_First .. Case_Last.
            --  Negative for loop back.

         when Begin_Nested_Block_Op =>
            Nested_Code_Block : aliased Code_Block_Descriptor;
            --  Description of nested block.  Pc_Offset always identifies self

            Nested_Block_Region : Block_Region_Index := No_Block_Region_Index;
            --  Region index of associated region.
            --  Convert to Symbols.Region_Index to identify Region

         when Loop_Op =>
            null;
      end case;
   end record;

   type Instruction_Sequence is array (Code_Index range <>) of Instruction;
   --  A sequence of machine instructions.

   type Code_Type (Code_Length : Code_Length_Type) is record
      Uses_Stg_Rgn    : Boolean := False;
      Num_Locals      : VM_Obj_Unique_Num := 0;
      Most_Recent_Var : VM_Obj_Unique_Num := 0;
      Instrs          : aliased Instruction_Sequence (1 .. Code_Length);
   end record;

   type Code_Ptr is access all Code_Type;

   function Code_Block
     (Code  : Code_Ptr;
      Index : Code_Index) return Code_Block_RW_Ptr;
   --  Return pointer into Code_Block_Descriptor in specified instruction
   --  Requires: Instructions is Call_Nested_Block_Op, Check_Nested_Block_Op,
   --           Start_Parallel_Op, Start_Handled_Op, Add_Parallel_Op,
   --           or Begin_Nested_Block_Op.

   type Routine;
   type Routine_Ptr is access constant Routine;
   type Routine_RW_Ptr is access all Routine;
   pragma No_Strict_Aliasing (Routine_Ptr);

   function Nth_Instruction
     (Index : Routine_Index;
      N     : Code_Index) return Instruction;
   --  Get Nth instruction of given routine

   function Nth_Routine
     (Index : Routine_Index) return Routine_Ptr;
   --  Return Nth routine, given its index

   type Parameter_Kind_Enum is (Formal_Type, Formal_Object, Formal_Operation);
   --  Kind of formal parameter of routine or module

   type Routine_Param_Info (Compiled : Boolean := False) is record
   --  Descriptor of parameter to routine.
      Is_Passed_By_Ref        : Boolean := False;
      Is_Var                  : Boolean := False;
      Is_Operation_Output     : Boolean := False;
      case Compiled is
      when False =>
         Kind                    : Parameter_Kind_Enum := Formal_Object;
         Type_Info               : Object_Locator := Null_Object_Locator;
         Is_Optional             : Boolean := False;
         Is_Of_Current_Inst_Type : Boolean := False;
         --  If true, then this parameter is
         --  of the "current-instance" type, and
         --  if this routine is inherited into a type extension, some number of
         --  levels of indirection may need to be applied to this parameter.
         Is_Declared_Ref         : Boolean := False;

         --  for debugging only:
         Name      : Strings.U_String    := Strings.Null_U_String;
         Type_Name : Strings.U_String    := Strings.Null_U_String;
         Decl      : Trees.Optional_Tree := Trees.Null_Optional_Tree;
      when True => null;
      end case;
   end record;

   type Routine_Param_Info_Array is
     array (Positive range <>) of Routine_Param_Info;
   type Routine_Param_Info_Array_Ptr is access all Routine_Param_Info_Array;

   --  Info for uplevel references
   type Access_Mode is (No_Access, Read_Access, Write_Access, Update_Access);

   type Uplevel_Info is record
      Locator : Object_Locator := Null_Object_Locator;
      Level   : Code_Nesting_Level := 0;
      Mode    : Access_Mode := No_Access;
   end record;

   type Uplevel_Info_Array is
     array (Positive range <>) of Uplevel_Info;
   type Uplevel_Info_Array_Ptr is access all Uplevel_Info_Array;

   type Boundary_Condition_Enum is (
     External_Precondition,
     Internal_Precondition,
     Internal_Postcondition,
     External_Postcondition);

   type Boundary_Condition_Array is
     array (Boundary_Condition_Enum) of aliased Code_Block_Descriptor;
   --  These are descriptors of local boolean functions which represent pre-
   --  and postconditions.
   --  An "external" pre/postcondition is independent of any "affiliated"
   --  operands. An "internal" pre/postcondition depends on "affiliated"
   --  operands. The distinction is only important for concurrent objects. The
   --  internal precondition becomes a "dequeue" condition if the call has a
   --  locked parameter.
   --  The internal postcondition is checked before returning,
   --  and might no longer be true by the time the caller resumes
   --  execution, because the concurrent parameter might be updated
   --  in the interim.
   --  NOTE: We make this separation to support passing individual
   --        objects marked "concurrent," where the external
   --        precondition is checked before the call, and the
   --        internal precondition becomes the dequeue condition.
   --        In general it is illegal to refer to a concurrent object
   --        in a precondition, since their values aren't stable, so
   --        for a concurrent module, there should be no mention of
   --        "affiliated" parameters in preconditions (since they
   --        are always concurrent), and the dequeue condition *must*
   --        refer to a concurrent object or else it would never
   --        change.

   type Convention_Descriptor is mod 2**32;
      --  A convention descriptor encodes the Convention as well as
      --  information about the number of parameters, presence of return
      --  value, etc.

   Conv_Width : constant := 8;
      --  Number of bits for convention enum within Convention_Descriptor

   pragma Assert (Conv_Width >= Languages.Convention_Enum'Size);
      --  Make sure it fits

   Null_Conv_Desc : constant Convention_Descriptor :=
     (Convention_Descriptor'Last and not (2**Conv_Width - 1)) or
       Languages.Convention_Enum'Pos (Languages.Convention_External_Default);
      --  Represents an unspecified convention descriptor
      --  (but with an external_default convention).

   function Convention (Conv_Desc : Convention_Descriptor)
     return Languages.Convention_Enum;
      --  Extract Convention from Convention_Descriptor

   function Num_Inputs (Conv_Desc : Convention_Descriptor)
     return Natural;
      --  Extract input parameter count from Convention_Descriptor

   function Num_Outputs (Conv_Desc : Convention_Descriptor)
     return Natural;
      --  Extract output parameter count from Convention_Descriptor

   function Output_Needs_Init (Conv_Desc : Convention_Descriptor)
     return Boolean;
      --  Whether output(s) need to be initialized

   function Uses_Queuing (Conv_Desc : Convention_Descriptor)
     return Boolean;
      --  Whether routine uses queuing internally

   function New_Conv_Desc
     (Convention : Languages.Convention_Enum;
      Num_Inputs : Natural; Num_Outputs : Natural;
      Output_Needs_Init : Boolean;
      Uses_Queuing : Boolean)
     return Convention_Descriptor;
      --  Create convention descriptor given convention,
      --  number of inputs, outputs, whether output needs init,
      --  and whether routine uses queuing.

   Too_Many_Inputs : exception;
   --  This exception is raised when the number of inputs exceeds
   --  the number supported by Call_Compiled_Routine (currently 5 inputs)

   type Routine_Code_Address is access procedure
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  These are used to point to a built-in or compiled operation

   Nested_Block_Return_Outcome : constant Nested_Block_Outcome_As_Int
      := 16#FFFF0000#;
   Nested_Block_Return_Outcome_Level : Integer := -1;
   --  Special value used to indicate should keep returning until
   --  return from non-nested block routine.

   type Nested_Blk_Address is access function
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Type_Descriptor_Ptr) return Nested_Block_Outcome_As_Int;
   --  These are used to point to a compiled nested block

   type Routine (Is_PSVM_Routine : Boolean) is record
   --  This is a sequence of instructions which represents the code for an
   --  operation plus the code for any nested blocks that are to be run in
   --  parallel with other code chunks.
      Index        : Routine_Index := 0;
      Name         : Strings.U_String := Strings.Null_U_String;
      Num_Prior_Homonyms : Natural := 0;
      Name_With_Overloading_Index : Strings.U_String := Strings.Null_U_String;
      Full_Module_Name : Strings.U_String := Strings.Null_U_String;
      Uses_Queuing : Boolean := False;
      --  This indicates whether routine uses queuing (or might).

      Local_Area_Length   : Offset_Within_Area := 0;
      Start_Callee_Locals : Offset_Within_Area := 0;
      Boundary_Conditions : Boundary_Condition_Array :=
                             (others => Null_Code_Block_Descriptor);
      Parameters          : Routine_Param_Info_Array_Ptr := null;

      Nesting_Level       : Code_Nesting_Level := 0;
      --  The nesting level of the routine relative to the module level.
      --  This is 0 for top-level operations, and > 0 for nested routines and
      --  nested blocks.
      --  NOTE: The static link refers to a type descriptor if this is 0,
      --        and refers to an enclosing local area if > 0.

      Convention          : Languages.Convention_Enum :=
                              Languages.Convention_Enum'First;
      --  The convention determines the parameter passing conventions
      --  for the routine when called from compiled code.

      Conv_Desc           : Convention_Descriptor := Null_Conv_Desc;
      --  The convention descriptor encodes both the convention and
      --  the number of parameters, presence of return value, etc.,
      --  sufficient to make a call from an out-of-line routine.

      case Is_PSVM_Routine is
         when False =>
            Routine_Addr       : Routine_Code_Address;
            --  Built-ins and compiled routines are represented by a
            --  pointer to an Ada procedure.
            Built_In_Desig     : Strings.U_String := Strings.Null_U_String;
            --  Designator used to identify built-in
            Is_Compiled_Routine : Boolean := False;
               --  If True, then this is a compiled ParaSail routine
               --  rather than a "built-in" and Built_In_Desig will be null.
               --  The ParaSail routine will actually have a return value
               --  if it is a nested block, containing the level/skip info
            Is_Nested_Block : Boolean := False;
            Internal_Precond_Addr : Nested_Blk_Address;
         when True =>
            Local_Master : Offset_Within_Area := 0;
            --  Offset in local area where thread master, if any, is located.

            Code         : Code_Ptr;
            Uplevel_Refs : Uplevel_Info_Array_Ptr := null;
            --  Info on uplevel references; will be null if Nesting_Level = 0

            Enc_Type_Desc : Type_Descriptor_Ptr := null;
            --  Type descriptor for current instance of enclosing module.
            --  Corresponds to (Type_Area, 0).
            --  Note that this type descriptor has formal types rather than
            --  actual types.
      end case;
   end record;

   Null_Routine : constant Routine_RW_Ptr;
   --  This routine does nothing when called

   procedure Install_Code (Code : in out Routine_RW_Ptr);
   --  Install some code in Routine table.
   --  Requires: Code.Index is 0 or Routine with Code.Index
   --           doesn't already exist in table, or it already
   --           exists with the same name, presumably due to having
   --           been compiled and loaded in before the front end started.
   --  Effects: If Code.Index is 0, will be set to unique Routine_Index.
   --           If a routine with the same name already appears in the table,
   --           merge the information and update Code to point to it if
   --           Code is marked as imported.

   function Routine_Name_With_Overloading_Index
     (Routine : Routine_Ptr)
     return Strings.U_String;
   --  Returns Routine.Name if Routine.Num_Prior_Homonyms is 0;
   --  otherwise concatenates "#XX" where XX = Num_Prior_Homonyms + 1.

   -------------------------------------
   -- Built-in-operation registration --
   -------------------------------------

   procedure Register_Builtin
     (Desig   : Strings.U_String;
      Builtin : Routine_Code_Address);
   --  Add Builtin into table of builtin operations under given designator.
   --  Raise Duplicate_Entry if Desig already in table.

   Duplicate_Entry : exception;

   function Find_Builtin (Desig : Strings.U_String)
     return Routine_Code_Address;
   --  Return pointer to builtin operation with given designator.
   --  Return null if not found.

   ----------------------------------------------------------
   -- Compiled-operation registration and associated types --
   ----------------------------------------------------------

   type Local_Operation_Count is
     range 0 .. 2 ** 15 - 1;
   subtype Local_Operation_Index is Local_Operation_Count
     range 1 .. Local_Operation_Count'Last;
      --  Index into local table of compiled operations

   type Operation_Id is record
      --  Pair of local-string indices used to identify the compiled operation
      Module_Name    : Per_File_Strings.Local_String_Index;
      Operation_Name : Per_File_Strings.Local_String_Index_Base;
                        --  If negative, means is "Built_In_Desig"
   end record;

   --  Table of names of compiled operations
   type Operation_Id_Table is array (Local_Operation_Index) of Operation_Id;

   --  Table of addresses of compiled operations
   type Operation_Address_Table is
     array (Local_Operation_Index) of Routine_Code_Address;

   --  Table of convention descriptors of compiled operations
   type Operation_Conv_Desc_Table is
     array (Local_Operation_Index) of Convention_Descriptor;
   for Operation_Conv_Desc_Table'Component_Size use 32;

   --  Use very large constant bounds so that we can pass
   --  llvm pointers and Ada won't check the bounds at run time
   type Local_Op_Index_List is array (Positive range 1 .. 100_000)
     of Local_Operation_Index;
      --  Need to use a constrained array type so no array "dope" is expected

   type Internal_Precond_Addr_List is array (Positive range 1 .. 100_000)
     of Nested_Blk_Address;
      --  Need to use a constrained array type so no array "dope" is expected

   procedure Register_Compiled_Operations
     (Num_Operations  : Local_Operation_Count;
      Operation_Ids   : Operation_Id_Table;
      Operation_Addrs : Operation_Address_Table;
      Operation_Conv_Descs : Operation_Conv_Desc_Table;
      String_Tab      : String_Table_Ptr;
      Num_Internal_Preconds  : Natural;
      Internal_Precond_Ops   : Local_Op_Index_List;
      Internal_Precond_Addrs : Internal_Precond_Addr_List);
   --  Register a set of compiled operations
   pragma Export (Ada, Register_Compiled_Operations,
                  "_psc_register_compiled_operations");

   procedure Set_Run_Time_Checks
     (On : Boolean);
   --  Set run-time checking flag On or Off

   procedure Dump_Routine
     (Code : Routine_Ptr);
   --  Display instructions of routine for debugging purposes.
   pragma Export (Ada, Dump_Routine, "dump_routine");

   procedure Dump_Code
     (Code : Code_Ptr);
   --  Display instructions of Code_Type for debugging purposes.
   pragma Export (Ada, Dump_Code, "dump_code");

   procedure Dump_One_Instruction
     (Instr : Instruction);
   --  Display one instruction
   pragma Export (Ada, Dump_One_Instruction, "dump_instr");

   procedure Dump_Type_Desc
     (Type_Desc : Type_Descriptor_Ptr);
   --  Produce a human-readable display of the contents of a type descriptor

   type Element_Info is record
   --  Info shared between Parameter_Info and Constant_Info
      Type_Desc : Type_Descriptor_Ptr := null;
      Addr      : Object_Virtual_Address := Null_Virtual_Address;
      Value     : Word_Type := Null_Value;
      --  Used if is a formal object or formal operation
   end record;

   --  Info on parameters of type
   type Parameter_Info is record
      Kind : Parameter_Kind_Enum := Formal_Type;
      Data : Element_Info;
      --  Used if is a formal object or formal operation
   end record;
   type Parameter_Info_Array is array (Positive range <>) of Parameter_Info;
   type Parameter_Info_Array_Ptr is access all Parameter_Info_Array;

   procedure Param_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Parameter_Info_Array_Ptr);
   for Parameter_Info_Array_Ptr'Write use Param_Array_Ptr_Write;
   --  Write out the contents of the param info array.

   procedure Param_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Parameter_Info_Array_Ptr);
   for Parameter_Info_Array_Ptr'Read use Param_Array_Ptr_Read;
   --  Read in the contents of the param info array

   --  Info on components of type
   type Component_Info is record
      Type_Desc   : Type_Descriptor_Ptr;
      Is_By_Ref   : Boolean := False;
      Is_Optional : Boolean := True;
   end record;
   type Component_Info_Array is array (Positive range <>) of Component_Info;
   type Component_Info_Array_Ptr is access all Component_Info_Array;

   procedure Comp_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Component_Info_Array_Ptr);
   for Component_Info_Array_Ptr'Write use Comp_Array_Ptr_Write;
   --  Write out the contents of the component info array.

   procedure Comp_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Component_Info_Array_Ptr);
   for Component_Info_Array_Ptr'Read use Comp_Array_Ptr_Read;
   --  Read in the contents of the component info array

   --  Info on nested types
   type Type_Desc_Array is array (Positive range <>) of Type_Descriptor_Ptr;
   type Type_Desc_Array_Ptr is access all Type_Desc_Array;

   procedure Desc_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Type_Desc_Array_Ptr);
   for Type_Desc_Array_Ptr'Write use Desc_Array_Ptr_Write;
   --  Write out the contents of the component info array.

   procedure Desc_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Type_Desc_Array_Ptr);
   for Type_Desc_Array_Ptr'Read use Desc_Array_Ptr_Read;
   --  Read in the contents of the component info array

   --  Info on instantiation-time-known constants
   type Const_Info is record
      Name : Strings.U_String := Strings.Null_U_String;  --  for debugging
      Data : Element_Info;
   end record;
   type Const_Info_Array is array (Positive range <>) of aliased Const_Info;
   type Const_Info_Array_Ptr is access all Const_Info_Array;

   type Const_Info_Ptr is access all Const_Info;
   pragma No_Strict_Aliasing (Const_Info_Ptr);

   procedure Const_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Const_Info_Array_Ptr);
   for Const_Info_Array_Ptr'Write use Const_Array_Ptr_Write;
   --  Write out the contents of the constant info array.

   procedure Const_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Const_Info_Array_Ptr);
   for Const_Info_Array_Ptr'Read use Const_Array_Ptr_Read;
   --  Read in the contents of the constant info array

   type Computable_Const_Info is record
      --  Combination of a Const_Info and a parameterless routine
      --  that will compute the constant.
      Info : aliased Const_Info;
      Computation : Routine_Index;
   end record;

   type Computable_Const_Info_Ptr is access all Computable_Const_Info;

   package CTK_Info_Vectors is new PSC.Vectors (Computable_Const_Info_Ptr);
   type CTK_Info_Vector is new CTK_Info_Vectors.Vector;
   subtype CTK_Info_Index is CTK_Info_Vectors.Elem_Index;
   --  CTK_Info_Index is equal to Const_Area offset

   Compile_Time_Known_Consts : CTK_Info_Vector;
   --  The table of compile-time-known constants,
   --  parallel to Compile_Time_Known_Const_Table created by the
   --  front end.

   procedure PSVM_Object_Input
     (Stream    : access Ada.Streams.Root_Stream_Type'Class;
      Type_Desc : out Type_Descriptor_Ptr;
      Object    : out Word_Type;
      Stg_Rgn   : Stg_Rgn_Ptr);
   --  Read name of Type and stream representation of Object from stream;
   --  return both type descriptor and value of object.
   --  Stg_Rgn must be a storage region in which to create a large object.

   procedure PSVM_Object_Output
     (Stream    : access Ada.Streams.Root_Stream_Type'Class;
      Type_Desc : Type_Descriptor_Ptr;
      Object    : Word_Type);
   --  Write name of Type and stream representation of Object into stream

   procedure PSVM_Object_Read
     (Stream    : access Ada.Streams.Root_Stream_Type'Class;
      Type_Desc : Type_Descriptor_Ptr;
      Object    : out Word_Type;
      Stg_Rgn   : Stg_Rgn_Ptr);
   --  Read stream representation of Object from stream given type;
   --  return value of object.
   --  Stg_Rgn must be a storage region in which to create a large object.

   procedure PSVM_Object_Write
     (Stream    : access Ada.Streams.Root_Stream_Type'Class;
      Type_Desc : Type_Descriptor_Ptr;
      Object    : Word_Type);
   --  Write stream representation of Object of given Type_Desc into stream

   type Operation_Index is range 0 .. 9_999;
   --  Index into operation table associated with a type.

   Optional_Operation_Index : constant Operation_Index := Operation_Index'Last;
   --  Indicator that operation is not defined, and default "null" operation
   --  should be used.

   type Wrapper_Action_Enum is (
     No_Action,
     Component_Extension_Action,
     Polymorphic_Type_Action);
   --  These identify the kind of wrapper action needed before or after calling
   --  the identified underlying operation.

   package Cur_Inst_Params is
      --  Info on "cur-inst" params of a routine

      Max_Cur_Inst_Params : constant := 8;
      --  Maximum number of cur-inst params supported

      type Routine_Cur_Inst_Param_Info is private;

      No_Cur_Inst_Param_Info : constant Routine_Cur_Inst_Param_Info;
      --  Initial value

      function To_Routine_Param_Info_Array (Info : Routine_Cur_Inst_Param_Info)
         return Routine_Param_Info_Array;
      --  Convert to Routine_Param_Info

      function Has_Cur_Inst_Param_Info (Info : Routine_Cur_Inst_Param_Info)
        return Boolean;
      --  Return True if Info has info on at least one cur-inst param

      function Has_Cur_Inst_Output (Info : Routine_Cur_Inst_Param_Info)
        return Boolean;
      --  Return True if Info has info on at least one cur-inst output

      function Has_Cur_Inst_Input (Info : Routine_Cur_Inst_Param_Info)
        return Boolean;
      --  Return True if Info has info on at least one cur-inst input

      Not_A_Param : constant Offset_Within_Area;
      --  Indicator for no param

      procedure Get_First_Cur_Inst_Input
        (Info : Routine_Cur_Inst_Param_Info;
         Param_Offset : out Offset_Within_Area;
         Is_By_Ref    : out Boolean);
      --  For first cur-inst input, get offset and whether is passed by-ref.
      --  Param_Offset = Not_A_Param if there are no cur-inst inputs.

      generic
         with procedure Action
                (Param_Offset : Offset_Within_Area;
                 Is_By_Ref    : Boolean;
                 Is_Var       : Boolean;
                 Is_Output    : Boolean);
      procedure Iterate_Cur_Inst_Params (Info : Routine_Cur_Inst_Param_Info);
      --  Iterate through the Routine_Cur_Inst_Param_Info calling Action
      --  for each "cur-inst" param.

      procedure Add_Cur_Inst_Param
        (Info         : in out Routine_Cur_Inst_Param_Info;
         Param_Offset : Offset_Within_Area;
         Is_By_Ref    : Boolean;
         Is_Var       : Boolean;
         Is_Output    : Boolean);
      --  This is called repeatedly to build up a Routine_Cur_Inst_Param_Info
      --  for cur-inst params.
      --  The exception Too_Many_Cur_Inst_Params is raised if the limit on
      --  Cur-inst params is reached.

      Too_Many_Cur_Inst_Params : exception;

   private
      Bits_For_Offset     : constant := 5;
      Max_Param_Offset    : constant := 2 ** Bits_For_Offset - 2;
      --  Maximum offset allowed for a parameter

      Not_A_Param : constant Offset_Within_Area := Max_Param_Offset + 1;
      --  Indicator for end of params

      type One_Param_Info is record
         Param_Offset : Offset_Within_Area range 0 .. Not_A_Param :=
           Not_A_Param;
         Is_By_Ref    : Boolean := False;
         Is_Var       : Boolean := False;
         Is_Output    : Boolean := False;
      end record;

      --  Lay this out to be as small as possible
      for One_Param_Info use record
         Param_Offset at 0 range 0 .. Bits_For_Offset - 1;
         Is_By_Ref    at 0 range Bits_For_Offset .. Bits_For_Offset;
         Is_Var       at 0 range Bits_For_Offset + 1 .. Bits_For_Offset + 1;
         Is_Output    at 0 range Bits_For_Offset + 2 .. Bits_For_Offset + 2;
      end record;
      for One_Param_Info'Size use Bits_For_Offset + 3;

      type Routine_Cur_Inst_Param_Info is
        array (1 .. Max_Cur_Inst_Params) of One_Param_Info;
      for Routine_Cur_Inst_Param_Info'Component_Size use Bits_For_Offset + 3;

      procedure Cur_Inst_Param_Info_Write
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Item : Routine_Cur_Inst_Param_Info);
      for Routine_Cur_Inst_Param_Info'Write use Cur_Inst_Param_Info_Write;
      --  Write out the contents of the routine cur-inst-param info array.

      procedure Cur_Inst_Param_Info_Read
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Item : out Routine_Cur_Inst_Param_Info);
      for Routine_Cur_Inst_Param_Info'Read use Cur_Inst_Param_Info_Read;
      --  Read in the contents of the routine cur-inst-param info array.

      No_Cur_Inst_Param_Info : constant Routine_Cur_Inst_Param_Info :=
        (others => (Param_Offset => Not_A_Param, others => False));

   end Cur_Inst_Params;

   use Cur_Inst_Params;

   type Routine_Info is record
   --  Info on a particular operation
      Index     : Routine_Index := Routine_Index'Last;
      Type_Desc : Non_Op_Map_Type_Ptr;
      --  Normally will be enclosing type desc

      Action    : Wrapper_Action_Enum := No_Action;
      Op_Index  : Operation_Index := 0;  --  Only used for Poly_T_Action

      Use_Static_Link_For_Type : Boolean := False;
      --  If True, indicates that Type_Desc is not proper
      --  type to use, and instead should use caller-supplied static link.
      --  This is for calls on generic ops.
      --  Type_Desc is generally the type enclosing the caller-supplied static
      --  link, but might be an ancestor of that if the operation is inherited.

      Cur_Inst_Param_Info : Routine_Cur_Inst_Param_Info;
      --  Information on the cur-inst params of the routine,
      --  used for (un)wrapping polymorphic parameters in Find_Routine_Context.
   end record;

   Null_Routine_Info : constant Routine_Info :=
     (Index     => 0,
      Type_Desc => null,
      Action    => No_Action,
      Op_Index  => 0,
      Use_Static_Link_For_Type => False,
      Cur_Inst_Param_Info      => No_Cur_Inst_Param_Info);

   type Routine_Info_Array is
     array (Operation_Index range <>) of Routine_Info;
   type Routine_Info_Array_Ptr is access all Routine_Info_Array;

   procedure Routine_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Routine_Info_Array_Ptr);
   for Routine_Info_Array_Ptr'Write use Routine_Array_Ptr_Write;
   --  Write out the contents of the routine info array.

   procedure Routine_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Routine_Info_Array_Ptr);
   for Routine_Info_Array_Ptr'Read use Routine_Array_Ptr_Read;
   --  Read in the contents of the routine info array

   type Routine_Index_Array is
     array (Operation_Index range <>) of Routine_Index;
   type Routine_Index_Array_Ptr is access all Routine_Index_Array;

   type Operation_Index_Array is
     array (Operation_Index range <>) of Operation_Index;
   type Operation_Index_Array_Ptr is access all Operation_Index_Array;
   --  Type used for operation maps

   procedure Operation_Index_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Operation_Index_Array_Ptr);
   for Operation_Index_Array_Ptr'Read use Operation_Index_Array_Ptr_Read;
   --  Read in the contents of the operation-index array

   procedure Operation_Index_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Operation_Index_Array_Ptr);
   for Operation_Index_Array_Ptr'Write use Operation_Index_Array_Ptr_Write;
   --  Write out the contents of the operation-index array

   type Op_Map_Count is range 0 .. 2**15 - 1;

   type Op_Map_Type_Array is
     array (Op_Map_Count range <>) of Op_Map_Type_Ptr;
   type Op_Map_Type_Array_Ptr is access all Op_Map_Type_Array;
   --  Type used for op-maps for (explicitly) implemented interfaces

   procedure Op_Map_Type_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Op_Map_Type_Array_Ptr);
   for Op_Map_Type_Array_Ptr'Read use Op_Map_Type_Array_Ptr_Read;
   --  Read in the contents of the op-map array

   procedure Op_Map_Type_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Op_Map_Type_Array_Ptr);
   for Op_Map_Type_Array_Ptr'Write use Op_Map_Type_Array_Ptr_Write;
   --  Write out the contents of the op-map array

   ------------ Type Area Offsets -------------

   type Type_Desc_Ref_Kind is (
     Self_Reference,
     Formal_Parameter_Reference,
     Actual_Of_Formal_Reference,
     Nested_Type_Reference,
     Operation_Reference,
     Nested_Object_Reference,
     Corresponding_Polymorphic_Type_Reference);
   --  This identifies the kind of reference being made into the type
   --  descriptor.

   Type_Self_Reference_Offset : constant Offset_Within_Area := 0;
   --  Offset used to indicate a reference to the enclosing type itself

   subtype Type_Formal_Parameter_Offsets is Offset_Within_Area range 0 .. 999;
   --  Offsets used to refer to the formal
   --  parameters of the enclosing module

   subtype Type_Actual_Of_Formal_Offsets is Offset_Within_Area range
     1000 .. 1999;
   --  Offsets used to refer to actuals of formal types.

   subtype Type_Nested_Type_Offsets is Offset_Within_Area range 2000 .. 9999;
   --  Offsets used to refer to nested types.

   subtype Type_Operation_Offsets is Offset_Within_Area range
     10_000 .. 10_000 + Offset_Within_Area (Operation_Index'Last);
   --  Range of offsets within a type area devoted to
   --  operations.

   subtype Type_Nested_Obj_Offsets is Offset_Within_Area range
     20_000 .. 29_999;
   --  Range of offsets within a type area devoted to
   --  nested objects

   subtype Corresponding_Polymorphic_Type_Offsets is
     Offset_Within_Area range 30_000 .. 39_999;
   --  Offsets used to refer to corresponding polymorphic type

   pragma Assert
     (Type_Operation_Offsets'Last < Type_Nested_Obj_Offsets'First);

   --  Start and end of each type sub-area
   Type_Subarea_Start : constant
     array (Type_Desc_Ref_Kind) of Offset_Within_Area :=
     (Self_Reference             => Type_Self_Reference_Offset,
      Formal_Parameter_Reference => Type_Formal_Parameter_Offsets'First,
      Actual_Of_Formal_Reference => Type_Actual_Of_Formal_Offsets'First,
      Nested_Type_Reference      => Type_Nested_Type_Offsets'First,
      Operation_Reference        => Type_Operation_Offsets'First,
      Nested_Object_Reference  => Type_Nested_Obj_Offsets'First,
      Corresponding_Polymorphic_Type_Reference =>
        Corresponding_Polymorphic_Type_Offsets'First);

   Type_Subarea_End : constant
     array (Type_Desc_Ref_Kind) of Offset_Within_Area :=
     (Self_Reference             => Type_Self_Reference_Offset,
      Formal_Parameter_Reference => Type_Formal_Parameter_Offsets'Last,
      Actual_Of_Formal_Reference => Type_Actual_Of_Formal_Offsets'Last,
      Nested_Type_Reference      => Type_Nested_Type_Offsets'Last,
      Operation_Reference        => Type_Operation_Offsets'Last,
      Nested_Object_Reference    => Type_Nested_Obj_Offsets'Last,
      Corresponding_Polymorphic_Type_Reference =>
        Corresponding_Polymorphic_Type_Offsets'Last);

   type Type_Kind_Enum is (
     Normal_Kind,
     Basic_Array_Kind,
     Univ_Integer_Kind,
     Univ_Real_Kind,
     Univ_String_Kind,
     Univ_Char_Kind,
     Univ_Enum_Kind,
     Unsigned_64_Kind,
     Integer_64_Kind,
     Aliased_Object_Kind);

   type Type_Descriptor (Has_Op_Map : Boolean) is record
   --  Run-time type information
   --  If Has_Op_Map is True, then there is an extra level of indirection, with
   --  an Op_Map to map from the interface op index to the actual type's op
   --  index.
      Magic_Number : Word_Type := Type_Indicator;
         --  This always has the value Type_Indicator to distinguish it
         --  from a local area, which contains a pointer to the enclosing
         --  local area.  We use a rep-clause to put it at offset 0.

      Index : Type_Index := 0;
      --  Index into global table of type descriptors

      Location : Object_Locator := Null_Object_Locator;
      --  Location used to identify type descriptor

      Corresponding_Polymorphic_Type_Desc : Type_Descriptor_Ptr := null;
      --  If non-null, then this points to the result of calling
      --  Build_Polymorphic_Type_Desc on this type-desc or type-map.

      Is_Finished : Boolean := False;
      --  Set True after all of the fields have been initialized, including
      --  formal objects and nested objects, which need to be evaluated.

      Next_Unfinished : Type_Descriptor_Ptr := null;
      Prev_Unfinished : Type_Descriptor_Ptr := null;
      --  Links on the doubly-linked list of unfinished type descriptors

      Num_Operations : Operation_Index := 0;
      --  Number of interface operations

      Name : Strings.U_String := Strings.Null_U_String;
      --  For debugging

      Type_Sem : Trees.Root_Sem_Ptr := null;  --  For debugging

      Type_Kind  : Type_Kind_Enum := Normal_Kind;
      All_Parameters_Known : Boolean := False;
      Is_Small   : Boolean := False;  --  Known to be small
      Is_Large   : Boolean := False;  --  Known to be large
      Is_Wrapper : Boolean := False;
      Is_Polymorphic : Boolean := False;
      Null_Value : Word_Type := Null_Virtual_Address;
      --  Null value if Is_Small.
      --  (If large, must use region-specific null.)

      case Has_Op_Map is
         when False =>
            Parent_Type : Non_Op_Map_Type_Ptr;
            --  If non-null, then this type is an extension of
            --  the specified Parent type.

            Component_Extension_Level : Natural := 0;
            --  If non-zero, then this type is an extension of another type,
            --  and the specified number of indirections through component #1
            --  would be necessary to extract its ultimate ancestor's value.
            --  NOTE: This level can be zero even if the type is
            --        an extension of another type, so long as no
            --        components were added.  The level will be
            --        the same as that of its immediate parent if no
            --        components were added when extending that type.

            Is_Abstract           : Boolean := False;
            Is_Partially_Abstract : Boolean := False;
            Is_Concurrent         : Boolean := False;

            Enclosing_Type : Non_Op_Map_Type_Ptr;
            --  If this type is from a nested module, this is the type
            --  descriptor for the type representing the enclosing module.

            Root_Type_Desc : Non_Op_Map_Type_Ptr := null;
            --  Must be non-null if Is_Polymorphic is True.
            --  Identifies type that is implemented by set of types represented
            --  by the polymorphic type. I.e. for X+, it is X.

            Num_Parameters         : Natural := 0;
            Parameters             : Parameter_Info_Array_Ptr;

            Num_Actuals_Of_Formals : Natural := 0;
            Actuals_Of_Formals     : Parameter_Info_Array_Ptr;

            Num_Components         : Natural := 0;
            Components             : Component_Info_Array_Ptr;

            Num_Nested_Types       : Natural := 0;
            Nested_Types           : Type_Desc_Array_Ptr;

            Num_Nested_Objs        : Natural := 0;
            Nested_Objs            : Const_Info_Array_Ptr;

            Operations             : Routine_Info_Array_Ptr;

            Num_Interface_Op_Maps  : Op_Map_Count := 0;
            Interface_Op_Maps      : Op_Map_Type_Array_Ptr;

         when True =>
            Op_Map          : Operation_Index_Array_Ptr;
            --  Map from interface operation index to actual operation index

            Actual_Type     : Non_Op_Map_Type_Ptr;
            --  Actual type descriptor

            Formal_Type_Sem : Trees.Root_Sem_Ptr := null;
            --  For debugging
      end case;
   end record;

   --------------------------------
   -- Type_Descriptor_Operations --
   --------------------------------

   package Type_Descriptor_Ops is

      function Get_Enclosing_Type
        (Static_Link : Word_Ptr;
         Type_Base   : Area_Base_Indicator := Type_Area)
         return Non_Op_Map_Type_Ptr;
      --  Return enclosing type descriptor given pointer to local area
      --  or static link

      function Get_Enclosing_Type_Or_Op_Map
        (Static_Link : Word_Ptr;
         Type_Base   : Area_Base_Indicator := Type_Area)
         return Type_Descriptor_Ptr;
      --  Return enclosing type descriptor given pointer to local area
      --  or static link; type descriptor might actually be an op-map

      function Get_Formal_Type_Or_Op_Map
        (Type_Desc : Non_Op_Map_Type_Ptr;
         Index     : Offset_Within_Area) return Type_Descriptor_Ptr;
         --  Return Nth formal type within Type parameter array
      pragma Export (Ada, Get_Formal_Type_Or_Op_Map,
        "_psc_get_formal_type_or_op_map");

      function Get_Nested_Type_Or_Op_Map
        (Type_Desc : Non_Op_Map_Type_Ptr;
         Index     : Offset_Within_Area) return Type_Descriptor_Ptr;
         --  Return Nth nested type within nested type array
      pragma Export (Ada, Get_Nested_Type_Or_Op_Map,
        "_psc_get_nested_type_or_op_map");

      function Get_Type_Desc_Or_Op_Map
        (Context   : Exec_Context;
         Type_Info : Object_Locator)
         return Type_Descriptor_Ptr;
      --  Return type descriptor/op-map given Object_Locator for type area.
      --  This does not skip over an op-map.

      function Get_Type_Desc_Or_Op_Map_Exported
        (Static_Link      : Word_Ptr;
         Type_Info_Base   : Area_Base_Indicator;
         Type_Info_Offset : Offset_Within_Area)
         return Type_Descriptor_Ptr;
      --  Return type descriptor/op-map given Object_Locator for type area.
      --  This does not skip over an op-map.
      --  This version takes static link and base/offset as separate params.
      pragma Export (Ada, Get_Type_Desc_Or_Op_Map_Exported,
        "_psc_get_type_desc_or_op_map");

      function Get_Type_Desc
        (Context   : Exec_Context;
         Type_Info : Object_Locator)
         return Non_Op_Map_Type_Ptr;
      --  Return type descriptor given Object_Locator for type area.
      --  This skips over the op-map, if any.

      function Get_Type_Desc_By_Name
        (Name : Strings.U_String) return Type_Descriptor_Ptr;
      --  Lookup to see if type-descriptor with name given by Name
      --  already exists.  If so return it.  If not return null.

      function Install_Type_Desc_By_Name
        (New_Type_Desc : Type_Descriptor;
         Is_New : access Boolean) return Type_Descriptor_Ptr;
      --  Lookup to see if type-descriptor with name New_Type_Desc.Name
      --  already exists.  If not, copy New_Type_Desc into heap and install it.
      --  In any case return pointer to installed type descriptor.
      --  Type_Info.Index and Type_Info.Location will be filled in.
      --  Is_New indicates whether the type descriptor already existed.

      function Is_Univ_String_Or_Enumeration (Type_Desc : Type_Descriptor_Ptr)
        return Boolean;
      --  Return True if, after unwrapping and skipping op-maps, we end up
      --  at a type descriptor for Univ_String or Univ_Enumeration.
      --  These values need to be handled specially when streaming values,
      --  since their value is actually an index into a global table.

      function Nth_Operation_Of_Type
        (Callee_Type_Area_Or_Map : Type_Descriptor_Ptr;
         Index                   : Operation_Index;
         Ignore_Abstract_Op      : Boolean := False)
         return Routine_Info;
      --  Return Nth operation given type desc or op-map.
      --  Return Null if operation is abstract and Ignore_Abstract_Op is true.

      function Get_Enclosing_Type (Type_Desc : Non_Op_Map_Type_Ptr;
         Type_Base : Area_Base_Indicator) return Non_Op_Map_Type_Ptr;
      --  If Type_Base is Enclosing Type_Areas,
      --    follow the enclosing type the specified number of times
      --  else do nothing and return Type_Desc

      function Nth_Type_Area_Element
        (Type_Desc   : Non_Op_Map_Type_Ptr;
         Item_Offset : Offset_Within_Area;
         Type_Base   : Area_Base_Indicator := Type_Area)
         return Element_Info;
      --  Return the Element_Info for the Nth item in the type area that
      --  represents the given type, presuming Nth item is an object, type,
      --  or operation.

      function Nth_Type_Area_Type
        (Type_Desc   : Non_Op_Map_Type_Ptr;
         Item_Offset : Offset_Within_Area)
         return Type_Descriptor_Ptr;
      --  Return the Nth item in the type area that represents the given type,
      --  presuming Nth item is a type.
      --  If Item_Index = 0, return the Type_Desc itself.

      function Nth_Type_Area_Word
        (Type_Desc   : Non_Op_Map_Type_Ptr;
         Item_Offset : Offset_Within_Area;
         Type_Base   : Area_Base_Indicator := Type_Area)
         return Word_Type;
      --  Return the content of (Type_Area, Item_Offset).
      pragma Export (Ada, Nth_Type_Area_Word, "_psc_nth_type_area_word");

      function Nth_Type_Area_Item_Address
        (Type_Desc   : Non_Op_Map_Type_Ptr;
         Item_Offset : Offset_Within_Area;
         Type_Base   : Area_Base_Indicator := Type_Area)
         return Object_Virtual_Address;
      --  Return the address associated with (Type_Area, Item_Index)
      pragma Export (Ada, Nth_Type_Area_Item_Address,
        "_psc_nth_type_area_item_address");

      function Nth_Type_Area_Item_Physical_Address
        (Type_Info   : Non_Op_Map_Type_Ptr;
         Item_Offset : Offset_Within_Area;
         Type_Base   : Area_Base_Indicator := Type_Area)
         return Word_Ptr;
      pragma Export (Ada, Nth_Type_Area_Item_Physical_Address,
         "_psc_nth_type_area_item_physical_address");

      function Null_For_Type
        (Type_Desc : Type_Descriptor_Ptr)
         return Word_Type;
      --  Return a null appropriate for the (small) type

      function Root_Of_Polymorphic_Type
        (Poly_Type : Non_Op_Map_Type_Ptr)
        return Type_Descriptor_Ptr;
      --  Return type that is "root" of hierarchy of types represented by the
      --  polymorphic type.

      function Skip_Over_Op_Map
        (Type_Desc_Or_Op_Map : Type_Descriptor_Ptr)
         return Non_Op_Map_Type_Ptr;
      --  Return Type descriptor, skipping over any op-map
      pragma Inline (Skip_Over_Op_Map);

      function To_Type_Desc (Type_Id : Type_Index) return Non_Op_Map_Type_Ptr;
      --  Convert virtual address of type descriptor to the corresponding type
      --  descriptor; skip over any op-map.

      function To_Type_Desc (Addr : Word_Ptr) return Non_Op_Map_Type_Ptr;
      --  Convert physical address of type descriptor to the corresponding type
      --  descriptor; skip over any op-map.

      function To_Type_Desc_Or_Op_Map
        (Type_Id : Type_Index)
         return Type_Descriptor_Ptr;
      --  Convert virtual address of type descriptor to the corresponding type
      --  descriptor (or op-map)

      function To_Type_Desc_Or_Op_Map is new Ada.Unchecked_Conversion
        (Word_Ptr, Type_Descriptor_Ptr);
      --  Convert physical addr of type descriptor to the corresponding type
      --  descriptor (or op-map)

      function Type_Desc_Name_And_Num
        (Type_Desc      : Type_Descriptor_Ptr;
         Use_Short_Form : Boolean := False) return String;
      --  Return name of Type_Desc, and # if polymorphic or op-map

      function Unwrapped_Type_Desc (Type_Desc : Type_Descriptor_Ptr)
        return Non_Op_Map_Type_Ptr;
      --  Return type descriptor stripped of Op_Maps and Wrappers

      function Generate_Stream_Rep (Type_Desc : Type_Descriptor_Ptr;
        Univ_Int_Array_Type : Non_Op_Map_Type_Ptr;
        Target : Word_Type;
        Server_Index : Thread_Server_Index;
        PFST : Per_File_Strings.Per_File_String_Table_Ptr := null)
        return Word_Type;
      --  Generate the stream representation for the type descriptor
      --  and return it in a basic array allocated in the region
      --  determined by Target.
      --  Update the Per-file String table, if provided.

      type Type_Desc_Table_Type is
        array (Stream_Local_Index) of Type_Descriptor_Ptr;

      procedure Reconstruct_Type_Descriptors
        (Num_Entries : Stream_Local_Count;
         Stream_Rep_Table : Stream_Rep_Table_Type;
         String_Table     : String_Table_Ptr;
         Type_Desc_Table  : out Type_Desc_Table_Type);
      --  Fill in per-file Type_Desc_Table by reconstructing type descriptors
      --  given their stream representation and a string table.
      pragma Export (Ada, Reconstruct_Type_Descriptors,
        "_psc_reconstruct_type_descriptors");

      type Finish_Type_Descriptor_Type is access procedure
        (Type_Desc : Type_Descriptor_Ptr;
         Return_On_Recursion : Boolean := False);

      Finish_Type_Descriptor_Ptr : Finish_Type_Descriptor_Type := null;
         --  Pointer to function that will
         --  finish up formal object parameters and nested objects
         --  If Return_On_Recursion is True, do not complain about recursion
         --  and simply return immediately.

   end Type_Descriptor_Ops;

   function Allocate_From_Stg_Rgn
     (Stg_Rgn       : Stg_Rgn_Ptr;
      Size_In_Words : Offset_Within_Area;
      Server_Index  : Thread_Server_Index) return Word_Type;
   --  This attempts to reuse storage freed by Deallocate_From_Stg_Rgn.
   --  Initialize allocated space with region/size/null-type.

   procedure Assign_Word
     (Context     : in out Exec_Context;
      Type_Desc   : Type_Descriptor_Ptr;
      Destination : Word_Ptr;
      New_Value   : Word_Type);
   --  We want to release the old value of the destination before overwriting
   --  it with source.
   pragma Export (Ada, Assign_Word, "_psc_assign_word");

   function Copy_Large_Obj
     (Type_Desc     : Non_Op_Map_Type_Ptr;
      Value_To_Copy : Word_Type;
      Stg_Rgn       : Stg_Rgn_Ptr;
      Server_Index  : Thread_Server_Index;
      Old_Stg_Rgn   : Stg_Rgn_Ptr := null) return Word_Type;
   --  Return copy of (large) object created in specified region
   --  NOTE: Object might be null.
   --        Old_Stg_Rgn is used for debugging, to verify that all
   --        subcomponents come from the same region.

   function Copy_Object
     (Context    : in out Exec_Context;
      Type_Desc  : Type_Descriptor_Ptr;
      Object     : Word_Type;
      Stg_Rgn_Of : Word_Type) return Word_Type;
   --  Copy object, given type descriptor and target object.
   --  Return result of copying.
   pragma Export (Ada, Copy_Object, "_psc_copy_object");

   function Create_Large_Obj
     (Type_Desc    : Non_Op_Map_Type_Ptr;
      Stg_Rgn      : Stg_Rgn_Ptr;
      Server_Index : Thread_Server_Index) return Word_Type;
   --  Create large (non-array) object in given region.
   --  Initialize all subobjects to appropriate kind of null

   procedure Init_Large_Obj
     (Type_Desc    : Non_Op_Map_Type_Ptr;
      Stg_Rgn      : Stg_Rgn_Ptr;
      New_Obj      : Word_Type);
   --  Initialize large (non-array) object in given region.
   --  Initialize all subobjects to appropriate kind of null

   function Create_Basic_Array_Obj
     (Array_Type_Desc : Non_Op_Map_Type_Ptr;
      Array_Len       : Natural;
      Stg_Rgn         : Stg_Rgn_Ptr;
      Server_Index    : Thread_Server_Index;
      Init_Elements   : Boolean := False;
      Element_Value   : Word_Type := Null_Value) return Word_Type;
   --  Create a "Basic_Array" object, given type descriptor, length,
   --  server, and, optionally, initial value for elements.

   function Basic_Array_Comp_Type (Basic_Array_Type : Non_Op_Map_Type_Ptr)
     return Non_Op_Map_Type_Ptr;
   --  Given a type identified as a Basic_Array kind,
   --  retrieve the component type.

   Malformed_Basic_Array_Error : exception;
   --  Create_Basic_Array_Obj or Basic_Array_Comp_Type raises this
   --  exception if the Array_Type_Desc doesn't have a recognizable form.

   procedure Create_Lock_For_Obj
     (Dest_Obj : Object_Virtual_Address; Server_Index : Thread_Server_Index);
   --  Create a lock for object if it doesn't already have one.

   procedure Create_Lock_For_Obj_Exported (Context : in out Exec_Context;
      Dest_Obj : Object_Virtual_Address);
   pragma Export (Ada, Create_Lock_For_Obj_Exported,
      "_psc_create_lock_for_obj");
   --  Create a lock for object if it doesn't already have one.

   procedure Deallocate_From_Stg_Rgn
     (Stg_Rgn         : Stg_Rgn_Ptr;
      Storage_Address : Word_Type;
      Server_Index    : Thread_Server_Index);
   --  This adds the given storage to a list indexed by the size.
   --  Requires: Object has size embedded in its header.

   function Generate_Value_Stream_Rep (Info : Const_Info;
     Univ_Int_Array_Type : Non_Op_Map_Type_Ptr;
     Target : Word_Type;
     Server_Index : Thread_Server_Index;
     PFST : Per_File_Strings.Per_File_String_Table_Ptr := null)
     return Word_Type;
   --  Create a basic array in region associated with Target and
   --  fill it in with a stream representation of the compile-time constant.

   procedure Install_Type_Info
     (Type_Desc : Type_Descriptor_Ptr);
   --  Install a type descriptor in the global type table.
   --  Type_Info.Index and Type_Info.Location will be filled in as a side
   --  effect.

   function Is_Small (Type_Desc : Type_Descriptor_Ptr) return Boolean;
   --  Return True if type is "small", or a wrapper of a "small" type.

   function Is_Large_Null (Value : Word_Type) return Boolean;
   --  Return True if Value is a large null value
   --  NOTE: We reserve a special chunk number for all large nulls.

   function Is_Null_Value
     (Word      : Word_Type;
      Type_Desc : Type_Descriptor_Ptr) return Boolean;
   --  Return True if Word is a null value of the specified type.
   --  If Type_Desc is null, return True if is Null_Virtual_Address.
   pragma Export (Ada, Is_Null_Value, "_psc_is_null_value");

   function Is_Special_Large_Value (Value : Word_Type) return Boolean;
   --  NOTE: We reserve special (negative) chunk #s for special large values.
   --        We also require special large values to be odd.

   subtype Indicator_Type is Unsigned_Word_Type;

   function Special_Value_Indicator (Value : Word_Type)
     return Indicator_Type;
   --  Return Indicator "buried" in special value of Univ_String, Univ_Integer
   pragma Inline (Special_Value_Indicator);

   Indicator_Multiplier    : constant Indicator_Type := 2**(24 + 32);
      --  Amount to multiply negated indicator to produce special value

   Null_Value_Indicator    : constant Indicator_Type := 2;
      --  NOTE: This needs to be consistent with Large_Null_Chunk
   pragma Assert
     (Indicator_Type'(-Null_Value_Indicator) * Indicator_Multiplier =
      Indicator_Type'(To_Unsigned_Word (Large_Null_Chunk) * Chunk_Divisor));

   --  Indicators used for Univ_String and Univ_Integer special values.
   subtype Univ_String_Indicators is Indicator_Type range 3 .. 7;
   subtype Univ_Integer_Indicators is Indicator_Type range 8 .. 9;

   function Known_Type_Desc
     (Location     : Object_Locator;
      Now_Building : Type_Descriptor_Ptr := null;
      Src_Pos      : Source_Positions.Source_Position :=
                       Source_Positions.Null_Source_Position)
     return Type_Descriptor_Ptr;
   --  Return type descriptor given its "locator"
   --   If Now_Building is non-null, then return that value
   --   If Location is (Type_Area, 0) implying a self-reference.

   function Large_Obj_Type_Desc
     (Addr : Object_Virtual_Address) return Non_Op_Map_Type_Ptr;
   --  Return type descriptor given a non-null large object

   function Large_Obj_Size
     (Addr : Object_Virtual_Address) return Offset_Within_Chunk;
   --  Return size in words given a non-null large object

   function Large_Obj_On_Stack
     (Addr : Object_Virtual_Address) return Boolean;
   --  Return True if given non-null large obj is residing on stack

   procedure Make_Copy_In_Stg_Rgn_Exported
     (Context                 : in out Exec_Context;
      Type_Info               : Type_Descriptor_Ptr;
      Destination             : Word_Ptr;
      Source                  : Word_Ptr;
      Existing_Obj_In_Stg_Rgn : Word_Ptr);
   pragma Export (Ada, Make_Copy_In_Stg_Rgn_Exported,
      "_psc_make_copy_in_stg_rgn");

   procedure Move_Object
     (Context   : in out Exec_Context;
      Type_Info : Type_Descriptor_Ptr;
      LHS_Ptr   : Word_Ptr;
      RHS_Ptr   : Word_Ptr);
   --  We want to release the old values of the destination before overwriting
   --  it, and set source to null without releasing its old value, unless a
   --  region change is necessary.
   pragma Export (Ada, Move_Object, "_psc_move_object");

   procedure Release_Large_Obj
     (Type_Desc          : Type_Descriptor_Ptr;
      Old_Value          : Word_Type;
      Server_Index       : Thread_Server_Index;
      Old_Stg_Rgn        : Stg_Rgn_Ptr := null;
      Enclosing_Lock_Obj : Lock_Obj_Index := 0);
   --  Reclaim storage of (large) object. Need to recursively release all
   --  subobjects.
   --  TBD: Farm this out to a separate thread.

   procedure Swap_Object_Exported
     (Context   : in out Exec_Context;
      Type_Info : Type_Descriptor_Ptr;
      LHS_Ptr   : Word_Ptr;
      RHS_Ptr   : Word_Ptr);
   --  We want to release the old values of
   --  the destination and source before overwriting them
   --  if necessary.
   pragma Export (Ada, Swap_Object_Exported, "_psc_swap_object");

   procedure Create_Object_Exported
     (Context      : in out Exec_Context;
      Type_Info    : Type_Descriptor_Ptr;
      Destination  : Word_Ptr;
      Existing_Obj : Word_Ptr);
   --  Slight change in arguments to make calling from llvm easy
   pragma Export (Ada, Create_Object_Exported, "_psc_create_object");

   function New_Object_Exported
     (Context      : in out Exec_Context;
      Type_Info    : Type_Descriptor_Ptr;
      Existing_Obj : Word_Type) return Word_Type;
   pragma Export (Ada, New_Object_Exported, "_psc_new_object");
   --  Create new object in region based on Existing_Obj, unless is 0.

   procedure Init_Large_Obj_Exported
     (Context      : in out Exec_Context;
      Type_Info    : Type_Descriptor_Ptr;
      New_Obj      : Word_Type);
   pragma Export (Ada, Init_Large_Obj_Exported, "_psc_init_large_obj");
   --  Init new large object in current region.

   procedure Store_Null_Of_Same_Stg_Rgn_Exported
     (Context                 : Exec_Context;
      Type_Info               : Type_Descriptor_Ptr;
      Destination             : Word_Ptr;
      Existing_Obj_In_Stg_Rgn : Word_Ptr);
   --  Slight change in arguments to make calling from llvm easy
   pragma Export (Ada, Store_Null_Of_Same_Stg_Rgn_Exported,
      "_psc_store_null_of_same_stg_rgn");

   procedure Create_Tcb (
      Context          : in out Exec_Context;
      Parallel_Master  : Word_Ptr;
      Parallel_Control : Word_Ptr;
      Num_Params : Integer);
   pragma Export (Ada, Create_Tcb, "_psc_create_tcb");
   --  Execute the Create_TCB instruction
   --  Fields of Instruction are:
   --   Parallel_Master : Object_Locator;
   --   Parallel_Control : Object_Locator;
   --   Parallel_Static_Link : Object_Locator;
   --   Num_Params : Natural;

   function New_Tcb (
      Context : in out Exec_Context;
      Parallel_Master : Word_Ptr;
      Num_Params : Integer) return Word_Ptr;
   pragma Export (Ada, New_Tcb, "_psc_new_tcb");
      --  Execute the main part of the Create_Tcb instruction.
      --  Return the value to be stored in the Parallel_Control slot.
      --  Fields of Instruction are:
      --   Parallel_Master : Object_Locator;
      --   Parallel_Control : Object_Locator;
      --   Parallel_Static_Link : Object_Locator;
      --   Num_Params : Natural;
      --   NOTE: Parallel_Master is a pseudo-large-object at the given address
      --         Parallel_Control is the address of a word which will be
      --         set to point at the space allocated for the Tcb.

   function Nth_Type_Area_Element
     (Type_Desc   : Non_Op_Map_Type_Ptr;
      Item_Offset : Offset_Within_Area;
      Type_Base   : Area_Base_Indicator := Type_Area) return Element_Info;
   --  Return the Element_Info for the Nth item in the type area that
   --  represents the given type, presuming Nth item is an object, type,
   --  or operation.

   function Null_For_Stg_Rgn (Stg_Rgn : Stg_Rgn_Ptr) return Word_Type;
   --  Return a (large) null for the given region

   function Null_For_Type_Or_Stg_Rgn
     (Type_Desc : Type_Descriptor_Ptr;
      Stg_Rgn   : Stg_Rgn_Ptr;
      Is_By_Ref : Boolean := False) return Word_Type;
   --  Return a null appropriate for the type and region and whether is a "ref"

   function Null_For_Local_Stg_Rgn (Local_Area : Word_Ptr) return Word_Type;
   --  Return a (large) null for the region associated with the local area
   --  starting at the given addr
   pragma Export (Ada, Null_For_Local_Stg_Rgn, "_psc_large_local_null");

   function Null_Of_Same_Stg_Rgn
     (Type_Info               : Type_Descriptor_Ptr;
      Existing_Obj_In_Stg_Rgn : Word_Type) return Word_Type;
   --  Return a null for the given type, and if large, for the region
   --  associated with the existing object.
   pragma Export (Ada, Null_Of_Same_Stg_Rgn,
      "_psc_null_of_same_stg_rgn");

   function Null_For_Type_Or_Local_Stg_Rgn
     (Context : Exec_Context;
      Type_Desc : Type_Descriptor_Ptr) return Word_Type;
   --  Return a (large or small) null for the local region associated with the
   --  current context, using the Type_Desc to decide whether is large.
   pragma Export (Ada, Null_For_Type_Or_Local_Stg_Rgn, "_psc_local_null");

   procedure Set_Stg_Rgn_Of_Large_Obj
     (New_Obj : Word_Type;
      Stg_Rgn : Stg_Rgn_Ptr);
   --  Set the region of a newly created (large) object

   function Stg_Rgn_Of_Large_Obj_Exported
     (Existing_Obj : Word_Type) return Stg_Rgn_Ptr;
   --  Return the region of the given (large) object
   pragma Export (Ada, Stg_Rgn_Of_Large_Obj_Exported,
                  "_psc_stg_rgn_of_large_obj");

   function Stg_Rgn_Of_Large_Obj
     (Existing_Obj : Word_Type) return Stg_Rgn_Ptr
     renames Stg_Rgn_Of_Large_Obj_Exported;

   function Stg_Rgn_Of_Large_Obj
     (Existing_Obj : Word_Ptr) return Stg_Rgn_Ptr;
   --  Return the region of the given (large) object

   function Stg_Rgn_Of_Existing_Large_Obj_Exported
     (Context                 : Exec_Context;
      Existing_Obj_In_Stg_Rgn : Word_Ptr) return Stg_Rgn_Ptr;
   --  For use by #Create_Polymorphic_Obj_Op
   pragma Export (Ada, Stg_Rgn_Of_Existing_Large_Obj_Exported,
      "_psc_stg_rgn_of_existing_large_obj");

   function Large_Obj_Stg_Rgn_Index
     (Large_Obj_Addr : Object_Virtual_Address)
      return Stg_Rgn_Index;
   --  Return region index associated with (possibly null) large object

   procedure Dump_Obj_With_Type
     (Value     : Word_Type;
      Type_Desc : Type_Descriptor_Ptr);
   --  Dump contents of obj given type descriptor

   procedure Dump_Obj (Value : Word_Type);
   --  Attempt to dump contents of object, guessing whether big or small
   pragma Export (Ada, Dump_Obj, "dump_obj");  --  For debugging

   --  Operations for extracting/inserting Object_Locator from/into ParaSail
   --  Representation in ParaSail is two integers, one is Base/Offset
   --  and the other is the "extra" LLVM information such as VM_Name,
   --  VM_Param_Name, and VM_Is_Indir.

   function Extract_VM_Obj_Id
     (Base : Area_Base_Indicator;
      Offset : Offset_Within_Area;
      VM_Info : Word_Type) return VM_Obj_Id_Type;
      --  Extract VM_Obj_Id from ParaSail representation for second word
      --  of ParaSail two-slot representation.

   function Extract_Object_Locator (Locator_PS : Word_Type)
     return Object_Locator;
      --  Extract an Object_Locator from the ParaSail 2-slot representation.

   function Insert_VM_Obj_Id (Locator : Object_Locator) return Word_Type;
      --  Create ParaSail representation for Locator.VM_Obj_Id

   function Insert_Object_Locator
     (Locator : Object_Locator;
      Obj_Locator_Type : Non_Op_Map_Type_Ptr;
      Target : Word_Type;
      Server_Index : Thread_Server_Index)
     return Word_Type;
      --  Convert object_locator to two-slot ParaSail representation
      --  given obj-locator type, target object, and server index.

--    type Large_Obj (Size : Offset_Within_Area) is private;
--    --  Type used for returning a large object from an "imported" function.
--    --  Intended to match, at least approximately, the representation used
--    --  for large objects in storage regions.
--
--    type Large_Obj_Ptr is access all Large_Obj;
--    pragma No_Strict_Aliasing (Large_Obj_Ptr);
--
   type Name_For_Object_Locator_Type is access function
     (Locator : Object_Locator;
      Enclosing_Type : Object_Locator := Null_Object_Locator)
      return String;

   Name_For_Object_Locator_Ptr : Name_For_Object_Locator_Type := null;
      --  Pointer to a function which will
      --  return a string representing the name associated with the
      --  given object locator.  Return "" if none available.

   -----------------------------------------------------------

   procedure Call_Through_Operation_Desc
     (Operation_Desc : Word_Type;
      Inputs         : Word_Array;
      Outputs        : in out Word_Array);
   --  Call a ParaSail function given an operation descriptor for it, an array
   --  of inputs, and an array of outputs to be updated.
   --  Current_Server_Index must return non-zero when making this call, as that
   --  is needed to determine the context.

   procedure Call_Through_Operation_Desc_Exported
     (Context        : in out Exec_Context;
      Operation_Desc : Word_Type;
      Params         : Word_Ptr);
   --  Call a ParaSail routine given the execution context,
   --  an operation descriptor for the routine,
   --  and a pointer to the list of parameters.
   pragma Export (Ada, Call_Through_Operation_Desc_Exported,
     "_psc_call_through_operation_desc");

   function Current_Server_Index return Thread_Server_Index;
   --  Return server index for current Ada task

   function Current_Server_Context
     (Server_Index : Thread_Server_Index := Current_Server_Index)
     return Exec_Context_RW_Ptr;
   --  Return pointer to current exec-context for given server

   procedure Execute
     (Instructions      : Routine_Ptr;
      Start_Pc          : Code_Index;
      Context           : in out Exec_Context;
      Thread_Was_Queued : out Boolean;
      Debugger_Console  : Boolean := False;
      Server_Index      : Thread_Server_Index := Main_Thread_Server_Index);
   --  This executes the instructions starting at Start_PC in the given
   --  routine, with the given Exec_Context.
   --  Note that the caller must pre-allocate a Local_Area of size indicated
   --  in the header of the Routine. Forcing the caller to pre-allocate this
   --  area can potentially minimize the number of allocations/deallocations
   --  required.
   --  Results are returned in the Params area, or in objects pointed-to from
   --  the Params area.
   --  Up-level references to an enclosing block, operation, or type area are
   --  done through static link stored in the pre-allocated local area.
   --  If Thread_Was_Queued is True upon return, then thread did *not*
   --  complete, but instead was queued waiting for a dequeue condition to
   --  be true.
   --  If Debugger_Console is true, then we immediately invoke the
   --  debugger console rather than executing instructions.
   --  Context.Server_Index is set to zero if user requests shutdown.

   function Invoke_Parameterless_Computation
     (Computation    : Routine_Ptr;
      Result_Type    : Type_Descriptor_Ptr;
      Enclosing_Type : Non_Op_Map_Type_Ptr := null;
      Server_Index   : Thread_Server_Index := Main_Thread_Server_Index)
     return Interpreter.Word_Type;
      --  Invoke a parameterless routine to compute the value of a
      --  compile-time-known constant.

   procedure Execute_Compiled_Indirect_Call_Op
     (Context            : in out Exec_Context;
      Params             : Word_Ptr;
      Static_Link        : Type_Descriptor_Ptr;
      Static_Link_Base   : Area_Base_Indicator;
      Static_Link_Offset : Offset_Within_Area;
      Target_Base        : Area_Base_Indicator;
      Target_Offset      : Offset_Within_Area;
      Op_Desc_Virt_Addr  : Object_Virtual_Address;
      Code_Address       : Routine_Code_Address;
      Info_As_Byte       : Locked_Param_Info_As_Byte_Type);
   --  Execute an indirect call on an operation in compiled code.
   --  Static link must refer to a type descriptor, not a local area.
   pragma Export (Ada, Execute_Compiled_Indirect_Call_Op,
     "_psc_execute_compiled_indirect_call_op");

   procedure Execute_Compiled_Indirect_Call_Op_With_Conv
     (Context            : in out Exec_Context;
      Params             : Word_Ptr;
      Static_Link        : Type_Descriptor_Ptr;
      Static_Link_Base   : Area_Base_Indicator;
      Static_Link_Offset : Offset_Within_Area;
      Target_Base        : Area_Base_Indicator;
      Target_Offset      : Offset_Within_Area;
      Op_Desc_Virt_Addr  : Object_Virtual_Address;
      Code_Address       : Routine_Code_Address;
      Info_As_Byte       : Locked_Param_Info_As_Byte_Type;
      Conv_Desc          : Convention_Descriptor);
   --  Execute an indirect call on an operation in compiled code.
   --  Static link must refer to a type descriptor, not a local area.
   --  Includes convention descriptor to determine calling convention.
   pragma Export (Ada, Execute_Compiled_Indirect_Call_Op_With_Conv,
     "_psc_execute_compiled_indirect_call_op_conv");

   procedure Execute_Compiled_Nth_Op_Of_Type
     (Context            : in out Exec_Context;
      Params             : Word_Ptr;
      Static_Link        : Type_Descriptor_Ptr;
      Target_Base        : Area_Base_Indicator;
      Op_Index           : Operation_Index);
   --  Execute the nth operation of the type identified by the Static link.
   --  Static link must refer to a type descriptor.
   pragma Export (Ada, Execute_Compiled_Nth_Op_Of_Type,
     "_psc_execute_compiled_nth_op_of_type");

   procedure Execute_Compiled_Parallel_Call_Op (Context : in out Exec_Context;
      Master_Address           : Word_Ptr;
      New_Tcb                  : Word_Ptr;       -- Parallel_Control
      Code_Address             : Routine_Code_Address; -- Parallel_Call_Target
      Static_Link              : Word_Ptr;  -- Parallel_Static_Link
      Internal_Precond_Address : Nested_Blk_Address;
      Tcb_Is_Local             : Boolean;   -- Whether tcb addr in Local_Area
      Is_Start_Op              : Boolean;   -- Whether is Start_Par vs. Add_Par
      Info_As_Byte             : Locked_Param_Info_As_Byte_Type);
   --  Execute a call on a parallel nested block in compiled code.
   pragma Export (Ada, Execute_Compiled_Parallel_Call_Op,
     "_psc_execute_compiled_parallel_call_op");

   procedure Execute_Compiled_Parallel_Call_Op_With_Conv
     (Context                  : in out Exec_Context;
      Master_Address           : Word_Ptr;
      New_Tcb                  : Word_Ptr;       -- Parallel_Control
      Code_Address             : Routine_Code_Address; -- Parallel_Call_Target
      Static_Link              : Word_Ptr;  -- Parallel_Static_Link
      Internal_Precond_Address : Nested_Blk_Address;
      Tcb_Is_Local             : Boolean;   -- Whether tcb addr in Local_Area
      Is_Start_Op              : Boolean;   -- Whether is Start_Par vs. Add_Par
      Info_As_Byte             : Locked_Param_Info_As_Byte_Type;
      Conv_Desc                : Convention_Descriptor);
   --  Execute a call on a parallel nested block in compiled code,
   --  with convention descriptor.
   pragma Export (Ada, Execute_Compiled_Parallel_Call_Op_With_Conv,
     "_psc_execute_compiled_parallel_call_op_conv");

   procedure Execute_Compiled_Indirect_Parallel_Call_Op (
      Context            : in out Exec_Context;
      Master_Address     : Word_Ptr;
      New_Tcb            : Word_Ptr;       -- Parallel_Control
      Target_Base        : Area_Base_Indicator;
      Target_Offset      : Offset_Within_Area;
      Static_Link        : Type_Descriptor_Ptr;       -- Parallel_Static_Link
      Static_Link_Base   : Area_Base_Indicator;
      Static_Link_Offset : Offset_Within_Area;
      Tcb_Is_Local       : Boolean;  -- Whether tcb address is in Local_Area
      Is_Start_Op        : Boolean;  -- Whether is Start_Par vs. Add_Par
      Info_As_Byte       : Locked_Param_Info_As_Byte_Type);
   pragma Export (Ada, Execute_Compiled_Indirect_Parallel_Call_Op,
      "_psc_execute_compiled_indirect_parallel_call_op");

   procedure Execute_Compiled_Parallel_Op (Context : in out Exec_Context;
      Master_Address : Word_Ptr;
      New_Tcb        : Word_Ptr;       -- Parallel_Control
      Code_Address   : Nested_Blk_Address;  -- Parallel_Code_Block
      Static_Link    : Word_Ptr;   -- Parallel_Static_Link
      Uses_Queuing   : Boolean;   -- Whether nested blk uses queuing
      Tcb_Is_Local   : Boolean;   -- Whether tcb address is in Local_Area
      Is_Start_Op    : Boolean);  -- Whether is Start_Par vs. Add_Par
   --  Execute a call on a parallel nested block in compiled code.
   pragma Export (Ada, Execute_Compiled_Parallel_Op,
     "_psc_execute_compiled_parallel_op");

   function Execute_Wait_For_Parallel_Op
     (Context      : Exec_Context_RW_Ptr;
      Master_Addr  : Word_Ptr) return Nested_Block_Outcome_As_Int;
   --  Execute the Wait_For_Parallel_Op instructions
   pragma Export (Ada, Execute_Wait_For_Parallel_Op,
     "_psc_execute_wait_for_parallel_op");

   function Execute_Prepare_To_Exit_Parallel_Op
     (Context        : in out Exec_Context;
      Master_Address : Word_Ptr) return Boolean;
   --  Execute the Prepare_To_Exit_Parallel_Op instructions
   --  Return True if it succeeds; return False if some other
   --  picothread already performed prepare-to-exit on specified
   --  master.
   --  If returning False, outcome has been set on enclosing master(s)
   --  so appropriate "abrupt" exit will occur.
   pragma Export (Ada, Execute_Prepare_To_Exit_Parallel_Op,
      "_psc_execute_prepare_to_exit_parallel_op");

   procedure Raise_Exception_Occurrence
     (Context : in out Exec_Context;
      Excep_Obj : in out Word_Type);
      --  Raise exception given the exception object
      --  The exception object is moved to the handler's stg-rgn.

   function Unwrapped_Polymorphic_Obj
     (Source_Type_Info : Type_Descriptor_Ptr;
      Dest_Type_Info   : Type_Descriptor_Ptr;
      Poly_Obj         : Word_Type)
     return Word_Type;
      --  Execute the Unwrap_Polymorphic_Obj instruction
      --  by checking if underlying type matches Type_Info,
      --  and return a ref to underlying obj if so, or null ref otherwise.
   pragma Export
      (Ada, Unwrapped_Polymorphic_Obj, "_psc_unwrapped_polymorphic_obj");

   procedure Create_Polymorphic_Obj (Context : in out Exec_Context;
      Stg_Rgn_For_Creation : Stg_Rgn_Ptr;
      Dest_Addr : Word_Ptr;
      Poly_Type_Desc : Non_Op_Map_Type_Ptr);
   --  Create a polymorphic object
   pragma Export (Ada, Create_Polymorphic_Obj, "_psc_create_polymorphic_obj");

   procedure Create_Poly_Obj_Exported (Context : in out Exec_Context;
      Existing_Obj : Word_Type;
      Dest_Addr : Word_Ptr;
      Poly_Type_Desc : Non_Op_Map_Type_Ptr);
   --  Create a polymorphic object in region of Existing_Obj, if any
   pragma Export (Ada, Create_Poly_Obj_Exported, "_psc_create_poly_obj");

   function Execution_Source_Pos
     (Server_Index : Thread_Server_Index := Current_Server_Index)
      return Source_Positions.Source_Position;
   --  Return source position associated with currently executing instruction
   --  for given server.

   procedure Free_Initial_Tcb
     (Initial_Tcb  : Word_Ptr;
      Server_Index : Thread_Server_Index);
   --  Release initial tcb/master resources

   function Initial_Tcb
     (Target_Routine : Routine_Ptr;
      New_Tcb        : Word_Ptr;
      Server_Index   : Thread_Server_Index) return Word_Ptr;
   --  Return the addr of an initialized TCB allocated at the specified
   --  address, suitable for use as the outermost TCB for a call on the
   --  specified routine.
   --  New_Tcb must provide room for both a TCB and a master, i.e.
   --  Thread_Control_Block_Size + Thread_Master_Size.

   function Initial_Context return Exec_Context_RW_Ptr;
   --  Return a pointer to an Exec_Context which can be passed to the
   --  main routine of a compiled program.
   pragma Export (Ada, Initial_Context, "_psc_initial_context");

   subtype Argv_Array_Type is Interfaces.C.Strings.chars_ptr_array (0 .. 1000);
   --  Using constrained array subtype to avoid issue of array dope

   function Arg_Array (Context : Exec_Context_RW_Ptr; Argc : Integer;
     Argv : Argv_Array_Type) return Word_Type;
   --  Create a Basic_Array<Univ_String> object from the arguments
   --  passed to the main routine of a compiled ParaSail program.
   pragma Export (Ada, Arg_Array, "_psc_arg_array");

   procedure Show_Stats (Clear : Boolean := False);
   --  Show statistics, and clear the accumulators if Clear is True

   procedure Show_Servers;
   --  Show number of servers and other information about servers.

   procedure Set_Server_Count (Max_Servers : Natural);
   --  Set maximum number of servers to be used by the interpreter.

   procedure Bump_Server_Count (Amount : Integer);
   --  Bump the maximum number of servers to be used by the interpreter by
   --  the given Amount.

   procedure Start_Up_Thread_Servers;
   --  Start up the default number of thread servers

   procedure Shut_Down_Thread_Servers (Total_Errors : Natural);
   --  Shut down all the thread servers so program can end normally.
   pragma Export
     (C, Shut_Down_Thread_Servers, "_psc_shut_down_thread_servers");

   --  Types and object for global linked list of per-file-initializers

   type Per_File_Init;
   type Per_File_Init_Ptr is access all Per_File_Init;
   for Per_File_Init_Ptr'Storage_Size use 0;  --  These do not live on heap

   type Per_File_Initializer is access procedure;
      --  Initializes one compiled ParaSail file's data

   type Per_File_Init is record
      Next  : Per_File_Init_Ptr;
      To_Do : Per_File_Initializer;
   end record;

   Per_File_Initializer_List : Per_File_Init_Ptr;
   pragma Export (Ada, Per_File_Initializer_List, "_psc_global_to_do");
      --  TBD: this is initialized before elaboration -- should be Import?

   procedure Invoke_Per_File_Initializers;
   --  Execute all of the per-file initializer functions

   --  Debug flags

   Debug_Threading  : Boolean := False;
   --  If true, then interpreter is verbose
   Debug_Stg_Rgns   : Boolean := False;
   --  If true, then interpreter indicates how regions are being used
   Debug_Calls      : Boolean := False;
   --  If true, gives a simple call trace
   Debug_Type_Descs : Boolean := False;
   --  If true, give info on duplicately named type descriptors

   Debug_Statistics : Boolean := True;
   --  If true, then threading statistics are produced at end
   pragma Export (Ada, Debug_Statistics, "_psc_debug_statistics");

private

   type Reclamation_Info_Record;
   type Reclamation_Info_Ptr is access all Reclamation_Info_Record;

   type Stg_Rgn_Manager_Type;
   type Stg_Rgn_Manager_Ptr is access Stg_Rgn_Manager_Type;

   type Stg_Rgn_Action_Type is abstract tagged limited null record;
   --  Extend this type and override Do_Stg_Rgn_Action to define
   --  an action to be performed on a shared stg rgn.

   procedure Do_Stg_Rgn_Action
     (Act : in out Stg_Rgn_Action_Type;
      Shared_Rgn : Stg_Rgn_Ptr;
      Server_Index : Thread_Server_Index) is abstract;
   --  A general mechanism for doing an action on a shared stg rgn,
   --  available to children of the Interpreter package.

   procedure Invoke_Shared_Stg_Rgn_Action
     (Shared_Rgn : Stg_Rgn_Ptr;
      Action : in out Stg_Rgn_Action_Type'Class;
      Server_Index : Thread_Server_Index);
   --  Invoke the given action while inside the manager of the
   --  given shared-stg-rgn manager.

   procedure Null_Routine_Body
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  This routine does nothing

   Null_Str     : constant Strings.U_String :=
                    Strings.String_Lookup ("null");

   Null_Routine : constant Routine_RW_Ptr :=
     new Routine'(
      Is_PSVM_Routine     => False,
      Index               => 0,
      Name                => Null_Str,
      Num_Prior_Homonyms  => 0,
      Name_With_Overloading_Index => Null_Str,
      Full_Module_Name    => Strings.Null_U_String,
      Uses_Queuing        => False,
      Local_Area_Length   => 0,
      Start_Callee_Locals => 0,
      Boundary_Conditions => (others => Null_Code_Block_Descriptor),
      Parameters          => null,
      Nesting_Level       => 0,
      Convention          => Languages.Convention_Enum'First,
      Conv_Desc           => Null_Conv_Desc,
      Routine_Addr        => Null_Routine_Body'Access,
      Built_In_Desig      => Strings.String_Lookup ("#null"),
      Is_Compiled_Routine => False,
      Is_Nested_Block     => False,
      Internal_Precond_Addr => null);

   -------- Header of large objects --------

   type Large_Obj_Header is record
      Size      : Offset_Within_Area := 0; --  64K size limit (in 64-bit words)
      Stg_Rgn   : Stg_Rgn_Index      := 0; --  16 bit unique region index
      Type_Info : Type_Index         := 0; --  16 bit unique type index
      Lock_Obj  : Lock_Obj_Index     := 0; --  15 bit unique lock index
      On_Stack  : Boolean            := False;  --  1 bit on-stack flag
   end record;

   for Large_Obj_Header use record
      --  NOTE: We are trying to match a pragma Pack
      --        but with a specified a layout.
      Size      at 0 range  0 .. 15;
      Stg_Rgn   at 0 range 16 .. 31;
      Type_Info at 0 range 32 .. 47;
      Lock_Obj  at 0 range 48 .. 62;
      On_Stack  at 0 range 63 .. 63;
   end record;

   for Large_Obj_Header'Size use Word_Size * Large_Obj_Header_Size;

   type Large_Obj_Header_Ptr is access all Large_Obj_Header;
   for Large_Obj_Header_Ptr'Storage_Size use 0;  --  not for allocators
   pragma No_Strict_Aliasing (Large_Obj_Header_Ptr);

--    type Large_Obj (Size : Offset_Within_Area) is record
--       Stg_Rgn   : Stg_Rgn_Index := 0;    --  16 bit unique region index
--       Type_Info : Type_Index := 0;       --  16 bit unique type index
--       Lock_Obj  : Lock_Obj_Index := 0;   --  15 bit unique lock index
--       On_Stack  : Boolean := False;      --   1 bit is-on-stack flag
--
--       Data      : Word_Array (2 .. Size) := (others => 0);
--       --  Rest of large object
--    end record;
--
--    for Large_Obj use record
--       --  Representation intended to match that used in the Large_Obj header
--       Size      at 0 range  0 .. 15;
--       Stg_Rgn   at 0 range 16 .. 31;
--       Type_Info at 0 range 32 .. 47;
--       Lock_Obj  at 0 range 48 .. 62;
--       On_Stack  at 0 range 63 .. 63;
--    end record;
--
   function To_Large_Obj_Ptr is
     new Ada.Unchecked_Conversion (Word_Ptr, Large_Obj_Header_Ptr);

   function To_Large_Obj_Ptr is
     new Ada.Unchecked_Conversion (Word_Type, Large_Obj_Header_Ptr);

   function To_Size_In_Words
     (Size_In_Bits : Natural) return Offset_Within_Area;
   --  Convert a bit count into a word count

   procedure Set_Large_Obj_Size
     (Large_Obj_Addr : Object_Virtual_Address;
      Size           : Offset_Within_Area);
   --  Set size in words associated with large object

   --------------------------
   -- Large_Obj_Header_Ops --
   --------------------------

   package Large_Obj_Header_Ops is

      function Large_Obj_Lock_Obj
        (Large_Obj_Addr : Object_Address) return Lock_Obj_Index;
      --  Return lock-obj index associated with given large obj; zero means it
      --  doesn't have a lock obj.

      function Large_Obj_Lock_Obj
        (Large_Obj_Addr : Object_Virtual_Address)
         return Lock_Obj_Index;
      --  Return lock-obj index associated with given large obj; zero means it
      --  doesn't have a lock obj.

      function Large_Obj_Lock_Obj
        (Large_Obj_Addr : Word_Ptr) return Lock_Obj_Index;
      --  Return lock-obj index associated with given large obj; zero means it
      --  doesn't have a lock obj.

      function Large_Obj_Next_Block
        (Large_Obj_Addr : Object_Virtual_Address)
         return Object_Virtual_Address;
      --  Return link to next block in free-block chain

      function Large_Obj_On_Stack
        (Addr : Object_Virtual_Address) return Boolean;
      --  Return True if given non-null large obj is residing on stack

      function Large_Obj_Size
        (Large_Obj_Addr : Object_Address)
         return Offset_Within_Chunk;
      --  Return size in words associated with large object

      function Large_Obj_Size
        (Large_Obj_Addr : Object_Virtual_Address)
         return Offset_Within_Chunk;
      --  Return size in words associated with large object

      function Large_Obj_Size
        (Large_Obj_Addr : Word_Ptr) return Offset_Within_Chunk;
      --  Return size in words associated with large object

      function Large_Obj_Stg_Rgn_Index
        (Large_Obj_Addr : Object_Virtual_Address)
         return Stg_Rgn_Index;
      --  Return region index associated with (possibly null) large object

      function Large_Obj_Stg_Rgn_Index
        (Large_Obj_Addr : Word_Ptr) return Stg_Rgn_Index;
      --  Return region associated with large object

      function Large_Obj_Type_Info
        (Large_Obj_Addr : Object_Address)
         return Type_Index;
      --  Return type index associated with given large obj; zero means it is a
      --  large "null."

      function Large_Obj_Type_Info
        (Large_Obj_Addr : Object_Virtual_Address)
         return Type_Index;
      --  Return type index associated with given large obj;

      function Large_Obj_Type_Info_Is_In_Range
        (Large_Obj_Addr : Object_Virtual_Address)
         return Boolean;
      --  Return True if type info of given large object is in range of
      --  existing type indices. This is for debugging mostly.

      procedure Set_Large_Obj_Header
        (Large_Obj_Addr : Object_Address;
         Size           : Offset_Within_Area;
         Stg_Rgn_Id     : Stg_Rgn_Index;
         Type_Id        : Type_Index;
         Lock_Obj       : Lock_Obj_Index := 0;
         On_Stack       : Boolean := False);
      --  Fill in header given large object address, and size/region/type
      --  information

      procedure Set_Large_Obj_Header
        (Large_Obj_Addr : Object_Virtual_Address;
         Size           : Offset_Within_Area;
         Stg_Rgn_Id     : Stg_Rgn_Index;
         Type_Id        : Type_Index;
         Lock_Obj       : Lock_Obj_Index := 0;
         On_Stack       : Boolean := False);
      --  Fill in header given large object address, and size/region/type
      --  information

      procedure Set_Large_Obj_Lock_Obj
        (Large_Obj_Addr : Object_Address;
         Lock_Obj       : Lock_Obj_Index);
      --  Set lock obj associated with large object

      procedure Set_Large_Obj_Lock_Obj
        (Large_Obj_Addr : Object_Virtual_Address;
         Lock_Obj       : Lock_Obj_Index);
      --  Set lock obj associated with large object

      procedure Set_Large_Obj_Lock_Obj
        (Large_Obj_Addr : Word_Ptr;
         Lock_Obj       : Lock_Obj_Index);
      --  Set lock obj associated with large object

      procedure Set_Large_Obj_Next_Block
        (Large_Obj_Addr : Object_Address;
         Next_Block     : Object_Virtual_Address);
      --  Set link to next block in free-block chain

      procedure Set_Large_Obj_Next_Block
        (Large_Obj_Addr : Object_Virtual_Address;
         Next_Block     : Object_Virtual_Address);
      --  Set link to next block in free-block chain

      procedure Set_Large_Obj_On_Stack
        (Large_Obj_Addr : Object_Virtual_Address;
         On_Stack       : Boolean);
      --  Indicate whether given object resides on stack rather than in stg rgn

      procedure Set_Large_Obj_Size
        (Large_Obj_Addr : Object_Address;
         Size           : Offset_Within_Area);
      --  Set size in words associated with large object

      procedure Set_Large_Obj_Size
        (Large_Obj_Addr : Object_Virtual_Address;
         Size           : Offset_Within_Area);
      --  Set size in words associated with large object

      procedure Set_Large_Obj_Stg_Rgn_Index
        (Large_Obj_Addr : Object_Virtual_Address;
         Stg_Rgn_Id     : Stg_Rgn_Index);
      --  Set region associated with large object

      procedure Set_Large_Obj_Type_Info
        (Large_Obj_Addr : Object_Address;
         Type_Id        : Type_Index);
      --  Set type associated with large object

      procedure Set_Large_Obj_Type_Info
        (Large_Obj_Addr : Object_Virtual_Address;
         Type_Id        : Type_Index);
      --  Set type associated with large object

      function To_Large_Obj_Ptr
        (Large_Obj_Addr : Object_Address)
         return Large_Obj_Header_Ptr;
      --  Convert Object_Address of large object to a pointer to header type

   end Large_Obj_Header_Ops;
   use Large_Obj_Header_Ops;

   procedure Set_Large_Obj_Size
     (Large_Obj_Addr : Object_Virtual_Address;
      Size           : Offset_Within_Area)
     renames Large_Obj_Header_Ops.Set_Large_Obj_Size;

   Type_Indicator : constant Word_Type := 16#5AFE_4DAD_5AFE_4DAD#;
      --  This is a special value stored in the first word of every type desc.
      --  It must be an odd number to ensure it doesn't equal any
      --  legal word address.

   for Type_Descriptor use record
      Magic_Number at 0 range 0 .. Word_Type'Size - 1;
         --  Make sure this "magic number" is first
   end record;

   Stg_Rgn_Table : array (1 .. Stg_Rgn_Index'Last) of Stg_Rgn_Ptr;
   --  Global table of Stg_Rgn pointers.

   procedure Allocate_From_Unshared_Stg_Rgn
     (Stg_Rgn       : Stg_Rgn_Ptr;
      Size_In_Words : Offset_Within_Area;
      Obj_Addr      : out Word_Type;
      Server_Index  : Thread_Server_Index);
      --  This attempts to reuse storage freed by
      --  Deallocate_From_Unshared_Stg_Rgn.
      --  Initialize allocated space with region/size/null-type.
      --  Caller gets a lock if needed.
   pragma Inline (Allocate_From_Unshared_Stg_Rgn);

   procedure Deallocate_From_Unshared_Stg_Rgn
     (Stg_Rgn         : Stg_Rgn_Ptr;
      Storage_Address : Object_Virtual_Address;
      Server_Index    : Thread_Server_Index);
      --  This adds the given storage to a list indexed by the size.
      --  Caller gets a lock if needed.
      --  Requires: Object has size embedded in its header.
   pragma Inline (Deallocate_From_Unshared_Stg_Rgn);

end PSC.Interpreter;
