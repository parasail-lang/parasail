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
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

with PSC.Source_Positions;
with PSC.Symbols;
with PSC.Trees;
package PSC.Object_Access is
   --  Keep track of which sets of objects are read and written
   --  by different parts of a program tree.  This is used
   --  to detect illegal aliasing and possible race conditions.

   type Read_Write_Combination_Enum is
     (Not_Combined, Sequential, Within_Operation, Uplevel_Refs, Parallel);
   --  How read/write access should be combined over a list
   --  of trees.
   --  "Not_Combined" means each element is independent.
   --  There is no carryover.
   --  Sequential means there is never a conflict, but the accesses
   --  are combined.
   --  Within_Operation means that these are parameters to an operation
   --  and there should be no aliasing between parameters that can have
   --  a subcomponent updated and any other parameter.
   --  Uplevel_Refs means that the Read_Write_Mapping being added in
   --  corresponds to uplevel references, which might conflict with
   --  other updates being performed via parameters.
   --  Parallel means the operations might execute in parallel.

   type Access_Mode_Enum is (
     No_Access,
     Param_Computation,
     Ref_Access,
     Ref_Const_Access,
     Ref_Var_Access,
     Read_Access,
     Update_Access);
   --  Kind of access performed on an object
   --  Param_Computation means that a parameter is computed,
   --  but not yet used for anything.
   --  Ref_Access means that a reference is made to the
   --  object, and it is potentially incorporated into the
   --  result of the operation.  This implies a Read_Access to the
   --  object, and possibly a later write access when the operation result
   --  is used.  Ref_Var_Access implies an immediate write to the
   --  object, as well as a writable-ref incorporated into the operation
   --  result.  Ref_Const_Access implies an immedate read as well as a
   --  possible future read.

   --  Objects are sorted into one of the following in the read-write map
   subtype Recorded_Access_Mode_Enum is
     Access_Mode_Enum range Ref_Const_Access .. Update_Access;

   type Component_Access_Enum is (
     No_Access,
     Selected_Component_Access,
     Indexed_Component_Access,
     Whole_Object_Access);
   --  Whether all or just part of the object is accessed.

   ---------- Mapping from objects to kinds of access -----------

   type Read_Write_Mapping is private;

   procedure Initialize (Overall : out Read_Write_Mapping);
   --  Initialize read/write mapping to empty

   procedure Move (From : in out Read_Write_Mapping;
     To : out Read_Write_Mapping);
   --  Move mapping "From" to "To" leaving "From" empty.

   procedure Copy (From : Read_Write_Mapping;
     To : out Read_Write_Mapping;
     New_Source_Pos : Source_Positions.Source_Position :=
       Source_Positions.Null_Source_Position);
   --  Copy mapping "From" to "To" leaving "From" as is.
   --  If New_Source_Pos is not null, then shift source positions
   --  of the references to the given source position (this is used
   --  for uplevel references associated with a given call).

   procedure Combine
     (Overall : in out Read_Write_Mapping;
      Addition : in out Read_Write_Mapping;
      How_Combined : Read_Write_Combination_Enum;
      Mode : Access_Mode_Enum := Read_Access);
   --  Combine two read-write mappings.
   --  Destroy Addition as a side-effect.
   --  Complain if two mappings conflict and How_Combined is Parallel
   --  or if two references conflict and How_Combined is Within_Operation,
   --  and one of them can be updated (or more precisely, can have
   --  a subcomponent updated).
   --  If How_Combined is Uplevel_Refs, then the Addition is the set of
   --  accesses made via uplevel references.

   procedure Check_Concurrent_Loop_Body
     (Loop_Region : Symbols.Region_Ptr; Loop_Body_RW : Read_Write_Mapping);
   --  Check for conflicts within a concurrent loop body,
   --  given the R/W mapping for the loop body, and the region of the loop.

   procedure Extract_Uplevel_Refs
     (Op_Region : Symbols.Region_Ptr; All_Refs : in out Read_Write_Mapping;
      Uplevel_Refs : out Read_Write_Mapping);
   --  Copy any uplevel references from All_Refs into Uplevel_Refs, given
   --  the parameter region for the current operation.

   function Is_Empty (Read_Write : Read_Write_Mapping) return Boolean;
   --  Return True if Read_Write has no objects in it.

   function Num_Objects (Read_Write : Read_Write_Mapping) return Natural;
   --  Return count of number of objects in Read_Write

   procedure Dump_Read_Write_Mapping (Read_Write : Read_Write_Mapping);
   --  Dump tables in read/write mapping

   ----------- Unique Object Ids ---------------

   type Object_Id_Type is private;
   Null_Object_Id : constant Object_Id_Type;

   function New_Object_Id (Sym : Symbols.Sym_Ptr) return Object_Id_Type;
   --  Assign a unique Id for an object.

   procedure Refer_To_Object
     (Read_Write : in out Read_Write_Mapping;
      Obj : Object_Id_Type;
      Mode : Access_Mode_Enum;
      Source_Pos : Source_Positions.Source_Position);
   --  Record an object reference of given Mode, at given source position.

   procedure Refer_To_Ref
     (Read_Write : in out Read_Write_Mapping;
      Saved_Mapping_Of_Ref : Read_Write_Mapping;
      Obj : Object_Id_Type;
      Mode : Access_Mode_Enum;
      Source_Pos : Source_Positions.Source_Position);
   --  Record a reference to a "ref" of given Mode, at given source position.

   procedure Refer_To_Selected_Component
     (Read_Write : in out Read_Write_Mapping;
      Enclosing_Object : in out Read_Write_Mapping;
      Selector : Symbols.Sym_Ptr;
      Mode : Access_Mode_Enum;
      Source_Pos : Source_Positions.Source_Position);
   --  Record a selected-component reference of given Mode,
   --  at given source position.

   procedure Refer_To_Indexed_Component
     (Read_Write : in out Read_Write_Mapping;
      Enclosing_Object : in out Read_Write_Mapping;
      Domain : Trees.Root_Sem_Ptr;
      Index : Trees.Root_Sem_Ptr;
      Mode : Access_Mode_Enum;
      Source_Pos : Source_Positions.Source_Position);
   --  Record an indexed-component reference of given Mode,
   --  at given source position.

   procedure Save_Mapping_For_Ref
     (Mapping_Of_Ref : out Read_Write_Mapping;
      Initial_Value_Mapping : Read_Write_Mapping;
      Obj : Object_Id_Type;
      Source_Pos : Source_Positions.Source_Position);
   --  Save r/w mapping from initial value for future use.

private

   type Read_Write_Mapping_Rec;
   type Read_Write_Mapping is access Read_Write_Mapping_Rec;
   --  TBD  Some kind of hash table indexed by object-id, etc.

   Max_Obj_Num : constant := 100_000;

   type Object_Id_Type is record
      Num : Natural range 0 .. Max_Obj_Num := 0;
      Sym : Symbols.Sym_Ptr; --  For debugging only
   end record;

   Null_Object_Id : constant Object_Id_Type := (Num => 0, Sym => null);

end PSC.Object_Access;
