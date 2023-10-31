------------------------------------------------------------------------------
--                            Ada 202X Parallelism                          --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
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
------------------------------------------------------------------------------

generic
   type Cursor;
   with function Has_Element (Position : Cursor) return Boolean;
   pragma Unreferenced (Has_Element);

package LWT.Par_Iterator_Interfaces is
   pragma Pure;

   type Forward_Iterator is limited interface;

   function First
     (Object : Forward_Iterator) return Cursor is abstract;
   function Next
     (Object   : Forward_Iterator;
      Position : Cursor) return Cursor is abstract;

   type Reversible_Iterator is limited interface and Forward_Iterator;

   function Last
     (Object : Reversible_Iterator) return Cursor is abstract;
   function Previous
     (Object   : Reversible_Iterator;
      Position : Cursor) return Cursor is abstract;

   type Parallel_Iterator is limited interface and Forward_Iterator;
   subtype Chunk_Index is Positive;
   function Is_Split (Object : Parallel_Iterator)
      return Boolean is abstract;
   procedure Split_Into_Chunks (Object     : in out Parallel_Iterator;
                                Max_Chunks :        Chunk_Index) is abstract
      with Pre'Class   => not Object.Is_Split or else raise Program_Error,
           Post'Class  => Object.Is_Split and then
                          Object.Chunk_Count <= Max_Chunks;
   function Chunk_Count (Object : Parallel_Iterator)
      return Chunk_Index is abstract
      with Pre'Class   => Object.Is_Split or else raise Program_Error;
   function First (Object : Parallel_Iterator;
                   Chunk  : Chunk_Index) return Cursor is abstract
      with Pre'Class   => (Object.Is_Split and then
                              Chunk <= Object.Chunk_Count)
                           or else raise Program_Error;
   function Next (Object   : Parallel_Iterator;
                  Position : Cursor;
                  Chunk    : Chunk_Index) return Cursor is abstract
      with Pre'Class   => (Object.Is_Split and then
                              Chunk <= Object.Chunk_Count)
                           or else raise Program_Error;
   type Parallel_Reversible_Iterator is limited interface
      and Parallel_Iterator and Reversible_Iterator;
end LWT.Par_Iterator_Interfaces;
