------------------------------------------------------------------------------
--                            Ada 202X Parallelism                          --
--                                                                          --
--                     Copyright (C) 2012-2025, AdaCore                     --
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
--  Provide Iterate for container that returns a parallel iterator.

with Ada.Containers;
with Ada.Iterator_Interfaces;
with LWT.Parallelism;
generic
   type Container (<>) is tagged limited private;
   type Cursor is private;
   type Element_Type is private;

   with package Seq_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor, others => <>);
   type Default_Iterator_Type (<>) is
     limited new Seq_Iterator_Interfaces.Forward_Iterator with private;

   with function Iterate (C : Container)
     return Default_Iterator_Type is <>;

   with function Length (C : Container)
     return Ada.Containers.Count_Type is <>;

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is
   limited private;
   with function Constant_Reference
     (C : aliased in Container;
      Position : Cursor)
      return Constant_Reference_Type is <>;

package LWT.Generic_Par_Iterators is

   package Par_Iterator_Interfaces is
     new LWT.Parallelism.Parallel_Iterator_Interfaces
       (Seq_Iterator_Interfaces);

   type Par_Iterable_Container  --  Read-only iterator for now
    (Con : not null access constant Container) is
     tagged limited null record
     with Constant_Indexing => Constant_Reference1,
          Default_Iterator  => Iterate,
          Iterator_Element  => Element_Type;

   function Par_Iterable (Con : Container)
     return Par_Iterable_Container
     is ((Con => Con'Unchecked_Access));
   
   function Constant_Reference1
     (C : aliased in Par_Iterable_Container;
      Position : Cursor)
     return Constant_Reference_Type
     is (Constant_Reference (C.Con.all, Position));

   function Iterate (Container : Par_Iterable_Container)
     return Par_Iterator_Interfaces.Parallel_Iterator'Class;

end LWT.Generic_Par_Iterators;
