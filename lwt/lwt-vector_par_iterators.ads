------------------------------------------------------------------------------
--                            Ada 202X Parallelism                          --
--                                                                          --
--                     Copyright (C) 2012-2023, AdaCore                     --
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
--  Provide version of Iterate for Vectors that returns a parallel iterator.

with Ada.Containers.Vectors;
with LWT.Generic_Par_Iterators;
generic
   with package Vectors is
     new Ada.Containers.Vectors (<>);
package LWT.Vector_Par_Iterators is
   use Vectors;

   package Par_Iterators is new
     Generic_Par_Iterators
       (Container => Vector,
        Cursor => Cursor,
        Element_Type => Element_Type,
        Seq_Iterator_Interfaces => Vector_Iterator_Interfaces,
        Default_Iterator_Type =>
          Vector_Iterator_Interfaces.Reversible_Iterator'Class,
        Constant_Reference_Type => Constant_Reference_Type);

   package Par_Iterator_Interfaces
     renames Par_Iterators.Par_Iterator_Interfaces;

   subtype Par_Iterable_Vector is Par_Iterators.Par_Iterable_Container;

   function Par_Iterable (Container : Vector)
     return Par_Iterable_Vector
     renames Par_Iterators.Par_Iterable;

   function Iterate (Container : Par_Iterable_Vector)
     return Par_Iterator_Interfaces.Parallel_Iterator'Class
     renames Par_Iterators.Iterate;
   --  Return iterator over Container that can be split into chunks

end LWT.Vector_Par_Iterators;
