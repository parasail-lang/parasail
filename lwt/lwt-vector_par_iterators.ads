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
--  Provide version of Iterate for Vectors that returns a parallel iterator.

with Ada.Containers.Vectors;
with LWT.Parallelism;
generic
   with package Vectors is
     new Ada.Containers.Vectors (<>);
package LWT.Vector_Par_Iterators is
   package Vector_Par_Iterator_Interfaces is
     new LWT.Parallelism.Parallel_Iterator_Interfaces
        (Vectors.Vector_Iterator_Interfaces);

   function Par_Iterate (Container : Vectors.Vector)
     return Vector_Par_Iterator_Interfaces.Parallel_Iterator'Class;
   --  Return iterator over Container that can be split into chunks

end LWT.Vector_Par_Iterators;
