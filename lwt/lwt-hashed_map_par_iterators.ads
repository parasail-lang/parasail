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
--  Provide Iterate for Hashed_Maps that returns a parallel iterator.

with Ada.Containers.Hashed_Maps;
with LWT.Parallelism;
generic
   with package Maps is
     new Ada.Containers.Hashed_Maps (<>);
package LWT.Hashed_Map_Par_Iterators is
   package Map_Par_Iterator_Interfaces is
     new LWT.Parallelism.Parallel_Iterator_Interfaces
        (Maps.Map_Iterator_Interfaces);

   function Par_Iterate (Container : Maps.Map)
     return Map_Par_Iterator_Interfaces.Parallel_Iterator'Class;
   --  Return iterator over Container that can be split into chunks

end LWT.Hashed_Map_Par_Iterators;
