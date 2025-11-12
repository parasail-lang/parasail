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
--  Provide Iterate for Hashed_Maps that returns a parallel iterator.

with Ada.Containers.Hashed_Maps;
with LWT.Generic_Par_Iterators;
generic
   with package Maps is
     new Ada.Containers.Hashed_Maps (<>);
package LWT.Hashed_Map_Par_Iterators is
   use Maps;

   package Par_Iterators is
     new LWT.Generic_Par_Iterators
       (Container => Map,
        Cursor => Cursor,
        Element_Type => Element_Type,
        Seq_Iterator_Interfaces => Map_Iterator_Interfaces,
        Default_Iterator_Type =>
          Map_Iterator_Interfaces.Forward_Iterator'Class,
        Iterate => Maps.Iterate,
        Constant_Reference_Type => Constant_Reference_Type);

   package Par_Iterator_Interfaces
     renames Par_Iterators.Par_Iterator_Interfaces;

   subtype Par_Iterable_Map is Par_Iterators.Par_Iterable_Container;

   function Par_Iterable (Container : Map)
     return Par_Iterable_Map
     renames Par_Iterators.Par_Iterable;

   function Iterate (Container : Par_Iterable_Map)
     return Par_Iterator_Interfaces.Parallel_Iterator'Class
     renames Par_Iterators.Iterate;
   --  Return iterator over Container that can be split into chunks

end LWT.Hashed_Map_Par_Iterators;
