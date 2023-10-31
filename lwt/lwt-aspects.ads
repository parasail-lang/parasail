------------------------------------------------------------------------------
--                          Generalized Aspects                             --
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

--  Prototype implementation of LWT.Aspects package

package LWT.Aspects is
   --  This supports a generalized aspect.

   type Root_Aspect is tagged limited null record;
      --  This type is used to define aspects that can be passed through
      --  to the underlying LWT scheduler.  Various operations given in
      --  LWT.Parallelism and LWT.Scheduler allow "access Root_Aspect'Class"
      --  as a parameter.
      --  Components of a record extension of this type may be specified
      --  in the optional aspect specification attached to a
      --  chunk specification of a parallel construct.
      --  Those without default values are mandatory; those with defaults
      --  need not be specified.  Only named notation may be used, but
      --  Boolean components can be specified just by their name, which
      --  is equivalent to "Bool_Component => True", as in a normal
      --  aspect specification.

end LWT.Aspects;
