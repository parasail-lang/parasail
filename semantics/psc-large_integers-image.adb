------------------------------------------------------------------------------
--                        L A R G E _ I N T E G E R S                       --
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
--                                                                          --
-- This package is based on the GENERIC LARGE INTEGER HANDLING PACKAGE      --
-- Created by Mats Weber on 24-MAY-1988                                     --
------------------------------------------------------------------------------
separate (PSC.Large_Integers)

function Image (X : Large_Integer) return String is
begin  --  Image

   if Is_Zero (X) then
      return "0";
   else
      declare

         function To_Large_Integer is
           new Variable_Length_Operations.Integer_To_Large_Integer (Natural);
         function To_Digit_Number  is
           new Large_Integer_To_Integer (Digit_Number);

         use Variable_Length_Operations;

         X_Length  : constant Slice_Index := Effective_Length (X);
         Result    : String (1 .. (Natural (X_Length) + 1) *
                       (Natural_Slice_Number'Width - 1) + 1);
         Ten       : constant Large_Integer := To_Large_Integer (10);
         N         : Natural := 0;
         Y         : Large_Integer (X_Length);

      begin
         Assign (Y, Value => abs X);
         while not Is_Zero (Y) loop
            Result (Result'Last - N) :=
              Digit_Image (To_Digit_Number (Y mod Ten));
            Assign (Y, Value => Y / Ten);
            N := N + 1;
         end loop;
         if X.Sign < 0 then
            Result (Result'Last - N) := '-';
            N := N + 1;
         end if;
         declare
            subtype String_1 is String (1 .. N);
         begin
            return String_1
                  (Result (Result'Last - N + 1 .. Result'Last));
         end;
      end;
   end if;
end Image;
