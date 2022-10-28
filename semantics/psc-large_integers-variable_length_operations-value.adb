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
separate (PSC.Large_Integers.Variable_Length_Operations)

function Value (Image : String) return Large_Integer is

   function First_Non_Blank
     (Within : String; From_Right : Boolean := False) return Positive is
   begin
      if From_Right then
         for I in reverse Within'Range loop
            if Within (I) /= ' ' then
               return I;
            end if;
         end loop;
      else
         for I in Within'Range loop
            if Within (I) /= ' ' then
               return I;
            end if;
         end loop;
      end if;
      raise Constraint_Error;
   end First_Non_Blank;

begin
   declare

      First     : Positive := First_Non_Blank (Within => Image);
      Last      : constant Natural :=
                    First_Non_Blank (Within => Image, From_Right => True);

      Negative  : Boolean;

      function To_Large_Integer is
        new Integer_To_Large_Integer (Digit_Number'Base);

      Ten       : constant Large_Integer := To_Large_Integer (10);

      function Val (Image : String) return Large_Integer is
      begin
         if Image'Length = 1 then
            return To_Large_Integer (Digit_Value (Image (Image'First)));
         else
            return To_Large_Integer (Digit_Value (Image (Image'Last))) +
                   Ten * Val (Image (Image'First .. Image'Last - 1));
         end if;
      end Val;

   begin
      if Image (First) = '-' then
         Negative := True;
         First := First + 1;
      elsif Image (First) = '+' then
         Negative := False;
         First := First + 1;
      else
         Negative := False;
      end if;
      if First > Last then
         raise Constraint_Error;
      end if;
      for I in First .. Last loop
         if not Is_Digit (Image (I)) then
            raise Constraint_Error;
         end if;
      end loop;
      if Negative then
         return -Val (Image (First .. Last));
      else
         return Val (Image (First .. Last));
      end if;
   end;
end Value;
