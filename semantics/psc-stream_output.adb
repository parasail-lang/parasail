------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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
------------------------------------------------------------------------------

package body PSC.Stream_Output is
   --  Character output routines
   procedure Put_Indent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Integer) is
   begin
      Put (S, String'(1 .. Indent => ' '));
   end Put_Indent;

   procedure Put
     (S : access Ada.Streams.Root_Stream_Type'Class;
      Str : String) is
   begin
      String'Write (S, Str);
   end Put;

   procedure Put
     (S : access Ada.Streams.Root_Stream_Type'Class;
      C : Character) is
   begin
      Character'Write (S, C);
   end Put;

   procedure Put
     (S : access Ada.Streams.Root_Stream_Type'Class;
      I : Integer) is
   begin
      String'Write (S, Integer'Image (I));
   end Put;

   procedure Put_Line
     (S : access Ada.Streams.Root_Stream_Type'Class;
      Str : String;
      Indent : Integer := 0) is
   begin
      if Indent > 0 then
         Put_Indent (S, Indent);
      end if;
      String'Write (S, Str & ASCII.LF);
   end Put_Line;

   procedure New_Line
     (S : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Integer := 0) is
   begin
      Character'Write (S, ASCII.LF);
      if Indent > 0 then
         Put_Indent (S, Indent);
      end if;
   end New_Line;

end PSC.Stream_Output;
