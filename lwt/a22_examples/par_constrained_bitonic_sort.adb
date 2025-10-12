------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      PAR_CONSTRAINED_BITONIC_SORT                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with the --
-- GNAT Pro compiler, including any required library run-time units,  using --
-- any licensing terms  of your choosing.  See the AdaCore Software License --
-- for full details.                                                        --
--                                                                          --
-- The sequential version of this unit was developed by Matthew J Heaney.   --
-- This parallel version was developed by Tucker Taft                       --
------------------------------------------------------------------------------

--  This algorithm is a parallel variant of bitonic sort

pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

procedure LWT.Par_Constrained_Bitonic_Sort
  (Container : in out Array_Type)
is
   --  See: "Bitonic sorting network for n not a power of 2"
   --  http://www.iti.fh-flensburg.de
   --       /lang/algorithmen/sortieren/bitonic/oddn.htm
   --  and https://en.wikipedia.org/wiki/Bitonic_sorter
   --  based on: K.E. Batcher: Sorting Networks and their Applications.
   --    Proc. AFIPS Spring Joint Comput. Conf., Vol. 32, 307-314 (1968)

   --  pragma Warnings (Off);

   Debug : constant Boolean := False;
   Less_Debug : constant Boolean := Debug or else False;
   Checks : constant Boolean := Less_Debug or else False;

   Num_Chunks : constant := 10;

   Arr : Array_Type renames Container;

   type Uns is mod 2 ** Integer'Size;

   First : constant Index_Type := Arr'First;
   Last : constant Index_Type'Base := Arr'Last;

   Len : constant Uns := Arr'Length;

   function Nearest_Lower_Power_Of_2 (N : Uns) return Uns is
      --  return nearest power of 2 < N

      --  This uses the trick that X & X-1 clears the low bit of X.
      --  We start at N-1 and keep and'ing with one less until we would
      --  get zero, at which point we have the nearest power of 2 < N.
      pragma Assert (N >= 2);
      X : Uns := N - 1;
   begin
      while (X and (X - 1)) /= 0 loop
         --  Clear the lowest bit
         X := X and (X - 1);
      end loop;

      if Checks then
         pragma Assert (X < N and then X * 2 >= N);
         null;
      end if;
      return X;
   end Nearest_Lower_Power_Of_2;

   Half_Len : constant Uns := Nearest_Lower_Power_Of_2 (Len);

   function Bit_Count_Odd (N : Uns) return Boolean is
   --  Return whether count of bits on in binary rep of N is an odd number
      X : Uns := N;
      Result : Boolean := False;
   begin
      while X /= 0 loop
         --  Still has a bit on, invert our result
         Result := not Result;

         --  Clear the bit
         X := X and (X - 1);
      end loop;
      return Result;
   end Bit_Count_Odd;

   procedure Swap (X, Y : in out Element_Type) with Inline is
      --  Swap X and Y
      Tmp : constant Element_Type := X;
   begin
      X := Y;
      Y := Tmp;
   end Swap;

   procedure Compare_And_Swap (X, Y : in out Element_Type; Ascending : Boolean)
     with Inline is
      --  Swap X and Y if:
      --      Y less than X and Ascending True
      --      Y not less than X and Ascending False

   begin
      if (Y < X) = Ascending then
         Swap (X, Y);
      end if;
   end Compare_And_Swap;

   procedure Bitonic_Sorting_Pass
     (Pow_Dir, Pow_Offset : Uns; Ascending_First : Boolean) is
   --  Perform one pass over array, swapping items that are Pow_Offset apart
   --  if they are out of order, as determined by:
   --     Ascending_First xor Bit_Count_Odd (Zero-based-index / Pow_Dir)

   begin  --  Bitonic_Sorting_Pass

      if Less_Debug then
         Put_Line (" Sorting_Pass(" & Pow_Dir'Image & "," &
            Pow_Offset'Image & ", " & Ascending_First'Image & ")");
      end if;

      --  Perform the parallel loop
      parallel (Num_Chunks)
      for I in 0 .. Len - 1 loop
         if (I and Pow_Offset) = 0
           and then I + Pow_Offset < Len
         then
            declare
               Ascending : constant Boolean :=
                 Ascending_First xor Bit_Count_Odd (I / Pow_Dir);
            begin
               if Debug and then I mod Pow_Dir = 0 then
                  Put_Line (" For I =" & I'Image &
                        " and Ascending_First = " &
                        Ascending_First'Image & ", Bit_Count_Odd(" &
                        Uns'Image (I / Pow_Dir) & ") = " &
                        Bit_Count_Odd (I / Pow_Dir)'Image &
                        " and Ascending = " & Ascending'Image);
               end if;

               --  Compare against "partner" item
               Compare_And_Swap
                 (Arr (Index_Type'Val
                   (Index_Type'Pos (Arr'First) + Uns'Pos (I))),
                  Arr (Index_Type'Val
                   (Index_Type'Pos (Arr'First) + Uns'Pos (I + Pow_Offset))),
                  Ascending => Ascending);
            end;
         end if;
      end loop;

      if Debug then
         for I in First .. Index_Type'Pred (Last) loop
            Put ((if Arr (I) < Arr (Index_Type'Succ (I)) then
                          "<" else ">"));
         end loop;
         New_Line;
      end if;
   end Bitonic_Sorting_Pass;

   Pow_Outer : Uns := 1;
   Ascending_First : Boolean := False;

begin  --  LWT.Par_Constrained_Bitonic_Sort

   --  Compute Ascending_First so it ends up True in final pass
   while Pow_Outer < Len loop
      Ascending_First := not Ascending_First;
      Pow_Outer := Pow_Outer * 2;
   end loop;

   if Debug then
      Put_Line (" For Len" & Len'Image & ", Ascending_First = " &
        Ascending_First'Image);
   end if;

   Pow_Outer := 1;
   while Pow_Outer < Len loop
      declare
         Pow_Offset : Uns := Pow_Outer;
      begin
         while Pow_Offset >= 1 loop
            Bitonic_Sorting_Pass
              (Pow_Dir => Pow_Outer * 2, Pow_Offset => Pow_Offset,
               Ascending_First => Ascending_First);

            Pow_Offset := Pow_Offset / 2;
         end loop;
         Pow_Outer := Pow_Outer * 2;
         Ascending_First := not Ascending_First;
      end;
   end loop;

   --  pragma Warnings (On);
end LWT.Par_Constrained_Bitonic_Sort;
