with Ada2X_Strings.Text_Buffers.Bounded;
with Ada2X_Strings.Text_Buffers.Unbounded;
use Ada2X_Strings.Text_Buffers;
with Ada.Calendar; use type Ada.Calendar.Time;
with Ada.Text_IO;
with Ada.Tags;
procedure Test_Buf is
   procedure Test_Dispatch (B : in out Root_Buffer_Type'Class) is
   begin
      Increase_Indent (B);
      for I in 1 .. 100 loop
         Put (B, I'Image);
         if I mod 10 = 0 then
            New_Line (B);
         else
            Put (B, ", ");
         end if;
      end loop;
      Decrease_Indent (B);
   end Test_Dispatch;

   Start, Now : Ada.Calendar.Time;
   Diff : Duration;

   My_Buf_B : Bounded.Buffer_Type
     (Max_Characters => 2000);

   My_Buf_UB : Unbounded.Buffer_Type;

   N : constant := 100_000;
   use Ada.Text_IO;

   procedure Time_Dispatch (My_Buf : in out Root_Buffer_Type'Class) is
   begin
      Start := Ada.Calendar.Clock;
      for I in 1 .. N loop
         Clear (My_Buf);
         Test_Dispatch (My_Buf);
         declare
            Content : String renames Get_UTF_8 (My_Buf);
         begin
            if I mod (N / 4) = 0 then
               Ada.Text_IO.Put_Line (Content);
            end if;
         end;
      end loop;
      Now := Ada.Calendar.Clock;
      Diff := Now - Start;

      Ada.Text_IO.Put_Line (Standard_Error,
         N'Image & " dispatching iterations using " &
           Ada.Tags.Expanded_Name (My_Buf'Tag) &
           " took " & Diff'Image & " seconds");

      Start := Ada.Calendar.Clock;
      for I in 1 .. N loop
         Clear (Root_Buffer_Type'Class (My_Buf));
         Test_Dispatch (My_Buf);
         --  Ada.Text_IO.Put_Line (Content (My_Buf));
      end loop;
      Now := Ada.Calendar.Clock;
      Diff := Now - Start;

      Ada.Text_IO.Put_Line (Standard_Error,
         N'Image & " dispatching no-output iterations using " &
           Ada.Tags.Expanded_Name (My_Buf'Tag) &
           " took " & Diff'Image & " seconds");
   end Time_Dispatch;

   generic
      type Known_Buffer_Type (<>) is new Root_Buffer_Type with private;
   procedure Time_No_Dispatch (My_Buf : in out Known_Buffer_Type);

   procedure Time_No_Dispatch (My_Buf : in out Known_Buffer_Type) is

      function Get_Content (Buffer : in out Known_Buffer_Type)
        return String is
         Size : constant Natural := Natural (Character_Count (Buffer));
         Result : String (1 .. Size);
         Last : Natural;
      begin
         Get (Buffer, Result, Last);
         return Result;
      end Get_Content;

      procedure Test_No_Dispatch (B : in out Known_Buffer_Type) is
      begin
         Increase_Indent (B);
         for I in 1 .. 100 loop
            Put (B, I'Image);
            if I mod 10 = 0 then
               New_Line (B);
            else
               Put (B, ", ");
            end if;
         end loop;
         Decrease_Indent (B);
      end Test_No_Dispatch;

   begin

      Start := Ada.Calendar.Clock;
      for I in 1 .. N loop
         Clear (My_Buf);
         Test_No_Dispatch (My_Buf);
         declare
            Content : String renames Get_Content (My_Buf);
         begin
            if I mod (N / 4) = 0 then
               Ada.Text_IO.Put_Line (Content);
            end if;
         end;
      end loop;
      Now := Ada.Calendar.Clock;
      Diff := Now - Start;

      Ada.Text_IO.Put_Line (Standard_Error,
         N'Image & " nondispatching iterations using " &
         Ada.Tags.Expanded_Name (Known_Buffer_Type'Tag) &
         " took " & Diff'Image & " seconds");

      Start := Ada.Calendar.Clock;
      for I in 1 .. N loop
         Clear (My_Buf);
         Test_No_Dispatch (My_Buf);
         --  Ada.Text_IO.Put_Line (Content (My_Buf));
      end loop;
      Now := Ada.Calendar.Clock;
      Diff := Now - Start;

      Ada.Text_IO.Put_Line (Standard_Error,
         N'Image & " nondispatching no-output iterations using " &
         Ada.Tags.Expanded_Name (Known_Buffer_Type'Tag) &
         " took " & Diff'Image & " seconds");
   end Time_No_Dispatch;

   procedure Time_No_Dispatch_B is
     new Time_No_Dispatch (Bounded.Buffer_Type);

   procedure Time_No_Dispatch_UB is
     new Time_No_Dispatch (Ada2X_Strings.Text_Buffers.Unbounded.Buffer_Type);

begin

   Time_Dispatch (My_Buf_B);
   Time_Dispatch (My_Buf_UB);

   Time_No_Dispatch_B (My_Buf_B);
   Time_No_Dispatch_UB (My_Buf_UB);
end Test_Buf;
