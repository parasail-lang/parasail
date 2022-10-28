with Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;
function Word_Count(S : String; Separators : String;
                    Debug : Boolean := False) return Natural is
   use Ada.Strings.Maps;
   Seps : constant Character_Set := To_Set(Separators);   
   task type TT(First, Last : Natural; Count : access Natural);
   subtype WC_TT is TT;  --  So is visible inside TT
   task body TT is begin
      if First > Last then   --  Empty string
         Count.all := 0;
      elsif First = Last then  --  A single character
         if Is_In(S(First), Seps) then
            Count.all := 0;  --  A single separator
         else
            Count.all := 1;  --  A single non-separator
         end if;

      else  --  Divide and conquer
         declare
            Midpoint : constant Positive := (First + Last) / 2;
            Left_Count, Right_Count : aliased Natural := 0;
         begin
            if Debug then
               Put_Line ("Divide and conquer: Count(" & S(First .. Midpoint) &
                  ") + Count(" & S(Midpoint + 1 .. Last) & ")");
            end if;
            declare  --  Spawn two subtasks for distinct slices
               Left : WC_TT(First, Midpoint, Left_Count'Access);
               Right : WC_TT(Midpoint + 1, Last, Right_Count'Access);
            begin
               null;
            end;  --  Wait for subtasks to complete
 
            if Is_In(S(Midpoint), Seps) or else Is_In(S(Midpoint+1), Seps)
            then  --  At least one separator at border
               Count.all := Left_Count + Right_Count;
            else  --  Combine words at border
               Count.all := Left_Count + Right_Count - 1;
            end if;
         end;
      end if;
   end TT;
   Result : aliased Natural := 0;
begin
   declare  --  Spawn task to do the computation
      Tsk : TT(S'First, S'Last, Result'Access);
   begin
      null;
   end;  --  Wait for subtask
   return Result;
end Word_Count;
