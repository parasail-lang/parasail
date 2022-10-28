---------------------------------------------------
--  Generic Hyper Sorting routine for a distributed program

--  Copyright (C) 2011-2021, AdaCore
--  This program is provided "as is" with no warranty.
--  Report errors at:
--    http://groups.google.com/group/parasail-programming-language

--  pragma Ada_2020;

with System_Distrib; use System_Distrib;
with System.Parallelism;
with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Buffered_Streams;
with Generic_Histograms;
pragma Elaborate (Generic_Histograms);
with Distributed_Reduction;
pragma Elaborate (Distributed_Reduction);
with Local_Vectors;
pragma Elaborate (Local_Vectors);
with Gen_Qsort;
package body Distrib_Hyper_Sorting is

   use Dis_Vec;

   subtype Nat_Index_Type is Index_Type'Base range 0 .. Index_Type'Last;

   type Nat_Array is array (Shard_Index_Type range <>) of Nat_Index_Type;

   Debug : constant Boolean := True;

   package Histos is new Generic_Histograms (Elem_Type);

   package Dis_Sort_Histo is new Distributed_Reduction
     (Accum_Type => Histos.Histogram,  --  Indefinite type
      Combiner => Histos.Combine,
      Index_Type => Index_Type,
      Distrib_Type_Id => Dis_Vec.Distrib_Type_Id & "_Sort_Histo");

   package Index_Vectors is new Local_Vectors (Index_Type);

   type Index_Vector_Array is
     array (Shard_Index_Type range <>) of Index_Vectors.Vector;

   generic
      Num_Shards : Shard_Index_Type;
      Shuffle_Type_Id : String;
   package Distrib_Shuffling is

      Shuffle_Type_Id_Ptr : constant String_Cptr :=
        new String'(Shuffle_Type_Id);

      type Shard_Boundary_Array is array (0 .. Num_Shards) of Elem_Type;

      type Chunk_Boundary_Array is array (Chunk_Count_Type range <>)
        of Elem_Type;

      type Chunk_Shuffle_Type is record
         --  NOTE: We put these two things into a per-chunk record
         --        to minimize the likelihood of false sharing of cache lines.
         New_Vector : Dis_Vec.Local_Vector;
            --  New vector for Chunk after the shuffle is complete
         To_Shard : Index_Vector_Array (1 .. Num_Shards);
            --  Vectors of indices for elements to be sent to other shards
      end record;

      --  The transfer matrix is built up by scanning
      --  each chunk of the local shard and using the Shard_Boundary
      --  array to create vectors of indices for each shard.
      type Transfer_Matrix_Type is array (Chunk_Index_Type range <>)
        of Chunk_Shuffle_Type;

      type Shuffle
        (Vec : not null access Dis_Vec.Vector;
         Num_Chunks : Chunk_Index_Type)
        is new Distrib_Object with record
         Time_Stamps : TS_Array (1 .. Num_Shards) := (others => 0);
         Shard_Boundary : Shard_Boundary_Array;
            --  These are non-inclusive bounds on the high side,
            --  so test is "Shard_Boundary (I-1) <= X < Shard_Boundary (I)"
         Chunk_Boundary : Chunk_Boundary_Array (0 .. Num_Chunks);
            --  These are non-inclusive bounds on the high side,
            --  so test is "Chunk_Boundary (I-1) <= X < Chunk_Boundary (I)"
         Transfer_Matrix : Transfer_Matrix_Type (1 .. Num_Chunks);
            --  This transfer matrix is built up in parallel using
            --  Shard_Boundary.
      end record;

      function Distrib_Type_Id_Ptr (Dis_Obj : Shuffle)
        return String_Cptr
        is (Shuffle_Type_Id_Ptr);
         --  Returns pointer to name of type of distributed object.

      function Distrib_Obj_Id (Dis_Obj : Shuffle)
        return Distrib_Object_Id_Type
        is (Dis_Obj.Vec.Distrib_Obj_Id);
         --  Returns index of object among all objects of given type.

      function Distrib_Obj_Group (Dis_Obj : Shuffle)
        return Group_Context_Ptr is
          (Dis_Obj.Vec.GC);
         --  Return reference to group context for given distributed object

      procedure Handle_Obj_Message
        (Dis_Obj : in out Shuffle;
         Msg : aliased Ada.Streams.Stream_Element_Array);

      type Shuffle_Message_Kind is (Append_Stream);
      type Shuffle_Message_Type
        (Kind : Shuffle_Message_Kind := Shuffle_Message_Kind'First) is record
         Time_Stamp : Time_Stamp_Type;
         Node_Index : Node_Index_Type;
         case Kind is
            when Append_Stream =>
               Stream_Length : Dis_Vec.Extended_Index;
         end case;
      end record;

      procedure Assemble_And_Send_Streams
        (Dis_Obj : in out Shuffle;
         Time_Stamp : Time_Stamp_Type);

   end Distrib_Shuffling;

   package body Distrib_Shuffling is
      procedure Assemble_And_Send_Streams
        (Dis_Obj : in out Shuffle;
         Time_Stamp : Time_Stamp_Type) is
      begin
         for Target_Shard in 1 .. Num_Shards loop
            if Target_Shard /= Dis_Obj.Vec.GC.Node_Index then
               --  Build up message for non-local shard
               declare
                  use Buffered_Streams;
                  Send_Stream_Msg : Shuffle_Message_Type
                                      (Kind => Append_Stream);
                  Stream_Length : Dis_Vec.Extended_Index := 0;
                  Stream_Buf : aliased Buffered_Stream;
               begin
                  --  Determine total size of stream
                  for I in 1 .. Dis_Obj.Num_Chunks loop
                     Stream_Length := Stream_Length +
                       Dis_Vec.Extended_Index
                         (Dis_Obj.Transfer_Matrix (I).
                           To_Shard (Target_Shard).Num_Elements);
                  end loop;

                  Send_Stream_Msg.Stream_Length := Stream_Length;
                  Send_Stream_Msg.Node_Index := Dis_Obj.Vec.GC.Node_Index;
                  Send_Stream_Msg.Time_Stamp := Time_Stamp;

                  --  Add header to stream
                  Shuffle_Message_Type'Output
                    (Stream_Buf'Access, Send_Stream_Msg);

                  --  Add elements to stream
                  for I in 1 .. Dis_Obj.Num_Chunks loop
                     for J in 1 ..
                       Dis_Obj.Transfer_Matrix (I).
                         To_Shard (Target_Shard).Num_Elements
                     loop
                        Elem_Type'Write (Stream_Buf'Access,
                          Dis_Obj.Vec.Element
                            (Dis_Obj.Transfer_Matrix (I).
                              To_Shard (Target_Shard) (J)));
                     end loop;
                  end loop;

                  --  Send stream to appropriate peer
                  System_Distrib.Send_To_Peer
                    (Dis_Obj.Vec.GC.all, Dis_Obj,
                     Node_Index_Type (Target_Shard), Stream_Buf.Contents);
               end;
            end if;
         end loop;

         --  Update time stamp
         Dis_Obj.Time_Stamps (Dis_Obj.Vec.GC.Node_Index) := Time_Stamp;
      end Assemble_And_Send_Streams;

      procedure Handle_Obj_Message
        (Dis_Obj : in out Shuffle;
         Msg : aliased Ada.Streams.Stream_Element_Array) is

         use Buffered_Streams;
         use System.Parallelism;

         Reader : aliased Buffered_Reader (Msg'Access);
         Shuffle_Msg : constant Shuffle_Message_Type :=
           Shuffle_Message_Type'Input (Reader'Access);

         procedure Split_Message
           (Low, High : Longest_Integer;
            Ext_Chunk_Index : Positive) is
         begin
            for Chunk_Index in Chunk_Index_Type (Low) ..
                                 Chunk_Index_Type'Base (High)
            loop
               declare
                  Chunk_Vec : Dis_Vec.Local_Vector
                    renames Dis_Obj.Transfer_Matrix (Chunk_Index).New_Vector;
                  Chunk_Reader : aliased Buffered_Reader (Msg'Access);
                  Low_Bound : constant Elem_Type :=
                    Dis_Obj.Chunk_Boundary (Chunk_Index - 1);
                  High_Bound : constant Elem_Type :=
                    Dis_Obj.Chunk_Boundary (Chunk_Index);
               begin
                  --  Start where outer reader finished
                  Chunk_Reader.Next_To_Read := Reader.Next_To_Read;

                  --  Select the elements relevant to this chunk
                  for I in 1 .. Shuffle_Msg.Stream_Length loop
                     declare
                        Val : constant Elem_Type :=
                          Elem_Type'Input (Chunk_Reader'Access);
                     begin
                        if Val >= Low_Bound and then Val < High_Bound then
                           Chunk_Vec.Append (Val);
                        end if;
                     end;
                  end loop;
               end;
            end loop;
         end Split_Message;

      begin
         --  Split up the message into appropriate chunks
         Par_Range_Loop
           (Low => 1,
            High => Longest_Integer (Dis_Obj.Num_Chunks),
            Num_Chunks => Dis_Obj.Num_Chunks,
            Loop_Body => Split_Message'Access);

         --  Copy time stamp for sending node
         Dis_Obj.Time_Stamps (Shuffle_Msg.Node_Index) :=
           Shuffle_Msg.Time_Stamp;
      end Handle_Obj_Message;

   end Distrib_Shuffling;

   procedure Hyper_Qsort
     (Vec : in out Dis_Vec.Vector) is

      Num_Shards : constant Shard_Index_Type :=
        Shard_Index_Type (Vec.GC.Num_Nodes);

      My_Index : constant Node_Index_Type := Vec.GC.Node_Index;

      Num_Chunks : constant Chunk_Index_Type := Vec.GC.Node_Size;

      package Sort_Shuffling is new Distrib_Shuffling
        (Num_Shards => Num_Shards,
         Shuffle_Type_Id => Dis_Vec.Distrib_Type_Id & "_Shuffler");

      Shuffler : aliased Sort_Shuffling.Shuffle
        (Vec'Access, Num_Chunks);

   begin  --  Hyper_Qsort

      Create_Distrib_Obj (Vec.GC.all, Shuffler'Unchecked_Access);

      if Debug then
         declare
            procedure Show_Size (Shard_Index : Shard_Index_Type) is
            begin
               if Shard_Index = 1 then
                  Put_Line ("Orig Shards:");
               end if;
               Put_Line (My_Index'Image & ": [ 1 .." &
                  Length (Vec)'Image & " ]");
            end Show_Size;
         begin
            --  Show size of each Shard in sequence
            Vec.For_Each_Shard
              (Show_Size'Access, Sequential => True);
         end;
      end if;

      --  Build up a histogram

      declare
         Histo_Coll : Dis_Sort_Histo.Collector
                     (Vec.GC, Distrib_Obj_Id => Vec.Distrib_Obj_Id);

         Histo : Histos.Histogram
           (Size => Natural'Max (10, 2 * Vec.GC.Total_Size));
            --  This default-init'ed object is also used as the identity.
            --  We set a size of twice the total number of cores to get
            --  adequate granularity.

         Sum_Of_Node_Sizes : constant Positive := Vec.GC.Total_Size;

         My_Shard : constant Shard_Index_Type := Shard_Index_Type (My_Index);

         Cum_Weight : Natural := 0;

         Prev_Elems : Natural := 0;

         Bucket_Index : Positive := 1;

         Bucket_Remainder : Natural := 0;

         Cur_Bucket : Histos.Bucket_Type;

         --  Length/starting/ending points for local shard
         Local_Shard_Length : Natural;

         Local_First_Bucket : Natural;
         Local_First_Remainder : Natural;

         procedure Add_One_Value (Accum : in out Histos.Histogram;
                                  Index : Index_Type) is
         --  Add one value to the Histogram
         begin
            Accum.Add_Value (Vec.Element (Index));
         end Add_One_Value;

      begin

         --  Iterate in parallel over local indices 1 to Vec.Length, in chunks.
         --  Call Add_One_Value to add a single value to the accumulator.
         --  Will call Combiner to combine accumulators built up
         --  in parallel (some day doing a tree-wise combination in case
         --  the number of threads on a given node is very large).
         --  The node/shard-level result is shared with other nodes/shards
         --  and then combined into a single global accumulator, namely Histo.
         Histo_Coll.Distrib_Reduce
           (Histo, First => 1, Last => Vec.Length,
            Add_One_Value => Add_One_Value'Access);

         if Debug then
            --  Display histogram
            Put_Line (My_Index'Image & ": Histogram before sorting:");
            Histo.Dump_Histo;
         end if;

         --  Now compute Shard_Boundary for each new shard's values.

         --  Initialize the zero-th high-bound
         Shuffler.Shard_Boundary (0) := Histo.Min_Value;

         for I in 1 .. Num_Shards loop
            declare
               Node_Index : constant Node_Index_Type := Node_Index_Type (I);
               Node_Weight : constant Positive :=
                 Vec.GC.Node_Size (Node_Index);
               Weight_Offset : constant Positive := Cum_Weight + Node_Weight;
               Cum_Elems : constant Natural :=
                 Histo.Num_Values * Weight_Offset / Sum_Of_Node_Sizes;
               Node_Elems : Natural := Cum_Elems - Prev_Elems;
            begin
               if Debug then
                  Put_Line (My_Index'Image & ": for Node" & I'Image &
                    ", Weight_Offset =" &
                    Weight_Offset'Image & ", Cum_Elems =" & Cum_Elems'Image &
                    ", Node_Elems =" & Node_Elems'Image);
               end if;

               if Node_Index = My_Index then
                  Local_Shard_Length := Node_Elems;
                  --  Remember starting point for local shard
                  Local_First_Bucket := Bucket_Index;
                  Local_First_Remainder := Bucket_Remainder;
               end if;

               while Node_Elems > Bucket_Remainder loop
                  Node_Elems := Node_Elems - Bucket_Remainder;
                  Cur_Bucket := Histo.Nth_Bucket (Bucket_Index);
                  Bucket_Index := Bucket_Index + 1;
                  Bucket_Remainder := Cur_Bucket.Count;
               end loop;

               --  Current bucket now has enough elements left in it.
               Bucket_Remainder := Bucket_Remainder - Node_Elems;

               --  Set the high bound by simple interpolation.
               --  TBD: Could do a more complex interpolation based
               --       on local "slope" of histogram.
               Shuffler.Shard_Boundary (I) := Cur_Bucket.Low_Bound +
                 Histo.Bucket_Width *
                   Elem_Type (Cur_Bucket.Count - Bucket_Remainder) /
                   Elem_Type (Cur_Bucket.Count);

               if Debug then
                  Put_Line (My_Index'Image & ": Shard_Boundary (" &
                    I'Image & " ) = " & Shuffler.Shard_Boundary (I)'Image);
               end if;

               --  Remember Cum_Weight and Cum_Elems
               Cum_Weight := Weight_Offset;
               Prev_Elems := Cum_Elems;
            end;
         end loop;

         --  Create transfer matrix using Shard boundaries
         declare
            use System.Parallelism;

            function Shard_Destination (Val : Elem_Type)
              return Shard_Index_Type is
               --  TBD: Use binary search at some point
            begin
               for I in 1 .. Num_Shards loop
                  if Val < Shuffler.Shard_Boundary (I) then
                     return I;
                  end if;
               end loop;
               return Num_Shards;
            end Shard_Destination;

            procedure Split_Chunk
              (Low, High : Longest_Integer;
               Chunk_Index : Positive) is
            begin
               for I in Index_Type (Low) ..
                                    Index_Type'Base (High)
               loop
                  --  Add index to appropriate transfer vector
                  Shuffler.Transfer_Matrix (Chunk_Index).
                    To_Shard (Shard_Destination (Vec.Element (I))).
                      Add_Element (I);
               end loop;
            end Split_Chunk;

         begin

            Par_Range_Loop
              (Low => Longest_Integer (Vec.First_Index),
               High => Longest_Integer (Vec.Last_Index),
               Num_Chunks => Num_Chunks,
               Loop_Body => Split_Chunk'Access);

            --  Send "append" messages to other shards
            Shuffler.Assemble_And_Send_Streams
              (Time_Stamp => Shuffler.Time_Stamps (My_Index) + 1);
         end;

         --  Compute the local shard's chunk boundaries
         Shuffler.Chunk_Boundary (0) := Shuffler.Shard_Boundary (My_Shard - 1);
         Bucket_Index := Local_First_Bucket;
         Bucket_Remainder := Local_First_Remainder;
         Prev_Elems := 0;
         for I in 1 .. Num_Chunks loop
            declare
               Cum_Elems : constant Natural :=
                 Local_Shard_Length * I / Num_Chunks;
               Chunk_Elems : Natural := Cum_Elems - Prev_Elems;
            begin
               if Debug then
                  Put_Line (My_Index'Image & ": for Chunk" & I'Image &
                    ", Cum_Elems =" & Cum_Elems'Image &
                    ", Chunk_Elems =" & Chunk_Elems'Image);
               end if;
               while Chunk_Elems > Bucket_Remainder loop
                  Chunk_Elems := Chunk_Elems - Bucket_Remainder;
                  Cur_Bucket := Histo.Nth_Bucket (Bucket_Index);
                  Bucket_Index := Bucket_Index + 1;
                  Bucket_Remainder := Cur_Bucket.Count;
               end loop;

               --  Current bucket now has enough elements left in it.
               Bucket_Remainder := Bucket_Remainder - Chunk_Elems;

               --  Set the high bound by simple interpolation.
               --  TBD: Could do a more complex interpolation based
               --       on local "slope" of histogram.
               Shuffler.Chunk_Boundary (I) := Cur_Bucket.Low_Bound +
                 Histo.Bucket_Width *
                   Elem_Type (Cur_Bucket.Count - Bucket_Remainder) /
                   Elem_Type (Cur_Bucket.Count);

               if Debug then
                  Put_Line (My_Index'Image & ": Chunk_Boundary (" &
                    I'Image & " ) = " & Shuffler.Chunk_Boundary (I)'Image);
               end if;

               --  Remember Cum_Elems
               Prev_Elems := Cum_Elems;
            end;
         end loop;

         if Debug
           and then Shuffler.Chunk_Boundary (Num_Chunks) /=
             Shuffler.Shard_Boundary (My_Shard)
         then
            Put_Line (My_Index'Image & ": Last chunk boundary =" &
              Shuffler.Chunk_Boundary (Num_Chunks)'Image);
            Put_Line (My_Index'Image & ": Shard boundary =" &
              Shuffler.Shard_Boundary (My_Shard)'Image);
         end if;

         --  Initialize new vectors for each chunk by
         --  scanning index vector targeting current shard.
         declare
            use System.Parallelism;

            procedure Start_New_Chunk
              (Low, High : Longest_Integer;
               Ext_Chunk_Index : Positive) is
            begin
               for Chunk_Index in Chunk_Index_Type (Low) ..
                                  Chunk_Index_Type (High)
               loop
                  declare
                     Chunk_Vec : Dis_Vec.Local_Vector renames
                       Shuffler.Transfer_Matrix (Chunk_Index).New_Vector;
                     Low_Bound : constant Elem_Type :=
                       Shuffler.Chunk_Boundary (Chunk_Index - 1);
                     High_Bound : constant Elem_Type :=
                       Shuffler.Chunk_Boundary (Chunk_Index);
                  begin
                     for I in 1 .. Num_Chunks loop
                        declare
                           Xfr_Vec : Index_Vectors.Vector renames
                             Shuffler.Transfer_Matrix (I).To_Shard (My_Shard);
                        begin
                           for J in 1 .. Xfr_Vec.Num_Elements loop
                              declare
                                 Val : constant Elem_Type :=
                                          Vec.Element (Xfr_Vec (J));
                              begin
                                 if Val >= Low_Bound and then Val < High_Bound
                                 then
                                    Chunk_Vec.Append (Val);
                                 end if;
                              end;
                           end loop;
                        end;
                     end loop;
                  end;
               end loop;
            end Start_New_Chunk;

            procedure Sort_New_Chunk
              (Low, High : Longest_Integer;
               Ext_Chunk_Index : Positive) is
            begin
               for Chunk_Index in Chunk_Index_Type (Low) ..
                                  Chunk_Index_Type (High)
               loop
                  declare
                     Chunk_Vec : Dis_Vec.Local_Vector renames
                       Shuffler.Transfer_Matrix (Chunk_Index).New_Vector;

                     function Before (Left, Right : Dis_Vec.Index_Type)
                       return Boolean is
                       (Chunk_Vec.Nth_Element (Left) <
                          Chunk_Vec.Nth_Element (Right));

                     procedure Swap (Left, Right : Dis_Vec.Index_Type) is
                        Tmp : constant Elem_Type :=
                          Chunk_Vec.Nth_Element (Left);
                     begin
                        Set_Nth_Element
                          (Chunk_Vec, Left, Chunk_Vec.Nth_Element (Right));
                        Set_Nth_Element (Chunk_Vec, Right, Tmp);
                     end Swap;

                     procedure Quicksort is
                       new Gen_Qsort (Dis_Vec.Index_Type, Before, Swap);
                  begin
                     if Debug then
                        Put_Line (My_Index'Image &
                          ": About to sort sequentially chunk" &
                          Chunk_Index'Image & " of length" &
                          Chunk_Vec.Length'Image);
                     end if;

                     Quicksort (1, Chunk_Vec.Length);
                  end;
               end loop;
            end Sort_New_Chunk;

         begin
            if Debug then
               declare
                  Total_Local : Index_Vectors.Elem_Index := 0;
                  use Index_Vectors;
               begin
                  for TM of Shuffler.Transfer_Matrix loop
                     Total_Local := @ + TM.To_Shard (My_Shard).Num_Elements;
                  end loop;
                  Put_Line (My_Index'Image & ": creating" & Num_Chunks'Image &
                    " new local chunks from index vectors of total length" &
                    Total_Local'Image);
               end;
            end if;

            Par_Range_Loop
              (Low => 1,
               High => Longest_Integer (Num_Chunks),
               Num_Chunks => Num_Chunks,
               Loop_Body => Start_New_Chunk'Access);

            --  Now pull in values from messages from other shards
            while (for some TS of Shuffler.Time_Stamps =>
                     TS < Shuffler.Time_Stamps (My_Index)) loop
               Shuffler.Handle_Queued_Messages (Wait_For_Message => True);
            end loop;

            --  Set the original vector to empty
            Vec.Set_Empty;

            --  Now sort the chunks
            Par_Range_Loop
              (Low => 1,
               High => Longest_Integer (Num_Chunks),
               Num_Chunks => Num_Chunks,
               Loop_Body => Sort_New_Chunk'Access);

            --  Append the chunks in order
            for I in 1 .. Num_Chunks loop
               Vec.Append_Local_Vector
                 (Shuffler.Transfer_Matrix (I).New_Vector);
            end loop;
         end;

      end;

   end Hyper_Qsort;

end Distrib_Hyper_Sorting;
