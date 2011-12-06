-----------------------------------------------------------------------
--  streams.buffered.tests -- Unit tests for buffered streams
--  Copyright (C) 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with Util.Test_Caller;

package body Util.Streams.Buffered.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Streams.Buffered");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Write, Read",
                       Test_Read_Write'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Write, Flush (String)",
                       Test_Write'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Write, Flush (Stream_Array)",
                       Test_Write_Stream'Access);
   end Add_Tests;

   --  ------------------------------
   --  Write on a buffered stream and read what was written.
   --  ------------------------------
   procedure Test_Read_Write (T : in out Test) is
      Stream     : Buffered_Stream;
      C          : Character;
   begin
      Stream.Initialize (Size => 4);
      Stream.Write ("abcd");

      Assert_Equals (T, 4, Integer (Stream.Get_Size), "Invalid size for stream");

      Stream.Read (C);
      Assert_Equals (T, 'a', C, "Invalid character read from the stream");

      Stream.Read (C);
      Assert_Equals (T, 'b', C, "Invalid character read from the stream");

      Stream.Read (C);
      Assert_Equals (T, 'c', C, "Invalid character read from the stream");

      Stream.Read (C);
      Assert_Equals (T, 'd', C, "Invalid character read from the stream");

      Assert_Equals (T, 0, Integer (Stream.Get_Size), "Invalid size for stream");

--        Stream.Write ("abc");
   end Test_Read_Write;

   --  ------------------------------
   --  Write on a buffer and force regular flush on a larger buffer
   --  ------------------------------
   procedure Test_Write (T : in out Test) is
      Big_Stream : aliased Buffered_Stream;
      Stream     : Buffered_Stream;
      Size       : Stream_Element_Offset := 0;
      Count      : constant Natural := 1000;
      Max_Size   : constant Natural := (Count * (Count + 1)) / 2;
   begin
      Big_Stream.Initialize (Size => Max_Size);
      Stream.Initialize (Output => Big_Stream'Unchecked_Access,
                         Input  => Big_Stream'Unchecked_Access, Size => 13);

      for I in 1 .. Count loop
         declare
            S : String (1 .. I);
         begin
            for J in S'Range loop
               S (J) := Character'Val (J mod 255);
            end loop;

            Stream.Write (S);
            Stream.Flush;

            Size := Size + Stream_Element_Offset (I);
            Assert_Equals (T, 1, Integer (Stream.Write_Pos), "Stream must be flushed");

            --  Verify that 'Big_Stream' holds the expected number of bytes.
            Assert_Equals (T, Integer (Size), Integer (Big_Stream.Write_Pos) - 1,
                           "Target stream has an invalid write position at "
                          & Integer'Image (I));
         end;
      end loop;
   end Test_Write;

   --  ------------------------------
   --  Write on a buffer and force regular flush on a larger buffer
   --  ------------------------------
   procedure Test_Write_Stream (T : in out Test) is
      Big_Stream : aliased Buffered_Stream;
      Stream     : Buffered_Stream;
      Size       : Stream_Element_Offset := 0;
      Count      : constant Stream_Element_Offset := 200;
      Max_Size   : constant Stream_Element_Offset := 5728500;
   begin
      Big_Stream.Initialize (Size => Natural (Max_Size));
      for Buf_Size in 1 .. 19 loop
         Stream.Initialize (Output => Big_Stream'Unchecked_Access,
                            Input  => Big_Stream'Unchecked_Access, Size => Buf_Size);

         for I in 1 .. Count loop
            for Repeat in 1 .. 5 loop
               declare
                  S : Stream_Element_Array (1 .. I);
               begin
                  for J in S'Range loop
                     S (J) := Stream_Element'Val (J mod 255);
                  end loop;

                  for J in 1 .. Repeat loop
                     Stream.Write (S);
                  end loop;
                  Stream.Flush;

                  Size := Size +  (I) * Stream_Element_Offset (Repeat);
                  Assert_Equals (T, 1, Integer (Stream.Write_Pos), "Stream must be flushed");

                  --  Verify that 'Big_Stream' holds the expected number of bytes.
                  Assert_Equals (T, Integer (Size), Integer (Big_Stream.Write_Pos) - 1,
                                 "Target stream has an invalid write position at "
                                 & Stream_Element_Offset'Image (I) & " with buffer "
                                 & Natural'Image (Buf_Size) & " repeat " & Natural'Image (Repeat));
               end;
            end loop;
         end loop;
      end loop;
      Assert_Equals (T, Integer (Max_Size), Integer (Big_Stream.Get_Size), "Invalid final size");
   end Test_Write_Stream;

end Util.Streams.Buffered.Tests;
