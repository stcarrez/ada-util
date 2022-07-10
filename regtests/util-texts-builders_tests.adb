-----------------------------------------------------------------------
--  util-texts-builders_tests -- Unit tests for text builders
--  Copyright (C) 2013, 2016, 2017, 2022 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Util.Test_Caller;
with Util.Texts.Builders;
with Util.Measures;

package body Util.Texts.Builders_Tests is

   package String_Builder is new Util.Texts.Builders (Element_Type => Character,
                                                      Input        => String,
                                                      Chunk_Size   => 100);

   package Caller is new Util.Test_Caller (Test, "Texts.Builders");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Length",
                       Test_Length'Access);
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Append",
                       Test_Append'Access);
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Inline_Append",
                       Test_Inline_Append'Access);
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Clear",
                       Test_Clear'Access);
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Iterate",
                       Test_Iterate'Access);
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Inline_Iterate",
                       Test_Inline_Iterate'Access);
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Tail",
                       Test_Tail'Access);
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Find",
                       Test_Find'Access);
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Perf",
                       Test_Perf'Access);
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Element",
                       Test_Element'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the length operation.
   --  ------------------------------
   procedure Test_Length (T : in out Test) is
      B : String_Builder.Builder (10);
   begin
      Util.Tests.Assert_Equals (T, 0, String_Builder.Length (B), "Invalid length");
      Util.Tests.Assert_Equals (T, 10, String_Builder.Capacity (B), "Invalid capacity");
   end Test_Length;

   --  ------------------------------
   --  Test the append operation.
   --  ------------------------------
   procedure Test_Append (T : in out Test) is
      S : constant String := "0123456789";
      B : String_Builder.Builder (3);
   begin
      String_Builder.Append (B, "a string");
      Util.Tests.Assert_Equals (T, 8, String_Builder.Length (B), "Invalid length");
      Util.Tests.Assert_Equals (T, "a string", String_Builder.To_Array (B), "Invalid content");
      Util.Tests.Assert_Equals (T, 100 + 3, String_Builder.Capacity (B), "Invalid capacity");

      --  Append new string and check content.
      String_Builder.Append (B, " b string");
      Util.Tests.Assert_Equals (T, 17, String_Builder.Length (B), "Invalid length");
      Util.Tests.Assert_Equals (T, "a string b string", String_Builder.To_Array (B),
                                "Invalid content");
      Util.Tests.Assert_Equals (T, 100 + 3, String_Builder.Capacity (B), "Invalid capacity");

      String_Builder.Clear (B);
      for I in S'Range loop
         String_Builder.Append (B, S (I));
      end loop;
      Util.Tests.Assert_Equals (T, 10, String_Builder.Length (B), "Invalid length");
      Util.Tests.Assert_Equals (T, S, String_Builder.To_Array (B), "Invalid append");
   end Test_Append;

   --  ------------------------------
   --  Test the append operation.
   --  ------------------------------
   procedure Test_Inline_Append (T : in out Test) is
      procedure Append (Into : in out String;
                        Last : out Natural);

      S : constant String := "0123456789";
      I : Positive := S'First;
      B : String_Builder.Builder (3);

      procedure Append (Into : in out String;
                        Last : out Natural) is
         Pos : Natural := Into'First;
      begin
         while Pos <= Into'Last and then I <= S'Last loop
            Into (Pos) := S (I);
            Pos := Pos + 1;
            I := I + 1;
         end loop;
         Last := Pos - 1;
      end Append;

      procedure Fill is new String_Builder.Inline_Append (Append);

   begin
      Fill (B);
      Util.Tests.Assert_Equals (T, S'Length, String_Builder.Length (B), "Invalid length");
      Util.Tests.Assert_Equals (T, "0123456789", String_Builder.To_Array (B), "Invalid content");

      I := S'First;
      Fill (B);
      Util.Tests.Assert_Equals (T, "01234567890123456789",
                                String_Builder.To_Array (B), "Invalid content");

      String_Builder.Clear (B);
      I := S'Last;
      Fill (B);
      Util.Tests.Assert_Equals (T, "9", String_Builder.To_Array (B), "Invalid content");

      String_Builder.Clear (B);
      I := S'Last + 1;
      Fill (B);
      Util.Tests.Assert_Equals (T, "", String_Builder.To_Array (B), "Invalid content");
   end Test_Inline_Append;

   --  ------------------------------
   --  Test the Find generic operation.
   --  ------------------------------
   procedure Test_Find (T : in out Test) is
      function Index (Content : in String) return Natural;

      B : String_Builder.Builder (3);

      function Index (Content : in String) return Natural is
      begin
         for I in Content'Range loop
            if Content (I) = 'b' then
               return I;
            end if;
         end loop;
         return 0;
      end Index;

      function Find is new String_Builder.Find (Index);

      Pos : Natural;
   begin
      String_Builder.Append (B, "ab");
      Pos := Find (B, 1);
      Util.Tests.Assert_Equals (T, 2, Pos, "Invalid find");

      String_Builder.Append (B, "deab");
      Pos := Find (B, Pos + 1);
      Util.Tests.Assert_Equals (T, 6, Pos, "Invalid find");

      Pos := Find (B, Pos + 1);
      Util.Tests.Assert_Equals (T, 0, Pos, "Invalid find");
   end Test_Find;

   --  ------------------------------
   --  Test the clear operation.
   --  ------------------------------
   procedure Test_Clear (T : in out Test) is
      B : String_Builder.Builder (7);
   begin
      for I in 1 .. 10 loop
         String_Builder.Append (B, "a string");
      end loop;
      Util.Tests.Assert_Equals (T, 8 * 10, String_Builder.Length (B), "Invalid length");
      Util.Tests.Assert_Equals (T, 100 + 7, String_Builder.Capacity (B), "Invalid capacity");

      String_Builder.Clear (B);
      Util.Tests.Assert_Equals (T, 0, String_Builder.Length (B), "Invalid length after clear");
      Util.Tests.Assert_Equals (T, 7, String_Builder.Capacity (B), "Invalid capacity after clear");
   end Test_Clear;

   --  ------------------------------
   --  Test the tail operation.
   --  ------------------------------
   procedure Test_Tail (T : in out Test) is

      procedure Check_Tail (Min : in Positive;
                            Max : in Positive;
                            L   : in Natural);

      procedure Check_Tail (Min : in Positive;
                            Max : in Positive;
                            L   : in Natural) is
         P : constant String := "0123456789";
         B : String_Builder.Builder (Min);
      begin
         for I in 1 .. Max loop
            String_Builder.Append (B, P (1 + (I mod 10)));
         end loop;
         declare
            S  : constant String := String_Builder.Tail (B, L);
            S2 : constant String := String_Builder.To_Array (B);
         begin
            Util.Tests.Assert_Equals (T, Max, S2'Length, "Invalid length");
            if L >= Max then
               Util.Tests.Assert_Equals (T, S2, S, "Invalid Tail result");
            else
               Util.Tests.Assert_Equals (T, S2 (S2'Last - L + 1 .. S2'Last), S,
                                         "Invalid Tail result {"
                                         & Positive'Image (Min) & ","
                                         & Positive'Image (Max) & ","
                                         & Positive'Image (L) & "]");
            end if;
         end;
      end Check_Tail;

   begin
      for I in 1 .. 100 loop
         for J in 1 .. 8 loop
            for K in 1 .. I + 3 loop
               Check_Tail (J, I, K);
            end loop;
         end loop;
      end loop;
   end Test_Tail;

   --  ------------------------------
   --  Test the iterate operation.
   --  ------------------------------
   procedure Test_Iterate (T : in out Test) is
      procedure Process (S : in String);

      B  : String_Builder.Builder (13);
      R  : Ada.Strings.Unbounded.Unbounded_String;

      procedure Process (S : in String) is
      begin
         Ada.Strings.Unbounded.Append (R, S);
      end Process;
   begin
      for I in 1 .. 100 loop
         String_Builder.Append (B, "The Iterate procedure avoids the string copy "
                                & "on the secondary stack");
      end loop;
      String_Builder.Iterate (B, Process'Access);
      Util.Tests.Assert_Equals (T, String_Builder.Length (B), Ada.Strings.Unbounded.Length (R),
                                "Invalid length in iterate string");
      Util.Tests.Assert_Equals (T, String_Builder.To_Array (B),
                                Ada.Strings.Unbounded.To_String (R), "Invalid Iterate");
   end Test_Iterate;

   --  ------------------------------
   --  Test the iterate operation.
   --  ------------------------------
   procedure Test_Inline_Iterate (T : in out Test) is
      procedure Process (S : in String);

      B  : String_Builder.Builder (13);
      R  : Ada.Strings.Unbounded.Unbounded_String;

      procedure Process (S : in String) is
      begin
         Ada.Strings.Unbounded.Append (R, S);
      end Process;

      procedure Get is new String_Builder.Inline_Iterate (Process);
   begin
      for I in 1 .. 100 loop
         String_Builder.Append (B, "The Iterate procedure avoids the string copy "
                                & "on the secondary stack");
      end loop;
      Get (B);
      Util.Tests.Assert_Equals (T, String_Builder.Length (B), Ada.Strings.Unbounded.Length (R),
                                "Invalid length in iterate string");
      Util.Tests.Assert_Equals (T, String_Builder.To_Array (B),
                                Ada.Strings.Unbounded.To_String (R), "Invalid Iterate");
   end Test_Inline_Iterate;

   --  ------------------------------
   --  Test the append and iterate performance.
   --  ------------------------------
   procedure Test_Perf (T : in out Test) is
      Perf : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File => Perf,
                          Name => Util.Tests.Get_Test_Path ("string-append.csv"));
      Ada.Text_IO.Put_Line (Perf, "Block_Size,Append Time,To_Array Time,Iterate Time");
      for Block_Size in 1 .. 300 loop
         declare
            B : String_Builder.Builder (10);
            N : constant String := Natural'Image (Block_Size * 10) & ",";
         begin
            String_Builder.Set_Block_Size (B, Block_Size * 10);
            declare
               S : Util.Measures.Stamp;
            begin
               for I in 1 .. 1000 loop
                  String_Builder.Append (B, "some item");
               end loop;
               Util.Measures.Report (S, Perf, N, Util.Measures.Microseconds);
            end;
            declare
               S : Util.Measures.Stamp;
               R : constant String := String_Builder.To_Array (B);
            begin
               Util.Measures.Report (S, Perf, ",", Util.Measures.Microseconds);
               T.Assert (R'Length > 0, "Invalid string length");
            end;

            declare
               Count : Natural := 0;

               procedure Process (Item : in String);

               procedure Process (Item : in String) is
                  pragma Unreferenced (Item);
               begin
                  Count := Count + 1;
               end Process;

               S : Util.Measures.Stamp;
            begin
               String_Builder.Iterate (B, Process'Access);
               Util.Measures.Report (S, Perf, ",", Util.Measures.Microseconds);
               T.Assert (Count > 0, "The string builder was empty");
            end;
         end;
         Ada.Text_IO.New_Line (Perf);
      end loop;
      Ada.Text_IO.Close (Perf);

      Ada.Text_IO.Create (File => Perf,
                          Name => Util.Tests.Get_Test_Path ("string.csv"));
      Ada.Text_IO.Put_Line (Perf, "Size,Append (100),Append (512),"
                            & "Append (1024),Unbounded,Iterate Time");
      for I in 1 .. 4000 loop
         declare
            N  : constant String := Natural'Image (I) & ",";
            B  : String_Builder.Builder (10);
            B2 : String_Builder.Builder (10);
            B3 : String_Builder.Builder (10);
            U  : Ada.Strings.Unbounded.Unbounded_String;
            S  : Util.Measures.Stamp;
         begin
            for J in 1 .. I loop
               String_Builder.Append (B, "some item");
            end loop;
            Util.Measures.Report (S, Perf, N, Util.Measures.Microseconds);

            String_Builder.Set_Block_Size (B2, 512);
            for J in 1 .. I loop
               String_Builder.Append (B2, "some item");
            end loop;
            Util.Measures.Report (S, Perf, ",", Util.Measures.Microseconds);

            String_Builder.Set_Block_Size (B3, 1024);
            for J in 1 .. I loop
               String_Builder.Append (B3, "some item");
            end loop;
            Util.Measures.Report (S, Perf, ",", Util.Measures.Microseconds);

            for J in 1 .. I loop
               Ada.Strings.Unbounded.Append (U, "some item");
            end loop;
            Util.Measures.Report (S, Perf, ",", Util.Measures.Microseconds);

            declare
               R : constant String := String_Builder.To_Array (B);
               pragma Unreferenced (R);
            begin
               Util.Measures.Report (S, Perf, ",", Util.Measures.Microseconds);
            end;

            declare
               R : constant String := Ada.Strings.Unbounded.To_String (U);
               pragma Unreferenced (R);
            begin
               Util.Measures.Report (S, Perf, ",", Util.Measures.Microseconds);
            end;
         end;
         Ada.Text_IO.New_Line (Perf);
      end loop;
   end Test_Perf;

   --  ------------------------------
   --  Test the Element function.
   --  ------------------------------
   procedure Test_Element (T : in out Test) is
      B : String_Builder.Builder (10);
   begin
      for I in 1 .. 1_000 loop
         String_Builder.Append (B, 'a');
      end loop;

      declare
         S   : Util.Measures.Stamp;
         C   : Character;
         Cnt : Natural := 0;
      begin
         for I in 1 .. String_Builder.Length (B) loop
            C := String_Builder.Element (B, I);
            Cnt := Cnt + (if C = 'a' then 1 else 0);
         end loop;
         Util.Measures.Report (S, "Util.Texts.Builders.Element", 1000);
         Util.Tests.Assert_Equals (T, 1_000, Cnt, "Invalid count");
      end;

      declare
         procedure Compute (Item : in String);

         S   : Util.Measures.Stamp;
         Cnt : Natural := 0;
         procedure Compute (Item : in String) is
         begin
            for C of Item loop
               Cnt := Cnt + (if C = 'a' then 1 else 0);
            end loop;
         end Compute;
         procedure Iterate is
           new String_Builder.Inline_Iterate (Compute);
      begin
         Iterate (B);
         Util.Measures.Report (S, "Util.Texts.Builders.Iterate", 1000);
         Util.Tests.Assert_Equals (T, 1_000, Cnt, "Invalid count");
      end;
   end Test_Element;

end Util.Texts.Builders_Tests;
