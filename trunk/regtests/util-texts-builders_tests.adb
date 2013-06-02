-----------------------------------------------------------------------
--  util-texts-builders_tests -- Unit tests for text builders
--  Copyright (C) 2013 Stephane Carrez
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
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Clear",
                       Test_Clear'Access);
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Iterate",
                       Test_Iterate'Access);
      Caller.Add_Test (Suite, "Test Util.Texts.Builders.Perf",
                       Test_Perf'Access);
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
   --  Test the iterate operation.
   --  ------------------------------
   procedure Test_Iterate (T : in out Test) is
      procedure Process (S : in String);

      B : String_Builder.Builder (13);
      R : Ada.Strings.Unbounded.Unbounded_String;

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

end Util.Texts.Builders_Tests;
