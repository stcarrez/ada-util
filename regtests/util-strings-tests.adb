-----------------------------------------------------------------------
--  strings.tests -- Unit tests for strings
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;
with Util.Test_Caller;
with Util.Tests;
with Util.Strings.Transforms;
with Util.Strings.Maps;
with Ada.Streams;
with Util.Measures;
package body Util.Strings.Tests is

   use Ada.Strings.Unbounded;
   use Util.Tests;
   use Util.Strings.Transforms;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Strings.Transforms.Escape_Javascript",
                       Test_Escape_Javascript'Access);
      Caller.Add_Test (Suite, "Test Util.Strings.Transforms.Escape_Xml",
                       Test_Escape_Xml'Access);
      Caller.Add_Test (Suite, "Test Util.Strings.Transforms.Capitalize",
                       Test_Capitalize'Access);
      Caller.Add_Test (Suite, "Test Util.Strings.Transforms.To_Upper_Case",
                       Test_To_Upper_Case'Access);
      Caller.Add_Test (Suite, "Test Util.Strings.Transforms.To_Lower_Case",
                       Test_To_Lower_Case'Access);
      Caller.Add_Test (Suite, "Test Util.Strings.Transforms.To_Hex",
                       Test_To_Hex'Access);
      Caller.Add_Test (Suite, "Test Measure",
                       Test_Measure_Copy'Access);
      Caller.Add_Test (Suite, "Test Util.Strings.Index",
                       Test_Index'Access);
      Caller.Add_Test (Suite, "Test Util.Strings.Rindex",
                       Test_Rindex'Access);
      Caller.Add_Test (Suite, "Test Util.Strings.Benchmark",
                       Test_Measure_Hash'Access);
      Caller.Add_Test (Suite, "Test Util.Strings.String_Ref",
                       Test_String_Ref'Access);
   end Add_Tests;

   procedure Test_Escape_Javascript (T : in out Test) is
      Result : Unbounded_String;
   begin
      Escape_Javascript (Content => ASCII.LF & " ""a string"" a 'single quote'",
                         Into    => Result);
      Assert_Equals (T, "\n \""a string\"" a \'single quote\'", Result);

      Result := To_Unbounded_String ("");
      Escape_Javascript (Content => ASCII.ESC & "[m " & Character'Val (255),
                         Into    => Result);
      Assert_Equals (T, "\u001B[m \u00FF", Result);
   end Test_Escape_Javascript;

   procedure Test_Escape_Xml (T : in out Test) is
      Result : Unbounded_String;
   begin
      Escape_Xml (Content => ASCII.LF & " < ""a string"" a 'single quote' >& ",
                  Into    => Result);
      Assert_Equals (T, ASCII.LF & " &lt; ""a string"" a &apos;single quote&apos; &gt;&amp; ",
                     Result);

      Result := To_Unbounded_String ("");
      Escape_Xml (Content => ASCII.ESC & "[m " & Character'Val (255),
                  Into    => Result);
      Assert_Equals (T, ASCII.ESC & "[m &#255;", Result);
   end Test_Escape_Xml;

   procedure Test_Capitalize (T : in out Test) is
      Result : Unbounded_String;
   begin
      Assert_Equals (T, "Capitalize_A_String", Capitalize ("capITalIZe_a_strING"));

      Capitalize ("CapAS_String", Result);
      Assert_Equals (T, "Capas_String", Result);
   end Test_Capitalize;

   procedure Test_To_Upper_Case (T : in out Test) is
   begin
      Assert_Equals (T, "UPPERCASE_0123_STR", To_Upper_Case ("upperCase_0123_str"));
   end Test_To_Upper_Case;

   procedure Test_To_Lower_Case (T : in out Test) is
   begin
      Assert_Equals (T, "lowercase_0123_str", To_Lower_Case ("LowERCase_0123_STR"));
   end Test_To_Lower_Case;

   procedure Test_To_Hex (T : in out Test) is
      Result : Unbounded_String;
   begin
      To_Hex (Result, Character'Val (23));
      Assert_Equals (T, "\u0017", Result);

      To_Hex (Result, Character'Val (31));
      Assert_Equals (T, "\u0017\u001F", Result);

      To_Hex (Result, Character'Val (255));
      Assert_Equals (T, "\u0017\u001F\u00FF", Result);
   end Test_To_Hex;

   procedure Test_Measure_Copy (T : in out Test) is
      pragma Unreferenced (T);

      Buf : constant Ada.Streams.Stream_Element_Array (1 .. 10_024) := (others => 23);
      pragma Suppress (All_Checks, Buf);
   begin
      declare
         T : Util.Measures.Stamp;
         R : Ada.Strings.Unbounded.Unbounded_String;
      begin
         for I in Buf'Range loop
            Append (R, Character'Val (Buf (I)));
         end loop;
         Util.Measures.Report (T, "Stream transform using Append (1024 bytes)");
      end;
      declare
         T : Util.Measures.Stamp;
         R : Ada.Strings.Unbounded.Unbounded_String;
         S : String (1 .. 10_024);
         pragma Suppress (All_Checks, S);
      begin
         for I in Buf'Range loop
            S (Natural (I)) := Character'Val (Buf (I));
         end loop;
         Append (R, S);
         Util.Measures.Report (T, "Stream transform using temporary string (1024 bytes)");
      end;
--        declare
--           T : Util.Measures.Stamp;
--           R : Ada.Strings.Unbounded.Unbounded_String;
--           P : constant Ptr := new String (1 .. Buf'Length);
--
--           pragma Suppress (All_Checks, P);
--        begin
--           for I in P'Range loop
--              P (I) := Character'Val (Buf (Ada.Streams.Stream_Element_Offset (I)));
--           end loop;
--           Ada.Strings.Unbounded.Aux.Set_String (R, P.all'Access);
--           Util.Measures.Report (T, "Stream transform using Aux string (1024 bytes)");
--        end;
   end Test_Measure_Copy;

   --  Test the Index operation
   procedure Test_Index (T : in out Test) is
      Str : constant String := "0123456789abcdefghijklmnopq";
   begin
      declare
         St  : Util.Measures.Stamp;
         Pos : Integer;
      begin
         for I in 1 .. 10 loop
            Pos := Index (Str, 'q');
         end loop;
         Util.Measures.Report (St, "Util.Strings.Index");
         Assert_Equals (T, 27, Pos, "Invalid index position");
      end;
      declare
         St  : Util.Measures.Stamp;
         Pos : Integer;
      begin
         for I in 1 .. 10 loop
            Pos := Ada.Strings.Fixed.Index (Str, "q");
         end loop;
         Util.Measures.Report (St, "Ada.Strings.Fixed.Index");
         Assert_Equals (T, 27, Pos, "Invalid index position");
      end;
   end Test_Index;

   --  Test the Rindex operation
   procedure Test_Rindex (T : in out Test) is
      Str : constant String := "0123456789abcdefghijklmnopq";
   begin
      declare
         St  : Util.Measures.Stamp;
         Pos : Natural;
      begin
         for I in 1 .. 10 loop
            Pos := Rindex (Str, '0');
         end loop;
         Util.Measures.Report (St, "Util.Strings.Rindex");
         Assert_Equals (T, 1, Pos, "Invalid rindex position");
      end;
      declare
         St  : Util.Measures.Stamp;
         Pos : Natural;
      begin
         for I in 1 .. 10 loop
            Pos := Ada.Strings.Fixed.Index (Str, "0", Ada.Strings.Backward);
         end loop;
         Util.Measures.Report (St, "Ada.Strings.Fixed.Rindex");
         Assert_Equals (T, 1, Pos, "Invalid rindex position");
      end;
   end Test_Rindex;

   package String_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Unbounded_String,
      Hash            => Hash,
      Equivalent_Keys => "=");

   package String_Ref_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => String_Ref,
      Element_Type    => String_Ref,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

   --  Do some benchmark on String -> X hash mapped.
   procedure Test_Measure_Hash (T : in out Test) is
      KEY     : aliased constant String := "testing";
      Str_Map : Util.Strings.Maps.Map;
      Ptr_Map : Util.Strings.String_Access_Map.Map;
      Ref_Map : String_Ref_Map.Map;
      Unb_Map : String_Map.Map;
      Name    : String_Access := new String '(KEY);
      Ref     : constant String_Ref := To_String_Ref (KEY);
   begin
      Str_Map.Insert (Name.all, Name.all);
      Ptr_Map.Insert (Name.all'Access, Name.all'Access);
      Unb_Map.Insert (To_Unbounded_String (KEY), To_Unbounded_String (KEY));
      Ref_Map.Insert (Ref, Ref);

      --  Performance of Hashed_Map Name_Access -> Name_Access
      --  (the fastest hash)
      declare
         St  : Util.Measures.Stamp;
         Pos : constant Util.Strings.String_Access_Map.Cursor := Ptr_Map.Find (KEY'Unchecked_Access);
         Val : constant Name_Access := Util.Strings.String_Access_Map.Element (Pos);
      begin
         Util.Measures.Report (St, "Util.Strings.String_Access_Maps.Find+Element");
         Assert_Equals (T, "testing", Val.all, "Invalid value returned");
      end;

      --  Performance of Hashed_Map String_Ref -> String_Ref
      --  (almost same performance as Hashed_Map Name_Access -> Name_Access)
      declare
         St  : Util.Measures.Stamp;
         Pos : constant String_Ref_Map.Cursor := Ref_Map.Find (Ref);
         Val : constant String_Ref := String_Ref_Map.Element (Pos);
      begin
         Util.Measures.Report (St, "Util.Strings.String_Ref_Maps.Find+Element");
         Assert_Equals (T, "testing", String '(To_String (Val)), "Invalid value returned");
      end;

      --  Performance of Hashed_Map Unbounded_String -> Unbounded_String
      --  (little overhead due to String copy made by Unbounded_String)
      declare
         St  : Util.Measures.Stamp;
         Pos : constant String_Map.Cursor := Unb_Map.Find (To_Unbounded_String (KEY));
         Val : constant Unbounded_String := String_Map.Element (Pos);
      begin
         Util.Measures.Report (St, "Hashed_Maps<Unbounded,Unbounded..Find+Element");
         Assert_Equals (T, "testing", Val, "Invalid value returned");
      end;

      --  Performance for Indefinite_Hashed_Map String -> String
      --  (the slowest hash, string copy to get the result, pointer to key and element
      --  in the hash map implementation)
      declare
         St  : Util.Measures.Stamp;
         Pos : constant Util.Strings.Maps.Cursor := Str_Map.Find (KEY);
         Val : constant String := Util.Strings.Maps.Element (Pos);
      begin
         Util.Measures.Report (St, "Util.Strings.Maps.Find+Element");
         Assert_Equals (T, "testing", Val, "Invalid value returned");
      end;

      Free (Name);
   end Test_Measure_Hash;

   --  ------------------------------
   --  Test String_Ref creation
   --  ------------------------------
   procedure Test_String_Ref (T : in out Test) is
      R1 : String_Ref := To_String_Ref ("testing a string");
   begin
      for I in 1 .. 1_000 loop
         declare
            S  : constant String (1 .. I) := (others => 'x');
            R2 : constant String_Ref := To_String_Ref (S);
         begin
            Assert_Equals (T, S, To_String (R2), "Invalid String_Ref");
            T.Assert (R2 = S, "Invalid comparison");
            Assert_Equals (T, I, Length (R2), "Invalid length");
            R1 := R2;
            T.Assert (R1 = R2, "Invalid String_Ref copy");
         end;
      end loop;
   end Test_String_Ref;

end Util.Strings.Tests;
