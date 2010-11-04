-----------------------------------------------------------------------
--  strings.tests -- Unit tests for strings
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with Ada.Strings.Unbounded.Aux;
with Ada.Strings.Fixed;
with AUnit.Test_Caller;
with Util.Tests;
with Util.Strings.Transforms;
with Ada.Streams;
with Util.Measures;
package body Util.Strings.Tests is

   use Ada.Strings.Unbounded;
   use Util.Tests;
   use Util.Strings.Transforms;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Suite.Add_Test (Caller.Create ("Test Util.Strings.Transforms.Escape_Javascript",
        Test_Escape_Javascript'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Strings.Transforms.Escape_Xml",
        Test_Escape_Xml'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Strings.Transforms.Capitalize",
        Test_Capitalize'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Strings.Transforms.To_Upper_Case",
        Test_To_Upper_Case'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Strings.Transforms.To_Lower_Case",
        Test_To_Lower_Case'Access));
      Suite.Add_Test (Caller.Create ("Test Measure",
        Test_Measure_Copy'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Strings.Index",
        Test_Index'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Strings.Rindex",
        Test_Rindex'Access));
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
   type Ptr is not null access all String;

   procedure Test_Measure_Copy (T : in out Test) is
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
      declare
         T : Util.Measures.Stamp;
         R : Ada.Strings.Unbounded.Unbounded_String;
         P : constant Ptr := new String (1 .. Buf'Length);

         pragma Suppress (All_Checks, P);
      begin
         for I in P'Range loop
            P (I) := Character'Val (Buf (Ada.Streams.Stream_Element_Offset (I)));
         end loop;
         Ada.Strings.Unbounded.Aux.Set_String (R, P.all'Access);
         Util.Measures.Report (T, "Stream transform using Aux string (1024 bytes)");
      end;
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

end Util.Strings.Tests;
