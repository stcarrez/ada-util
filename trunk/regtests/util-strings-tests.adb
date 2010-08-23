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

with Ada.Strings.Unbounded;
with AUnit.Test_Caller;
with Util.Tests;
with Util.Strings.Transforms;
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

end Util.Strings.Tests;
