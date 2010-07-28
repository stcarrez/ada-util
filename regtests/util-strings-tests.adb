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
with AUnit.Assertions;
with AUnit.Test_Caller;
with Util.Tests;
with Ada.Strings.Unbounded;
with Util.Strings.Escapes;
package body Util.Strings.Tests is

   use Ada.Strings.Unbounded;
   use Util.Tests;
   use Util.Strings.Escapes;
   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Suite.Add_Test (Caller.Create ("Test Util.Strings.Escapes.Escape_Javascript",
        Test_Escape_Javascript'Access));
   end Add_Tests;

   procedure Test_Escape_Javascript (T : in out Test) is
      pragma Unreferenced (T);

      Result : Unbounded_String;
   begin
      Escape_Javascript (Content => ASCII.LF & " ""a string"" a 'single quote'",
			 Into    => Result);
      Assert_Equals ("\n \""a string\"" a \'single quote\'", Result);

      Result := To_Unbounded_String ("");
      Escape_Javascript (Content => ASCII.ESC & "[m " & Character'Val (255),
			 Into    => Result);
      Assert_Equals ("\u001B[m \u00FF", Result);
   end Test_Escape_Javascript;

end Util.Strings.Tests;
