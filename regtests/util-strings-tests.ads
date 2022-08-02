-----------------------------------------------------------------------
--  util-strings-tests -- Unit tests for Strings
--  Copyright (C) 2009, 2010, 2011, 2012, 2015, 2018, 2020, 2021, 2022 Stephane Carrez
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

with Util.Tests;

package Util.Strings.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Escape_Javascript (T : in out Test);

   procedure Test_Escape_Xml (T : in out Test);
   procedure Test_Escape_Java (T : in out Test);

   procedure Test_Unescape_Xml (T : in out Test);

   procedure Test_Capitalize (T : in out Test);

   procedure Test_To_Upper_Case (T : in out Test);

   procedure Test_To_Lower_Case (T : in out Test);
   procedure Test_To_Hex (T : in out Test);
   procedure Test_Measure_Copy (T : in out Test);
   procedure Test_Index (T : in out Test);
   procedure Test_Rindex (T : in out Test);
   procedure Test_Starts_With (T : in out Test);
   procedure Test_Ends_With (T : in out Test);
   procedure Test_Replace (T : in out Test);

   --  Do some benchmark on String -> X hash mapped.
   procedure Test_Measure_Hash (T : in out Test);

   --  Test String_Ref creation
   procedure Test_String_Ref (T : in out Test);

   --  Benchmark comparison between the use of Iterate vs Query_Element.
   procedure Test_Perf_Vector (T : in out Test);

   --  Test perfect hash (samples/gperfhash)
   procedure Test_Perfect_Hash (T : in out Test);

   --  Test the token iteration.
   procedure Test_Iterate_Token (T : in out Test);

   --  Test formatting strings.
   procedure Test_Format (T : in out Test);

end Util.Strings.Tests;
