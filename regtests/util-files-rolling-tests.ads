-----------------------------------------------------------------------
--  util-files-rolling-tests -- Unit tests for rolling file manager
--  Copyright (C) 2022 Stephane Carrez
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

package Util.Files.Rolling.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the Format procedure with its pattern replacement.
   procedure Test_Format (T : in out Test);

   --  Test a rolling manager configured with ascending rolling mode.
   procedure Test_Rolling_Ascending (T : in out Test);

   --  Test a rolling manager configured with descending rolling mode.
   procedure Test_Rolling_Descending (T : in out Test);

end Util.Files.Rolling.Tests;
