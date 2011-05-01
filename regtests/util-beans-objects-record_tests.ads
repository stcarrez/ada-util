-----------------------------------------------------------------------
--  util-beans-objects-record_tests -- Unit tests for objects.records package
--  Copyright (C) 2011 Stephane Carrez
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

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Util.Beans.Objects.Record_Tests is

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite);

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Record (T : in out Test);

end Util.Beans.Objects.Record_Tests;
