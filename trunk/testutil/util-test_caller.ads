-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
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

with AUnit.Test_Fixtures;
with AUnit.Test_Caller;
with AUnit.Test_Suites;
generic

   type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with private;

package Util.Test_Caller is

   package Caller is new AUnit.Test_Caller (Test_Fixture);

   procedure Add_Test (Suite  : in AUnit.Test_Suites.Access_Test_Suite;
                       Name   : in String;
                       Method : in Caller.Test_Method);
end Util.Test_Caller;
