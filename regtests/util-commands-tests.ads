-----------------------------------------------------------------------
--  util-commands-tests - Test for commands
--  Copyright (C) 2018, 2022 Stephane Carrez
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
package Util.Commands.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Tests when the execution of commands.
   procedure Test_Execute (T : in out Test);

   --  Test execution of help.
   procedure Test_Help (T : in out Test);

   --  Test usage operation.
   procedure Test_Usage (T : in out Test);

   --  Test command based on the No_Parser.
   procedure Test_Simple_Command (T : in out Test);

end Util.Commands.Tests;
