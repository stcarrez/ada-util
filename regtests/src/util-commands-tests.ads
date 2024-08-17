-----------------------------------------------------------------------
--  util-commands-tests - Test for commands
--  Copyright (C) 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
