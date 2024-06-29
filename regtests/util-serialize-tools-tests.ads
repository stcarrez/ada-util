-----------------------------------------------------------------------
--  serialize-tools-tests -- Unit tests for serialization tools
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Serialize.Tools.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the To_JSON operation.
   procedure Test_To_JSON (T : in out Test);

   --  Test the From_JSON operation.
   procedure Test_From_JSON (T : in out Test);

   --  Test the To_JSON and From_JSON
   procedure Test_To_From_JSON (T : in out Test);

end Util.Serialize.Tools.Tests;
