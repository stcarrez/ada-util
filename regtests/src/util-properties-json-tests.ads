-----------------------------------------------------------------------
--  util-properties-json-tests -- Test reading JSON file into properties
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Properties.JSON.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test loading a JSON file into a properties object.
   procedure Test_Parse_JSON (T : in out Test);

   --  Test loading a JSON file into a properties object.
   procedure Test_Read_JSON (T : in out Test);

end Util.Properties.JSON.Tests;
