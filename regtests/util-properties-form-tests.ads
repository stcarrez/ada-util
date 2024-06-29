-----------------------------------------------------------------------
--  util-properties-form-tests -- Test reading form file into properties
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Properties.Form.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test loading a form file into a properties object.
   procedure Test_Parse_Form (T : in out Test);

end Util.Properties.Form.Tests;
