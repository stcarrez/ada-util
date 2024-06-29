-----------------------------------------------------------------------
--  util-serialize-io-form-tests -- Unit tests for form parser
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Serialize.IO.Form.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Parse_Error (T : in out Test);

   procedure Test_Parser (T : in out Test);

   --  Generate some output stream for the test.
   procedure Write_Stream (Stream : in out Util.Serialize.IO.Output_Stream'Class);

   --  Test the form output stream generation.
   procedure Test_Output (T : in out Test);

   --  Test reading a form content into an Object tree.
   procedure Test_Read (T : in out Test);

end Util.Serialize.IO.Form.Tests;
