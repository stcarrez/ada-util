-----------------------------------------------------------------------
--  serialize-io-json-tests -- Unit tests for JSON parser
--  Copyright (C) 2011, 2016, 2017, 2021, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Serialize.IO.JSON.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Parse_Error (T : in out Test);

   procedure Test_Parser (T : in out Test);

   --  Generate some output stream for the test.
   procedure Write_Stream (Stream : in out Util.Serialize.IO.Output_Stream'Class);

   --  Test the JSON output stream generation.
   procedure Test_Output (T : in out Test);
   procedure Test_Output_Indented (T : in out Test);

   --  Test the JSON output stream generation (simple JSON documents).
   procedure Test_Simple_Output (T : in out Test);

   --  Test the JSON output stream generation and parsing for nullable basic types.
   procedure Test_Nullable (T : in out Test);

   --  Test reading a JSON content into an Object tree.
   procedure Test_Read (T : in out Test);

   --  Test writing a JSON content from an Object tree.
   procedure Test_Write (T : in out Test);

   --  Test writing a JSON content from an Object tree.
   procedure Test_Write_Complex (T : in out Test);

end Util.Serialize.IO.JSON.Tests;
