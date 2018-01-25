-----------------------------------------------------------------------
--  util-serialize-io-form-tests -- Unit tests for form parser
--  Copyright (C) 2018 Stephane Carrez
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
