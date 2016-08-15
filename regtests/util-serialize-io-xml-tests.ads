-----------------------------------------------------------------------
--  serialize-io-xml-tests -- Unit tests for XML serialization
--  Copyright (C) 2011, 2012, 2016 Stephane Carrez
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

package Util.Serialize.IO.XML.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test XML de-serialization
   procedure Test_Parser (T : in out Test);

   procedure Test_Parser2 (T : in out Test);

   --  Test (*) wildcard mapping for serialization.
   procedure Test_Parser_Wildcard_Mapping (T : in out Test);

   --  Test (**) wildcard mapping for serialization.
   procedure Test_Parser_Deep_Wildcard_Mapping (T : in out Test);

   --  Test XML de-serialization with some errors.
   procedure Test_Parser_Error (T : in out Test);

   --  Test XML serialization
   procedure Test_Writer (T : in out Test);

   --  Test the XML output stream generation.
   procedure Test_Output (T : in out Test);

end Util.Serialize.IO.XML.Tests;
