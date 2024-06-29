-----------------------------------------------------------------------
--  serialize-io-xml-tests -- Unit tests for XML serialization
--  Copyright (C) 2011, 2012, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
