-----------------------------------------------------------------------
--  serialize-io-jcsv-tests -- Unit tests for CSV parser
--  Copyright (C) 2011, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Serialize.IO.CSV.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Parser (T : in out Test);

   --  Test the CSV output stream generation.
   procedure Test_Output (T : in out Test);

end Util.Serialize.IO.CSV.Tests;
