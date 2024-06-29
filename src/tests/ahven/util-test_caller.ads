-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
--  Copyright (C) 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ahven.Framework;
with Util.Tests;
generic

   type Test_Fixture is new Ahven.Framework.Test_Case with private;

   Name : String := "Test";
package Util.Test_Caller is

   type Test_Method is access procedure (T : in out Test_Fixture);

   procedure Add_Test (Suite       : in Util.Tests.Access_Test_Suite;
                       Test_Name   : in String;
                       Method      : in Test_Method);
end Util.Test_Caller;
