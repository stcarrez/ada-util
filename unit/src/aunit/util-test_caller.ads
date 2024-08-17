-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AUnit.Test_Caller;
with Util.Tests;
generic
   type Test_Fixture is new Util.Tests.Test with private;

   Name : String := "Test";

   pragma Unreferenced (Name);
package Util.Test_Caller is

   package Caller is new AUnit.Test_Caller (Test_Fixture);

   procedure Add_Test (Suite     : in Util.Tests.Access_Test_Suite;
                       Test_Name : in String;
                       Method    : in Caller.Test_Method);
end Util.Test_Caller;
