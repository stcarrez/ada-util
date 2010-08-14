-----------------------------------------------------------------------
--  Util -- Utilities
--  Copyright (C) 2009, 2010 Stephane Carrez
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


with AUnit.Assertions;
with AUnit.Test_Caller;

with Util.Tests;
with Util.Properties.Bundles;
with Util.Properties.Basic;

package body Util.Properties.Bundles.Tests is

   use AUnit.Assertions;
   use Util.Tests;
   use Util.Properties.Basic;

   --  Test the bundle
   procedure Test_Bundle (T : in out Test) is
      pragma Unreferenced (T);

      Bundle : Properties.Bundles.Manager;
      Props  : constant Properties.Manager_Access := new Properties.Manager;
      V : Integer := 23;
   begin
      --  Create a first property (while the bundle is empty)
--        Integer_Property.Set (Props.all, "test-integer", 123);
--        Assert (Bundle.Exists ("test-integer"), "Invalid properties");
--
--        V := Integer_Property.Get (Bundle, "test-integer");
--        Assert (V = 123, "Property was not inserted");

      --  Add a property set to the bundle
      Bundle.Add_Bundle (Props);
      Integer_Property.Set (Props.all, "test-integer-second", 24);
      V := Integer_Property.Get (Props.all, "test-integer-second");
      Assert (V = 24, "Property was not inserted");

      V := Integer_Property.Get (Bundle, "test-integer-second");
      Assert (V = 24, "Property was not inserted");

--        Bundle.Remove ("test-integer-second");
--        Assert (Props.all.Exists ("test-integer-second") = False,
--                "The 'test-integer-second' property was not removed");

--        Assert (Bundle.Exists ("test-integer-second") = False,
--                "Property not removed from bundle");
   end Test_Bundle;

   procedure Test_Bundle_Loader (T : in out Test) is
      Factory : Loader;
      Bundle  : Util.Properties.Bundles.Manager;
   begin
      Initialize (Factory, Util.Tests.Get_Test_Path ("regtests/bundles"));
      Load_Bundle (Factory, "bundle", "fr", Bundle);

      Assert_Equals ("Message France", String '(Bundle.Get ("message")),
                     "Load fr bundle failed");

      Assert_Equals ("Default", String '(Bundle.Get ("message_default")),
                     "Load fr bundle failed");

      Load_Bundle (Factory, "bundle", "en_GB", Bundle);

      Assert_Equals ("GB message", String '(Bundle.Get ("message")),
                     "Load en_GB bundle failed");

      Assert_Equals ("Default", String '(Bundle.Get ("message_default")),
                     "Load en_GB bundle failed");

   end Test_Bundle_Loader;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Suite.Add_Test (Caller.Create ("Test Util.Properties.Bundles",
        Test_Bundle'Access));

      Suite.Add_Test (Caller.Create ("Test Util.Properties.Bundles.Load_Bundle",
        Test_Bundle_Loader'Access));
   end Add_Tests;

end Util.Properties.Bundles.Tests;
