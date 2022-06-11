-----------------------------------------------------------------------
--  util-properties-bundles -- Test for property bundles
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2021, 2022 Stephane Carrez
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

with Util.Test_Caller;
with Util.Properties.Basic;
with Util.Measures;

package body Util.Properties.Bundles.Tests is

   use Util.Tests;
   use Util.Properties.Basic;

   --  Test the bundle
   procedure Test_Bundle (T : in out Test) is
      Props  : aliased Properties.Manager;
      Bundle : Properties.Bundles.Manager;
      V : Integer := 23;
   begin
      --  Create a first property (while the bundle is empty)
--        Integer_Property.Set (Props.all, "test-integer", 123);
--        Assert (Bundle.Exists ("test-integer"), "Invalid properties");
--
--        V := Integer_Property.Get (Bundle, "test-integer");
--        Assert (V = 123, "Property was not inserted");

      --  Add a property set to the bundle
      Bundle.Add_Bundle (Props'Unchecked_Access);
      Integer_Property.Set (Props, "test-integer-second", 24);
      V := Integer_Property.Get (Props, "test-integer-second");
      T.Assert (V = 24, "Property was not inserted");

      V := Integer_Property.Get (Bundle, "test-integer-second");
      T.Assert (V = 24, "Property was not inserted");

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
      Initialize (Factory, Util.Tests.Get_Path ("regtests/bundles"));
      Load_Bundle (Factory, "bundle", "fr", Bundle);

      Assert_Equals (T, "Message France", String '(Bundle.Get ("message")),
                     "Load fr bundle failed");

      Assert_Equals (T, "Default", String '(Bundle.Get ("message_default")),
                     "Load fr bundle failed");

      Load_Bundle (Factory, "bundle", "en_GB", Bundle);

      Assert_Equals (T, "GB message", String '(Bundle.Get ("message")),
                     "Load en_GB bundle failed");

      Assert_Equals (T, "Default", String '(Bundle.Get ("message_default")),
                     "Load en_GB bundle failed");

   end Test_Bundle_Loader;

   --  Test overloading some bundle definition by having incomplete files.
   procedure Test_Bundle_Overload (T : in out Test) is
      Factory : Loader;
      Bundle  : Util.Properties.Bundles.Manager;
      P1      : constant String := Util.Tests.Get_Path ("regtests/bundles");
      P2      : constant String := Util.Tests.Get_Path ("bundles");
   begin
      Initialize (Factory, P1 & ";" & P2);
      Load_Bundle (Factory, "dates", "fr", Bundle);

      --  Overloaded by regtests/bundles/dates.properties
      Assert_Equals (T, "New", String '(Bundle.Get ("util.test_variable")),
                     "Load fr bundle failed (not defined)");

      Assert_Equals (T, "Jan", String '(Bundle.Get ("util.month1.short")),
                     "Load fr bundle failed (should not be overloaded)");

      --  Not overloaded, value comes from bundles/dates_fr.properties
      Assert_Equals (T, "Mar", String '(Bundle.Get ("util.month3.short")),
                     "Load fr bundle failed");

      Load_Bundle (Factory, "dates", "en_GB", Bundle);

      Assert_Equals (T, "Jan_Overloaded", String '(Bundle.Get ("util.month1.short")),
                     "Load en_GB bundle failed (should be overloaded)");

      --  Not overloaded, value comes from bundles/dates_fr.properties
      Assert_Equals (T, "Mar", String '(Bundle.Get ("util.month3.short")),
                     "Load en_GB bundle failed");
   end Test_Bundle_Overload;

   --  ------------------------------
   --  Test bundle resolution perf.
   --  ------------------------------
   procedure Test_Bundle_Perf (T : in out Test) is
      Factory : Loader;
      Bundle  : Util.Properties.Bundles.Manager;
      P1      : constant String := Util.Tests.Get_Path ("regtests/bundles");
      P2      : constant String := Util.Tests.Get_Path ("bundles");
   begin
      Initialize (Factory, P1 & ";" & P2);
      declare
         S1      : Util.Measures.Stamp;
      begin
         Load_Bundle (Factory, "dates", "fr", Bundle);
         Util.Measures.Report (S1, "Load_Bundle (first time)");
      end;
      declare
         S1      : Util.Measures.Stamp;
      begin
         Load_Bundle (Factory, "dates", "fr", Bundle);
         Util.Measures.Report (S1, "Load_Bundle (second time)");
      end;
      declare
         S1      : Util.Measures.Stamp;
      begin
         for I in 1 .. 1000 loop
            Load_Bundle (Factory, "dates", "fr", Bundle);
         end loop;
         Util.Measures.Report (S1, "Load_Bundle", 1000);
      end;

      --  Not overloaded, value comes from bundles/dates_fr.properties
      Assert_Equals (T, "Mar", String '(Bundle.Get ("util.month3.short")),
                     "Load fr bundle failed");
   end Test_Bundle_Perf;

   package Caller is new Util.Test_Caller (Test, "Properties.Bundles");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Properties.Bundles",
                       Test_Bundle'Access);

      Caller.Add_Test (Suite, "Test Util.Properties.Bundles.Load_Bundle",
                       Test_Bundle_Loader'Access);

      Caller.Add_Test (Suite, "Test Util.Properties.Bundles.Load_Bundle (overloading)",
                       Test_Bundle_Overload'Access);

      Caller.Add_Test (Suite, "Test Util.Properties.Bundles.Load_Bundle (perf)",
                       Test_Bundle_Perf'Access);
   end Add_Tests;

end Util.Properties.Bundles.Tests;
