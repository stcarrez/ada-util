-----------------------------------------------------------------------
--  locales.tests -- Unit tests for locales
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

with AUnit.Test_Caller;
with Util.Tests;
package body Util.Locales.Tests is

   use Util.Tests;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Suite.Add_Test (Caller.Create ("Test Util.Locales.Get_Locale",
        Test_Get_Locale'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Locales.Hash",
        Test_Hash_Locale'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Locales.=",
        Test_Compare_Locale'Access));
   end Add_Tests;

   procedure Test_Get_Locale (T : in out Test) is
      Loc : Locale;
   begin
      Loc := Get_Locale ("en");
      Assert_Equals (T, "en", Get_Language (Loc), "Invalid language");
      Assert_Equals (T, "", Get_Country (Loc), "Invalid country");
      Assert_Equals (T, "", Get_Variant (Loc), "Invalid variant");
   end Test_Get_Locale;

--     procedure Test_Get_Country (T : in out Test);
--     procedure Test_Get_Language (T : in out Test);

   procedure Test_Hash_Locale (T : in out Test) is
      use type Ada.Containers.Hash_Type;
   begin
      T.Assert (Hash (FRANCE) /= Hash (FRENCH), "Hash should be different");
      T.Assert (Hash (FRANCE) /= Hash (ENGLISH), "Hash should be different");
      T.Assert (Hash (FRENCH) /= Hash (ENGLISH), "Hash should be different");
   end Test_Hash_Locale;

   procedure Test_Compare_Locale (T : in out Test) is
   begin
      T.Assert (FRANCE /= FRENCH, "Equality");
      T.Assert (FRANCE = FRANCE, "Equality");
      T.Assert (FRANCE = Get_Locale ("fr", "FR"), "Equality");
      T.Assert (FRANCE /= ENGLISH, "Equality");
      T.Assert (FRENCH /= ENGLISH, "Equaliy");
   end Test_Compare_Locale;

end Util.Locales.Tests;
