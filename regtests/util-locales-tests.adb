-----------------------------------------------------------------------
--  util-locales-tests -- Unit tests for locales
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
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
package body Util.Locales.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Locales");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Locales.Get_Locale",
                       Test_Get_Locale'Access);
      Caller.Add_Test (Suite, "Test Util.Locales.Get_Language",
                       Test_Get_Locale'Access);
      Caller.Add_Test (Suite, "Test Util.Locales.Get_Country",
                       Test_Get_Locale'Access);
      Caller.Add_Test (Suite, "Test Util.Locales.Get_Variant",
                       Test_Get_Locale'Access);
      Caller.Add_Test (Suite, "Test Util.Locales.Hash",
                       Test_Hash_Locale'Access);
      Caller.Add_Test (Suite, "Test Util.Locales.=",
                       Test_Compare_Locale'Access);
      Caller.Add_Test (Suite, "Test Util.Locales.Locales",
                       Test_Get_Locales'Access);
      Caller.Add_Test (Suite, "Test Util.Locales.NULL_LOCALE",
                       Test_Null_Locale'Access);
   end Add_Tests;

   procedure Test_Get_Locale (T : in out Test) is
      Loc : Locale;
   begin
      Loc := Get_Locale ("en");
      Assert_Equals (T, "en", Get_Language (Loc), "Invalid language");
      Assert_Equals (T, "", Get_Country (Loc), "Invalid country");
      Assert_Equals (T, "", Get_Variant (Loc), "Invalid variant");
      Assert_Equals (T, "", Get_ISO3_Country (Loc), "Invalid country");
      Assert_Equals (T, "eng", Get_ISO3_Language (Loc), "Invalid language");

      Loc := Get_Locale ("ar_DZ");
      Assert_Equals (T, "ar", Get_Language (Loc), "Invalid language");
      Assert_Equals (T, "DZ", Get_Country (Loc), "Invalid country");
      Assert_Equals (T, "", Get_Variant (Loc), "Invalid variant");
      Assert_Equals (T, "DZA", Get_ISO3_Country (Loc), "Invalid country");
      Assert_Equals (T, "ara", Get_ISO3_Language (Loc), "Invalid language");

      Loc := Get_Locale ("fr");
      Assert_Equals (T, "fr", Get_Language (Loc), "Invalid language");
      Assert_Equals (T, "", Get_Country (Loc), "Invalid country");
      Assert_Equals (T, "", Get_Variant (Loc), "Invalid variant");
      Assert_Equals (T, "fra", Get_ISO3_Language (Loc), "Invalid language");
      Assert_Equals (T, "", Get_ISO3_Country (Loc), "Invalid country");

      Loc := Get_Locale ("ja", "JP", "JP");
      Assert_Equals (T, "ja", Get_Language (Loc), "Invalid language");
      Assert_Equals (T, "JP", Get_Country (Loc), "Invalid country");
      Assert_Equals (T, "JP", Get_Variant (Loc), "Invalid variant");
      Assert_Equals (T, "JPN", Get_ISO3_Country (Loc), "Invalid country");
      Assert_Equals (T, "jpn_JP", Get_ISO3_Language (Loc), "Invalid language");

      Loc := Get_Locale ("no", "NO", "NY");
      Assert_Equals (T, "no", Get_Language (Loc), "Invalid language");
      Assert_Equals (T, "NO", Get_Country (Loc), "Invalid country");
      Assert_Equals (T, "NY", Get_Variant (Loc), "Invalid variant");
   end Test_Get_Locale;

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

   procedure Test_Get_Locales (T : in out Test) is
   begin
      for I in Locales'Range loop
         declare
            Language : constant String := Get_Language (Locales (I));
            Country  : constant String := Get_Country (Locales (I));
            Variant  : constant String := Get_Variant (Locales (I));
            Loc      : constant Locale := Get_Locale (Language, Country, Variant);
            Name     : constant String := To_String (Loc);
            Iso      : constant String := Get_ISO3_Language (Locales (I));
            Iso_Country : constant String := Get_ISO3_Country (Locales (I));

            pragma Unreferenced (Iso, Iso_Country);
         begin
            T.Assert (Loc = Locales (I), "Invalid locale at " & Positive'Image (I)
                      & " " & Loc.all);
            if Variant'Length > 0 then
               Assert_Equals (T, Name, Language & "_" & Country & "_" & Variant,
                              "Invalid To_String");
            elsif Country'Length > 0 then
               Assert_Equals (T, Name, Language & "_" & Country, "Invalid To_String");
            else
               Assert_Equals (T, Name, Language, "Invalid To_String");
            end if;
         end;
      end loop;
   end Test_Get_Locales;

   procedure Test_Null_Locale (T : in out Test) is
      Loc : Locale;
   begin
      Assert_Equals (T, "", Get_Language (NULL_LOCALE), "Invalid language");
      Assert_Equals (T, "", Get_ISO3_Language (NULL_LOCALE), "Invalid language");
      Assert_Equals (T, "", To_String (NULL_LOCALE), "Invalid To_String");

      Loc := Get_Locale ("", "");
      T.Assert (Loc = NULL_LOCALE, "Invalid Get_Locale");

      Loc := Get_Locale ("xz", "");
      T.Assert (Loc = NULL_LOCALE, "Invalid Get_Locale");

      Loc := Get_Locale ("en", "blob");
      T.Assert (Loc = NULL_LOCALE, "Invalid Get_Locale");

      Loc := Get_Locale ("xx");
      T.Assert (Loc = NULL_LOCALE, "Invalid Get_Locale");

      Loc := Get_Locale ("en", "de", "plop");
      T.Assert (Loc = NULL_LOCALE, "Invalid Get_Locale");

   end Test_Null_Locale;

end Util.Locales.Tests;
