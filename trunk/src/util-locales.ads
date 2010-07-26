-----------------------------------------------------------------------
--  Util.Locales -- Locale
--  Copyright (C) 2001, 2002, 2003, 2009, 2010 Stephane Carrez
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

with Util.Strings;
with Ada.Containers;

--  The <b>Locales</b> package defines the <b>Locale</b> type to represent
--  the language, country and variant.
--
--  The language is a valid <b>ISO language code</b>.  This is a two-letter
--  lower case code defined by IS-639
--  See http://www.loc.gov/standards/iso639-2/englangn.html
--
--  The country is a valid <b>ISO country code</b>.  These codes are
--  a two letter upper-case code defined by ISO-3166.
--  See http://www.iso.ch/iso/en/prods-services/iso3166ma/02iso-3166-code-lists/list-en1.html
--
--  The variant part is a vendor or browser specific code.
--
--  The <b>Locales</b> package tries to follow the Java <b>Locale</b> class.
package Util.Locales is

   type Locale is private;

   --  Get the lowercase two-letter ISO-639 code.
   function Get_Language (Loc : Locale) return String;

   --  Get the uppercase two-letter ISO-3166 code.
   function Get_Country (Loc : Locale) return String;

   --  Get the variant code
   function Get_Variant (Loc : Locale) return String;

   --  Get the locale for the given language code
   function Get_Locale (Language : String) return Locale;

   --  Get the locale for the given language and country code
   function Get_Locale (Language : String;
                        Country  : String) return Locale;

   --  Get the locale for the given language, country and variant code
   function Get_Locale (Language : String;
                        Country  : String;
                        Variant  : String) return Locale;

   --  Check whether two locales are the same.
   function "=" (Left, Right : Locale) return Boolean;

   --  Compute the hash value of the locale.
   function Hash (Loc : Locale) return Ada.Containers.Hash_Type;

   --  French language
   FRENCH  : constant Locale;

   --  English language
   ENGLISH : constant Locale;

   --  German language
   GERMAN  : constant Locale;

   --  Italian language
   ITALIAN : constant Locale;

   FRANCE  : constant Locale;
   CANADA  : constant Locale;
   GERMANY : constant Locale;
   ITALY   : constant Locale;
   US      : constant Locale;
   UK      : constant Locale;
private

   use Util.Strings;

   type Locale is record
      Language : Name_Access;
      Country  : Name_Access;
      Variant  : Name_Access;
   end record;

   EMPTY_STR    : aliased constant String := "";

   FRENCH_STR   : aliased constant String := "fr";
   ITALIAN_STR  : aliased constant String := "it";
   ENGLISH_STR  : aliased constant String := "en";
   GERMAN_STR   : aliased constant String := "de";

   CANADA_STR   : aliased constant String := "CA";
   FRANCE_STR   : aliased constant String := "FR";
   ITALY_STR    : aliased constant String := "IT";
   US_STR       : aliased constant String := "US";
   UK_STR       : aliased constant String := "UK";
   GERMANY_STR  : aliased constant String := "DE";

   FRENCH  : constant Locale := (Language => FRENCH_STR'Access,
                                 others   => EMPTY_STR'Access);

   ITALIAN : constant Locale := (Language => ITALIAN_STR'Access,
                                 others   => EMPTY_STR'Access);

   GERMAN  : constant Locale := (Language => GERMAN_STR'Access,
                                 others   => EMPTY_STR'Access);

   ENGLISH : constant Locale := (Language => ENGLISH_STR'Access,
                                 others   => EMPTY_STR'Access);

   FRANCE  : constant Locale := (Language => FRENCH_STR'Access,
                                 Country  => FRANCE_STR'Access,
                                 others   => EMPTY_STR'Access);

   ITALY   : constant Locale := (Language => ITALIAN_STR'Access,
                                 Country  => ITALY_STR'Access,
                                 others   => EMPTY_STR'Access);

   CANADA  : constant Locale := (Language => ENGLISH_STR'Access,
                                 Country  => CANADA_STR'Access,
                                 others   => EMPTY_STR'Access);

   GERMANY : constant Locale := (Language => GERMAN_STR'Access,
                                 Country  => GERMANY_STR'Access,
                                 others   => EMPTY_STR'Access);

   US      : constant Locale := (Language => ENGLISH_STR'Access,
                                 Country  => US_STR'Access,
                                 others   => EMPTY_STR'Access);

   UK      : constant Locale := (Language => ENGLISH_STR'Access,
                                 Country  => UK_STR'Access,
                                 others   => EMPTY_STR'Access);

end Util.Locales;
