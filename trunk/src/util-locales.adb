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
with Ada.Containers.Indefinite_Hashed_Sets;
with Util.Concurrent.Locks;

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
package body Util.Locales is

   package Locale_Set is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type    => Locale,
      Hash            => Hash,
      Equivalent_Elements => "=");
   use Locale_Set;

   Locales : Locale_Set.Set;
   Lock    : Util.Concurrent.Locks.RW_Lock;

   --  ------------------------------
   --  Get the lowercase two-letter ISO-639 code.
   --  ------------------------------
   function Get_Language (Loc : Locale) return String is
   begin
      return Loc.Language.all;
   end Get_Language;

   --  ------------------------------
   --  Get the uppercase two-letter ISO-3166 code.
   --  ------------------------------
   function Get_Country (Loc : Locale) return String is
   begin
      return Loc.Country.all;
   end Get_Country;

   --  ------------------------------
   --  Get the variant code
   --  ------------------------------
   function Get_Variant (Loc : Locale) return String is
   begin
      return Loc.Variant.all;
   end Get_Variant;

   --  ------------------------------
   --  Get the locale for the given language code
   --  ------------------------------
   function Get_Locale (Language : String) return Locale is
   begin
      return Get_Locale (Language, EMPTY_STR, EMPTY_STR);
   end Get_Locale;

   --  ------------------------------
   --  Get the locale for the given language and country code
   --  ------------------------------
   function Get_Locale (Language : String;
                        Country  : String) return Locale is
   begin
      return Get_Locale (Language, Country, EMPTY_STR);
   end Get_Locale;

   --  ------------------------------
   --  Get the locale for the given language, country and variant code
   --  ------------------------------
   function Get_Locale (Language : String;
                        Country  : String;
                        Variant  : String) return Locale is
      Loc : Locale := (Language => Language'Unrestricted_Access,
                       Country  => Country'Unrestricted_Access,
                       Variant  => Variant'Unrestricted_Access);
      Pos : Cursor;
   begin
      loop
         --  Get the locale from our set.
         Lock.Read;
         Pos := Locales.Find (Loc);
         if Has_Element (Pos) then
            Loc := Element (Pos);
            Lock.Release_Read;
            return Loc;
         end if;
         Lock.Release_Read;

         --  Insert the new locale in our set.
         Lock.Write;
         Pos := Locales.Find (Loc);
         if not Has_Element (Pos) then
            Loc.Country := new String '(Country);
            Loc.Language := new String '(Language);
            Loc.Variant  := new String '(Variant);
            Locales.Insert (Loc);
         end if;
         Lock.Release_Write;
      end loop;
   end Get_Locale;

   --  ------------------------------
   --  Check whether two locales are the same.
   --  ------------------------------
   function "=" (Left, Right : Locale) return Boolean is
   begin
      --  Check pointer comparison first
      if Left.Language /= Right.Language
        and then Left.Language.all /= Right.Language.all then
         return False;
      end if;
      if Left.Country /= Right.Country
        and then Left.Country.all /= Right.Country.all then
         return False;
      end if;
      return Left.Variant = Right.Variant
        or else Left.Variant.all = Right.Variant.all;
   end "=";

   --  ------------------------------
   --  Compute the hash value of the locale.
   --  ------------------------------
   function Hash (Loc : Locale) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Hash (Loc.Country) xor Hash (Loc.Language) xor Hash (Loc.Variant);
   end Hash;

begin
   Locales.Insert (FRENCH);
   Locales.Insert (ENGLISH);
   Locales.Insert (ITALIAN);
   Locales.Insert (GERMAN);

   Locales.Insert (FRANCE);
   Locales.Insert (CANADA);
   Locales.Insert (GERMANY);
   Locales.Insert (ITALY);
   Locales.Insert (UK);
   Locales.Insert (US);
end Util.Locales;
