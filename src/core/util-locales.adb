-----------------------------------------------------------------------
--  util-locales -- Locale support
--  Copyright (C) 2001 - 2022 Stephane Carrez
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

with Ada.Strings.Hash;

package body Util.Locales is

   --  ------------------------------
   --  Get the lowercase two-letter ISO-639 code.
   --  ------------------------------
   function Get_Language (Loc : in Locale) return String is
   begin
      if Loc = null then
         return "";
      else
         return Loc (1 .. 2);
      end if;
   end Get_Language;

   --  ------------------------------
   --  Get the ISO 639-2 language code.
   --  ------------------------------
   function Get_ISO3_Language (Loc : in Locale) return String is
   begin
      if Loc = null or else Loc'Length <= 3 then
         return "";
      end if;
      if Loc'Length <= 6 then
         return Loc (4 .. 6);
      end if;
      if Loc'Length <= 13 then
         return Loc (7 .. 9);
      end if;
      return Loc (10  .. 15);
   end Get_ISO3_Language;

   --  ------------------------------
   --  Get the uppercase two-letter ISO-3166 code.
   --  ------------------------------
   function Get_Country (Loc : in Locale) return String is
   begin
      if Loc = null or else Loc'Length <= 2 or else Loc (3) /= '_' then
         return "";
      else
         return Loc (4 .. 5);
      end if;
   end Get_Country;

   --  ------------------------------
   --  Get the ISO-3166-2 country code.
   --  ------------------------------
   function Get_ISO3_Country (Loc : in Locale) return String is
   begin
      if Loc = null or else Loc'Length <= 9 then
         return "";
      else
         return Loc (Loc'Last - 2 .. Loc'Last);
      end if;
   end Get_ISO3_Country;

   --  ------------------------------
   --  Get the variant code
   --  ------------------------------
   function Get_Variant (Loc : in Locale) return String is
   begin
      if Loc = null or else Loc'Length <= 13 then
         return "";
      else
         return Loc (7 .. 8);
      end if;
   end Get_Variant;

   --  ------------------------------
   --  Get the locale for the given language code
   --  ------------------------------
   function Get_Locale (Language : in String) return Locale is
      Length : constant Natural := Language'Length;
      Lower  : Natural := Locales'First;
      Upper  : Natural := Locales'Last;
      Pos    : Natural;
      Result : Locale;
   begin
      while Lower <= Upper loop
         Pos := (Lower + Upper) / 2;
         Result := Locales (Pos);
         if Result'Length < Length then
            if Result.all < Language (Language'First .. Language'First + Result'Length - 1) then
               Lower := Pos + 1;
            else
               Upper := Pos - 1;
            end if;
         elsif Result'Length > Length and then Result (Length + 1) = '.'
           and then Result (1 .. Length) = Language
         then
            return Result;
         elsif Result (1 .. Length) < Language then
            Lower := Pos + 1;
         else
            Upper := Pos - 1;
         end if;
      end loop;
      return NULL_LOCALE;
   end Get_Locale;

   --  ------------------------------
   --  Get the locale for the given language and country code
   --  ------------------------------
   function Get_Locale (Language : in String;
                        Country  : in String) return Locale is
   begin
      if Language'Length /= 2 or else (Country'Length /= 0 and then Country'Length /= 2) then
         return NULL_LOCALE;
      elsif Country'Length = 0 then
         return Get_Locale (Language);
      else
         return Get_Locale (Language & "_" & Country);
      end if;
   end Get_Locale;

   --  ------------------------------
   --  Get the locale for the given language, country and variant code
   --  ------------------------------
   function Get_Locale (Language : in String;
                        Country  : in String;
                        Variant  : in String) return Locale is
   begin
      if Language'Length /= 2
        or else (Country'Length /= 0 and then Country'Length /= 2)
        or else (Variant'Length /= 0 and then Variant'Length /= 2)
      then
         return NULL_LOCALE;
      end if;
      if Country'Length = 0 and then Variant'Length = 0 then
         return Get_Locale (Language);
      elsif Variant'Length = 0 then
         return Get_Locale (Language, Country);
      else
         return Get_Locale (Language & "_" & Country & "_" & Variant);
      end if;
   end Get_Locale;

   --  ------------------------------
   --  Get the locale code in the form <i>language</i>_<i>country</i>_<i>variant</i>.
   --  ------------------------------
   function To_String (Loc : in Locale) return String is
   begin
      if Loc = null then
         return "";
      elsif Loc'Length > 3 and then Loc (3) = '.' then
         return Loc (1 .. 2);
      elsif Loc'Length > 6 and then Loc (6) = '.' then
         return Loc (1 .. 5);
      else
         return Loc (1 .. 8);
      end if;
   end To_String;

   --  ------------------------------
   --  Compute the hash value of the locale.
   --  ------------------------------
   function Hash (Loc : in Locale) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Loc.all);
   end Hash;

end Util.Locales;
