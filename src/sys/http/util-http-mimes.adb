-----------------------------------------------------------------------
--  util-http-mimes -- HTTP Headers
--  Copyright (C) 2022 Stephane Carrez
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
with Ada.Characters.Handling;
with Ada.Strings.Equal_Case_Insensitive;
package body Util.Http.Mimes is

   use Ada.Characters.Handling;
   use Ada.Strings;

   function Is_Mime (Header : in String;
                     Mime   : in String) return Boolean is
      Sep : Natural;
   begin
      Sep := Util.Strings.Index (Header, ';');
      if Sep = 0 then
         Sep := Header'Last;
      else
         Sep := Sep - 1;
         while Sep > Header'First and then Is_Space (Header (Sep)) loop
            Sep := Sep - 1;
         end loop;
      end if;
      return Equal_Case_Insensitive (Header (Header'First .. Sep), Mime);
   end Is_Mime;

end Util.Http.Mimes;
