-----------------------------------------------------------------------
--  util-http-mimes -- HTTP Headers
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
