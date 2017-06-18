-----------------------------------------------------------------------
--  util-mail -- Mail Utility Library
--  Copyright (C) 2017 Stephane Carrez
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
with Ada.Strings.Fixed;

package body Util.Mail is

   --  ------------------------------
   --  Parse the email address and separate the name from the address.
   --  ------------------------------
   function Parse_Address (E_Mail : in String) return Email_Address is
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;
      use Ada.Strings;

      Result    : Email_Address;
      First_Pos : constant Natural := Index (E_Mail, "<");
      Last_Pos  : constant Natural := Index (E_Mail, ">");
   begin
      if First_Pos > 0 and Last_Pos > 0 then
         Result.Name := To_Unbounded_String (Trim (E_Mail (E_Mail'First .. First_Pos - 1),
                                             Both));
         Result.Address := To_Unbounded_String (Trim (E_Mail (First_Pos + 1 .. Last_Pos - 1),
                                                Both));
      else
         Result.Address := To_Unbounded_String (Trim (E_Mail, Both));
      end if;
      return Result;
   end Parse_Address;

end Util.Mail;
