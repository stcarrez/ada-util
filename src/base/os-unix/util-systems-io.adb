-----------------------------------------------------------------------
--  util-systems-io -- System low level and raw IO operations
--  Copyright (C) 2023 Stephane Carrez
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

with Ada.IO_Exceptions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Util.Systems.IO is

   use Ada.Strings.UTF_Encoding;

   --  ------------------------------
   --  Write the Latin-1 string and encode it in UTF-8.
   --  ------------------------------
   procedure Put (File    : in File_Type;
                  Content : in String) is
      S : constant UTF_8_String := Strings.Encode (Content);
   begin
      Put_Raw (File, S);
   end Put;

   procedure Put (File    : in File_Type;
                  C       : in Character) is
      S : constant String (1 .. 1) := (others => C);
   begin
      Put (File, S);
   end Put;

   --  ------------------------------
   --  Write the string or wide character content using UTF-8 encoding.
   --  ------------------------------
   procedure Put_UTF_8 (File    : in File_Type;
                       Content : in Wide_Wide_String) is
      S : constant UTF_8_String := Wide_Wide_Strings.Encode (Content);
   begin
      Put_Raw (File, S);
   end Put_UTF_8;

   procedure Put_UTF_8 (File : in File_Type;
                        C    : in Wide_Wide_Character) is
      S : constant Wide_Wide_String (1 .. 1) := (others => C);
   begin
      Put_UTF_8 (File, S);
   end Put_UTF_8;

   --  ------------------------------
   --  Write the string considered as a sequence of bytes (no change).
   --  ------------------------------
   procedure Put_Raw (File    : in File_Type;
                      Content : in Ada.Strings.UTF_Encoding.UTF_8_String) is
      use Util.Systems.Os;
   begin
      if Write (File, Content'Address, Content'Length) < 0 then
         raise Ada.IO_Exceptions.Device_Error;
      end if;
   end Put_Raw;

end Util.Systems.IO;
