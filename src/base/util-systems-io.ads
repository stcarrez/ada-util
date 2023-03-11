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

with Ada.Strings.UTF_Encoding;
with Util.Systems.Types;
with Util.Systems.Os;
package Util.Systems.IO is

   subtype File_Type is Util.Systems.Types.File_Type;

   STDIN_FILENO  : constant File_Type := Util.Systems.Os.STDIN_FILENO;
   STDOUT_FILENO : constant File_Type := Util.Systems.Os.STDOUT_FILENO;
   STDERR_FILENO : constant File_Type := Util.Systems.Os.STDERR_FILENO;

   --  Write the Latin-1 string and encode it in UTF-8.
   procedure Put (File    : in File_Type;
                  Content : in String);
   procedure Put (File    : in File_Type;
                  C       : in Character);

   --  Write the string or wide character content using UTF-8 encoding.
   procedure Put_UTF_8 (File    : in File_Type;
                        Content : in Wide_Wide_String);
   procedure Put_UTF_8 (File : in File_Type;
                        C    : in Wide_Wide_Character);

   --  Write the string considered as a sequence of bytes (no change).
   procedure Put_Raw (File    : in File_Type;
                      Content : in Ada.Strings.UTF_Encoding.UTF_8_String);

end Util.Systems.IO;
