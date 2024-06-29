-----------------------------------------------------------------------
--  util-systems-io -- System low level and raw IO operations
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
