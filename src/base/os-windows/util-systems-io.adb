-----------------------------------------------------------------------
--  util-systems-io -- System low level and raw IO operations
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.IO_Exceptions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with System;

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
      Res    : aliased DWORD := 0;
      Status : BOOL;
   begin
      Status := Write_File (File, Content'Address, Content'Length,
                            Res'Unchecked_Access, System.Null_Address);
      if Status = 0 then
         raise Ada.IO_Exceptions.Device_Error;
      end if;
   end Put_Raw;

end Util.Systems.IO;
