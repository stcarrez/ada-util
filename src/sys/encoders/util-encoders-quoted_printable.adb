-----------------------------------------------------------------------
--  util-encoders-quoted_printable -- Encode/Decode a stream in quoted-printable
--  Copyright (C) 2020 Stephane Carrez
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
with Util.Encoders.Base16;
package body Util.Encoders.Quoted_Printable is

   use Ada.Characters.Handling;

   --  ------------------------------
   --  Decode the Quoted-Printable string and return the result.
   --  When Strict is true, raises the Encoding_Error exception if the
   --  format is invalid.  Otherwise, ignore invalid encoding.
   --  ------------------------------
   function Decode (Content : in String;
                    Strict  : in Boolean := True) return String is
      Result    : String (1 .. Content'Length);
      Read_Pos  : Natural := Content'First;
      Write_Pos : Natural := Result'First - 1;
      C         : Character;
      C2        : Character;
   begin
      while Read_Pos <= Content'Last loop
         C := Content (Read_Pos);
         if C = '=' then
            exit when Read_Pos = Content'Last;
            if Read_Pos + 2 > Content'Last then
               exit when not Strict;
               raise Encoding_Error;
            end if;
            Read_Pos := Read_Pos + 1;
            C := Content (Read_Pos);
            if not Is_Hexadecimal_Digit (C) then
               exit when not Strict;
               raise Encoding_Error;
            end if;
            C2 := Content (Read_Pos + 1);
            if not Is_Hexadecimal_Digit (C) then
               exit when not Strict;
               raise Encoding_Error;
            end if;
            Write_Pos := Write_Pos + 1;
            Result (Write_Pos) := Base16.From_Hex (C, C2);
            Read_Pos := Read_Pos + 1;
         else
            Write_Pos := Write_Pos + 1;
            Result (Write_Pos) := C;
         end if;
         Read_Pos := Read_Pos + 1;
      end loop;
      return Result (1 .. Write_Pos);
   end Decode;

   --  ------------------------------
   --  Decode the "Q" encoding, similar to Quoted-Printable but with
   --  spaces that can be replaced by '_'.
   --  See RFC 2047.
   --  ------------------------------
   function Q_Decode (Content : in String) return String is
      Result    : String (1 .. Content'Length);
      Read_Pos  : Natural := Content'First;
      Write_Pos : Natural := Result'First - 1;
      C         : Character;
      C2        : Character;
   begin
      while Read_Pos <= Content'Last loop
         C := Content (Read_Pos);
         if C = '=' then
            exit when Read_Pos = Content'Last or else Read_Pos + 2 > Content'Last;
            Read_Pos := Read_Pos + 1;
            C := Content (Read_Pos);
            exit when not Is_Hexadecimal_Digit (C);
            C2 := Content (Read_Pos + 1);
            exit when  not Is_Hexadecimal_Digit (C);
            Write_Pos := Write_Pos + 1;
            Result (Write_Pos) := Base16.From_Hex (C, C2);
            Read_Pos := Read_Pos + 1;
         elsif C = '_' then
            Write_Pos := Write_Pos + 1;
            Result (Write_Pos) := ' ';
         else
            Write_Pos := Write_Pos + 1;
            Result (Write_Pos) := C;
         end if;
         Read_Pos := Read_Pos + 1;
      end loop;
      return Result (1 .. Write_Pos);
   end Q_Decode;

end Util.Encoders.Quoted_Printable;
