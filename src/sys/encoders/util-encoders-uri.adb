-----------------------------------------------------------------------
--  util-encoders-uri -- Encode and decode URI using percent encoding
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
with Util.Encoders.Base16;

package body Util.Encoders.URI is

   --  ------------------------------
   --  Compute the length of the encoded URI string with percent encoding
   --  and with the given encoding array.  Characters for which the `Encoding` array
   --  returns True are encoded using %HEXDIGIT HEXDIGIT.
   --  Returns the length of encoded string.
   --  ------------------------------
   function Encoded_Length (URI      : in String;
                            Encoding : in Encoding_Array := HREF_STRICT) return Natural is
      Length : Natural := 0;
   begin
      for C of URI loop
         if Encoding (C) then
            Length := Length + 3;
         else
            Length := Length + 1;
         end if;
      end loop;
      return Length;
   end Encoded_Length;

   --  ------------------------------
   --  Encode the string using URI percent encoding.
   --  Characters for which the `Encoding` array returns True are encoded
   --  using %HEXDIGIT HEXDIGIT.  Returns the percent encoded string.
   --  ------------------------------
   function Encode (URI      : in String;
                    Encoding : in Encoding_Array := HREF_STRICT) return String is
      Length    : constant Natural := Encoded_Length (URI, Encoding);
      Result    : String (1 .. Length);
      Write_Pos : Positive := 1;
   begin
      for C of URI loop
         if Encoding (C) then
            Result (Write_Pos) := '%';
            Result (Write_Pos + 1) := Base16.To_Hex_High (C);
            Result (Write_Pos + 2) := Base16.To_Hex_Low (C);
            Write_Pos := Write_Pos + 3;
         else
            Result (Write_Pos) := C;
            Write_Pos := Write_Pos + 1;
         end if;
      end loop;
      return Result;
   end Encode;

   --  ------------------------------
   --  Decode the percent encoded URI string.
   --  ------------------------------
   function Decode (URI : in String) return String is
      Result    : String (1 .. URI'Length);
      Read_Pos  : Natural := URI'First;
      Write_Pos : Natural := Result'First - 1;
      C         : Character;
   begin
      while Read_Pos <= URI'Last loop
         C := URI (Read_Pos);
         if C = '%' and then Read_Pos + 2 <= URI'Last then
            C := Base16.From_Hex (URI (Read_Pos + 1), URI (Read_Pos + 2));
            Read_Pos := Read_Pos + 3;
         else
            Read_Pos := Read_Pos + 1;
         end if;
         Write_Pos := Write_Pos + 1;
         Result (Write_Pos) := C;
      end loop;
      return Result (1 .. Write_Pos);
   end Decode;

end Util.Encoders.URI;
