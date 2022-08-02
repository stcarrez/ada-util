-----------------------------------------------------------------------
--  util-encoders-base16 -- Encode/Decode a stream in hexadecimal
--  Copyright (C) 2009, 2010, 2011, 2017, 2022 Stephane Carrez
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

package body Util.Encoders.Base16 is

   package body Encoding is

      type Code is mod 2**32;

      function To_Output_Char (Ch : Input_Char) return Code;

      type Conv_Table is array (0 .. 15) of Output_Char;

      Conversion : constant Conv_Table :=
        (0 => Output_Char'Val (Character'Pos ('0')),
         1 => Output_Char'Val (Character'Pos ('1')),
         2 => Output_Char'Val (Character'Pos ('2')),
         3 => Output_Char'Val (Character'Pos ('3')),
         4 => Output_Char'Val (Character'Pos ('4')),
         5 => Output_Char'Val (Character'Pos ('5')),
         6 => Output_Char'Val (Character'Pos ('6')),
         7 => Output_Char'Val (Character'Pos ('7')),
         8 => Output_Char'Val (Character'Pos ('8')),
         9 => Output_Char'Val (Character'Pos ('9')),
         10 => Output_Char'Val (Character'Pos ('A')),
         11 => Output_Char'Val (Character'Pos ('B')),
         12 => Output_Char'Val (Character'Pos ('C')),
         13 => Output_Char'Val (Character'Pos ('D')),
         14 => Output_Char'Val (Character'Pos ('E')),
         15 => Output_Char'Val (Character'Pos ('F')));

      --  Encode the input stream in hexadecimal and write the result
      --  in the output stream
      procedure Encode (From    : in Input;
                        Into    : in out Output;
                        Last    : out Output_Index;
                        Encoded : out Index) is

         N   : constant Output_Index := (Input_Char'Size / 8) * 2;
         Pos : Output_Index := Into'First;
      begin
         for I in From'Range loop
            if Pos + N > Into'Last + 1 then
               Last    := Pos - 1;
               Encoded := I - 1;
               return;
            end if;
            declare
               Value : Code := Input_Char'Pos (From (I));
               P     : Code;
            begin
               Pos := Pos + N;
               for J in 1 .. N / 2 loop
                  P := Value;
                  Value := Value / 16;
                  Into (Pos - J) := Conversion (Natural (P and 16#0F#));

                  P := Value;
                  Into (Pos - J - 1) := Conversion (Natural (P and 16#0F#));
                  Value := Value / 16;
               end loop;
            end;
         end loop;
         Last    := Pos - 1;
         Encoded := From'Last;
      end Encode;

      function To_Output_Char (Ch : Input_Char) return Code is
         C : constant Code := Input_Char'Pos (Ch);
      begin
         if C >= Character'Pos ('a') and then C <= Character'Pos ('f') then
            return C - Character'Pos ('a') + 10;

         elsif C >= Character'Pos ('A') and then C <= Character'Pos ('F') then
            return C - Character'Pos ('A') + 10;

         elsif C >= Character'Pos ('0') and then C <= Character'Pos ('9') then
            return C - Character'Pos ('0');

         else
            raise Encoding_Error with "Invalid character: " & Character'Val (C);
         end if;
      end To_Output_Char;

      procedure Decode (From    : in Input;
                        Into    : in out Output;
                        Last    : out Output_Index;
                        Encoded : out Index) is
         First : Boolean := True;
         Pos   : Output_Index := Into'First;
         Value : Code;
      begin
         if Into'Length < From'Length / 2 then
            Encoded := Into'Length * 2;
         elsif From'Last mod 2 /= 0 then
            Encoded := From'Last - 1;
         else
            Encoded := From'Last;
         end if;
         if Encoded < From'First then
            raise Encoding_Error with "Hexadecimal stream is too short";
         end if;
         for I in From'First .. Encoded loop
            if First then
               Value := To_Output_Char (From (I));
               First := False;
            else
               Value := Value * 16 + To_Output_Char (From (I));
               Into (Pos) := Output_Char'Val (Value);
               Pos := Pos + 1;
               First := True;
            end if;
         end loop;
         Last := Pos - 1;
      end Decode;

   end Encoding;

   package Encoding_Stream is new Encoding (Output => Ada.Streams.Stream_Element_Array,
                                            Index  => Ada.Streams.Stream_Element_Offset,
                                            Output_Index  => Ada.Streams.Stream_Element_Offset,
                                            Input_Char   => Ada.Streams.Stream_Element,
                                            Output_Char   => Ada.Streams.Stream_Element,
                                            Input  => Ada.Streams.Stream_Element_Array);

   --  ------------------------------
   --  Encodes the binary input stream represented by <b>Data</b> into
   --  the a base16 (hexadecimal) output stream <b>Into</b>.
   --
   --  If the transformer does not have enough room to write the result,
   --  it must return in <b>Encoded</b> the index of the last encoded
   --  position in the <b>Data</b> stream.
   --
   --  The transformer returns in <b>Last</b> the last valid position
   --  in the output stream <b>Into</b>.
   --
   --  The <b>Encoding_Error</b> exception is raised if the input
   --  stream cannot be transformed.
   --  ------------------------------
   overriding
   procedure Transform (E       : in out Encoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is
      pragma Unreferenced (E);
   begin
      Encoding_Stream.Encode (Data, Into, Last, Encoded);
   end Transform;

   --  ------------------------------
   --  Decodes the base16 input stream represented by <b>Data</b> into
   --  the binary output stream <b>Into</b>.
   --
   --  If the transformer does not have enough room to write the result,
   --  it must return in <b>Encoded</b> the index of the last encoded
   --  position in the <b>Data</b> stream.
   --
   --  The transformer returns in <b>Last</b> the last valid position
   --  in the output stream <b>Into</b>.
   --
   --  The <b>Encoding_Error</b> exception is raised if the input
   --  stream cannot be transformed.
   --  ------------------------------
   overriding
   procedure Transform (E       : in out Decoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is
      pragma Unreferenced (E);
   begin
      Encoding_Stream.Decode (Data, Into, Last, Encoded);
   end Transform;

end Util.Encoders.Base16;
