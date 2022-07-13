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
with Ada.Streams;

--  The <b>Util.Encodes.Base16</b> packages encodes and decodes streams
--  in hexadecimal.
package Util.Encoders.Base16 is

   pragma Preelaborate;

   use type Interfaces.Unsigned_8;

   Conversion : constant String (1 .. 16) := "0123456789ABCDEF";

   function From_Hex (C : in Character) return Interfaces.Unsigned_8 is
     (if C >= '0' and then C <= '9' then Character'Pos (C) - Character'Pos ('0')
      elsif C >= 'A' and then C <= 'F' then Character'Pos (C) - Character'Pos ('A') + 10
      else 0);

   function From_Hex (C1, C2 : in Character) return Character is
     (Character'Val (From_Hex (C2) + Interfaces.Shift_Left (From_Hex (C1), 4)));

   function To_Hex_Low (C : in Character) return Character is
     (Conversion (1 + (Character'Pos (C) mod 16)));

   function To_Hex_High (C : in Character) return Character is
     (Conversion (1 + (Character'Pos (C) / 16)));

   --  ------------------------------
   --  Base16 encoder
   --  ------------------------------
   --  This <b>Encoder</b> translates the (binary) input stream into
   --  an ascii hexadecimal stream.  The encoding alphabet is: 0123456789ABCDEF.
   type Encoder is new Util.Encoders.Transformer with private;

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
   overriding
   procedure Transform (E       : in out Encoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset);

   --  ------------------------------
   --  Base16 decoder
   --  ------------------------------
   --  The <b>Decoder</b> decodes an hexadecimal stream into a binary stream.
   type Decoder is new Util.Encoders.Transformer with private;

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
   overriding
   procedure Transform (E       : in out Decoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset);

private

   type Encoder is new Util.Encoders.Transformer with null record;

   type Decoder is new Util.Encoders.Transformer with null record;

   generic
      type Input_Char is (<>);
      type Output_Char is (<>);
      type Index is range <>;
      type Output_Index is range <>;
      type Input is array (Index range <>) of Input_Char;
      type Output is array (Output_Index range <>) of Output_Char;
   package Encoding is
      procedure Encode (From : in Input;
                        Into : in out Output;
                        Last : out Output_Index;
                        Encoded : out Index);
      procedure Decode (From    : in Input;
                        Into    : in out Output;
                        Last    : out Output_Index;
                        Encoded : out Index);
   end Encoding;

end Util.Encoders.Base16;
