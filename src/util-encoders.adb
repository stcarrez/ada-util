-----------------------------------------------------------------------
--  util-encoders -- Encode/Decode streams and strings from one format to another
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with Ada.Unchecked_Deallocation;
with Util.Encoders.Base16;
with Util.Encoders.Base64;
with Util.Encoders.SHA1;
package body Util.Encoders is

   use Ada;
   use Ada.Strings.Unbounded;

   use type Ada.Streams.Stream_Element_Offset;

   --  ------------------------------
   --  Encodes the input string <b>Data</b> using the transformation
   --  rules provided by the <b>E</b> encoder.
   --
   --  Returns the encoded string.
   --
   --  Raises the <b>Encoding_Error</b> exception if the source string
   --  cannot be encoded.
   --  Raises the <b>Not_Supported</b> exception if the encoding is not
   --  supported.
   --  ------------------------------
   function Encode (E    : in Encoder;
                    Data : in String) return String is
   begin
      if E.Encode = null then
         raise Not_Supported with "There is no encoder";
      end if;
      return E.Encode.Transform (Data);
   end Encode;

   --  ------------------------------
   --  Decodes the input string <b>Data</b> using the transformation
   --  rules provided by the <b>E</b> encoder.
   --
   --  Returns the encoded string.
   --
   --  Raises the <b>Encoding_Error</b> exception if the source string
   --  cannot be decoded.
   --  Raises the <b>Not_Supported</b> exception if the decoding is not
   --  supported.
   --  ------------------------------
   function Decode (E    : in Encoder;
                    Data : in String) return String is
   begin
      if E.Decode = null then
         raise Not_Supported with "There is no decoder";
      end if;
      return E.Decode.Transform (Data);
   end Decode;

   MIN_BUFFER_SIZE : constant Streams.Stream_Element_Offset := 64;
   MAX_BUFFER_SIZE : constant Streams.Stream_Element_Offset := 2_048;

   function Best_Size (Length : Natural) return Streams.Stream_Element_Offset;
   pragma Inline (Best_Size);

   --  ------------------------------
   --  Compute a good size for allocating a buffer on the stack
   --  ------------------------------
   function Best_Size (Length : Natural) return Streams.Stream_Element_Offset is
   begin
      if Length < Natural (MIN_BUFFER_SIZE) then
         return MIN_BUFFER_SIZE;
      elsif Length > Natural (MAX_BUFFER_SIZE) then
         return MAX_BUFFER_SIZE;
      else
         return Streams.Stream_Element_Offset (((Length + 15) / 16) * 16);
      end if;
   end Best_Size;

   --  ------------------------------
   --  Transform the input string <b>Data</b> using the transformation
   --  rules provided by the <b>E</b> transformer.
   --
   --  Returns the transformed string.
   --
   --  Raises the <b>Encoding_Error</b> exception if the source string
   --  cannot be transformed
   --  ------------------------------
   function Transform (E    : in Transformer'Class;
                       Data : in String) return String is
      Buf_Size : constant Streams.Stream_Element_Offset := Best_Size (Data'Length);
      Buf      : Streams.Stream_Element_Array (1 .. Buf_Size);
      Res      : Streams.Stream_Element_Array (1 .. Buf_Size);
      Tmp      : String (1 .. Natural (Buf_Size));
      Result   : Ada.Strings.Unbounded.Unbounded_String;
      Pos      : Natural := Data'First;
   begin
      while Pos <= Data'Last loop
         declare
            Last_Encoded  : Streams.Stream_Element_Offset;
            First_Encoded : Streams.Stream_Element_Offset := 1;
            Last          : Streams.Stream_Element_Offset;
            Size          : Streams.Stream_Element_Offset;
            Next_Pos      : Natural;
         begin
            --  Fill the stream buffer with our input string
            Size := Streams.Stream_Element_Offset (Data'Last - Pos + 1);
            if Size > Buf'Length then
               Size := Buf'Length;
            end if;
            for I in 1 .. Size loop
               Buf (I) := Character'Pos (Data (Natural (I) + Pos - 1));
            end loop;
            Next_Pos := Pos + Natural (Size);

            --  Encode that buffer and put the result in out result string.
            loop
               E.Transform (Data    => Buf (First_Encoded .. Size),
                            Into    => Res,
                            Encoded => Last_Encoded,
                            Last    => Last);

               --  If the encoder generated nothing, move the position backward
               --  to take into account the remaining bytes not taken into account.
               if Last < 1 then
                  Next_Pos := Next_Pos - Natural (Size - First_Encoded + 1);
                  exit;
               end if;
               for I in 1 .. Last loop
                  Tmp (Natural (I)) := Character'Val (Res (I));
               end loop;
               Append (Result, Tmp (1 .. Natural (Last)));
               exit when Last_Encoded = Size;
               First_Encoded := Last_Encoded + 1;
            end loop;

            --  The encoder cannot encode the data
            if Pos = Next_Pos then
               raise Encoding_Error with "Encoding cannot proceed";
            end if;
            Pos := Next_Pos;
         end;
      end loop;
      return To_String (Result);
   end Transform;

   --  ------------------------------
   --  Transform the input string <b>Data</b> using the transformation
   --  rules provided by the <b>E</b> transformer.
   --
   --  Returns the transformed string.
   --
   --  Raises the <b>Encoding_Error</b> exception if the source string
   --  cannot be transformed
   --  ------------------------------
   function Transform (E    : in Transformer'Class;
                       Data : in Streams.Stream_Element_Array) return String is
      Buf_Size : constant Streams.Stream_Element_Offset := Best_Size (Data'Length);
      Res      : Streams.Stream_Element_Array (1 .. Buf_Size);
      Tmp      : String (1 .. Natural (Buf_Size));
      Result   : Ada.Strings.Unbounded.Unbounded_String;
      Last_Encoded  : Streams.Stream_Element_Offset;
      Last          : Streams.Stream_Element_Offset;
   begin

      --  Encode that buffer and put the result in out result string.
      E.Transform (Data    => Data,
                   Into    => Res,
                   Encoded => Last_Encoded,
                   Last    => Last);

      for I in 1 .. Last loop
         Tmp (Natural (I)) := Character'Val (Res (I));
      end loop;
      Append (Result, Tmp (1 .. Natural (Last)));
      return To_String (Result);
   end Transform;

   --  ------------------------------
   --  Encode the value represented by <tt>Val</tt> in the stream array <tt>Into</tt> starting
   --  at position <tt>Pos</tt> in that array.  The value is encoded using LEB128 format, 7-bits
   --  at a time until all non zero bits are written.  The <tt>Last</tt> parameter is updated
   --  to indicate the position of the last valid byte written in <tt>Into</tt>.
   --  ------------------------------
   procedure Encode_LEB128 (Into  : in out Ada.Streams.Stream_Element_Array;
                            Pos   : in Ada.Streams.Stream_Element_Offset;
                            Val   : in Interfaces.Unsigned_64;
                            Last  : out Ada.Streams.Stream_Element_Offset) is
      use type Interfaces.Unsigned_64;

      P    : Ada.Streams.Stream_Element_Offset := Pos;
      V, U : Interfaces.Unsigned_64;
   begin
      V := Val;
      loop
         if V < 16#07F# then
            Into (P) := Ada.Streams.Stream_Element (V);
            Last := P;
            return;
         end if;
         U := V and 16#07F#;
         Into (P) := Ada.Streams.Stream_Element (U or 16#80#);
         P := P + 1;
         V := Interfaces.Shift_Right (V, 7);
      end loop;
   end Encode_LEB128;

   --  ------------------------------
   --  Decode from the byte array <tt>From</tt> the value represented as LEB128 format starting
   --  at position <tt>Pos</tt> in that array.  After decoding, the <tt>Last</tt> index is updated
   --  to indicate the last position in the byte array.
   --  ------------------------------
   procedure Decode_LEB128 (From  : in Ada.Streams.Stream_Element_Array;
                            Pos   : in Ada.Streams.Stream_Element_Offset;
                            Val   : out Interfaces.Unsigned_64;
                            Last  : out Ada.Streams.Stream_Element_Offset) is
      use type Interfaces.Unsigned_64;
      use type Interfaces.Unsigned_8;

      P     : Ada.Streams.Stream_Element_Offset := Pos;
      Value : Interfaces.Unsigned_64 := 0;
      V     : Interfaces.Unsigned_8;
      Shift : Integer := 0;
   begin
      loop
         V := Interfaces.Unsigned_8 (From (P));
         if (V and 16#80#) = 0 then
            Val := Interfaces.Shift_Left (Interfaces.Unsigned_64 (V), Shift) or Value;
            Last := P + 1;
            return;
         end if;
         V := V and 16#07F#;
         Value := Interfaces.Shift_Left (Interfaces.Unsigned_64 (V), Shift) or Value;
         P := P + 1;
         Shift := Shift + 7;
      end loop;
   end Decode_LEB128;

   --  ------------------------------
   --  Create the encoder object for the specified algorithm.
   --  ------------------------------
   function Create (Name : String) return Encoder is
   begin
      if Name = BASE_16 or Name = HEX then
         return E : Encoder do
            E.Encode := new Util.Encoders.Base16.Encoder;
            E.Decode := new Util.Encoders.Base16.Decoder;
         end return;

      elsif Name = BASE_64 then
         return E : Encoder do
            E.Encode := new Util.Encoders.Base64.Encoder;
            E.Decode := new Util.Encoders.Base64.Decoder;
         end return;

      elsif Name = BASE_64_URL then
         return E : Encoder do
            E.Encode := Util.Encoders.Base64.Create_URL_Encoder;
            E.Decode := Util.Encoders.Base64.Create_URL_Decoder;
         end return;

      elsif Name = HASH_SHA1 then
         return E : Encoder do
            E.Encode := new Util.Encoders.SHA1.Encoder;
            E.Decode := new Util.Encoders.Base64.Decoder;
         end return;
      end if;
      raise Not_Supported with "Invalid encoder: " & Name;
   end Create;

   --  ------------------------------
   --  Delete the transformers
   --  ------------------------------
   overriding
   procedure Finalize (E : in out Encoder) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Transformer'Class, Transformer_Access);

   begin
      Free (E.Encode);
      Free (E.Decode);
   end Finalize;

end Util.Encoders;
