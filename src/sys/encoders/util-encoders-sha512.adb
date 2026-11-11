-----------------------------------------------------------------------
--  util-encoders-sha512 -- Compute SHA-512 hash
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Encoders.Base16;
with Util.Encoders.Base64;

package body Util.Encoders.SHA512 is

   --  ------------------------------
   --  Computes the SHA512 hash and returns the raw binary hash in <b>Hash</b>.
   --  ------------------------------
   procedure Finish (E    : in out Context;
                     Hash : out Hash_Array) is
   begin
      Hash := GNAT.SHA512.Digest (E);
      E := GNAT.SHA512.Initial_Context;
   end Finish;

   --  ------------------------------
   --  Computes the SHA512 hash and returns the hexadecimal hash in <b>Hash</b>.
   --  ------------------------------
   procedure Finish (E    : in out Context;
                     Hash : out Digest) is
   begin
      Hash := GNAT.SHA512.Digest (E);
      E := GNAT.SHA512.Initial_Context;
   end Finish;

   --  ------------------------------
   --  Computes the SHA512 hash and returns the base64 hash in <b>Hash</b>.
   --  ------------------------------
   procedure Finish_Base64 (E    : in out Context;
                            Hash : out Base64_Digest) is
      H : Hash_Array;
      B : Util.Encoders.Base64.Encoder;
   begin
      Finish (E, H);
      B.Convert (H, Hash);
      E := GNAT.SHA512.Initial_Context;
   end Finish_Base64;

   --  ------------------------------
   --  Set the output format of the SHA-512 encoder.
   --  ------------------------------
   procedure Set_Format (E      : in out Encoder;
                         Format : in Format_Type) is
   begin
      E.Format := Format;
   end Set_Format;

   --  ------------------------------
   --  Accumulate the input data in the encoder context.  The hash is computed
   --  by the Finish procedure so that several Transform calls compute the hash
   --  of the whole input.
   --  ------------------------------
   overriding
   procedure Transform (E       : in out Encoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is
      pragma Unmodified (Into);
   begin
      Update (E.Ctx, Data);
      Last    := Into'First - 1;
      Encoded := Data'Last;
   end Transform;

   --  ------------------------------
   --  Compute the SHA-512 hash of the accumulated data and write the digest in
   --  <b>Into</b> using the output format set by <b>Set_Format</b>.
   --  ------------------------------
   overriding
   procedure Finish (E    : in out Encoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset) is
      Hash    : Hash_Array;
      Encoded : Ada.Streams.Stream_Element_Offset;
   begin
      Finish (E.Ctx, Hash);
      case E.Format is
         when Binary =>
            Into (Into'First .. Into'First + Hash'Length - 1) := Hash;
            Last    := Into'First + Hash'Length - 1;
            Encoded := Last;

         when Base16 =>
            declare
               Hex_Encoder : Util.Encoders.Base16.Encoder;
            begin
               Hex_Encoder.Transform (Data    => Hash,
                                      Into    => Into,
                                      Last    => Last,
                                      Encoded => Encoded);
            end;

         when Base64 | Base64URL =>
            declare
               B64_Encoder : Util.Encoders.Base64.Encoder;
            begin
               B64_Encoder.Set_URL_Mode (E.Format = Base64URL);
               B64_Encoder.Transform (Data    => Hash,
                                      Into    => Into,
                                      Last    => Last,
                                      Encoded => Encoded);
               --  Flush the trailing partial group (with the '=' padding).
               B64_Encoder.Finish (Into => Into (Last + 1 .. Into'Last),
                                   Last => Last);
            end;

      end case;
   end Finish;

end Util.Encoders.SHA512;
