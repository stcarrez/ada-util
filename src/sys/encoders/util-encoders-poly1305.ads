-----------------------------------------------------------------------
--  util-encoders-poly1305 -- Poly1305 one-time authenticator (RFC 8439)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;
with Interfaces;
private with Ada.Finalization;

--  == Poly1305 one-time authenticator ==
--  The `Util.Encoders.Poly1305` package implements the Poly1305 one-time
--  authenticator defined by RFC 8439.  Poly1305 computes a 128-bit tag of a
--  message with a 256-bit one-time key `r || s`: the first 16
--  bytes are clamped to form `r` and the last 16 bytes are `s`.
--
--  The key is a `one-time` key: it must never be reused for another
--  message, otherwise the authenticity is lost.
--
--  The `Context` type allows to compute the tag incrementally:
--
--    Ctx : Util.Encoders.Poly1305.Context;
--    ...
--    Util.Encoders.Poly1305.Set_Key (Ctx, Key32);
--    Util.Encoders.Poly1305.Update (Ctx, Part1);
--    Util.Encoders.Poly1305.Update (Ctx, Part2);
--    Util.Encoders.Poly1305.Finish (Ctx, Tag);
--
--  The `Mac` procedure computes the tag of a complete message in one call.
package Util.Encoders.Poly1305 with Preelaborate is

   --  Number of bytes of the Poly1305 one-time key (r || s).
   Key_Size : constant := 32;

   --  Number of bytes of the Poly1305 tag.
   Tag_Size : constant := 16;

   --  The 128-bit Poly1305 tag.
   subtype Tag is Ada.Streams.Stream_Element_Array (1 .. Tag_Size);

   --  ------------------------------
   --  Poly1305 Context
   --  ------------------------------
   type Context is limited private;

   --  Set the 32-byte one-time key (r || s) of the context.  The key is
   --  clamped as specified by RFC 8439 and must not be reused for another
   --  message.
   procedure Set_Key (Ctx   : in out Context;
                      Key32 : in Ada.Streams.Stream_Element_Array) with
      Pre => Key32'Length = Key_Size;

   --  Update the authenticator with the data.
   procedure Update (Ctx  : in out Context;
                     Data : in Ada.Streams.Stream_Element_Array);

   --  Finish the computation and return the 128-bit tag in <b>Tag</b>.
   procedure Finish (Ctx : in out Context;
                     Tag : out Ada.Streams.Stream_Element_Array) with
      Pre => Tag'Length = Tag_Size;

   --  ------------------------------
   --  Compute the Poly1305 tag of a message with a 32-byte one-time key
   --  (r || s) in a single call.
   --  ------------------------------
   procedure Mac (Key32 : in Ada.Streams.Stream_Element_Array;
                  Msg   : in Ada.Streams.Stream_Element_Array;
                  Tag   : out Ada.Streams.Stream_Element_Array) with
      Pre => Key32'Length = Key_Size and then Tag'Length = Tag_Size;

private

   --  Arithmetic modulo 2^130 - 5 with 5 limbs of 26 bits (poly1305-donna).
   type Poly5 is array (0 .. 4) of Interfaces.Unsigned_32;

   subtype Block_Type is Ada.Streams.Stream_Element_Array (0 .. 15);

   type Context is limited new Ada.Finalization.Limited_Controlled with record
      H                          : Poly5 := [others => 0];
      R0, R1, R2, R3, R4         : Interfaces.Unsigned_32 := 0;
      S1, S2, S3, S4             : Interfaces.Unsigned_32 := 0;
      SW0, SW1, SW2, SW3         : Interfaces.Unsigned_32 := 0;
      Buf                        : Block_Type := [others => 0];
      Buf_Len                    : Natural := 0;
   end record;

   overriding
   procedure Finalize (Object : in out Context);

   --  Arithmetic modulo 2^130 - 5 with 5 limbs of 26 bits (poly1305-donna).
   procedure Poly1305_Mul (H : in out Poly5;
                           R0, R1, R2, R3, R4 : in Interfaces.Unsigned_32;
                           S1, S2, S3, S4     : in Interfaces.Unsigned_32);

   procedure Process_Block (Ctx  : in out Context;
                            Buf  : in Block_Type;
                            Take : in Natural);

end Util.Encoders.Poly1305;
