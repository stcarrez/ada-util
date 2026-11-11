-----------------------------------------------------------------------
--  util-encoders-poly1305-tests - Poly1305 authenticator tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Streams;
with Util.Test_Caller;

package body Util.Encoders.Poly1305.Tests is

   package Caller is new Util.Test_Caller (Test, "Encoders.Poly1305");

   Hex_Dec : constant Util.Encoders.Decoder := Util.Encoders.Create ("hex");
   Hex_Enc : constant Util.Encoders.Encoder := Util.Encoders.Create ("hex");

   --  RFC 8439 section 2.5.2 test vector.
   Key_Hex : constant String :=
     "85D6BE7857556D337F4452FE42D506A80103808AFB0DB2FD4ABFF6AF4149F51B";
   Msg     : constant String := "Cryptographic Forum Research Group";
   Tag_Hex : constant String := "A8061DC1305136C6C22B8BAF0C0127A9";

   function To_Bytes (S : in String) return Ada.Streams.Stream_Element_Array;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Encoders.Poly1305.Mac (RFC 8439 vector)",
                       Test_Mac_Vector'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Poly1305 (incremental)",
                       Test_Mac_Incremental'Access);
   end Add_Tests;

   function To_Bytes (S : in String) return Ada.Streams.Stream_Element_Array is
      R : Ada.Streams.Stream_Element_Array (1 .. S'Length);
   begin
      for I in S'Range loop
         R (Ada.Streams.Stream_Element_Offset (I - S'First + 1))
           := Ada.Streams.Stream_Element (Character'Pos (S (I)));
      end loop;
      return R;
   end To_Bytes;

   --  ------------------------------
   --  Test the Poly1305 one-shot Mac with the RFC 8439 test vector.
   --  ------------------------------
   procedure Test_Mac_Vector (T : in out Test) is
      Key : constant Ada.Streams.Stream_Element_Array := Hex_Dec.Decode_Binary (Key_Hex);
      Data : constant Ada.Streams.Stream_Element_Array := To_Bytes (Msg);
      Tag : Util.Encoders.Poly1305.Tag;
   begin
      Util.Encoders.Poly1305.Mac (Key, Data, Tag);
      Util.Tests.Assert_Equals (T, Tag_Hex, Hex_Enc.Encode_Binary (Tag),
                                "Invalid Poly1305 tag");
   end Test_Mac_Vector;

   --  ------------------------------
   --  Test the Poly1305 incremental Context (Set_Key/Update/Finish) with
   --  several Update calls of various sizes.
   --  ------------------------------
   procedure Test_Mac_Incremental (T : in out Test) is
      Key : constant Ada.Streams.Stream_Element_Array := Hex_Dec.Decode_Binary (Key_Hex);
      Data : constant Ada.Streams.Stream_Element_Array := To_Bytes (Msg);
      Ctx : Util.Encoders.Poly1305.Context;
      Tag : Util.Encoders.Poly1305.Tag;
      Pos : Ada.Streams.Stream_Element_Offset := Data'First;
   begin
      Util.Encoders.Poly1305.Set_Key (Ctx, Key);
      --  Feed the message in chunks of 7 bytes (less than the 16-byte block).
      while Pos <= Data'Last loop
         declare
            N : constant Natural
              := Natural'Min (7, Natural (Data'Last - Pos + 1));
         begin
            Util.Encoders.Poly1305.Update
              (Ctx, Data (Pos .. Pos + Ada.Streams.Stream_Element_Offset (N) - 1));
            Pos := Pos + Ada.Streams.Stream_Element_Offset (N);
         end;
      end loop;
      Util.Encoders.Poly1305.Finish (Ctx, Tag);
      Util.Tests.Assert_Equals (T, Tag_Hex, Hex_Enc.Encode_Binary (Tag),
                                "Invalid incremental Poly1305 tag");
   end Test_Mac_Incremental;

end Util.Encoders.Poly1305.Tests;
