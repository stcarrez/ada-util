-----------------------------------------------------------------------
--  util-encoders-chacha20-tests - ChaCha20 cipher tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.IO_Exceptions;
with Ada.Streams;
with Util.Streams.ChaCha20;
with Util.Streams.Texts;
with Util.Test_Caller;

package body Util.Encoders.ChaCha20.Tests is

   package Caller is new Util.Test_Caller (Test, "Encoders.ChaCha20");

   function To_Bytes (S : in String) return Ada.Streams.Stream_Element_Array;
   function To_String (B : in Ada.Streams.Stream_Element_Array) return String;
   function Nonce_Vector return Util.Encoders.ChaCha20.Nonce;

   Hex_Dec : constant Util.Encoders.Decoder := Util.Encoders.Create ("hex");
   Hex_Enc : constant Util.Encoders.Encoder := Util.Encoders.Create ("hex");

   --  RFC 8439 section 2.4.2 test vector.
   Key_Hex   : constant String :=
     "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F";
   Nonce_Hex : constant String := "000000000000004A00000000";
   --  Keystream block for counter = 1.
   KS_Hex    : constant String :=
     "224F51F3401BD9E12FDE276FB8631DED8C131F823D2C06E27E4FCAEC9EF3CF78" &
     "8A3B0AA372600A92B57974CDED2B9334794CBA40C63E34CDEA212C4CF07D41B7";
   Plain     : constant String :=
     "Ladies and Gentlemen of the class of '99: If I could offer you only " &
     "one tip for the future, sunscreen would be it.";
   CT_Hex    : constant String :=
     "6E2E359A2568F98041BA0728DD0D6981E97E7AEC1D4360C20A27AFCCFD9FAE0B" &
     "F91B65C5524733AB8F593DABCD62B3571639D624E65152AB8F530C359F0861D8" &
     "07CA0DBF500D6A6156A38E088A22B65E52BC514D16CCF806818CE91AB7793736" &
     "5AF90BBF74A35BE6B40B8EEDF2785E42874D";

   Secret_Key_Vector : constant Util.Encoders.Secret_Key
     := Util.Encoders.Decode_Key (Hex_Dec, Key_Hex);

   function To_Bytes (S : in String) return Ada.Streams.Stream_Element_Array is
      R : Ada.Streams.Stream_Element_Array (1 .. S'Length);
   begin
      for I in S'Range loop
         R (Ada.Streams.Stream_Element_Offset (I - S'First + 1)) :=
           Ada.Streams.Stream_Element (Character'Pos (S (I)));
      end loop;
      return R;
   end To_Bytes;

   function To_String (B : in Ada.Streams.Stream_Element_Array) return String is
      R : String (1 .. B'Length);
   begin
      for I in B'Range loop
         R (Integer (I - B'First) + 1) := Character'Val (B (I));
      end loop;
      return R;
   end To_String;

   function Nonce_Vector return Util.Encoders.ChaCha20.Nonce is
     (Util.Encoders.ChaCha20.Nonce (Hex_Dec.Decode_Binary (Nonce_Hex)));

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Encoders.ChaCha20.Block (RFC 8439 vector)",
                       Test_Block_Vector'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.ChaCha20.Encrypt (RFC 8439 vector)",
                       Test_Encrypt_Vector'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.ChaCha20.Encrypt (chunked)",
                       Test_Encrypt_Chunked'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.ChaCha20.Decrypt (RFC 8439 vector)",
                       Test_Decrypt_Vector'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.ChaCha20 (streams)",
                       Test_Streams'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the ChaCha20 block function with the RFC 8439 test vector.
   --  ------------------------------
   procedure Test_Block_Vector (T : in out Test) is
      Key : Util.Encoders.ChaCha20.Key;
      Buf : Ada.Streams.Stream_Element_Array (1 .. Util.Encoders.ChaCha20.Block_Size);
   begin
      Key.Secret := Hex_Dec.Decode_Binary (Key_Hex);
      Util.Encoders.ChaCha20.Block (Key, 1, Nonce_Vector, Buf);
      Util.Tests.Assert_Equals (T, KS_Hex, Hex_Enc.Encode_Binary (Buf),
                                "Invalid ChaCha20 keystream block");
   end Test_Block_Vector;

   --  ------------------------------
   --  Test ChaCha20 encryption with the RFC 8439 test vector.
   --  ------------------------------
   procedure Test_Encrypt_Vector (T : in out Test) is
      E         : Util.Encoders.ChaCha20.Encoder;
      Data      : constant Ada.Streams.Stream_Element_Array := To_Bytes (Plain);
      CT        : Ada.Streams.Stream_Element_Array (1 .. Data'Length);
      Last      : Ada.Streams.Stream_Element_Offset;
      Encoded   : Ada.Streams.Stream_Element_Offset;
   begin
      E.Set_Key (Secret_Key_Vector);
      E.Set_Nonce (Nonce_Vector);
      E.Set_Counter (1);
      E.Transform (Data, CT, Last, Encoded);
      Util.Tests.Assert_Equals (T, CT_Hex, Hex_Enc.Encode_Binary (CT),
                                "Invalid ChaCha20 ciphertext");
      T.Assert (Last = CT'Last and then Encoded = Data'Last,
                         "Invalid Last/Encoded positions");
   end Test_Encrypt_Vector;

   --  ------------------------------
   --  Test ChaCha20 encryption with several Transform calls on small buffers.
   --  ------------------------------
   procedure Test_Encrypt_Chunked (T : in out Test) is
      E      : Util.Encoders.ChaCha20.Encoder;
      Data   : constant Stream_Element_Array := To_Bytes (Plain);
      CT     : Ada.Streams.Stream_Element_Array (1 .. Data'Length) := (others => 0);
      Pos    : Ada.Streams.Stream_Element_Offset := Data'First;
      Chunk  : constant := 7;
   begin
      E.Set_Key (Secret_Key_Vector);
      E.Set_Nonce (Nonce_Vector);
      E.Set_Counter (1);
      while Pos <= Data'Last loop
         declare
            Remaining : constant Natural := Natural (Data'Last - Pos + 1);
            Take      : constant Natural := Natural'Min (Chunk, Remaining);
            Last      : Ada.Streams.Stream_Element_Offset;
            Encoded   : Ada.Streams.Stream_Element_Offset;
            Out_First : constant Ada.Streams.Stream_Element_Offset
              := CT'First + (Pos - Data'First);
         begin
            E.Transform (Data (Pos .. Data'Last),
                         CT (Out_First .. Out_First
                             + Ada.Streams.Stream_Element_Offset (Take) - 1),
                         Last, Encoded);
            T.Assert (Last = Out_First + Ada.Streams.Stream_Element_Offset (Take) - 1
                      and then Encoded = Pos + Ada.Streams.Stream_Element_Offset (Take) - 1,
                      "Invalid Last/Encoded positions");
            Pos := Encoded + 1;
         end;
      end loop;
      Util.Tests.Assert_Equals (T, CT_Hex, Hex_Enc.Encode_Binary (CT),
                                "Invalid chunked ChaCha20 ciphertext");
   end Test_Encrypt_Chunked;

   --  ------------------------------
   --  Test ChaCha20 decryption with the RFC 8439 test vector.
   --  ------------------------------
   procedure Test_Decrypt_Vector (T : in out Test) is
      D       : Util.Encoders.ChaCha20.Decoder;
      Data    : constant Ada.Streams.Stream_Element_Array := Hex_Dec.Decode_Binary (CT_Hex);
      Back    : Ada.Streams.Stream_Element_Array (1 .. Data'Length);
      Last    : Stream_Element_Offset;
      Encoded : Stream_Element_Offset;
   begin
      D.Set_Key (Secret_Key_Vector);
      D.Set_Nonce (Nonce_Vector);
      D.Set_Counter (1);
      D.Transform (Data, Back, Last, Encoded);
      Util.Tests.Assert_Equals (T, Plain, To_String (Back),
                                "Invalid ChaCha20 decrypted content");
      T.Assert (Last = Back'Last and then Encoded = Data'Last,
                         "Invalid Last/Encoded positions");
   end Test_Decrypt_Vector;

   --  ------------------------------
   --  Test ChaCha20 encryption/decryption through the Util.Streams.ChaCha20 streams.
   --  ------------------------------
   procedure Test_Streams (T : in out Test) is
      Reader   : aliased Util.Streams.Texts.Reader_Stream;
      Decipher : aliased Util.Streams.ChaCha20.Decoding_Stream;
      Cipher   : aliased Util.Streams.ChaCha20.Encoding_Stream;
      Print    : Util.Streams.Texts.Print_Stream;
      Key      : constant Util.Encoders.Secret_Key
        := Util.Encoders.Create ("0123456789abcdef0123456789abcdef");
      Item     : constant String := "Hello ChaCha20!" & ASCII.LF;
      Count    : constant := 10;
   begin
      --  Print -> Cipher -> Decipher
      Decipher.Initialize (64 * 1024);
      Decipher.Set_Key (Key);
      Decipher.Set_Nonce (Nonce_Vector);
      Cipher.Produces (Decipher'Unchecked_Access, 1024);
      Cipher.Set_Key (Key);
      Cipher.Set_Nonce (Nonce_Vector);
      Print.Initialize (Cipher'Unchecked_Access);
      for I in 1 .. Count loop
         Print.Write (Item);
      end loop;
      Print.Flush;
      Util.Tests.Assert_Equals (T,
                                Item'Length * Count,
                                Decipher.Get_Size,
                                "decipher buffer has the wrong size");

      --  Read the decrypted content from Decipher.
      Reader.Initialize (Decipher);
      for I in 1 .. Count loop
         declare
            L : String (Item'Range) := (others => ' ');
         begin
            for J in L'Range loop
               Reader.Read (L (J));
            end loop;
            Util.Tests.Assert_Equals (T, Item, L, "wrong decrypted value");

         exception
            when Ada.IO_Exceptions.Data_Error =>
               Util.Tests.Assert_Equals (T, Item, L, "wrong decrypted value (DATA error)");
         end;
      end loop;
   end Test_Streams;

end Util.Encoders.ChaCha20.Tests;
