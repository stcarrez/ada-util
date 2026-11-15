-----------------------------------------------------------------------
--  util-streams-aead-tests - AEAD (ChaCha20-Poly1305) stream tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.IO_Exceptions;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Util.Streams.Buffered;
with Util.Streams.Files;
with Util.Streams.Texts;
with Util.Test_Caller;

package body Util.Streams.AEAD.Tests is

   package Caller is new Util.Test_Caller (Test, "Streams.AEAD");

   use Ada.Streams;

   function To_Bytes (S : in String) return Stream_Element_Array;
   function To_String (B : in Stream_Element_Array) return String;
   function Nonce_Vector return Util.Encoders.AEAD.Nonce;

   Hex_Dec : constant Util.Encoders.Decoder := Util.Encoders.Create ("hex");
   Hex_Enc : constant Util.Encoders.Encoder := Util.Encoders.Create ("hex");

   --  RFC 8439 section 2.8.2 test vector.
   Key_Hex   : constant String :=
     "808182838485868788898A8B8C8D8E8F909192939495969798999A9B9C9D9E9F";
   Nonce_Hex : constant String := "070000004041424344454647";
   AAD_Hex   : constant String := "50515253C0C1C2C3C4C5C6C7";
   Plain     : constant String :=
     "Ladies and Gentlemen of the class of '99: If I could offer you only " &
     "one tip for the future, sunscreen would be it.";
   CT_Hex    : constant String :=
     "D31A8D34648E60DB7B86AFBC53EF7EC2A4ADED51296E08FEA9E2B5A736EE62D6" &
     "3DBEA45E8CA9671282FAFB69DA92728B1A71DE0A9E060B2905D6A5B67ECD3B36" &
     "92DDBD7F2D778B8C9803AEE328091B58FAB324E4FAD675945585808B4831D7BC" &
     "3FF4DEF08E4B7A9DE576D26586CEC64B6116";
   Tag_Hex   : constant String := "1AE10B594F09E26A7E902ECBD0600691";

   Secret_Key_Vector : constant Util.Encoders.Secret_Key
     := Util.Encoders.Decode_Key (Hex_Dec, Key_Hex);

   function To_Bytes (S : in String) return Stream_Element_Array is
      R : Stream_Element_Array (1 .. S'Length);
   begin
      for I in S'Range loop
         R (Stream_Element_Offset (I - S'First + 1)) := Stream_Element (Character'Pos (S (I)));
      end loop;
      return R;
   end To_Bytes;

   function To_String (B : in Stream_Element_Array) return String is
      R : String (1 .. B'Length);
   begin
      for I in B'Range loop
         R (Integer (I - B'First) + 1) := Character'Val (B (I));
      end loop;
      return R;
   end To_String;

   function Nonce_Vector return Util.Encoders.AEAD.Nonce is
     (Util.Encoders.AEAD.Nonce (Hex_Dec.Decode_Binary (Nonce_Hex)));

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Streams.AEAD (streams)",
                       Test_Streams'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.AEAD.Encrypt (RFC 8439 vector)",
                       Test_Encrypt_Vector'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.AEAD.Decrypt (RFC 8439 vector)",
                       Test_Decrypt_Vector'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.AEAD (file)",
                       Test_File'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.AEAD.Decrypt (tampered tag)",
                       Test_Tampered_Tag'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.AEAD.Decrypt (wrong AAD)",
                       Test_Wrong_AAD'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test AEAD encryption/decryption through the Util.Streams.AEAD streams.
   --  ------------------------------
   procedure Test_Streams (T : in out Test) is
      Reader   : aliased Util.Streams.Texts.Reader_Stream;
      Decipher : aliased Util.Streams.AEAD.Decoding_Stream;
      Cipher   : aliased Util.Streams.AEAD.Encoding_Stream;
      Print    : Util.Streams.Texts.Print_Stream;
      Key      : constant Util.Encoders.Secret_Key
        := Util.Encoders.Create ("0123456789abcdef0123456789abcdef");
      AAD      : constant Stream_Element_Array := To_Bytes ("authenticated header");
      Item     : constant String := "Hello AEAD!" & ASCII.LF;
      Count    : constant := 10;
   begin
      --  Print -> Cipher -> Decipher
      Decipher.Initialize (64 * 1024);
      Decipher.Set_Key (Key);
      Decipher.Set_Nonce (Nonce_Vector);
      Decipher.Update_AAD (AAD);
      Cipher.Produces (Decipher'Unchecked_Access, 1024);
      Cipher.Set_Key (Key);
      Cipher.Set_Nonce (Nonce_Vector);
      Cipher.Update_AAD (AAD);
      Print.Initialize (Cipher'Unchecked_Access);
      for I in 1 .. Count loop
         Print.Write (Item);
      end loop;
      Print.Flush;
      --  The decoder holds back the 16-byte tag: only the plaintext is buffered.
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

   --  ------------------------------
   --  Test AEAD encryption through the stream with the RFC 8439 section 2.8.2 vector.
   --  ------------------------------
   procedure Test_Encrypt_Vector (T : in out Test) is
      Sink   : aliased Util.Streams.Buffered.Output_Buffer_Stream;
      Cipher : aliased Util.Streams.AEAD.Encoding_Stream;
      Print  : Util.Streams.Texts.Print_Stream;
      AAD    : constant Stream_Element_Array := Hex_Dec.Decode_Binary (AAD_Hex);
   begin
      --  Print -> Cipher -> Sink (use a small cipher buffer to force chunking).
      Sink.Initialize (Size => 4096);
      Cipher.Produces (Sink'Unchecked_Access, 64);
      Cipher.Set_Key (Secret_Key_Vector);
      Cipher.Set_Nonce (Nonce_Vector);
      Cipher.Update_AAD (AAD);
      Print.Initialize (Cipher'Unchecked_Access);
      Print.Write (Plain (Plain'First .. Plain'First + 59));
      Print.Write (Plain (Plain'First + 60 .. Plain'Last));
      Print.Flush;
      declare
         Size : constant Natural := Sink.Get_Size;
         Buf  : constant Stream_Element_Array
           := Sink.Get_Buffer.all (1 .. Stream_Element_Offset (Size));
      begin
         Util.Tests.Assert_Equals (T,
                                   Plain'Length + Util.Encoders.AEAD.Tag_Size,
                                   Size,
                                   "Invalid encoded stream size");
         Util.Tests.Assert_Equals (T, CT_Hex & Tag_Hex, Hex_Enc.Encode_Binary (Buf),
                                   "Invalid AEAD ciphertext or tag");
      end;
   end Test_Encrypt_Vector;

   --  ------------------------------
   --  Test AEAD decryption through the stream with the RFC 8439 section 2.8.2 vector.
   --  ------------------------------
   procedure Test_Decrypt_Vector (T : in out Test) is
      Data     : constant Stream_Element_Array := Hex_Dec.Decode_Binary (CT_Hex & Tag_Hex);
      Source   : aliased Util.Streams.Buffered.Input_Buffer_Stream;
      Decipher : aliased Util.Streams.AEAD.Decoding_Stream;
      AAD      : constant Stream_Element_Array := Hex_Dec.Decode_Binary (AAD_Hex);
      Decoded      : Stream_Element_Array (1 .. Data'Length) := (others => 0);
      Pos      : Stream_Element_Offset := Decoded'First;
      Last     : Stream_Element_Offset;
   begin
      Source.Initialize (Content => To_String (Data));
      Decipher.Consumes (Source'Unchecked_Access, 32);
      Decipher.Set_Key (Secret_Key_Vector);
      Decipher.Set_Nonce (Nonce_Vector);
      Decipher.Update_AAD (AAD);
      Read_All : loop
         Decipher.Read (Decoded (Pos .. Decoded'Last), Last);
         exit Read_All when Last < Pos;
         Pos := Last + 1;
      end loop Read_All;
      Util.Tests.Assert_Equals (T, Plain'Length, Natural (Pos - Decoded'First),
                                "Invalid decoded stream size");
      Util.Tests.Assert_Equals (T, Plain, To_String (Decoded (Decoded'First .. Pos - 1)),
                                "Invalid AEAD decrypted content");
   end Test_Decrypt_Vector;

   --  ------------------------------
   --  Test AEAD encryption to a file and decryption from the file.
   --  ------------------------------
   procedure Test_File (T : in out Test) is
      use Ada.Strings.Unbounded;
      use Ada.Streams.Stream_IO;

      Path     : constant String := Util.Tests.Get_Test_Path ("stream-aead.aead");
      File     : aliased Util.Streams.Files.File_Stream;
      Decipher : aliased Util.Streams.AEAD.Decoding_Stream;
      Cipher   : aliased Util.Streams.AEAD.Encoding_Stream;
      Print    : Util.Streams.Texts.Print_Stream;
      Reader   : Util.Streams.Texts.Reader_Stream;
      Key      : constant Util.Encoders.Secret_Key
        := Util.Encoders.Create ("0123456789abcdef0123456789abcdef");
      AAD      : constant Stream_Element_Array := To_Bytes ("file header");
      Item     : constant String := "Hello AEAD file!" & ASCII.LF;
      Count    : constant := 20;
   begin
      --  Print -> Cipher -> File
      File.Create (Mode => Out_File, Name => Path);
      Cipher.Produces (File'Unchecked_Access, 32);
      Cipher.Set_Key (Key);
      Cipher.Set_Nonce (Nonce_Vector);
      Cipher.Update_AAD (AAD);
      Print.Initialize (Cipher'Unchecked_Access);
      for I in 1 .. Count loop
         Print.Write (Item);
      end loop;
      Print.Close;

      --  File -> Decipher -> Reader
      File.Open (Mode => In_File, Name => Path);
      Decipher.Consumes (File'Unchecked_Access, 10240);
      Decipher.Set_Key (Key);
      Decipher.Set_Nonce (Nonce_Vector);
      Decipher.Update_AAD (AAD);
      Reader.Initialize (From => Decipher'Unchecked_Access);
      declare
         Line_Count : Natural := 0;
      begin
         while not Reader.Is_Eof loop
            declare
               Line : Unbounded_String;
            begin
               Reader.Read_Line (Line);
               exit when Length (Line) = 0;
               Util.Tests.Assert_Equals (T, Item, To_String (Line),
                                         "wrong decrypted line");
               Line_Count := Line_Count + 1;
            end;
         end loop;
         File.Close;
         Util.Tests.Assert_Equals (T, Count, Line_Count, "wrong line count");
      end;
   end Test_File;

   --  ------------------------------
   --  Test that a corrupted authentication tag is detected when reading the stream.
   --  ------------------------------
   procedure Test_Tampered_Tag (T : in out Test) is
      Data     : Stream_Element_Array := Hex_Dec.Decode_Binary (CT_Hex & Tag_Hex);
      Source   : aliased Util.Streams.Buffered.Input_Buffer_Stream;
      Decipher : aliased Util.Streams.AEAD.Decoding_Stream;
      AAD      : constant Stream_Element_Array := Hex_Dec.Decode_Binary (AAD_Hex);
      Decoded      : Stream_Element_Array (1 .. Data'Length) := (others => 0);
      Pos      : Stream_Element_Offset := Decoded'First;
      Last     : Stream_Element_Offset;
   begin
      Data (Data'Last) := Data (Data'Last) xor 1;
      Source.Initialize (Content => To_String (Data));
      Decipher.Consumes (Source'Unchecked_Access, 1024);
      Decipher.Set_Key (Secret_Key_Vector);
      Decipher.Set_Nonce (Nonce_Vector);
      Decipher.Update_AAD (AAD);
      begin
         Read_All : loop
            Decipher.Read (Decoded (Pos .. Decoded'Last), Last);
            exit Read_All when Last < Pos;
            Pos := Last + 1;
         end loop Read_All;
         T.Fail ("Encoding_Error not raised for a tampered authentication tag");
      exception
         when Util.Encoders.Encoding_Error =>
            null;
      end;
   end Test_Tampered_Tag;

   --  ------------------------------
   --  Test that a wrong AAD is detected when reading the stream.
   --  ------------------------------
   procedure Test_Wrong_AAD (T : in out Test) is
      Data     : constant Stream_Element_Array := Hex_Dec.Decode_Binary (CT_Hex & Tag_Hex);
      Source   : aliased Util.Streams.Buffered.Input_Buffer_Stream;
      Decipher : aliased Util.Streams.AEAD.Decoding_Stream;
      AAD      : constant Stream_Element_Array := To_Bytes ("another header");
      Decoded      : Stream_Element_Array (1 .. Data'Length) := (others => 0);
      Pos      : Stream_Element_Offset := Decoded'First;
      Last     : Stream_Element_Offset;
   begin
      Source.Initialize (Content => To_String (Data));
      Decipher.Consumes (Source'Unchecked_Access, 1024);
      Decipher.Set_Key (Secret_Key_Vector);
      Decipher.Set_Nonce (Nonce_Vector);
      Decipher.Update_AAD (AAD);
      begin
         Read_All : loop
            Decipher.Read (Decoded (Pos .. Decoded'Last), Last);
            exit Read_All when Last < Pos;
            Pos := Last + 1;
         end loop Read_All;
         T.Fail ("Encoding_Error not raised for a wrong AAD");
      exception
         when Util.Encoders.Encoding_Error =>
            null;
      end;
   end Test_Wrong_AAD;

end Util.Streams.AEAD.Tests;
