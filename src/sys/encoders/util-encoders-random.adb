-----------------------------------------------------------------------
--  util-encoders-random -- Random bytes and random token generation
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Encoders.Base64;

package body Util.Encoders.Random is

   --  ------------------------------
   --  Fill the array with random non-zero bytes.
   --  ------------------------------
   procedure Generate_Nonzero (Gen  : in out Generator'Class;
                               Into : out Ada.Streams.Stream_Element_Array) is
   begin
      Generate (Gen, Into);
      for I in Into'Range loop
         while Into (I) = 0 loop
            declare
               B : Ada.Streams.Stream_Element_Array (1 .. 1);
            begin
               Generate (Gen, B);
               Into (I) := B (1);
            end;
         end loop;
      end loop;
   end Generate_Nonzero;

   --  ------------------------------
   --  Fill the string with random bytes.
   --  ------------------------------
   procedure Generate (Gen  : in out Generator'Class;
                       Into : out String) is
      Buf : Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (Into'Length));
      for Buf'Address use Into'Address;
   begin
      Generate (Gen, Buf);
   end Generate;

   --  ------------------------------
   --  Generate a random sequence of bits and return the result in base64url.
   --  ------------------------------
   function Generate (Gen  : in out Generator'Class;
                      Bits : in Positive) return String is
      Rand_Count : constant Natural := 4 * ((Bits + 31) / 32);
      Rand       : Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (Rand_Count));
      Buffer     : Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (Rand_Count * 4 + 4));
      Encoder    : Util.Encoders.Base64.Encoder;
      Last       : Ada.Streams.Stream_Element_Offset;
      Encoded    : Ada.Streams.Stream_Element_Offset;
   begin
      Generate (Gen, Rand);

      --  Encode the random stream in base64url and save it into the result string.
      Encoder.Set_URL_Mode (True);
      Encoder.Transform (Data    => Rand,
                         Into    => Buffer,
                         Last    => Last,
                         Encoded => Encoded);
      Encoder.Finish (Into => Buffer (Last + 1 .. Buffer'Last),
                      Last => Last);
      while Last >= Buffer'First and then Character'Val (Buffer (Last)) = '=' loop
         Last := Last - 1;
      end loop;
      declare
         Result : String (1 .. Natural (Last));
      begin
         for I in Buffer'First .. Last loop
            Result (Natural (I - Buffer'First + 1)) := Character'Val (Buffer (I));
         end loop;
         return Result;
      end;
   end Generate;

   --  ------------------------------
   --  Generate a random sequence of bits, convert the result into a base64url
   --  string and append it to the buffer.
   --  ------------------------------
   procedure Generate (Gen  : in out Generator'Class;
                       Bits : in Positive;
                       Into : in out Ada.Strings.Unbounded.Unbounded_String) is
      Token : constant String := Generate (Gen, Bits);
   begin
      Ada.Strings.Unbounded.Append (Into, Token);
   end Generate;

end Util.Encoders.Random;
