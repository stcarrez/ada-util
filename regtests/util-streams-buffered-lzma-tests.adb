-----------------------------------------------------------------------
--  util-streams-buffered-lzma-tests -- Unit tests for encoding buffered streams
--  Copyright (C) 2018, 2019, 2021 Stephane Carrez
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
with Util.Test_Caller;
with Util.Streams.Files;
with Util.Streams.Texts;
with Util.Streams.AES;
with Util.Encoders.AES;
with Ada.Streams.Stream_IO;
package body Util.Streams.Buffered.Lzma.Tests is

   use Util.Streams.Files;
   use Ada.Streams.Stream_IO;

   procedure Test_Stream_File (T       : in out Test;
                               Item    : in String;
                               Count   : in Positive;
                               Encrypt : in Boolean;
                               Mode    : in Util.Encoders.AES.AES_Mode;
                               Label   : in String);

   package Caller is new Util.Test_Caller (Test, "Streams.Buffered.Lzma");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Lzma.Write",
                       Test_Compress_Stream'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Lzma.Write (2)",
                       Test_Compress_File_Stream'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Lzma.Read+Write",
                       Test_Compress_Decompress_Stream'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Lzma.Read+Write+AES-CBC",
                       Test_Compress_Encrypt_Decompress_Decrypt_Stream'Access);
   end Add_Tests;

   procedure Test_Compress_Stream (T : in out Test) is
      Stream  : aliased File_Stream;
      Buffer  : aliased Util.Streams.Buffered.Lzma.Compress_Stream;
      Print   : Util.Streams.Texts.Print_Stream;
      Path    : constant String := Util.Tests.Get_Test_Path ("test-stream.lzma");
      Expect  : constant String := Util.Tests.Get_Path ("regtests/expect/test-stream.lzma");
   begin
      Stream.Create (Mode => Out_File, Name => Path);
      Buffer.Initialize (Output => Stream'Unchecked_Access,
                         Size   => 1024);
      Print.Initialize (Output => Buffer'Unchecked_Access, Size => 5);
      for I in 1 .. 32 loop
         Print.Write ("abcd");
         Print.Write (" fghij");
         Print.Write (ASCII.LF);
      end loop;
      Print.Flush;
      Stream.Close;

      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Expect,
                                     Test    => Path,
                                     Message => "LZMA stream");
   end Test_Compress_Stream;

   procedure Test_Compress_File_Stream (T : in out Test) is
      Stream    : aliased File_Stream;
      In_Stream : aliased File_Stream;
      Buffer    : aliased Util.Streams.Buffered.Lzma.Compress_Stream;
      Path      : constant String
        := Util.Tests.Get_Test_Path ("test-big-stream.lzma");
      Expect    : constant String
        := Util.Tests.Get_Path ("regtests/expect/test-big-stream.lzma");
   begin
      In_Stream.Open (Ada.Streams.Stream_IO.In_File,
                      Util.Tests.Get_Path ("regtests/files/test-big-stream.bin"));
      Stream.Create (Mode => Out_File, Name => Path);
      Buffer.Initialize (Output => Stream'Unchecked_Access,
                         Size   => 32768);
      Util.Streams.Copy (From => In_Stream, Into => Buffer);
      Buffer.Flush;
      Buffer.Close;
      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Expect,
                                     Test    => Path,
                                     Message => "LZMA stream");
   end Test_Compress_File_Stream;

   procedure Test_Stream_File (T       : in out Test;
                               Item    : in String;
                               Count   : in Positive;
                               Encrypt : in Boolean;
                               Mode    : in Util.Encoders.AES.AES_Mode;
                               Label   : in String) is
      use Ada.Strings.Unbounded;

      Path       : constant String
        := Util.Tests.Get_Test_Path ("stream-lzma-aes-" & Label & ".aes");
      Key        : constant Util.Encoders.Secret_Key
        := Util.Encoders.Create ("0123456789abcdef0123456789abcdef");
      File       : aliased File_Stream;
      Decipher   : aliased Util.Streams.AES.Decoding_Stream;
      Cipher     : aliased Util.Streams.AES.Encoding_Stream;
      Compress   : aliased Util.Streams.Buffered.Lzma.Compress_Stream;
      Decompress : aliased Util.Streams.Buffered.Lzma.Decompress_Stream;
      Print      : Util.Streams.Texts.Print_Stream;
      Reader     : Util.Streams.Texts.Reader_Stream;
   begin
      --  Print -> Compress -> Cipher -> File
      File.Create (Mode => Out_File, Name => Path);
      if Encrypt then
         Cipher.Produces (File'Unchecked_Access, 64);
         Cipher.Set_Key (Key, Mode);
         Compress.Initialize (Cipher'Unchecked_Access, 1024);
      else
         Compress.Initialize (File'Unchecked_Access, 1024);
      end if;
      Print.Initialize (Compress'Unchecked_Access);
      for I in 1 .. Count loop
         Print.Write (Item & ASCII.LF);
      end loop;
      Print.Close;

      --  File -> Decipher -> Decompress -> Reader
      File.Open (Mode => In_File, Name => Path);
      if Encrypt then
         Decipher.Consumes (File'Unchecked_Access, 128);
         Decipher.Set_Key (Key, Mode);
         Decompress.Initialize (Decipher'Unchecked_Access, 1024);
      else
         Decompress.Initialize (File'Unchecked_Access, 1024);
      end if;
      Reader.Initialize (From => Decompress'Unchecked_Access);
      declare
         Line_Count : Natural := 0;
      begin
         while not Reader.Is_Eof loop
            declare
               Line : Unbounded_String;
            begin
               Reader.Read_Line (Line);
               exit when Length (Line) = 0;
               if Item & ASCII.LF /= Line then
                  Util.Tests.Assert_Equals (T, Item & ASCII.LF, To_String (Line));
               end if;
               Line_Count := Line_Count + 1;
            end;
         end loop;
         File.Close;
         Util.Tests.Assert_Equals (T, Count, Line_Count);
      end;
   end Test_Stream_File;

   procedure Test_Compress_Decompress_Stream (T : in out Test) is
   begin
      Test_Stream_File (T, "abcdefgh", 1000, False, Util.Encoders.AES.CBC, "NONE");
   end Test_Compress_Decompress_Stream;

   procedure Test_Compress_Encrypt_Decompress_Decrypt_Stream (T : in out Test) is
   begin
      Test_Stream_File (T, "abcdefgh", 1000, True, Util.Encoders.AES.CBC, "AES-CBC");
   end Test_Compress_Encrypt_Decompress_Decrypt_Stream;

end Util.Streams.Buffered.Lzma.Tests;
