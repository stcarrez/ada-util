-----------------------------------------------------------------------
--  util-streams-tests -- Unit tests for encoding buffered streams
--  Copyright (C) 2017, 2018, 2019, 2021 Stephane Carrez
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
with Util.Streams.Base64;
with Util.Streams.AES;
with Util.Measures;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
package body Util.Streams.Tests is

   use Util.Streams.Files;
   use Ada.Streams.Stream_IO;

   package AES_Caller is new Util.Test_Caller (Test, "Streams.AES");

   procedure Test_AES_File (T     : in out Test;
                            Item  : in String;
                            Count : in Positive;
                            Mode  : in Util.Encoders.AES.AES_Mode;
                            Label : in String);

   package Caller is new Util.Test_Caller (Test, "Streams.Main");

   generic
      Mode  : in Util.Encoders.AES.AES_Mode;
      Label : in String;
   procedure Test_AES_Mode (T : in out Test);

   procedure Test_AES (T     : in out Test;
                       Item  : in String;
                       Count : in Positive;
                       Mode  : in Util.Encoders.AES.AES_Mode;
                       Label : in String) is
      Reader   : aliased Util.Streams.Texts.Reader_Stream;
      Decipher : aliased Util.Streams.AES.Decoding_Stream;
      Cipher   : aliased Util.Streams.AES.Encoding_Stream;
      Print    : Util.Streams.Texts.Print_Stream;
      Key      : Util.Encoders.Secret_Key
        := Util.Encoders.Create ("0123456789abcdef0123456789abcdef");
   begin
      --  Print -> Cipher -> Decipher
      Decipher.Initialize (64 * 1024);
      Decipher.Set_Key (Key, Mode);
      Cipher.Produces (Decipher'Unchecked_Access, 1024);
      Cipher.Set_Key (Key, Mode);
      Print.Initialize (Cipher'Unchecked_Access);
      for I in 1 .. Count loop
         Print.Write (Item);
      end loop;
      Print.Flush;
      Util.Tests.Assert_Equals (T,
                                Item'Length * Count,
                                Decipher.Get_Size,
                                Label & ": decipher buffer has the wrong size mode");

      --  Read content in Decipher
      Reader.Initialize (Decipher);
      for I in 1 .. Count loop
         declare
            L : String (Item'Range) := (others => ' ');
         begin
            for J in L'Range loop
               Reader.Read (L (J));
            end loop;
            Util.Tests.Assert_Equals (T, Item, L, Label & ": wrong value");

         exception
            when Ada.IO_Exceptions.Data_Error =>
               Util.Tests.Assert_Equals (T, Item, L, Label & ": wrong value (DATA error)");
         end;
      end loop;
   end Test_AES;

   procedure Test_AES_File (T     : in out Test;
                            Item  : in String;
                            Count : in Positive;
                            Mode  : in Util.Encoders.AES.AES_Mode;
                            Label : in String) is
      use Ada.Strings.Unbounded;

      Path       : constant String := Util.Tests.Get_Test_Path ("stream-aes-" & Label & ".aes");
      File       : aliased File_Stream;
      Decipher   : aliased Util.Streams.AES.Decoding_Stream;
      Cipher     : aliased Util.Streams.AES.Encoding_Stream;
      Print      : Util.Streams.Texts.Print_Stream;
      Reader     : Util.Streams.Texts.Reader_Stream;
      Key        : Util.Encoders.Secret_Key
        := Util.Encoders.Create ("0123456789abcdef0123456789abcdef");
   begin
      --  Print -> Cipher -> File
      File.Create (Mode => Out_File, Name => Path);
      Cipher.Produces (File'Unchecked_Access, 32);
      Cipher.Set_Key (Key, Mode);
      Print.Initialize (Cipher'Unchecked_Access);
      for I in 1 .. Count loop
         Print.Write (Item & ASCII.LF);
      end loop;
      Print.Close;

      --  File -> Decipher -> Reader
      File.Open (Mode => In_File, Name => Path);
      Decipher.Consumes (File'Unchecked_Access, 10240);
      Decipher.Set_Key (Key, Mode);
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
               if Item & ASCII.LF /= Line then
                  Util.Tests.Assert_Equals (T, Item & ASCII.LF, To_String (Line));
               end if;
               Line_Count := Line_Count + 1;
            end;
         end loop;
         File.Close;
         Util.Tests.Assert_Equals (T, Count, Line_Count);
      end;
   end Test_AES_File;

   procedure Test_AES_Mode (T : in out Test) is
   begin
      for I in 1 .. 128 loop
         Test_AES (T, "a", I, Mode, Label);
      end loop;
      for I in 1 .. 128 loop
         Test_AES (T, "ab", I, Mode, Label);
      end loop;
      for I in 1 .. 128 loop
         Test_AES (T, "abc", I, Mode, Label);
      end loop;

      Test_AES_File (T, "abcdefgh", 1000, Mode, Label);
   end Test_AES_Mode;

   procedure Test_AES_ECB is
      new Test_AES_Mode (Mode => Util.Encoders.AES.ECB, Label => "AES-ECB");

   procedure Test_AES_CBC is
      new Test_AES_Mode (Mode => Util.Encoders.AES.CBC, Label => "AES-CBC");

   procedure Test_AES_PCBC is
      new Test_AES_Mode (Mode => Util.Encoders.AES.PCBC, Label => "AES-PCBC");

   procedure Test_AES_CFB is
      new Test_AES_Mode (Mode => Util.Encoders.AES.PCBC, Label => "AES-CFB");

   procedure Test_AES_OFB is
      new Test_AES_Mode (Mode => Util.Encoders.AES.PCBC, Label => "AES-OFB");

   procedure Test_AES_CTR is
      new Test_AES_Mode (Mode => Util.Encoders.AES.PCBC, Label => "AES-CTR");

   procedure Test_Base64_Stream (T : in out Test) is
      Stream  : aliased File_Stream;
      Buffer  : aliased Util.Streams.Base64.Encoding_Stream;
      Print   : Util.Streams.Texts.Print_Stream;
      Path    : constant String := Util.Tests.Get_Test_Path ("test-stream.b64");
      Expect  : constant String := Util.Tests.Get_Path ("regtests/expect/test-stream.b64");
   begin
      Print.Initialize (Output => Buffer'Unchecked_Access, Size => 5);
      Buffer.Produces (Output => Stream'Unchecked_Access,
                       Size   => 1024);
      Stream.Create (Mode => Out_File, Name => Path);
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
                                     Message => "Base64 stream");
   end Test_Base64_Stream;

   procedure Test_Copy_Stream (T : in out Test) is
      Pat : constant String := "123456789abcdef0123456789";
      Buf : Ada.Streams.Stream_Element_Array (1 .. Pat'Length);
      Res : String (10 .. 10 + Pat'Length - 1);
   begin
      declare
         S   : Util.Measures.Stamp;
      begin
         for I in 1 .. 1000 loop
            Util.Streams.Copy (Pat, Buf);
         end loop;
         Util.Measures.Report (S, "Util.Streams.Copy (String)", 1000);
      end;
      declare
         S   : Util.Measures.Stamp;
      begin
         for I in 1 .. 1000 loop
            Util.Streams.Copy (Buf, Res);
         end loop;
         Util.Measures.Report (S, "Util.Streams.Copy (Stream_Element_Array)", 1000);
      end;
      Util.Tests.Assert_Equals (T, Pat, Res, "Invalid copy");
   end Test_Copy_Stream;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Streams.Copy",
                       Test_Copy_Stream'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Base64.Write, Read",
                       Test_Base64_Stream'Access);
      AES_Caller.Add_Test (Suite, "Test Util.Streams.AES (AES-ECB)",
                           Test_AES_ECB'Access);
      AES_Caller.Add_Test (Suite, "Test Util.Streams.AES (AES-CBC)",
                           Test_AES_CBC'Access);
      AES_Caller.Add_Test (Suite, "Test Util.Streams.AES (AES-PCBC)",
                           Test_AES_PCBC'Access);
      AES_Caller.Add_Test (Suite, "Test Util.Streams.AES (AES-CFB)",
                           Test_AES_CFB'Access);
      AES_Caller.Add_Test (Suite, "Test Util.Streams.AES (AES-OFB)",
                           Test_AES_OFB'Access);
      AES_Caller.Add_Test (Suite, "Test Util.Streams.AES (AES-CTR)",
                           Test_AES_CTR'Access);
   end Add_Tests;

end Util.Streams.Tests;
