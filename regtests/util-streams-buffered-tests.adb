-----------------------------------------------------------------------
--  streams.buffered.tests -- Unit tests for buffered streams
--  Copyright (C) 2010, 2011, 2017, 2018, 2019, 2020, 2021, 2023 Stephane Carrez
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

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Streams.Stream_IO;
with Util.Test_Caller;
with Util.Streams.Texts;
with Util.Streams.Files;
with Util.Streams.Buffered.Parts;
package body Util.Streams.Buffered.Tests is

   pragma Wide_Character_Encoding (UTF8);

   use Util.Tests;
   use Util.Streams.Texts;

   package Caller is new Util.Test_Caller (Test, "Streams.Buffered");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Write, Read",
                       Test_Read_Write'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Write, Flush (String)",
                       Test_Write'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Write, Flush (Stream_Array)",
                       Test_Write_Stream'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Read (UTF-8)",
                       Test_Read_UTF_8'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Parts (1)",
                       Test_Parts'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Parts (2)",
                       Test_Parts_2'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Parts (3)",
                       Test_Parts_3'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Parts (4)",
                       Test_Parts_4'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Buffered.Parts (5)",
                       Test_Parts_5'Access);
   end Add_Tests;

   --  ------------------------------
   --  Write on a buffered stream and read what was written.
   --  ------------------------------
   procedure Test_Read_Write (T : in out Test) is
      Stream  : Print_Stream;
      Buf     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Stream.Initialize (Size => 4);
      Stream.Write ("abcd");

      Assert_Equals (T, 4, Integer (Stream.Get_Size), "Invalid size for stream");

      Stream.Flush (Buf);
      Assert_Equals (T, 4, Ada.Strings.Unbounded.Length (Buf), "Invalid size for string");

      Assert_Equals (T, "abcd", Ada.Strings.Unbounded.To_String (Buf), "Invalid stream content");

      Assert_Equals (T, 0, Integer (Stream.Get_Size), "Invalid size for stream after Flush");
   end Test_Read_Write;

   --  ------------------------------
   --  Write on a buffer and force regular flush on a larger buffer
   --  ------------------------------
   procedure Test_Write (T : in out Test) is
      Big_Stream : aliased Output_Buffer_Stream;
      Stream     : Output_Buffer_Stream;
      Size       : Stream_Element_Offset := 0;
      Count      : constant Stream_Element_Offset := 1000;
      Max_Size   : constant Stream_Element_Offset := (Count * (Count + 1)) / 2;
   begin
      Big_Stream.Initialize (Size => Natural (Max_Size));
      Stream.Initialize (Output => Big_Stream'Unchecked_Access, Size => 13);

      for I in 1 .. Count loop
         declare
            S : Stream_Element_Array (1 .. I);
         begin
            for J in S'Range loop
               S (J) := Stream_Element (J mod 255);
            end loop;

            Stream.Write (S);
            Stream.Flush;

            Size := Size + I;
            Assert_Equals (T, 1, Integer (Stream.Write_Pos), "Stream must be flushed");

            --  Verify that 'Big_Stream' holds the expected number of bytes.
            Assert_Equals (T, Integer (Size), Integer (Big_Stream.Write_Pos) - 1,
                           "Target stream has an invalid write position at "
                          & Stream_Element_Offset'Image (I));
         end;
      end loop;
   end Test_Write;

   --  ------------------------------
   --  Write on a buffer and force regular flush on a larger buffer
   --  ------------------------------
   procedure Test_Write_Stream (T : in out Test) is
      Big_Stream : aliased Output_Buffer_Stream;
      Stream     : Output_Buffer_Stream;
      Size       : Stream_Element_Offset := 0;
      Count      : constant Stream_Element_Offset := 200;
      Max_Size   : constant Stream_Element_Offset := 5728500;
   begin
      Big_Stream.Initialize (Size => Natural (Max_Size));
      for Buf_Size in 1 .. 19 loop
         Stream.Initialize (Output => Big_Stream'Unchecked_Access,
                            Size => Buf_Size);

         for I in 1 .. Count loop
            for Repeat in 1 .. 5 loop
               declare
                  S : Stream_Element_Array (1 .. I);
               begin
                  for J in S'Range loop
                     S (J) := Stream_Element'Val (J mod 255);
                  end loop;

                  for J in 1 .. Repeat loop
                     Stream.Write (S);
                  end loop;
                  Stream.Flush;

                  Size := Size +  (I) * Stream_Element_Offset (Repeat);
                  Assert_Equals (T, 1, Integer (Stream.Write_Pos), "Stream must be flushed");

                  --  Verify that 'Big_Stream' holds the expected number of bytes.
                  Assert_Equals (T, Integer (Size), Integer (Big_Stream.Write_Pos) - 1,
                                 "Target stream has an invalid write position at "
                                 & Stream_Element_Offset'Image (I) & " with buffer "
                                 & Natural'Image (Buf_Size) & " repeat " & Natural'Image (Repeat));
               end;
            end loop;
         end loop;
      end loop;
      Assert_Equals (T, Integer (Max_Size), Integer (Big_Stream.Get_Size), "Invalid final size");
   end Test_Write_Stream;

   --  ------------------------------
   --  Test reading UTF-8 file.
   --  ------------------------------
   procedure Test_Read_UTF_8 (T : in out Test) is
      File    : aliased Util.Streams.Files.File_Stream;
      Stream  : Util.Streams.Buffered.Input_Buffer_Stream;
      Path    : constant String := Util.Tests.Get_Path ("regtests/files/utf-8.txt");
   begin
      Stream.Initialize (Input => File'Unchecked_Access, Size => 1024);

      File.Open (Ada.Streams.Stream_IO.In_File, Path);
      declare
         S : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
         Expect : constant Wide_Wide_String := "àéúíòࠀ€ಜ࠴𝄞" & Wide_Wide_Character'Val (16#0A#);
      begin
         Stream.Read (S);

         declare
            Result : constant Wide_Wide_String
              := Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (S);
         begin
            if Expect /= Result then
               T.Fail ("Invalid UTF-8 string");
            end if;
         end;
      end;
   end Test_Read_UTF_8;

   --  ------------------------------
   --  Test reading a streams with several parts separated by boundaries.
   --  ------------------------------
   procedure Test_Parts (T : in out Test) is
      SEP     : constant String := "" & ASCII.LF;
      Path    : constant String := Util.Tests.Get_Path ("regtests/files/test-parts-1.txt");
      File    : aliased Util.Streams.Files.File_Stream;
      Parts   : Util.Streams.Buffered.Parts.Input_Part_Stream;
      Head    : Ada.Strings.Unbounded.Unbounded_String;
      Msg     : Ada.Strings.Unbounded.Unbounded_String;
      Sign    : Ada.Strings.Unbounded.Unbounded_String;
   begin
      File.Open (Ada.Streams.Stream_IO.In_File, Path);
      Parts.Initialize (Input => File'Unchecked_Access, Size => 128);
      Parts.Set_Boundary (SEP & "-----BEGIN PGP SIGNED MESSAGE-----" & SEP);
      Parts.Read (Head);
      Assert_Equals (T, "head", Head, "invalid head extraction");
      Assert (T, Parts.Is_Eob, "Eob must be true");

      Parts.Set_Boundary (SEP & "-----BEGIN PGP SIGNATURE-----" & SEP);
      Assert (T, not Parts.Is_Eob, "Eob must be false");
      Parts.Read (Msg);
      Assert_Equals (T, "message", Msg, "invalid message extraction");
      Assert (T, Parts.Is_Eob, "Eob must be true");

      Parts.Set_Boundary (SEP & "-----END PGP SIGNATURE-----" & SEP);
      Assert (T, not Parts.Is_Eob, "Eob must be false");
      Parts.Read (Sign);
      Assert_Equals (T, "signature", Sign, "invalid signature extraction");
      Assert (T, Parts.Is_Eob, "Eob must be true");

   end Test_Parts;

   --  ------------------------------
   --  Test reading a streams with several parts separated by boundaries.
   --  ------------------------------
   procedure Test_Parts_2 (T : in out Test) is
      SEP     : constant String := "" & ASCII.LF;
      Path    : constant String := Util.Tests.Get_Path ("regtests/files/test-parts-2.txt");

   begin
      for I in 16 .. 99 loop
         declare
            File    : aliased Util.Streams.Files.File_Stream;
            Parts   : Util.Streams.Buffered.Parts.Input_Part_Stream;
            Head    : Ada.Strings.Unbounded.Unbounded_String;
            Msg     : Ada.Strings.Unbounded.Unbounded_String;
            Sign    : Ada.Strings.Unbounded.Unbounded_String;
         begin
            File.Open (Ada.Streams.Stream_IO.In_File, Path);
            Parts.Initialize (Input => File'Unchecked_Access, Size => I);
            Parts.Set_Boundary (SEP & "-----SEP-----" & SEP);
            Parts.Read (Head);
            Assert_Equals (T, "A-----SEP-----", Head, "invalid first extraction");
            Assert (T, Parts.Is_Eob, "Eob must be true");

            Parts.Set_Boundary (SEP & "-----SEP-----" & SEP);
            Assert (T, not Parts.Is_Eob, "Eob must be false " & I'Image);
            Parts.Read (Msg);
            Assert_Equals (T, "-----SEP----B", Msg, "invalid second extraction");
            Assert (T, Parts.Is_Eob, "Eob must be true " & I'Image);

            Parts.Set_Boundary (SEP & "-----SEP-----" & SEP);
            Assert (T, not Parts.Is_Eob, "Eob must be false");
            Parts.Read (Sign);
            Assert_Equals (T, "-----SEP----C-----SEP----", Sign, "invalid third extraction");
            Assert (T, Parts.Is_Eob, "Eob must be true");
         end;
      end loop;
   end Test_Parts_2;

   procedure Test_Parts_3 (T : in out Test) is
      SEP     : constant String := "" & ASCII.LF;
      Parts   : Util.Streams.Buffered.Parts.Input_Part_Stream;
      C       : Stream_Element;
   begin
      Parts.Initialize ("--AB--" & SEP & SEP & "--CD--" & SEP & "a" & SEP & "--EF--" & SEP);
      Parts.Set_Boundary ("--AB--" & SEP);
      Assert (T, Parts.Is_Eob, "First part must be empty");

      Parts.Set_Boundary (SEP & "--CD--" & SEP);
      Assert (T, Parts.Is_Eob, "Second part must be empty");

      Parts.Set_Boundary (SEP & "--EF--" & SEP);
      Assert (T, not Parts.Is_Eob, "Third part must not be empty");
      Parts.Read (C);

   end Test_Parts_3;

   procedure Test_Parts_4 (T : in out Test) is
      SEP     : constant String := "" & ASCII.LF;
      Path    : constant String := Util.Tests.Get_Path ("regtests/files/public.pem");
   begin
      for I in 30 .. 114 loop
         declare
            File    : aliased Util.Streams.Files.File_Stream;
            Parts   : Util.Streams.Buffered.Parts.Input_Part_Stream;
            Content : Ada.Strings.Unbounded.Unbounded_String;
         begin
            File.Open (Ada.Streams.Stream_IO.In_File, Path);
            Parts.Initialize (Input => File'Unchecked_Access, Size => 1024);
            Parts.Set_Boundary ("-----BEGIN PUBLIC KEY-----" & SEP);
            Assert (T, Parts.Is_Eob, "Start public key header not found");

            Parts.Set_Boundary (SEP & "-----END PUBLIC KEY-----" & SEP);
            Parts.Read (Content);
            Assert (T, Parts.Is_Eob, "End public key header not found");
            Assert_Equals (T, "MCowBQYDK2VwAyEAOq9Igie0zBxiRE9HctjYr+lK9w6yJhR7U0dkffx1tfk=",
               Content, "Invalid public key extracted");
         end;
      end loop;
   end Test_Parts_4;

   procedure Test_Parts_5 (T : in out Test) is
      SEP     : constant String := "" & ASCII.LF;
      Path    : constant String := Util.Tests.Get_Path ("regtests/files/test-parts-5.txt");
   begin
      for I in 30 .. 114 loop
         declare
            File    : aliased Util.Streams.Files.File_Stream;
            Parts   : Util.Streams.Buffered.Parts.Input_Part_Stream;
            K1, K2, K3 : Ada.Strings.Unbounded.Unbounded_String;
         begin
            File.Open (Ada.Streams.Stream_IO.In_File, Path);
            Parts.Initialize (Input => File'Unchecked_Access, Size => 1024);
            Parts.Set_Boundary ("-----KEY-----" & SEP);
            Assert (T, Parts.Is_Eob, "Start key header not found");

            Parts.Next_Part;
            Assert (T, not Parts.Is_Eob, "Moving to next key 1 failed");
            Parts.Read (K1);
            Assert (T, Parts.Is_Eob, "End key1 not found");

            Parts.Next_Part;
            Assert (T, not Parts.Is_Eob, "Moving to next key 2 failed");
            Parts.Read (K2);
            Assert (T, Parts.Is_Eob, "End key2 not found");

            Parts.Next_Part;
            Assert (T, not Parts.Is_Eob, "Moving to next key 2 failed");
            Parts.Read (K3);
            Assert (T, Parts.Is_Eob, "End key3 not found");

            Assert_Equals (T, "K1" & SEP, K1, "Invalid K1 extracted");
            Assert_Equals (T, "K2" & SEP, K2, "Invalid K2 extracted");
            Assert_Equals (T, "K3" & SEP, K3, "Invalid K3 extracted");
         end;
      end loop;
   end Test_Parts_5;

end Util.Streams.Buffered.Tests;
