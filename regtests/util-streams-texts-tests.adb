-----------------------------------------------------------------------
--  streams.files.tests -- Unit tests for buffered streams
--  Copyright (C) 2012, 2018, 2019, 2021, 2022 Stephane Carrez
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

with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Util.Test_Caller;

with Util.Streams.Files;
package body Util.Streams.Texts.Tests is

   pragma Wide_Character_Encoding (UTF8);

   use Ada.Streams.Stream_IO;
   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Streams.Texts");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Streams.Texts.Open, Read_Line, Close",
                       Test_Read_Line'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Texts.Write (Integer)",
                       Test_Write_Integer'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Texts.Write (Long_Long_Integer)",
                       Test_Write_Long_Integer'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Texts.Write (Unbounded_String)",
                       Test_Write'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test reading a text stream.
   --  ------------------------------
   procedure Test_Read_Line (T : in out Test) is
      Stream : aliased Files.File_Stream;
      Reader : Util.Streams.Texts.Reader_Stream;
      Count  : Natural := 0;
   begin
      Stream.Open (Name => "Makefile", Mode => In_File);
      Reader.Initialize (From => Stream'Unchecked_Access);

      while not Reader.Is_Eof loop
         declare
            Line : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Reader.Read_Line (Line);
            Count := Count + 1;
         end;
      end loop;
      Stream.Close;
      T.Assert (Count > 100, "Too few lines read");
   end Test_Read_Line;

   --  ------------------------------
   --  Write on a text stream converting an integer and writing it.
   --  ------------------------------
   procedure Test_Write_Integer (T : in out Test) is
      Stream  : Print_Stream;
      Buf     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Stream.Initialize (Size => 4);

      --  Write '0' (we don't want the Ada spurious space).
      Stream.Write (Integer (0));

      Assert_Equals (T, 1, Integer (Stream.Get_Size), "Invalid size for stream");

      Stream.Flush (Buf);
      Assert_Equals (T, 1, Ada.Strings.Unbounded.Length (Buf), "Invalid size for string");

      Assert_Equals (T, "0", Ada.Strings.Unbounded.To_String (Buf), "Invalid stream content");

      Assert_Equals (T, 0, Integer (Stream.Get_Size), "Invalid size for stream after Flush");

      --  Write '1234'
      Stream.Write (Integer (1234));

      Assert_Equals (T, 4, Integer (Stream.Get_Size), "Invalid size for stream");

      Stream.Flush (Buf);
      Assert_Equals (T, 4, Ada.Strings.Unbounded.Length (Buf), "Invalid size for string");

      Assert_Equals (T, "1234", Ada.Strings.Unbounded.To_String (Buf), "Invalid stream content");

      Assert_Equals (T, 0, Integer (Stream.Get_Size), "Invalid size for stream after Flush");

      --  Write '-234'
      Stream.Write (Integer (-234));

      Assert_Equals (T, 4, Integer (Stream.Get_Size), "Invalid size for stream");

      Stream.Flush (Buf);
      Assert_Equals (T, 4, Ada.Strings.Unbounded.Length (Buf), "Invalid size for string");

      Assert_Equals (T, "-234", Ada.Strings.Unbounded.To_String (Buf), "Invalid stream content");

      Assert_Equals (T, 0, Integer (Stream.Get_Size), "Invalid size for stream after Flush");
   end Test_Write_Integer;

   --  ------------------------------
   --  Write on a text stream converting an integer and writing it.
   --  ------------------------------
   procedure Test_Write_Long_Integer (T : in out Test) is
      Stream  : Print_Stream;
      Buf     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Stream.Initialize (Size => 64);

      --  Write '0' (we don't want the Ada spurious space).
      Stream.Write (Long_Long_Integer (0));

      Assert_Equals (T, 1, Integer (Stream.Get_Size), "Invalid size for stream");

      Stream.Flush (Buf);
      Assert_Equals (T, 1, Ada.Strings.Unbounded.Length (Buf), "Invalid size for string");

      Assert_Equals (T, "0", Ada.Strings.Unbounded.To_String (Buf), "Invalid stream content");

      Assert_Equals (T, 0, Integer (Stream.Get_Size), "Invalid size for stream after Flush");

      --  Write '1234'
      Stream.Write (Long_Long_Integer (123456789012345));

      Assert_Equals (T, 15, Integer (Stream.Get_Size), "Invalid size for stream");

      Stream.Flush (Buf);
      Assert_Equals (T, 15, Ada.Strings.Unbounded.Length (Buf), "Invalid size for string");

      Assert_Equals (T, "123456789012345", Ada.Strings.Unbounded.To_String (Buf),
                     "Invalid stream content");

      Assert_Equals (T, 0, Integer (Stream.Get_Size), "Invalid size for stream after Flush");

      --  Write '-2345678901234'
      Stream.Write (Long_Long_Integer (-2345678901234));

      Assert_Equals (T, 14, Integer (Stream.Get_Size), "Invalid size for stream");

      Stream.Flush (Buf);
      Assert_Equals (T, 14, Ada.Strings.Unbounded.Length (Buf), "Invalid size for string");

      Assert_Equals (T, "-2345678901234", Ada.Strings.Unbounded.To_String (Buf),
                     "Invalid stream content");

      Assert_Equals (T, 0, Integer (Stream.Get_Size), "Invalid size for stream after Flush");
   end Test_Write_Long_Integer;

   --  ------------------------------
   --  Write on a text stream converting an integer and writing it.
   --  ------------------------------
   procedure Test_Write (T : in out Test) is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

      Stream  : Print_Stream;
      Buf     : Ada.Strings.Unbounded.Unbounded_String;

      Expect  : constant Wide_Wide_String := "àéúíòࠀ€ಜ࠴𝄞" & Wide_Wide_Character'Val (16#0A#);
   begin
      Stream.Initialize (Size => 60);

      Stream.Write (Ada.Strings.Unbounded.To_Unbounded_String ("hello"));

      Assert_Equals (T, 5, Integer (Stream.Get_Size), "Invalid size for stream");

      Stream.Flush (Buf);
      Assert_Equals (T, 5, Ada.Strings.Unbounded.Length (Buf), "Invalid size for string");

      Assert_Equals (T, "hello", Ada.Strings.Unbounded.To_String (Buf), "Invalid stream content");

      Assert_Equals (T, 0, Integer (Stream.Get_Size), "Invalid size for stream after Flush");

      --  Wide string
      Stream.Write (Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String ("hello"));

      Assert_Equals (T, 5, Integer (Stream.Get_Size), "Invalid size for stream");

      Stream.Flush (Buf);
      Assert_Equals (T, 5, Ada.Strings.Unbounded.Length (Buf), "Invalid size for string");

      Assert_Equals (T, "hello", Ada.Strings.Unbounded.To_String (Buf), "Invalid stream content");

      Assert_Equals (T, 0, Integer (Stream.Get_Size), "Invalid size for stream after Flush");

      --  Wide string
      Stream.Write_Wide (Expect);

      Assert_Equals (T, 27, Integer (Stream.Get_Size), "Invalid size for stream");

      Stream.Flush (Buf);
      Assert_Equals (T, 27, Ada.Strings.Unbounded.Length (Buf), "Invalid size for string");

      Assert_Equals (T, Encode (Expect),
                     Ada.Strings.Unbounded.To_String (Buf),
                     "Invalid stream content");

      Assert_Equals (T, 0, Integer (Stream.Get_Size), "Invalid size for stream after Flush");

   end Test_Write;

end Util.Streams.Texts.Tests;
