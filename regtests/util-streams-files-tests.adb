-----------------------------------------------------------------------
--  streams.files.tests -- Unit tests for buffered streams
--  Copyright (C) 2010, 2011, 2017, 2019, 2021 Stephane Carrez
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
with Util.Test_Caller;

with Util.Files;
with Util.Streams.Texts;
package body Util.Streams.Files.Tests is

   use Util.Tests;
   use Ada.Streams.Stream_IO;

   package Caller is new Util.Test_Caller (Test, "Streams.Files");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Streams.Files.Create, Write, Flush, Close",
                       Test_Read_Write'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Files.Write, Flush",
                       Test_Write'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Copy",
                       Test_Copy_Stream'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test reading and writing on a buffered stream with various buffer sizes
   --  ------------------------------
   procedure Test_Read_Write (T : in out Test) is
      Stream     : aliased File_Stream;
      Buffer     : Util.Streams.Texts.Print_Stream;
   begin
      for I in 1 .. 32 loop
         Buffer.Initialize (Output => Stream'Unchecked_Access,
                            Size   => I);
         Stream.Create (Mode => Out_File, Name => "test-stream.txt");
         Buffer.Write ("abcd");
         Buffer.Write (" fghij");
         Buffer.Flush;
         Stream.Close;

         declare
            Content : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Util.Files.Read_File (Path     => "test-stream.txt",
                                  Into     => Content,
                                  Max_Size => 10000);
            Assert_Equals (T, "abcd fghij", Content, "Invalid content written to the file stream");
         end;
      end loop;
   end Test_Read_Write;

   procedure Test_Write (T : in out Test) is
   begin
      null;
   end Test_Write;

   procedure Test_Copy_Stream (T : in out Test) is
      Path    : constant String := Util.Tests.Get_Path ("regtests/files/utf-8.txt");
      Target  : constant String := Util.Tests.Get_Test_Path ("copy-stream.txt");
      Output  : Util.Streams.Files.File_Stream;
      Input   : Util.Streams.Files.File_Stream;
   begin
      Output.Create (Name => Target, Mode => Ada.Streams.Stream_IO.Out_File);
      Input.Open (Name => Path, Mode => Ada.Streams.Stream_IO.In_File);
      Util.Streams.Copy (From => Input, Into => Output);
      Input.Close;
      Output.Close;

      Util.Tests.Assert_Equal_Files (T, Path, Target, "Copy stream failed");
   end Test_Copy_Stream;

end Util.Streams.Files.Tests;
