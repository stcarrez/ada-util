-----------------------------------------------------------------------
--  streams.files.tests -- Unit tests for buffered streams
--  Copyright (C) 2010 Stephane Carrez
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
with AUnit.Test_Caller;

with Util.Files;
with Util.Tests;
with Util.Streams.Buffered;
package body Util.Streams.Files.Tests is

   use Util.Tests;
   use Ada.Streams.Stream_IO;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Suite.Add_Test (Caller.Create ("Test Util.Streams.Files.Create, Write, Flush, Close",
        Test_Read_Write'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Streams.Files.Write, Flush",
        Test_Write'Access));
   end Add_Tests;

   procedure Test_Read_Write (T : in out Test) is
      Stream     : aliased File_Stream;
      Buffer     : Util.Streams.Buffered.Buffered_Stream;
   begin
      Buffer.Initialize (Output => Stream'Unchecked_Access,
                         Input  => null,
                         Size   => 1024);
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
   end Test_Read_Write;

   procedure Test_Write (T : in out Test) is
   begin
      null;
   end Test_Write;

end Util.Streams.Files.Tests;
