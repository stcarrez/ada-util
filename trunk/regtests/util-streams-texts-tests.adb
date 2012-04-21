-----------------------------------------------------------------------
--  streams.files.tests -- Unit tests for buffered streams
--  Copyright (C) 2012 Stephane Carrez
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

with Util.Test_Caller;

with Util.Streams.Files;
with Util.Streams.Texts;
package body Util.Streams.Texts.Tests is

   use Util.Tests;
   use Ada.Streams.Stream_IO;

   package Caller is new Util.Test_Caller (Test, "Streams.Texts");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Streams.Texts.Open, Read_Line, Close",
                       Test_Read_Line'Access);
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

end Util.Streams.Texts.Tests;
