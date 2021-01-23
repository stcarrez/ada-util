-----------------------------------------------------------------------
--  serialize-io-csv-tests -- Unit tests for CSV parser
--  Copyright (C) 2011, 2016, 2021 Stephane Carrez
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
with Ada.Streams.Stream_IO;
with Util.Test_Caller;
with Util.Streams.Files;
with Util.Serialize.Mappers.Tests;
with Util.Serialize.IO.JSON.Tests;
package body Util.Serialize.IO.CSV.Tests is

   package Caller is new Util.Test_Caller (Test, "Serialize.IO.CSV");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.CSV.Parse (parse Ok)",
                       Test_Parser'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.CSV.Write",
                       Test_Output'Access);
   end Add_Tests;

   --  ------------------------------
   --  Check various (basic) JSON valid strings (no mapper).
   --  ------------------------------
   procedure Test_Parser (T : in out Test) is
      use Util.Serialize.Mappers.Tests;

      procedure Check_Parse (Content : in String;
                             Expect   : in Integer);

      Mapping        : aliased Util.Serialize.Mappers.Tests.Map_Test_Mapper.Mapper;
      Vector_Mapper  : aliased Util.Serialize.Mappers.Tests.Map_Test_Vector_Mapper.Mapper;

      procedure Check_Parse (Content : in String;
                             Expect : in Integer) is
         P : Parser;

         Value   : aliased Map_Test_Vector.Vector;
         Mapper  : Util.Serialize.Mappers.Processing;
      begin
         Mapper.Add_Mapping ("", Vector_Mapper'Unchecked_Access);
         Map_Test_Vector_Mapper.Set_Context (Mapper, Value'Unchecked_Access);

         P.Parse_String (Content, Mapper);

         T.Assert (not P.Has_Error, "Parse error for: " & Content);
         Util.Tests.Assert_Equals (T, 1, Integer (Value.Length), "Invalid result length");

         Util.Tests.Assert_Equals (T, Expect, Integer (Value.Element (1).Value), "Invalid value");
      end Check_Parse;

      HDR : constant String := "name,status,value,bool" & ASCII.CR & ASCII.LF;
   begin
      Mapping.Add_Mapping ("name", FIELD_NAME);
      Mapping.Add_Mapping ("value", FIELD_VALUE);
      Mapping.Add_Mapping ("status", FIELD_BOOL);
      Mapping.Add_Mapping ("bool", FIELD_BOOL);
      Vector_Mapper.Set_Mapping (Mapping'Unchecked_Access);

      Check_Parse (HDR & "joe,false,23,true", 23);
      Check_Parse (HDR & "billy,false,""12"",true", 12);
      Check_Parse (HDR & """John Potter"",false,""1234"",true", 1234);
      Check_Parse (HDR & """John" & ASCII.CR & "Potter"",False,""3234"",True", 3234);
      Check_Parse (HDR & """John" & ASCII.LF & "Potter"",False,""3234"",True", 3234);
   end Test_Parser;

   --  ------------------------------
   --  Test the CSV output stream generation.
   --  ------------------------------
   procedure Test_Output (T : in out Test) is
      File   : aliased Util.Streams.Files.File_Stream;
      Stream : Util.Serialize.IO.CSV.Output_Stream;
      Expect : constant String := Util.Tests.Get_Path ("regtests/expect/test-stream.csv");
      Path   : constant String := Util.Tests.Get_Test_Path ("test-stream.csv");
   begin
      File.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Path);
      Stream.Initialize (Output => File'Unchecked_Access, Size => 10000);
      Util.Serialize.IO.JSON.Tests.Write_Stream (Stream);
      Stream.Close;
      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Expect,
                                     Test    => Path,
                                     Message => "CSV output serialization");
   end Test_Output;

end Util.Serialize.IO.CSV.Tests;
