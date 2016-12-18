-----------------------------------------------------------------------
--  serialize-io-json-tests -- Unit tests for JSON parser
--  Copyright (C) 2011, 2016 Stephane Carrez
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
with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Calendar.Formatting;

with Util.Test_Caller;
with Util.Log.Loggers;
with Util.Streams.Files;

package body Util.Serialize.IO.JSON.Tests is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Util.Serialize.IO.JSON");

   package Caller is new Util.Test_Caller (Test, "Serialize.IO.JSON");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.JSON.Parse (parse errors)",
                       Test_Parse_Error'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.JSON.Parse (parse Ok)",
                       Test_Parser'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.JSON.Write",
                       Test_Output'Access);
   end Add_Tests;

   --  ------------------------------
   --  Check various JSON parsing errors.
   --  ------------------------------
   procedure Test_Parse_Error (T : in out Test) is
      pragma Unreferenced (T);
      procedure Check_Parse_Error (Content : in String);

      procedure Check_Parse_Error (Content : in String) is
         P : Parser;
      begin
         P.Parse_String (Content);
         Log.Error ("No exception raised for: {0}", Content);

      exception
         when Parse_Error =>
            null;
      end Check_Parse_Error;

   begin
      Check_Parse_Error ("{ ""person"":23");
      Check_Parse_Error ("{ person: 23]");
      Check_Parse_Error ("[ }");
      Check_Parse_Error ("{[]}");
      Check_Parse_Error ("{");
      Check_Parse_Error ("{[");
      Check_Parse_Error ("{ ""person");
      Check_Parse_Error ("{ ""person"":");
      Check_Parse_Error ("{ ""person"":""asf");
      Check_Parse_Error ("{ ""person"":""asf""");
      Check_Parse_Error ("{ ""person"":""asf"",");
      Check_Parse_Error ("{ ""person"":""\uze""}");
      Check_Parse_Error ("{ ""person"":""\u012-""}");
      Check_Parse_Error ("{ ""person"":""\u012G""}");
   end Test_Parse_Error;

   --  ------------------------------
   --  Check various (basic) JSON valid strings (no mapper).
   --  ------------------------------
   procedure Test_Parser (T : in out Test) is
      pragma Unreferenced (T);
      procedure Check_Parse (Content : in String);

      procedure Check_Parse (Content : in String) is
         P : Parser;
      begin
         P.Parse_String (Content);

      exception
         when Parse_Error =>
            Log.Error ("Parse error for: " & Content);
            raise;
      end Check_Parse;

   begin
      Check_Parse ("{ ""person"":23}");
      Check_Parse ("{ }");
      Check_Parse ("{""person"":""asf""}");
      Check_Parse ("{""person"":""asf"",""age"":""2""}");
      Check_Parse ("{ ""person"":""\u0123""}");
      Check_Parse ("{ ""person"":""\u4567""}");
      Check_Parse ("{ ""person"":""\u89ab""}");
      Check_Parse ("{ ""person"":""\ucdef""}");
      Check_Parse ("{ ""person"":""\u1CDE""}");
      Check_Parse ("{ ""person"":""\u2ABF""}");
   end Test_Parser;

   --  ------------------------------
   --  Generate some output stream for the test.
   --  ------------------------------
   procedure Write_Stream (Stream : in out Util.Serialize.IO.Output_Stream'Class) is
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Wide : constant Wide_Wide_String :=
        Ada.Characters.Wide_Wide_Latin_1.CR &
        Ada.Characters.Wide_Wide_Latin_1.LF &
        Ada.Characters.Wide_Wide_Latin_1.HT &
        Wide_Wide_Character'Val (16#080#) &
        Wide_Wide_Character'Val (16#1fC#) &
        Wide_Wide_Character'Val (16#20AC#) & -- Euro sign
        Wide_Wide_Character'Val (16#2acbf#);
      T : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of (2011, 11, 19, 23, 0, 0);
   begin
      Ada.Strings.Unbounded.Append (Name, "Katniss Everdeen");
      Stream.Start_Document;
      Stream.Start_Entity ("root");
      Stream.Start_Entity ("person");
      Stream.Write_Attribute ("id", 1);
      Stream.Write_Attribute ("name", Name);
      Stream.Write_Entity ("name", Name);
      Stream.Write_Entity ("gender", "female");
      Stream.Write_Entity ("volunteer", True);
      Stream.Write_Entity ("age", 17);
      Stream.Write_Entity ("date", T);
      Stream.Write_Wide_Entity ("skin", "olive skin");
      Stream.Start_Array ("badges");

      Stream.Start_Entity ("badge");
      Stream.Write_Entity ("level", "gold");
      Stream.Write_Entity ("name", "hunter");
      Stream.End_Entity ("badge");

      Stream.Start_Entity ("badge");
      Stream.Write_Entity ("level", "gold");
      Stream.Write_Entity ("name", "archery");
      Stream.End_Entity ("badge");
      Stream.End_Array ("badges");

      Stream.Start_Entity ("district");
      Stream.Write_Attribute ("id", 12);
      Stream.Write_Wide_Attribute ("industry", "Coal mining");
      Stream.Write_Attribute ("state", "<destroyed>");
      Stream.Write_Long_Entity ("members", 10_000);
      Stream.Write_Entity ("description", "<TBW>&""");
      Stream.Write_Wide_Entity ("wescape", "'""<>&;,@!`~[]{}^%*()-+=");
      Stream.Write_Entity ("escape", "'""<>&;,@!`~\[]{}^%*()-+=");
      Stream.Write_Wide_Entity ("wide", Wide);
      Stream.End_Entity ("district");

      Stream.End_Entity ("person");
      Stream.End_Entity ("root");
      Stream.End_Document;
   end Write_Stream;

   --  ------------------------------
   --  Test the JSON output stream generation.
   --  ------------------------------
   procedure Test_Output (T : in out Test) is
      File   : aliased Util.Streams.Files.File_Stream;
      Buffer : aliased Util.Streams.Texts.Print_Stream;
      Stream : Util.Serialize.IO.JSON.Output_Stream;
      Expect : constant String := Util.Tests.Get_Path ("regtests/expect/test-stream.json");
      Path   : constant String := Util.Tests.Get_Test_Path ("regtests/result/test-stream.json");
   begin
      File.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Path);
      Buffer.Initialize (Output => File'Unchecked_Access, Input => null, Size => 10000);
      Stream.Initialize (Output => Buffer'Unchecked_Access);
      Write_Stream (Stream);
      Stream.Close;
      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Expect,
                                     Test    => Path,
                                     Message => "JSON output serialization");
   end Test_Output;

end Util.Serialize.IO.JSON.Tests;
