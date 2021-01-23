-----------------------------------------------------------------------
--  util-serialize-io-form-tests -- Unit tests for form parser
--  Copyright (C) 2018, 2021 Stephane Carrez
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
with Util.Beans.Objects.Readers;

package body Util.Serialize.IO.Form.Tests is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Util.Serialize.IO.Form");

   package Caller is new Util.Test_Caller (Test, "Serialize.IO.Form");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.Form.Parse (parse errors)",
                       Test_Parse_Error'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.Form.Parse (parse Ok)",
                       Test_Parser'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.Form.Write",
                       Test_Output'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.Form.Read",
                       Test_Read'Access);
   end Add_Tests;

   --  ------------------------------
   --  Check various form parsing errors.
   --  ------------------------------
   procedure Test_Parse_Error (T : in out Test) is

      procedure Check_Parse_Error (Content : in String);

      procedure Check_Parse_Error (Content : in String) is
         P : Parser;
         R : Util.Beans.Objects.Readers.Reader;
      begin
         P.Parse_String (Content, R);
         T.Assert (P.Has_Error, "No error detected for '" & Content & "'");
      end Check_Parse_Error;

   begin
      Check_Parse_Error ("bad");
      Check_Parse_Error ("bad=%rw%ad");
   end Test_Parse_Error;

   --  ------------------------------
   --  Check various (basic) JSformON valid strings (no mapper).
   --  ------------------------------
   procedure Test_Parser (T : in out Test) is

      procedure Check_Parse (Content : in String);

      procedure Check_Parse (Content : in String) is
         P : Parser;
         R : Util.Beans.Objects.Readers.Reader;
      begin
         P.Parse_String (Content, R);

      exception
         when Parse_Error =>
            Log.Error ("Parse error for: " & Content);
            T.Fail ("Parse error for: " & Content);
      end Check_Parse;

   begin
      Check_Parse ("name=value");
      Check_Parse ("name=value&param=value");
      Check_Parse ("name+name=value+value");
      Check_Parse ("name%20%30=value%23%ce");
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
      Stream : Util.Serialize.IO.Form.Output_Stream;
      Expect : constant String := Util.Tests.Get_Path ("regtests/expect/test-stream.form");
      Path   : constant String := Util.Tests.Get_Test_Path ("test-stream.form");
   begin
      File.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Path);
      Buffer.Initialize (Output => File'Unchecked_Access, Size => 10000);
      Stream.Initialize (Output => Buffer'Unchecked_Access);
      Write_Stream (Stream);
      Stream.Close;
      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Expect,
                                     Test    => Path,
                                     Message => "Form output serialization");
   end Test_Output;

   --  ------------------------------
   --  Test reading a form content into an Object tree.
   --  ------------------------------
   procedure Test_Read (T : in out Test) is
      Path  : constant String := Util.Tests.Get_Path ("regtests/files/pass-01.form");
      Root  : Util.Beans.Objects.Object;
      Value : Util.Beans.Objects.Object;
   begin
      Root := Read (Path);
      T.Assert (not Util.Beans.Objects.Is_Null (Root), "Read should not return null object");
      T.Assert (not Util.Beans.Objects.Is_Array (Root), "Root object is not an array");

      Value := Util.Beans.Objects.Get_Value (Root, "home");
      Util.Tests.Assert_Equals (T, "Cosby",
                                Util.Beans.Objects.To_String (Value),
                                "Invalid first parameter");
      Value := Util.Beans.Objects.Get_Value (Root, "favorite flavor");
      Util.Tests.Assert_Equals (T, "flies",
                                Util.Beans.Objects.To_String (Value),
                                "Invalid second parameter");
   end Test_Read;

end Util.Serialize.IO.Form.Tests;
