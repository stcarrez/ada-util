-----------------------------------------------------------------------
--  serialize-io-json-tests -- Unit tests for JSON parser
--  Copyright (C) 2011, 2016, 2017, 2020, 2021 Stephane Carrez
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
with Util.Beans.Objects.Maps;
with Util.Beans.Objects.Readers;

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
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.JSON.Write (Simple)",
                       Test_Simple_Output'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.JSON.Write (Nullable)",
                       Test_Nullable'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.JSON.Read",
                       Test_Read'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.JSON.Write_Entity",
                       Test_Write'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.JSON.Start_Entity",
                       Test_Write_Complex'Access);
   end Add_Tests;

   --  ------------------------------
   --  Check various JSON parsing errors.
   --  ------------------------------
   procedure Test_Parse_Error (T : in out Test) is
      pragma Unreferenced (T);
      procedure Check_Parse_Error (Content : in String);

      procedure Check_Parse_Error (Content : in String) is
         P : Parser;
         R : Util.Beans.Objects.Readers.Reader;
      begin
         P.Parse_String (Content, R);
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

      procedure Check_Parse (Content : in String);

      procedure Check_Parse (Content : in String) is
         P    : Parser;
         R    : Util.Beans.Objects.Readers.Reader;
         Root : Util.Beans.Objects.Object;
      begin
         P.Parse_String (Content, R);
         Root := R.Get_Root;
         T.Assert (not Util.Beans.Objects.Is_Null (Root), "Null result for " & Content);

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
      Check_Parse ("[{ ""person"":""\u2ABF""}]");
      Check_Parse ("""testt""");
      Check_Parse ("""""");
      Check_Parse ("123");
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
      Path   : constant String := Util.Tests.Get_Test_Path ("test-stream.json");
   begin
      File.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Path);
      Buffer.Initialize (Output => File'Unchecked_Access, Size => 10000);
      Stream.Initialize (Output => Buffer'Unchecked_Access);
      Write_Stream (Stream);
      Stream.Close;
      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Expect,
                                     Test    => Path,
                                     Message => "JSON output serialization");
   end Test_Output;

   --  ------------------------------
   --  Test the JSON output stream generation (simple JSON documents).
   --  ------------------------------
   procedure Test_Simple_Output (T : in out Test) is
      function Get_Array return String;
      function Get_Struct return String;
      function Get_Named_Struct return String;
      function Get_Integer return String;
      function Get_String return String;

      function Get_Array return String is
         Buffer : aliased Util.Streams.Texts.Print_Stream;
         Stream : Util.Serialize.IO.JSON.Output_Stream;
      begin
         Buffer.Initialize (Output => null, Size => 10000);
         Stream.Initialize (Output => Buffer'Unchecked_Access);
         Stream.Start_Document;
         Stream.Start_Array ("");
         Stream.Write_Entity ("", 23);
         Stream.Write_Entity ("", 45);
         Stream.End_Array ("");
         Stream.End_Document;
         Stream.Close;
         return Util.Streams.Texts.To_String (Buffer);
      end Get_Array;

      function Get_Struct return String is
         Buffer : aliased Util.Streams.Texts.Print_Stream;
         Stream : Util.Serialize.IO.JSON.Output_Stream;
      begin
         Buffer.Initialize (Output => null, Size => 10000);
         Stream.Initialize (Output => Buffer'Unchecked_Access);
         Stream.Start_Document;
         Stream.Start_Entity ("");
         Stream.Write_Entity ("age", 23);
         Stream.Write_Entity ("id", 45);
         Stream.End_Entity ("");
         Stream.End_Document;
         Stream.Close;
         return Util.Streams.Texts.To_String (Buffer);
      end Get_Struct;

      function Get_Named_Struct return String is
         Buffer : aliased Util.Streams.Texts.Print_Stream;
         Stream : Util.Serialize.IO.JSON.Output_Stream;
      begin
         Buffer.Initialize (Output => null, Size => 10000);
         Stream.Initialize (Output => Buffer'Unchecked_Access);
         Stream.Start_Document;
         Stream.Start_Entity ("name");
         Stream.Write_Entity ("age", 23);
         Stream.Write_Entity ("id", 45);
         Stream.End_Entity ("");
         Stream.End_Document;
         Stream.Close;
         return Util.Streams.Texts.To_String (Buffer);
      end Get_Named_Struct;

      function Get_Integer return String is
         Buffer : aliased Util.Streams.Texts.Print_Stream;
         Stream : Util.Serialize.IO.JSON.Output_Stream;
      begin
         Buffer.Initialize (Output => null, Size => 10000);
         Stream.Initialize (Output => Buffer'Unchecked_Access);
         Stream.Start_Document;
         Stream.Write_Entity ("", 23);
         Stream.End_Document;
         Stream.Close;
         return Util.Streams.Texts.To_String (Buffer);
      end Get_Integer;

      function Get_String return String is
         Buffer : aliased Util.Streams.Texts.Print_Stream;
         Stream : Util.Serialize.IO.JSON.Output_Stream;
      begin
         Buffer.Initialize (Output => null, Size => 10000);
         Stream.Initialize (Output => Buffer'Unchecked_Access);
         Stream.Start_Document;
         Stream.Write_Entity ("", "test");
         Stream.End_Document;
         Stream.Close;
         return Util.Streams.Texts.To_String (Buffer);
      end Get_String;

      A1 : constant String := Get_Array;
      S1 : constant String := Get_Struct;
      S2 : constant String := Get_Named_Struct;
      I1 : constant String := Get_Integer;
      S3 : constant String := Get_String;
   begin
      Log.Error ("Array: {0}", A1);
      Util.Tests.Assert_Equals (T, "[ 23, 45]", A1,
                                "Invalid JSON array");

      Log.Error ("Struct: {0}", S1);
      Util.Tests.Assert_Equals (T, "{""age"": 23,""id"": 45}", S1,
                                "Invalid JSON struct");

      Log.Error ("Struct: {0}", S2);
      Util.Tests.Assert_Equals (T, "{""name"":{""age"": 23,""id"": 45}}", S2,
                                "Invalid JSON struct");

      Log.Error ("Struct: {0}", I1);
      Util.Tests.Assert_Equals (T, " 23", I1,
                                "Invalid JSON struct");

      Log.Error ("Struct: {0}", S3);
      Util.Tests.Assert_Equals (T, """test""", S3,
                                "Invalid JSON struct");
   end Test_Simple_Output;

   --  ------------------------------
   --  Test the JSON output stream generation and parsing for nullable basic types.
   --  ------------------------------
   procedure Test_Nullable (T : in out Test) is
      Buffer : aliased Util.Streams.Texts.Print_Stream;
      Stream : Util.Serialize.IO.JSON.Output_Stream;
      S      : Util.Nullables.Nullable_String;
      B      : Util.Nullables.Nullable_Boolean;
      I      : Util.Nullables.Nullable_Integer;
      D      : Util.Nullables.Nullable_Time;
   begin
      Buffer.Initialize (Output => null, Size => 10000);
      Stream.Initialize (Output => Buffer'Unchecked_Access);
      Stream.Start_Document;
      Stream.Start_Entity ("");
      Stream.Write_Entity ("string", S);
      Stream.Write_Entity ("bool", B);
      Stream.Write_Entity ("int", I);
      Stream.Write_Entity ("date", D);
      Stream.End_Entity ("");
      Stream.End_Document;
      Stream.Close;
      declare
         Data : constant String := Util.Streams.Texts.To_String (Buffer);
      begin
         Log.Error ("JSON: {0}", Data);
         Util.Tests.Assert_Equals (T,
                                   "{""string"":null,""bool"":null,""int"":null,""date"":null}",
                                   Data,
                                   "Invalid JSON for nullable values");
      end;
   end Test_Nullable;

   --  ------------------------------
   --  Test reading a JSON content into an Object tree.
   --  ------------------------------
   procedure Test_Read (T : in out Test) is
      use Util.Beans.Objects;
      Path  : constant String := Util.Tests.Get_Path ("regtests/files/pass01.json");
      Root  : Util.Beans.Objects.Object;
      Value : Util.Beans.Objects.Object;
      Item  : Util.Beans.Objects.Object;
   begin
      Root := Read (Path);
      T.Assert (not Util.Beans.Objects.Is_Null (Root), "Read should not return null object");
      T.Assert (Util.Beans.Objects.Is_Array (Root), "Root object is an array");

      Value := Util.Beans.Objects.Get_Value (Root, 1);
      Util.Tests.Assert_Equals (T, "JSON Test Pattern pass1",
                                Util.Beans.Objects.To_String (Value), "Invalid first element");
      Value := Util.Beans.Objects.Get_Value (Root, 4);
      T.Assert (Util.Beans.Objects.Is_Array (Value), "Element 4 should be an empty array");
      Util.Tests.Assert_Equals (T, 0, Util.Beans.Objects.Get_Count (Value), "Invalid array");

      Value := Util.Beans.Objects.Get_Value (Root, 8);
      T.Assert (Util.Beans.Objects.Is_Null (Value), "Element 8 should be null");

      Value := Util.Beans.Objects.Get_Value (Root, 9);
      T.Assert (not Util.Beans.Objects.Is_Null (Value), "Element 9 should not be null");

      Item := Util.Beans.Objects.Get_Value (Value, "integer");
      Util.Tests.Assert_Equals (T, 1234567890, Util.Beans.Objects.To_Integer (Item),
                                "Invalid integer value");

      Item := Util.Beans.Objects.Get_Value (Value, "zero");
      Util.Tests.Assert_Equals (T, 0, Util.Beans.Objects.To_Integer (Item),
                                "Invalid integer value (0)");

      Item := Util.Beans.Objects.Get_Value (Value, "one");
      Util.Tests.Assert_Equals (T, 1, Util.Beans.Objects.To_Integer (Item),
                                "Invalid integer value (1)");

      Item := Util.Beans.Objects.Get_Value (Value, "true");
      T.Assert (Util.Beans.Objects.Get_Type (Item) = TYPE_BOOLEAN,
                "The value true should be a boolean");
      T.Assert (Util.Beans.Objects.To_Boolean (Item),
                "The value true should be... true!");

      Item := Util.Beans.Objects.Get_Value (Value, "false");
      T.Assert (Util.Beans.Objects.Get_Type (Item) = TYPE_BOOLEAN,
                "The value false should be a boolean");
      T.Assert (not Util.Beans.Objects.To_Boolean (Item),
                "The value false should be... false!");

      Item := Util.Beans.Objects.Get_Value (Value, " s p a c e d ");
      T.Assert (Is_Array (Item), "The value should be an array");
      for I in 1 .. 7 loop
         Util.Tests.Assert_Equals (T, I, To_Integer (Get_Value (Item, I)),
                                   "Invalid array value at " & Integer'Image (I));
      end loop;
   end Test_Read;

   --  ------------------------------
   --  Test writing a JSON content from an Object tree.
   --  ------------------------------
   procedure Test_Write (T : in out Test) is
      use Util.Beans.Objects;
      Path     : constant String := Util.Tests.Get_Path ("regtests/files/pass01.json");
      Out_Path : constant String := Util.Tests.Get_Test_Path ("test-write.json");
      Root     : Util.Beans.Objects.Object;
      Value    : Util.Beans.Objects.Object;
      Item     : Util.Beans.Objects.Object;
      File     : aliased Util.Streams.Files.File_Stream;
      Print    : aliased Util.Streams.Texts.Print_Stream;
      Output   : Util.Serialize.IO.JSON.Output_Stream;
   begin
      Root := Read (Path);
      T.Assert (not Util.Beans.Objects.Is_Null (Root), "Read should not return null object");
      T.Assert (Util.Beans.Objects.Is_Array (Root), "Root object is an array");

      File.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Out_Path);
      Print.Initialize (File'Unchecked_Access);
      Output.Initialize (Print'Unchecked_Access);
      Output.Write_Entity ("", Root);
      Output.Close;

      Root := Read (Out_Path);
      Value := Util.Beans.Objects.Get_Value (Root, 1);
      Util.Tests.Assert_Equals (T, "JSON Test Pattern pass1",
                                Util.Beans.Objects.To_String (Value), "Invalid first element");
      Value := Util.Beans.Objects.Get_Value (Root, 4);
      T.Assert (Util.Beans.Objects.Is_Array (Value), "Element 4 should be an empty array");
      Util.Tests.Assert_Equals (T, 0, Util.Beans.Objects.Get_Count (Value), "Invalid array");

      Value := Util.Beans.Objects.Get_Value (Root, 8);
      T.Assert (Util.Beans.Objects.Is_Null (Value), "Element 8 should be null");

      Value := Util.Beans.Objects.Get_Value (Root, 9);
      T.Assert (not Util.Beans.Objects.Is_Null (Value), "Element 9 should not be null");

      Item := Util.Beans.Objects.Get_Value (Value, "integer");
      Util.Tests.Assert_Equals (T, 1234567890, Util.Beans.Objects.To_Integer (Item),
                                "Invalid integer value");

      Item := Util.Beans.Objects.Get_Value (Value, "zero");
      Util.Tests.Assert_Equals (T, 0, Util.Beans.Objects.To_Integer (Item),
                                "Invalid integer value (0)");

      Item := Util.Beans.Objects.Get_Value (Value, "one");
      Util.Tests.Assert_Equals (T, 1, Util.Beans.Objects.To_Integer (Item),
                                "Invalid integer value (1)");

      Item := Util.Beans.Objects.Get_Value (Value, "true");
      T.Assert (Util.Beans.Objects.Get_Type (Item) = TYPE_BOOLEAN,
                "The value true should be a boolean");
      T.Assert (Util.Beans.Objects.To_Boolean (Item),
                "The value true should be... true!");

      Item := Util.Beans.Objects.Get_Value (Value, "false");
      T.Assert (Util.Beans.Objects.Get_Type (Item) = TYPE_BOOLEAN,
                "The value false should be a boolean");
      T.Assert (not Util.Beans.Objects.To_Boolean (Item),
                "The value false should be... false!");

      Item := Util.Beans.Objects.Get_Value (Value, " s p a c e d ");
      T.Assert (Is_Array (Item), "The value should be an array");
      for I in 1 .. 7 loop
         Util.Tests.Assert_Equals (T, I, To_Integer (Get_Value (Item, I)),
                                   "Invalid array value at " & Integer'Image (I));
      end loop;
   end Test_Write;

   --  ------------------------------
   --  Test writing a JSON content from an Object tree.
   --  ------------------------------
   procedure Test_Write_Complex (T : in out Test) is
      use Util.Beans.Objects;
      Path     : constant String := Util.Tests.Get_Path ("regtests/files/pass01.json");
      Out_Path : constant String := Util.Tests.Get_Test_Path ("test-write-complex.json");
      Root     : Util.Beans.Objects.Object;
      Value    : Util.Beans.Objects.Object;
      Item     : Util.Beans.Objects.Object;
      File     : aliased Util.Streams.Files.File_Stream;
      Print    : aliased Util.Streams.Texts.Print_Stream;
      Output   : Util.Serialize.IO.JSON.Output_Stream;
      Empty    : constant Util.Beans.Objects.Object := Util.Beans.Objects.Maps.Create;
   begin
      Root := Read (Path);
      T.Assert (not Util.Beans.Objects.Is_Null (Root), "Read should not return null object");
      T.Assert (Util.Beans.Objects.Is_Array (Root), "Root object is an array");

      File.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Out_Path);
      Print.Initialize (File'Unchecked_Access);
      Output.Initialize (Print'Unchecked_Access);
      Output.Start_Entity ("");
      Output.Write_Entity ("count", 23);
      Output.Write_Entity ("event", Root);
      Output.Write_Entity ("empty", Empty);
      Output.End_Entity ("");
      Output.Close;

      Root := Read (Out_Path);
      T.Assert (not Util.Beans.Objects.Is_Null (Root), "Root should not be null");

      Value := Util.Beans.Objects.Get_Value (Root, "count");
      Util.Tests.Assert_Equals (T, "23",
                                Util.Beans.Objects.To_String (Value), "Invalid count");
      T.Assert (Util.Beans.Objects.Get_Type (Value) = Util.Beans.Objects.TYPE_INTEGER,
                "Invalid type for count");

      Root := Util.Beans.Objects.Get_Value (Root, "event");
      Value := Util.Beans.Objects.Get_Value (Root, 1);

      Util.Tests.Assert_Equals (T, "JSON Test Pattern pass1",
                                Util.Beans.Objects.To_String (Value), "Invalid first element");

      Value := Util.Beans.Objects.Get_Value (Root, 4);
      T.Assert (Util.Beans.Objects.Is_Array (Value), "Element 4 should be an empty array");
      Util.Tests.Assert_Equals (T, 0, Util.Beans.Objects.Get_Count (Value), "Invalid array");

      Value := Util.Beans.Objects.Get_Value (Root, 8);
      T.Assert (Util.Beans.Objects.Is_Null (Value), "Element 8 should be null");

      Value := Util.Beans.Objects.Get_Value (Root, 9);
      T.Assert (not Util.Beans.Objects.Is_Null (Value), "Element 9 should not be null");

      Item := Util.Beans.Objects.Get_Value (Value, "integer");
      Util.Tests.Assert_Equals (T, 1234567890, Util.Beans.Objects.To_Integer (Item),
                                "Invalid integer value");

      Item := Util.Beans.Objects.Get_Value (Value, "zero");
      Util.Tests.Assert_Equals (T, 0, Util.Beans.Objects.To_Integer (Item),
                                "Invalid integer value (0)");

      Item := Util.Beans.Objects.Get_Value (Value, "one");
      Util.Tests.Assert_Equals (T, 1, Util.Beans.Objects.To_Integer (Item),
                                "Invalid integer value (1)");

      Item := Util.Beans.Objects.Get_Value (Value, "true");
      T.Assert (Util.Beans.Objects.Get_Type (Item) = TYPE_BOOLEAN,
                "The value true should be a boolean");
      T.Assert (Util.Beans.Objects.To_Boolean (Item),
                "The value true should be... true!");

      Item := Util.Beans.Objects.Get_Value (Value, "false");
      T.Assert (Util.Beans.Objects.Get_Type (Item) = TYPE_BOOLEAN,
                "The value false should be a boolean");
      T.Assert (not Util.Beans.Objects.To_Boolean (Item),
                "The value false should be... false!");

      Item := Util.Beans.Objects.Get_Value (Value, " s p a c e d ");
      T.Assert (Is_Array (Item), "The value should be an array");
      for I in 1 .. 7 loop
         Util.Tests.Assert_Equals (T, I, To_Integer (Get_Value (Item, I)),
                                   "Invalid array value at " & Integer'Image (I));
      end loop;
   end Test_Write_Complex;

end Util.Serialize.IO.JSON.Tests;
