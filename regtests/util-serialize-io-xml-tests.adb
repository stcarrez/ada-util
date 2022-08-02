-----------------------------------------------------------------------
--  serialize-io-xml-tests -- Unit tests for XML serialization
--  Copyright (C) 2011, 2012, 2016, 2017, 2018, 2021, 2022 Stephane Carrez
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
with Util.Log.Loggers;
with Util.Streams.Files;
with Util.Serialize.IO.JSON.Tests;
with Util.Serialize.Mappers.Record_Mapper;
package body Util.Serialize.IO.XML.Tests is

   use Util.Log;
   use Util.Tests;
   use Ada.Strings.Unbounded;

   Log : constant Loggers.Logger := Loggers.Create ("Util.Serialize.IO.Tests");

   type Map_Test is record
      Value : Natural := 0;
      Bool  : Boolean := False;
      Name  : Unbounded_String;
      Node  : Util.Beans.Objects.Object;
   end record;
   type Map_Test_Access is access all Map_Test;

   type Map_Test_Fields is (FIELD_VALUE, FIELD_BOOL, FIELD_NAME, FIELD_NODE);
   function Get_Member (P : in Map_Test;
                        Field : in Map_Test_Fields) return Util.Beans.Objects.Object;
   procedure Set_Member (P     : in out Map_Test;
                         Field : in Map_Test_Fields;
                         Value : in Util.Beans.Objects.Object);

   procedure Set_Member (P     : in out Map_Test;
                         Field : in Map_Test_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_VALUE =>
            P.Value := Natural (Util.Beans.Objects.To_Integer (Value));

         when FIELD_BOOL =>
            P.Bool := Util.Beans.Objects.To_Boolean (Value);

         when FIELD_NAME =>
            P.Name := Util.Beans.Objects.To_Unbounded_String (Value);
            if P.Name = "raise-field-error" then
               raise Util.Serialize.Mappers.Field_Error with "Testing Field_Error exception";
            end if;
            if P.Name = "raise-field-fatal-error" then
               raise Util.Serialize.Mappers.Field_Fatal_Error with "Testing Fatal_Error exception";
            end if;

         when FIELD_NODE =>
            P.Node := Value;
      end case;
   end Set_Member;

   function Get_Member (P : in Map_Test;
                        Field : in Map_Test_Fields) return Util.Beans.Objects.Object is
   begin
      case Field is
         when FIELD_VALUE =>
            return Util.Beans.Objects.To_Object (P.Value);

         when FIELD_BOOL =>
            return Util.Beans.Objects.To_Object (P.Bool);

         when FIELD_NAME =>
            return Util.Beans.Objects.To_Object (P.Name);

         when FIELD_NODE =>
            return P.Node;

      end case;
   end Get_Member;

   package Map_Test_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Map_Test,
                                               Element_Type_Access => Map_Test_Access,
                                               Fields              => Map_Test_Fields,
                                               Set_Member          => Set_Member);

   package Caller is new Util.Test_Caller (Test, "Serialize.IO.XML");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.XML.Parser",
                       Test_Parser'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.XML.Parser2",
                       Test_Parser2'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.XML.Parser Wildcard Mapping",
                       Test_Parser_Wildcard_Mapping'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.XML.Parser Deep_Wildcard Mapping",
                       Test_Parser_Deep_Wildcard_Mapping'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.XML.Parser_Error",
                       Test_Parser_Error'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.XML.Write",
                       Test_Writer'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.XML.Write",
                       Test_Output'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test XML de-serialization
   --  ------------------------------
   procedure Test_Parser (T : in out Test) is
      Mapping : aliased Map_Test_Mapper.Mapper;
      Result  : aliased Map_Test;

      Reader  : Util.Serialize.IO.XML.Parser;
      Mapper  : Util.Serialize.Mappers.Processing;
   begin
      Mapping.Add_Mapping ("name", FIELD_NAME);
      Mapping.Add_Mapping ("value", FIELD_VALUE);
      Mapping.Add_Mapping ("status", FIELD_BOOL);
      Mapping.Add_Mapping ("@bool", FIELD_BOOL);
      Mapping.Add_Mapping ("@id", FIELD_VALUE);
      Mapper.Add_Mapping ("info/node", Mapping'Unchecked_Access);

      Map_Test_Mapper.Set_Context (Mapper, Result'Unchecked_Access);

      --  Extract XML and check name, value, status
      Reader.Parse_String ("<info><node><name>A</name><value>2</value>"
                           & "<status>1</status></node></info>", Mapper);
      T.Assert (not Reader.Has_Error, "The parser indicates an error");
      Assert_Equals (T, "A", Result.Name, "Invalid name");
      Assert_Equals (T, 2, Result.Value, "Invalid value");
      T.Assert (Result.Bool, "Invalid boolean");

      --  Another extraction.
      Map_Test_Mapper.Set_Context (Mapper, Result'Unchecked_Access);
      Reader.Parse_String ("<info><node><name>B</name><value>20</value>"
                           & "<status>0</status></node></info>", Mapper);
      T.Assert (not Reader.Has_Error, "The parser indicates an error");
      Assert_Equals (T, "B", Result.Name, "Invalid name");
      Assert_Equals (T, 20, Result.Value, "Invalid value");
      T.Assert (not Result.Bool, "Invalid boolean");

      --  Another extraction using attribute mappings.
      Reader.Parse_String ("<info><node id='23' bool='true'><name>TOTO</name></node></info>",
                           Mapper);
      T.Assert (not Reader.Has_Error, "The parser indicates an error");
      Assert_Equals (T, "TOTO", Result.Name, "Invalid name");
      Assert_Equals (T, 23, Result.Value, "Invalid value");
      T.Assert (Result.Bool, "Invalid boolean");

   end Test_Parser;

   --  ------------------------------
   --  Test XML de-serialization
   --  ------------------------------
   procedure Test_Parser2 (T : in out Test) is
      use type Util.Beans.Objects.Data_Type;

      Mapping : aliased Map_Test_Mapper.Mapper;
      Result  : aliased Map_Test;

      Reader  : Util.Serialize.IO.XML.Parser;
      Mapper  : Util.Serialize.Mappers.Processing;
   begin
      Mapping.Add_Mapping ("node/name", FIELD_NAME);
      Mapping.Add_Mapping ("node/value", FIELD_VALUE);
      Mapping.Add_Mapping ("node/status", FIELD_BOOL);
      Mapping.Add_Mapping ("node/@bool", FIELD_BOOL);
      Mapping.Add_Mapping ("node/@id", FIELD_VALUE);
      Mapping.Add_Mapping ("node", FIELD_NODE);
      Mapper.Add_Mapping ("info", Mapping'Unchecked_Access);

      Map_Test_Mapper.Set_Context (Mapper, Result'Unchecked_Access);

      Result.Node := Util.Beans.Objects.Null_Object;
      --  Extract XML and check name, value, status
      Reader.Parse_String ("<info><node><name>A</name><value>2</value>"
                           & "<status>1</status></node></info>", Mapper);
      T.Assert (not Reader.Has_Error, "The parser indicates an error");
      Assert_Equals (T, "A", Result.Name, "Invalid name");
      Assert_Equals (T, 2, Result.Value, "Invalid value");
      T.Assert (Result.Bool, "Invalid boolean");
      T.Assert (Util.Beans.Objects.Get_Type (Result.Node) = Util.Beans.Objects.TYPE_STRING,
                "Invalid node type");

      --  Another extraction.
      Reader.Parse_String ("<info><node><name>B</name><value>20</value>"
                           & "<status>0</status></node></info>", Mapper);
      Assert_Equals (T, "B", Result.Name, "Invalid name");
      Assert_Equals (T, 20, Result.Value, "Invalid value");
      T.Assert (not Result.Bool, "Invalid boolean");

      --  Another extraction using attribute mappings.
      Reader.Parse_String ("<info><node id='23' bool='true'><name>TOTO</name></node></info>",
                           Mapper);
      Assert_Equals (T, "TOTO", Result.Name, "Invalid name");
      Assert_Equals (T, 23, Result.Value, "Invalid value");
      T.Assert (Result.Bool, "Invalid boolean");

   end Test_Parser2;

   --  -----------------------
   --  Test wildcard mapping for serialization.
   --  -----------------------
   procedure Test_Parser_Wildcard_Mapping (T : in out Test) is
      use type Util.Beans.Objects.Data_Type;

      Mapping : aliased Map_Test_Mapper.Mapper;
      Result  : aliased Map_Test;

      Reader  : Util.Serialize.IO.XML.Parser;
      Mapper  : Util.Serialize.Mappers.Processing;
   begin
      Mapping.Add_Mapping ("node/*/status", FIELD_BOOL);
      Mapping.Add_Mapping ("node/*/name", FIELD_NAME);
      Mapping.Add_Mapping ("node/*/value", FIELD_VALUE);
      Mapping.Add_Mapping ("node/*/name/@bool", FIELD_BOOL);
      Mapping.Add_Mapping ("node/@id", FIELD_VALUE);
      Mapping.Add_Mapping ("node", FIELD_NODE);
      Mapper.Add_Mapping ("info", Mapping'Unchecked_Access);

      Map_Test_Mapper.Set_Context (Mapper, Result'Unchecked_Access);

      Result.Node := Util.Beans.Objects.Null_Object;
      --  Extract XML and check name, value, status
      Reader.Parse_String ("<info><node><inner><name>A</name><value>2</value></inner>"
                           & "<status>1</status></node></info>", Mapper);
      T.Assert (not Reader.Has_Error, "The parser indicates an error");
      Assert_Equals (T, "A", Result.Name, "Invalid name");
      Assert_Equals (T, 2, Result.Value, "Invalid value");
      T.Assert (Result.Bool, "Invalid boolean");
      T.Assert (Util.Beans.Objects.Get_Type (Result.Node) = Util.Beans.Objects.TYPE_STRING,
                "Invalid node type");

      --  Another extraction.
      Reader.Parse_String ("<info><node><b><c><d><name>B</name><value>20</value></d></c></b>"
                           & "<status>0</status></node></info>", Mapper);
      Assert_Equals (T, "B", Result.Name, "Invalid name");
      Assert_Equals (T, 20, Result.Value, "Invalid value");
      T.Assert (not Result.Bool, "Invalid boolean");

      --  Another extraction using attribute mappings.
      Reader.Parse_String ("<info><node id='23'><name bool='true'>TOTO</name></node></info>",
                           Mapper);
      Assert_Equals (T, "TOTO", Result.Name, "Invalid name");
      Assert_Equals (T, 23, Result.Value, "Invalid value");
      T.Assert (Result.Bool, "Invalid boolean");

   end Test_Parser_Wildcard_Mapping;

   --  -----------------------
   --  Test (**) wildcard mapping for serialization.
   --  -----------------------
   procedure Test_Parser_Deep_Wildcard_Mapping (T : in out Test) is
      use type Util.Beans.Objects.Data_Type;

      Mapping : aliased Map_Test_Mapper.Mapper;
      Result  : aliased Map_Test;

      Reader  : Util.Serialize.IO.XML.Parser;
      Mapper  : Util.Serialize.Mappers.Processing;
   begin
      Mapping.Add_Mapping ("**/node/@id", FIELD_VALUE);
      Mapping.Add_Mapping ("**/node/name", FIELD_NAME);
      Mapping.Add_Mapping ("**/node/name/@bool", FIELD_BOOL);
      Mapping.Add_Mapping ("**/node", FIELD_NODE);
      Mapping.Dump (Log);

      Mapper.Add_Mapping ("info", Mapping'Unchecked_Access);

      Map_Test_Mapper.Set_Context (Mapper, Result'Unchecked_Access);

      Mapper.Dump (Log);

      Result.Node := Util.Beans.Objects.Null_Object;

      --  Extract XML and check name, value, status
      Reader.Parse_String ("<info><node><node id='2'><name bool='1'>A</name>"
                           & "<value>2</value></node>"
                           & "<status>1</status></node></info>", Mapper);
      T.Assert (not Reader.Has_Error, "The parser indicates an error");
      Assert_Equals (T, "A", Result.Name, "Invalid name");
      Assert_Equals (T, 2, Result.Value, "Invalid value");
      T.Assert (Result.Bool, "Invalid boolean");
      T.Assert (Util.Beans.Objects.Get_Type (Result.Node) = Util.Beans.Objects.TYPE_STRING,
                "Invalid node type");

      Reader.Parse_String ("<info><node><a><b><node id='3'><name bool='0'>B</name>"
                           & "<d><value>2</value></d></node></b></a>"
                           & "<status>1</status></node></info>", Mapper);
      T.Assert (not Reader.Has_Error, "The parser indicates an error");
      Assert_Equals (T, "B", Result.Name, "Invalid name");
      Assert_Equals (T, 3, Result.Value, "Invalid value");
      T.Assert (not Result.Bool, "Invalid boolean");
      T.Assert (Util.Beans.Objects.Get_Type (Result.Node) = Util.Beans.Objects.TYPE_STRING,
                "Invalid node type");

   end Test_Parser_Deep_Wildcard_Mapping;

   --  ------------------------------
   --  Test XML de-serialization with some errors.
   --  ------------------------------
   procedure Test_Parser_Error (T : in out Test) is

      procedure Check_Error (Content : in String;
                             Msg     : in String);

      procedure Check_Error (Content : in String;
                             Msg     : in String) is

         Mapping : aliased Map_Test_Mapper.Mapper;
         Result  : aliased Map_Test;

         Reader  : Util.Serialize.IO.XML.Parser;
         Mapper  : Util.Serialize.Mappers.Processing;
      begin
         Mapping.Add_Mapping ("node/name", FIELD_NAME);
         Mapping.Add_Mapping ("node/value", FIELD_VALUE);
         Mapping.Add_Mapping ("node/status", FIELD_BOOL);
         Mapping.Add_Mapping ("node/@bool", FIELD_BOOL);
         Mapping.Add_Mapping ("node/@id", FIELD_VALUE);
         Mapping.Add_Mapping ("node", FIELD_NODE);
         Mapper.Add_Mapping ("info", Mapping'Unchecked_Access);

         Map_Test_Mapper.Set_Context (Mapper, Result'Unchecked_Access);

         Result.Node := Util.Beans.Objects.Null_Object;

         --  Extract XML and check name, value, status
         Reader.Parse_String (Content, Mapper);
         T.Assert (Reader.Has_Error, "No error reported by the parser for an invalid XML: " & Msg);
      end Check_Error;

   begin
      Check_Error ("<info><node><name>A</name><value>2</value>"
                   & "<status>1</status></node>", "XML element is not closed");
      Check_Error ("<info><node><name attr=>A</name><value>2</value>"
                   & "<status>1</status></node></info>", "XML attribute is not correct");
      Check_Error ("<info><node><name attr='x'>A</name><value>&something;</value>"
                   & "<status>1</status></node></info>", "XML attribute is not correct");
      Check_Error ("<info><node><name attr='x'>A</name><value>raise-field-error</value>"
                   & "<status>1</status></node></info>", "Field_Error exception");
      Check_Error ("<info><node><name attr='x'>A</name><value>raise-fatal-error</value>"
                   & "<status>1</status></node></info>", "Field_Error exception");
      Check_Error ("<info><node><name attr='x'>raise-field-error</name><value>3</value>"
                   & "<status>1</status></node></info>", "Field_Error exception");
      Check_Error ("<info><node><name attr='x'>raise-field-fatal-error</name><value>3</value>"
                   & "<status>1</status></node></info>", "Field_Error exception");
   end Test_Parser_Error;

   --  ------------------------------
   --  Test XML serialization
   --  ------------------------------
   procedure Test_Writer (T : in out Test) is
      function Serialize (Value : in Map_Test) return String;

      Mapping : aliased Map_Test_Mapper.Mapper;
      Result  : aliased Map_Test;
      Mapper  : Util.Serialize.Mappers.Processing;

      function Serialize (Value : in Map_Test) return String is
         Buffer : aliased Util.Streams.Texts.Print_Stream;
         Output : Util.Serialize.IO.XML.Output_Stream;
      begin
         Buffer.Initialize (Size => 10000);
         Output.Initialize (Output => Buffer'Unchecked_Access);
         Mapping.Write (Output, Value);
         return Util.Streams.Texts.To_String (Buffer);
      end Serialize;

   begin
      Mapping.Add_Mapping ("name", FIELD_NAME);
      Mapping.Add_Mapping ("value", FIELD_VALUE);
      Mapping.Add_Mapping ("status", FIELD_BOOL);
--        Mapping.Add_Mapping ("@bool", FIELD_BOOL);
--        Mapping.Add_Mapping ("@id", FIELD_VALUE);
      Mapping.Bind (Get_Member'Access);
      Mapper.Add_Mapping ("info/node", Mapping'Unchecked_Access);

      Result.Name  := To_Unbounded_String ("test");
      Result.Bool  := False;
      Result.Value := 23;
      declare
         XML : constant String := Serialize (Result);
      begin
         Log.Info ("Serialize XML: {0}", XML);
         T.Assert (XML'Length > 0, "Invalid XML serialization");
      end;
   end Test_Writer;

   --  ------------------------------
   --  Test the XML output stream generation.
   --  ------------------------------
   procedure Test_Output (T : in out Test) is
      File   : aliased Util.Streams.Files.File_Stream;
      Buffer : aliased Util.Streams.Texts.Print_Stream;
      Stream : Util.Serialize.IO.XML.Output_Stream;
      Expect : constant String := Util.Tests.Get_Path ("regtests/expect/test-stream.xml");
      Path   : constant String := Util.Tests.Get_Test_Path ("test-stream.xml");
   begin
      File.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Path);
      Buffer.Initialize (Output => File'Unchecked_Access, Size => 10000);
      Stream.Initialize (Output => Buffer'Unchecked_Access);
      Util.Serialize.IO.JSON.Tests.Write_Stream (Stream);
      Stream.Close;
      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Expect,
                                     Test    => Path,
                                     Message => "XML output serialization");
   end Test_Output;

end Util.Serialize.IO.XML.Tests;
