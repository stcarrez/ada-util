-----------------------------------------------------------------------
--  serialize-io-xml-tests -- Unit tests for XML serialization
--  Copyright (C) 2011 Stephane Carrez
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
with Util.Tests;
with Util.Log.Loggers;
with Util.Streams.Buffered;
with Util.Serialize.IO.XML;
with Util.Serialize.Mappers.Record_Mapper;
package body Util.Serialize.IO.XML.Tests is

   use Util.Log;
   use Util.Tests;
   use Ada.Strings.Unbounded;

   Log : constant Loggers.Logger := Loggers.Create ("Util.Serialize.IO.Tests");

   type Map_Test is record
      Value : Natural;
      Bool  : Boolean;
      Name  : Unbounded_String;
   end record;
   type Map_Test_Access is access all Map_Test;

   type Map_Test_Fields is (FIELD_VALUE, FIELD_BOOL, FIELD_NAME);


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
      end case;
   end Get_Member;

   package Map_Test_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Map_Test,
                                               Element_Type_Access => Map_Test_Access,
                                               Fields              => Map_Test_Fields,
                                               Set_Member          => Set_Member);

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.XML.Parser",
                       Test_Parser'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.XML.Write",
                       Test_Writer'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test XML de-serialization
   --  ------------------------------
   procedure Test_Parser (T : in out Test) is
      Mapping : aliased Map_Test_Mapper.Mapper;
      Result  : aliased Map_Test;

      Reader  : Util.Serialize.IO.XML.Parser;
   begin
      Mapping.Add_Mapping ("name", FIELD_NAME);
      Mapping.Add_Mapping ("value", FIELD_VALUE);
      Mapping.Add_Mapping ("status", FIELD_BOOL);
      Mapping.Add_Mapping ("@bool", FIELD_BOOL);
      Mapping.Add_Mapping ("@id", FIELD_VALUE);
      Reader.Add_Mapping ("info/node", Mapping'Unchecked_Access);

      Map_Test_Mapper.Set_Context (Reader, Result'Unchecked_Access);

      --  Extract XML and check name, value, status
      Reader.Parse_String ("<info><node><name>A</name><value>2</value>"
                           & "<status>1</status></node></info>");
      Assert_Equals (T, "A", Result.Name, "Invalid name");
      Assert_Equals (T, 2, Result.Value, "Invalid value");
      Assert (T, Result.Bool, "Invalid boolean");

      --  Another extraction.
      Reader.Parse_String ("<info><node><name>B</name><value>20</value>"
                           & "<status>0</status></node></info>");
      Assert_Equals (T, "B", Result.Name, "Invalid name");
      Assert_Equals (T, 20, Result.Value, "Invalid value");
      Assert (T, not Result.Bool, "Invalid boolean");

      --  Another extraction using attribute mappings.
      Reader.Parse_String ("<info><node id='23' bool='true'><name>TOTO</name></node></info>");
      Assert_Equals (T, "TOTO", Result.Name, "Invalid name");
      Assert_Equals (T, 23, Result.Value, "Invalid value");
      Assert (T, Result.Bool, "Invalid boolean");

   end Test_Parser;

   --  ------------------------------
   --  Test XML serialization
   --  ------------------------------
   procedure Test_Writer (T : in out Test) is
      use Util.Streams.Buffered;
      Mapping : aliased Map_Test_Mapper.Mapper;
      Result  : aliased Map_Test;

      Reader  : Util.Serialize.IO.XML.Parser;

      function Serialize (Value : in Map_Test) return String is
         Output : Util.Serialize.IO.XML.Output_Stream;
      begin
         Output.Initialize (Size => 10000);
         Mapping.Write (Output, Value);
         return Util.Streams.Texts.To_String (Buffered_Stream (Output));
      end Serialize;

   begin
      Mapping.Add_Mapping ("name", FIELD_NAME);
      Mapping.Add_Mapping ("value", FIELD_VALUE);
      Mapping.Add_Mapping ("status", FIELD_BOOL);
--        Mapping.Add_Mapping ("@bool", FIELD_BOOL);
--        Mapping.Add_Mapping ("@id", FIELD_VALUE);
      Mapping.Bind (Get_Member'Access);
      Reader.Add_Mapping ("info/node", Mapping'Unchecked_Access);

      Result.Name  := To_Unbounded_String ("test");
      Result.Bool  := False;
      Result.Value := 23;
      declare
         XML : constant String := Serialize (Result);
      begin
         Log.Info ("Serialize XML: {0}", XML);
      end;
   end Test_Writer;

end Util.Serialize.IO.XML.Tests;
