-----------------------------------------------------------------------
--  util-beans-objects-record_tests -- Unit tests for objects.records package
--  Copyright (C) 2011, 2017, 2021, 2022 Stephane Carrez
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

with Util.Test_Caller;

with Util.Strings;
with Util.Beans.Basic;
with Util.Beans.Objects.Vectors;
with Util.Beans.Objects.Records;
package body Util.Beans.Objects.Record_Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Beans.Records");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.Records",
                       Test_Record'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Basic",
                       Test_Bean'Access);
   end Add_Tests;

   type Data is record
      Name  : Unbounded_String;
      Value : Util.Beans.Objects.Object;
   end record;

   package Data_Bean is new Util.Beans.Objects.Records (Data);
   use type Data_Bean.Element_Type_Access;
   subtype Data_Access is Data_Bean.Element_Type_Access;

   procedure Test_Record (T : in out Test) is
      D : Data;
   begin
      D.Name  := To_Unbounded_String ("testing");
      D.Value := To_Object (Integer (23));

      declare
         V  : Object := Data_Bean.To_Object (D);
         P  : constant Data_Access := Data_Bean.To_Element_Access (V);
         V2 : constant Object := V;
      begin
         T.Assert (not Is_Empty (V), "Object with data record should not be empty");
         T.Assert (not Is_Null (V), "Object with data record should not be null");
         T.Assert (P /= null, "To_Element_Access returned null");
         Assert_Equals (T, "testing", To_String (P.Name), "Data name is not the same");
         Assert_Equals (T, 23, To_Integer (P.Value), "Data value is not the same");

         V := Data_Bean.Create;

         declare
            D2 : constant Data_Access := Data_Bean.To_Element_Access (V);
         begin
            T.Assert (D2 /= null, "Null element");
            D2.Name  := To_Unbounded_String ("second test");
            D2.Value := V2;
         end;

         V := Data_Bean.To_Object (D);
         Assert_Equals (T, "<UTIL.BEANS.OBJECTS.RECORD_TESTS.DATA_BEAN.ELEMENT_PROXY>",
                        To_String (V), "Data name is not the same");
      end;
   end Test_Record;

   type Bean_Type is new Util.Beans.Basic.Readonly_Bean with record
      Name : Unbounded_String;
   end record;
   type Bean_Type_Access is access all Bean_Type'Class;

   overriding
   function Get_Value (Bean : in Bean_Type;
                       Name : in String) return Util.Beans.Objects.Object;

   overriding
   function Get_Value (Bean : in Bean_Type;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "name" then
         return Util.Beans.Objects.To_Object (Bean.Name);
      elsif Name = "length" then
         return Util.Beans.Objects.To_Object (Length (Bean.Name));
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   procedure Test_Bean (T : in out Test) is
      use Basic;
      Static : aliased Bean_Type;
   begin
      Static.Name := To_Unbounded_String ("Static");

      --  Allocate dynamically several Bean_Type objects and drop the list.
      --  The memory held by internal proxy as well as the Bean_Type must be freed.
      --  The static bean should never be freed!
      for I in 1 .. 10 loop
         declare
            List  : Util.Beans.Objects.Vectors.Vector;
            Value : Util.Beans.Objects.Object;
            Bean  : Bean_Type_Access;
            P     : access Readonly_Bean'Class;
         begin
            for J in 1 .. 1_000 loop
               if I = J then
                  Value := To_Object (Static'Unchecked_Access, Objects.STATIC);
                  List.Append (Value);
               end if;
               Bean := new Bean_Type;
               Bean.Name := To_Unbounded_String ("B" & Util.Strings.Image (J));
               Value := To_Object (Bean);
               List.Append (Value);
            end loop;

            --  Verify each bean of the list
            for J in 1 .. 1_000 + 1 loop
               Value := List.Element (J);

               --  Check some common status.
               T.Assert (not Is_Null (Value), "The value should hold a bean");
               T.Assert (Get_Type (Value) = TYPE_BEAN, "The value should hold a bean");
               T.Assert (not Is_Empty (Value), "The value should not be empty");

               --  Check the bean access.
               P := To_Bean (Value);
               T.Assert (P /= null, "To_Bean returned null");
               Bean := Bean_Type'Class (P.all)'Unchecked_Access;

               --  Check we have the good bean object.
               if I = J then
                  Assert_Equals (T, "Static", To_String (Bean.Name),
                                 "Bean at" & Integer'Image (J) & " is invalid");
               elsif J > I then
                  Assert_Equals (T, "B" & Util.Strings.Image (J - 1), To_String (Bean.Name),
                                 "Bean at" & Integer'Image (J) & " is invalid");
               else
                  Assert_Equals (T, "B" & Util.Strings.Image (J), To_String (Bean.Name),
                                 "Bean at" & Integer'Image (J) & " is invalid");
               end if;
            end loop;
         end;
      end loop;
   end Test_Bean;

end Util.Beans.Objects.Record_Tests;
