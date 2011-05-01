-----------------------------------------------------------------------
--  util-beans-objects-record_tests -- Unit tests for objects.records package
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

with Util.Test_Caller;
with Util.Tests;

with Util.Beans.Objects.Records;
package body Util.Beans.Objects.Record_Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Concurrent.Counter.Increment",
                       Test_Record'Access);
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
      end;
   end Test_Record;

end Util.Beans.Objects.Record_Tests;
