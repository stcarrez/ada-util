-----------------------------------------------------------------------
--  util-beans-objects-datasets-tests -- Unit tests for dataset beans
--  Copyright (C) 2013, 2015, 2017, 2021, 2022 Stephane Carrez
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
with Util.Beans.Objects.Iterators;

package body Util.Beans.Objects.Datasets.Tests is

   package Caller is new Util.Test_Caller (Test, "Beans.Datasets");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.Datasets",
                       Test_Fill_Dataset'Access);
   end Add_Tests;

   --  Test the creation, initialization and retrieval of dataset content.
   procedure Test_Fill_Dataset (T : in out Test) is
      procedure Fill (Row : in out Object_Array);

      Set : aliased Dataset;

      procedure Fill (Row : in out Object_Array) is
      begin
         Row (Row'First)     := To_Object (String '("john"));
         Row (Row'First + 1) := To_Object (String '("jj@gmail.com"));
         Row (Row'First + 2) := To_Object (Set.Get_Count);
      end Fill;

   begin
      Set.Add_Column ("name");
      Set.Add_Column ("email");
      Set.Add_Column ("age");

      for I in 1 .. 100 loop
         Set.Append (Fill'Access);
      end loop;
      Util.Tests.Assert_Equals (T, 100, Set.Get_Count, "Invalid number of rows");

      for I in 1 .. 100 loop
         Set.Set_Row_Index (I);
         declare
            R : constant Object := Set.Get_Row;
         begin
            T.Assert (not Is_Null (R), "Row is null");
            Util.Tests.Assert_Equals (T, "john", To_String (Get_Value (R, "name")),
                                      "Invalid 'name' attribute");
            Util.Tests.Assert_Equals (T, I, To_Integer (Get_Value (R, "age")),
                                      "Invalid 'age' attribute");
         end;
      end loop;

      declare
         List  : constant Object := To_Object (Set'Unchecked_Access, STATIC);
         Iter  : Iterators.Iterator := Iterators.First (List);
         Count : Natural := 0;
      begin
         while Iterators.Has_Element (Iter) loop
            Count := Count + 1;
            declare
               R : constant Object := Iterators.Element (Iter);
            begin
               T.Assert (not Is_Null (R), "Row is null");
               Util.Tests.Assert_Equals (T, "john", To_String (Get_Value (R, "name")),
                                         "Invalid 'name' attribute");
               Util.Tests.Assert_Equals (T, Count, To_Integer (Get_Value (R, "age")),
                                         "Invalid 'age' attribute");
            end;
            Iterators.Next (Iter);
         end loop;
         Util.Tests.Assert_Equals (T, 100, Count, "Invalid number of rows");
      end;
   end Test_Fill_Dataset;

end Util.Beans.Objects.Datasets.Tests;
