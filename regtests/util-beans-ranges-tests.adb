-----------------------------------------------------------------------
--  util-beans-ranges-tests -- Unit tests for bean range definitions
--  Copyright (C) 2011, 2021 Stephane Carrez
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

package body Util.Beans.Ranges.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Beans.Ranges");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Beans.Ranges.Create",
                       Test_Range'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Ranges.Iterate",
                       Test_Iterate_Range'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the creation and range definition.
   --  ------------------------------
   procedure Test_Range (T : in out Test) is
      C : Integer_Ranges.Range_Bean := Integer_Ranges.Create (1, 10);
   begin
      Assert_Equals (T, 1, C.Get_First, "Invalid first range value");
      Assert_Equals (T, 10, C.Get_Last, "Invalid first range value");
      Assert_Equals (T, 10, C.Get_Count, "Invalid range count");

      C := Integer_Ranges.Create (10, 10);
      Assert_Equals (T, 10, C.Get_First, "Invalid first range value");
      Assert_Equals (T, 10, C.Get_Last, "Invalid first range value");
      Assert_Equals (T, 1, C.Get_Count, "Invalid range count");

   end Test_Range;

   --  ------------------------------
   --  Test iterating over a range definition.
   --  ------------------------------
   procedure Test_Iterate_Range (T : in out Test) is
      use Util.Beans.Objects;

      C     : aliased Integer_Ranges.Range_Bean := Integer_Ranges.Create (-3, 10);
      List  : constant Basic.List_Bean_Access := C'Unchecked_Access;
      Value : Util.Beans.Objects.Object;
   begin
      for I in 1 .. List.Get_Count loop
         List.Set_Row_Index (I);
         Value := List.Get_Row;
         Assert (T, not Util.Beans.Objects.Is_Null (Value), "Null row returned");
         Assert (T, Util.Beans.Objects.Get_Type (Value) = Util.Beans.Objects.TYPE_INTEGER,
                 "Invalid value type");
         Assert_Equals (T, -3 + Integer (I - 1), To_Integer (Value), "Invalid value");
      end loop;
   end Test_Iterate_Range;

end Util.Beans.Ranges.Tests;
