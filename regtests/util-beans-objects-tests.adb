-----------------------------------------------------------------------
--  util-beans-objects-tests -- Unit tests for objects
--  Copyright (C) 2017, 2021, 2022 Stephane Carrez
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
with Util.Beans.Objects.Maps;
package body Util.Beans.Objects.Tests is

   package Caller is new Util.Test_Caller (Test, "Beans.Objects");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.Is_Array",
                       Test_Is_Array'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.Get_Count",
                       Test_Get_Count'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.Get_Value",
                       Test_Get_Value'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.Set_Value",
                       Test_Set_Value'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.&",
                       Test_And_Operator'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.To_Blob",
                       Test_Blob'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the creation, initialization and retrieval of objects holding arrays.
   --  ------------------------------
   procedure Test_Is_Array (T : in out Test) is
      List  : Object_Array (1 .. 10);
      Value : Object;
   begin
      for I in List'Range loop
         List (I) := To_Object (I);
      end loop;
      Value := To_Object (List);
      T.Assert (not Is_Null (Value), "Array object must not be null");
      T.Assert (not Is_Empty (Value), "Array object must not be empty");
      T.Assert (Is_Array (Value), "Array object must be an array");
      for I in List'Range loop
         T.Assert (not Is_Array (List (I)), "Is_Array returned true for non array object");
      end loop;
      T.Assert (not Is_Array (Null_Object), "The Null_Object is not an array");
   end Test_Is_Array;

   --  ------------------------------
   --  Test the Get_Count operation.
   --  ------------------------------
   procedure Test_Get_Count (T : in out Test) is
      List  : Object_Array (1 .. 10);
      Value : Object;
   begin
      for I in List'Range loop
         List (I) := To_Object (I);
      end loop;
      Value := To_Object (List);
      Util.Tests.Assert_Equals (T, List'Length, Get_Count (Value), "Get_Count is invalid");
      Util.Tests.Assert_Equals (T, 0, Get_Count (Null_Object), "Get_Count is invalid");
   end Test_Get_Count;

   --  ------------------------------
   --  Test the Get_Value operation.
   --  ------------------------------
   procedure Test_Get_Value (T : in out Test) is
      List  : Object_Array (1 .. 10);
      Value : Object;
      Item  : Object;
   begin
      for I in List'Range loop
         List (I) := To_Object (I);
      end loop;
      Value := To_Object (List);
      for I in 1 .. Get_Count (Value) loop
         Item := Get_Value (Value, I);
         T.Assert (not Is_Null (Item), "Item at " & Positive'Image (I) & " is null");
      end loop;
   end Test_Get_Value;

   --  ------------------------------
   --  Test the Set_Value operation.
   --  ------------------------------
   procedure Test_Set_Value (T : in out Test) is
      Value : constant Object := Maps.Create;
   begin
      Set_Value (Value, "username", To_Object (String '("joe")));
      Set_Value (Value, "age", To_Object (Integer (32)));
      Util.Tests.Assert_Equals (T, "joe", To_String (Get_Value (Value, "username")));
      Util.Tests.Assert_Equals (T, 32, To_Integer (Get_Value (Value, "age")));
   end Test_Set_Value;

   --  ------------------------------
   --  Test the "&" operator.
   --  ------------------------------
   procedure Test_And_Operator (T : in out Test) is
      S1 : constant Object := To_Object (Wide_Wide_String '("wide hello"));
      S2 : constant Object := To_Object (String '("normal hello"));
      S3 : constant Object := To_Object (String '(" "));
      I1 : constant Object := To_Object (Integer '(12345));
      R  : Object;
   begin
      R := S1 & S3 & I1 & S3 & S2;
      Util.Tests.Assert_Equals (T, "wide hello 12345 normal hello", To_String (R));
   end Test_And_Operator;

   --  ------------------------------
   --  Test the Blob bean.
   --  ------------------------------
   procedure Test_Blob (T : in out Test) is
      Blob  : constant Util.Blobs.Blob_Ref := Util.Blobs.Create_Blob (Size => 10);
      R     : Object;
      Blob2 : Util.Blobs.Blob_Ref;
   begin
      R := To_Object (Blob);
      T.Assert (not Is_Null (R), "Is_Null must be false");
      Blob2 := To_Blob (R);
      T.Assert (not Blob2.Is_Null, "Blob2 must not be null");
   end Test_Blob;

end Util.Beans.Objects.Tests;
