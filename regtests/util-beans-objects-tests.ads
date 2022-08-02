-----------------------------------------------------------------------
--  util-beans-objects-tests -- Unit tests for objects
--  Copyright (C) 2017, 2022 Stephane Carrez
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

with Util.Tests;

package Util.Beans.Objects.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the creation, initialization and retrieval of objects holding arrays.
   procedure Test_Is_Array (T : in out Test);

   --  Test the Get_Count operation.
   procedure Test_Get_Count (T : in out Test);

   --  Test the Get_Value operation.
   procedure Test_Get_Value (T : in out Test);

   --  Test the Set_Value operation.
   procedure Test_Set_Value (T : in out Test);

   --  Test the "&" operator.
   procedure Test_And_Operator (T : in out Test);

   --  Test the Blob bean.
   procedure Test_Blob (T : in out Test);

end Util.Beans.Objects.Tests;
