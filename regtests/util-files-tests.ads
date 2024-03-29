-----------------------------------------------------------------------
--  files.tests -- Unit tests for files
--  Copyright (C) 2009, 2010, 2011, 2012, 2022, 2024 Stephane Carrez
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

package Util.Files.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Read_File (T : in out Test);
   procedure Test_Read_File_Missing (T : in out Test);
   procedure Test_Read_File_Truncate (T : in out Test);
   procedure Test_Write_File (T : in out Test);
   procedure Test_Find_File_Path (T : in out Test);
   procedure Test_Iterate_Path (T : in out Test);
   procedure Test_Compose_Path (T : in out Test);

   --  Test the Get_Relative_Path operation.
   procedure Test_Get_Relative_Path (T : in out Test);

   --  Test the Delete_Tree operation.
   procedure Test_Delete_Tree (T : in out Test);

   --  Test the Realpath function.
   procedure Test_Realpath (T : in out Test);

   --  Test the Util.Files.Walk package
   procedure Test_Walk (T : in out Test);

   --  Test the Util.Files.Filters package.
   procedure Test_Path_Filter (T : in out Test);

   --  Test the Util.Files.Filter operations.
   procedure Test_Filter (T : in out Test);

   procedure Test_Filter_Recursive (T : in out Test);

   procedure Test_Filter_Wildcard (T : in out Test);

end Util.Files.Tests;
