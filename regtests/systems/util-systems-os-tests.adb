-----------------------------------------------------------------------
--  util-systems-os-tests -- Unit tests for OS specific operations
--  Copyright (C) 2014, 2015, 2016, 2021 Stephane Carrez
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
with Ada.Directories;
with Util.Test_Caller;
with Util.Systems.Types;
with Interfaces.C.Strings;
package body Util.Systems.Os.Tests is

   package Caller is new Util.Test_Caller (Test, "Systems.OS");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Systems.Os.Sys_Stat (File)",
                       Test_Stat'Access);
      Caller.Add_Test (Suite, "Test Util.Systems.Os.Sys_Stat (Directory)",
                       Test_Stat_Directory'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the Sys_Stat operation.
   --  ------------------------------
   procedure Test_Stat (T : in out Test) is
      use Interfaces.C;
      use Util.Systems.Types;

      Info : array (1 .. 2) of aliased Util.Systems.Types.Stat_Type;
      Path : constant String := Util.Tests.Get_Path ("regtests/files/test-1.json");
      Name : Util.Systems.Os.Ptr := Interfaces.C.Strings.New_String (Path);
      Res  : Integer;
   begin
      Info (2).st_dev := 16#12345678#;
      Res := Sys_Stat (Name, Info (1)'Unchecked_Access);
      T.Assert (Res = 0, "Sys_Stat must return 0");
      T.Assert (Info (1).st_size > 0, "Sys_Stat must return the correct size");
      T.Assert (Info (2).st_dev = 16#12345678#, "Suspecting invalid size for Stat_Type");
      T.Assert (Info (1).st_size = off_t (Ada.Directories.Size (Path)),
                "Invalid size returned by Sys_Stat");
      T.Assert ((Info (1).st_mode and S_IFMT) = S_IFREG, "st_mode must indicate a regular file");
      Interfaces.C.Strings.Free (Name);
   end Test_Stat;

   --  ------------------------------
   --  Test the Sys_Stat operation.
   --  ------------------------------
   procedure Test_Stat_Directory (T : in out Test) is
      use Interfaces.C;
      use Util.Systems.Types;

      Stat : aliased Util.Systems.Types.Stat_Type;
      Path : constant String := Util.Tests.Get_Path ("regtests/files");
      Name : Util.Systems.Os.Ptr := Interfaces.C.Strings.New_String (Path);
      Res  : Integer;
   begin
      Res := Sys_Stat (Name, Stat'Unchecked_Access);
      T.Assert (Res = 0, "Sys_Stat must return 0");
      T.Assert ((Stat.st_mode and S_IFMT) = S_IFDIR, "st_mode must indicate a directory");
      Interfaces.C.Strings.Free (Name);
   end Test_Stat_Directory;

end Util.Systems.Os.Tests;
