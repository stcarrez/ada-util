-----------------------------------------------------------------------
--  Util -- Utilities
--  Copyright (C) 2009, 2010, 2021 Stephane Carrez
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
with Util.Testsuite;
with Util.Tests;
with Util.Properties;
procedure Util_Harness is

   procedure Initialize (Props : in Util.Properties.Manager);

   procedure Harness is new Util.Tests.Harness (Util.Testsuite.Suite, Initialize);

   procedure Initialize (Props : in Util.Properties.Manager) is
      pragma Unreferenced (Props);

      Path : constant String := Util.Tests.Get_Test_Path ("");
   begin
      if not Ada.Directories.Exists (Path) then
         Ada.Directories.Create_Directory (Path);
      end if;
   end Initialize;

begin
   Harness ("util-tests.xml");
end Util_Harness;
