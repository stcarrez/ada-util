-----------------------------------------------------------------------
--  Util -- Utilities
--  Copyright (C) 2009, 2010, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
