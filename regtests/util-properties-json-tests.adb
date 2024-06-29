-----------------------------------------------------------------------
--  util-properties-json-tests -- Test reading JSON file into properties
--  Copyright (C) 2013, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;
with Util.Files;
package body Util.Properties.JSON.Tests is

   package Caller is new Util.Test_Caller (Test, "Properties.JSON");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Properties.JSON.Parse_JSON",
                       Test_Parse_JSON'Access);
      Caller.Add_Test (Suite, "Test Util.Properties.JSON.Read_JSON (flattening)",
                       Test_Read_JSON'Access);
   end Add_Tests;

   --  Test loading a JSON file into a properties object.
   procedure Test_Parse_JSON (T : in out Test) is
      procedure Check (Name : in String;
                       Value : in String);

      P    : Util.Properties.Manager;

      procedure Check (Name : in String;
                       Value : in String) is
      begin
         T.Assert (P.Exists (Name), "Missing property: " & Name);
         Util.Tests.Assert_Equals (T, Value, String '(P.Get (Name)),
                                   "Invalid property: " & Name);
      end Check;

      Path : constant String := Util.Tests.Get_Path ("regtests/files/test-1.json");
      S    : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Util.Files.Read_File (Path, S);
      Util.Properties.JSON.Parse_JSON (P, Ada.Strings.Unbounded.To_String (S));

      Check ("access_token", "ya");
      Check ("token_type", "Bearer");
      Check ("expires_in", "3600");
      Check ("id_token", "ey");
   end Test_Parse_JSON;

   --  Test loading a JSON file into a properties object.
   procedure Test_Read_JSON (T : in out Test) is
      procedure Check (Name : in String;
                       Value : in String);

      P    : Util.Properties.Manager;

      procedure Check (Name : in String;
                       Value : in String) is
      begin
         T.Assert (P.Exists (Name), "Missing property: " & Name);
         Util.Tests.Assert_Equals (T, Value, String '(P.Get (Name)),
                                   "Invalid property: " & Name);
      end Check;

      Path : constant String := Util.Tests.Get_Path ("regtests/files/test-2.json");
   begin
      Util.Properties.JSON.Read_JSON (P, Path);

      Check ("user.name", "joe");
      Check ("user.first_name", "doe");
      Check ("user.age", "23");
      Check ("user.address.street", "somewhere");
      Check ("user.address.country.name", "41");
      Check ("user.address.country.location", "moon");
   end Test_Read_JSON;

end Util.Properties.JSON.Tests;
