-----------------------------------------------------------------------
--  util-properties-form-tests -- Test reading JSON file into properties
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;
with Util.Files;
package body Util.Properties.Form.Tests is

   package Caller is new Util.Test_Caller (Test, "Properties.Form");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Properties.Form.Parse_Form",
                       Test_Parse_Form'Access);
   end Add_Tests;

   --  Test loading a JSON file into a properties object.
   procedure Test_Parse_Form (T : in out Test) is
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

      Path : constant String := Util.Tests.Get_Path ("regtests/files/test-1.form");
      S    : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Util.Files.Read_File (Path, S);
      Util.Properties.Form.Parse_Form (P, Ada.Strings.Unbounded.To_String (S));

      Check ("access_token", "97356");
      Check ("token_type", "bearer");
      Check ("refresh_token_expires_in", "15724800");
      Check ("refresh_token", "r1.714b6");
      Check ("scope", "");
      Check ("scope", "");
   end Test_Parse_Form;

end Util.Properties.Form.Tests;
