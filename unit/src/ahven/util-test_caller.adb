-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
--  Copyright (C) 2009, 2010, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.XUnit;
with Ada.Unchecked_Conversion;
package body Util.Test_Caller is

   Test     : aliased Util.XUnit.Test_Object;
   Instance : aliased Test_Fixture;

   function To_X is
      new Ada.Unchecked_Conversion (Source => Test_Method,
                                    Target => Ahven.Framework.Object_Test_Routine_Access);

   Added : Boolean := False;

   procedure Add_Test (Suite     : in Util.Tests.Access_Test_Suite;
                       Test_Name : in String;
                       Method    : in Test_Method) is
      pragma Unreferenced (Suite);
   begin
      if Util.Tests.Is_Test_Enabled (Test_Name) then
         if not Added then
            Instance.Set_Name (Util.Tests.Get_Harness_Prefix & Name);
            Test.Test := Instance'Access;
            Util.XUnit.Register (Test'Access);
            Added := True;
         end if;
         Ahven.Framework.Add_Test_Routine (Instance, To_X (Method), Test_Name);
      end if;
   end Add_Test;

end Util.Test_Caller;
