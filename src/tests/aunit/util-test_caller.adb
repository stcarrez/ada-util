-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
--  Copyright (C) 2009, 2010, 2011, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Test_Caller is

   procedure Add_Test (Suite     : in Util.Tests.Access_Test_Suite;
                       Test_Name : in String;
                       Method    : in Caller.Test_Method) is
   begin
      if Util.Tests.Is_Test_Enabled (Test_Name) then
         Suite.Add_Test (Caller.Create (Test_Name, Method));
      end if;
   end Add_Test;

end Util.Test_Caller;
