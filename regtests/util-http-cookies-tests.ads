-----------------------------------------------------------------------
--  util-http-cookies-tests - Unit tests for Cookies
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Http.Cookies.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test creation of cookie
   procedure Test_Create_Cookie (T : in out Test);
   procedure Test_To_Http_Header (T : in out Test);
   procedure Test_Parse_Http_Header (T : in out Test);

end Util.Http.Cookies.Tests;
