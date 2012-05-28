-----------------------------------------------------------------------
--  util-http-clients-tests -- Unit tests for HTTP client
--  Copyright (C) 2012 Stephane Carrez
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

package Util.Http.Clients.Tests is

   type Test is new Util.Tests.Test with null record;

   --  Test the http Get operation.
   procedure Test_Http_Get (T : in out Test);

   --  The <b>Http_Tests</b> package must be instantiated with one of the HTTP implementation.
   --  The <b>Register</b> procedure configures the Http.Client to use the given HTTP
   --  implementation before running the test.
   generic
      with procedure Register;
      NAME : in String;
   package Http_Tests is
      procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

      type Http_Test is new Test with null record;

      overriding
      procedure Set_Up (T : in out Http_Test);

   end Http_Tests;

end Util.Http.Clients.Tests;
