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

with Util.Test_Caller;

with Util.Strings.Transforms;
with Util.Http.Tools;
with Util.Http.Clients.Curl;
package body Util.Http.Clients.Tests is

   package Caller is new Util.Test_Caller (Test, "Http");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Http.Clients.Get",
                       Test_Http_Get'Access);
   end Add_Tests;

   overriding
   procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Util.Http.Clients.Curl.Register;
   end Set_Up;

   --  ------------------------------
   --  Test the http Get operation.
   --  ------------------------------
   procedure Test_Http_Get (T : in out Test) is
      Request : Client;
      Reply   : Response;
   begin
      Request.Get ("http://www.google.com", Reply);
      T.Assert (Reply.Get_Status = 200 or Reply.Get_Status = 302, "Get status is invalid");

      Util.Http.Tools.Save_Response (Util.Tests.Get_Test_Path ("http_get.txt"), Reply, True);

      --  Check the content.
      declare
         Content : constant String := Util.Strings.Transforms.To_Lower_Case (Reply.Get_Body);
      begin
         Util.Tests.Assert_Matches (T, ".*html.*", Content, "Invalid GET content");
      end;

      --  Check one header.
      declare
         Content : constant String := Reply.Get_Header ("Content-Type");
      begin
         T.Assert (Content'Length > 0, "Empty Content-Type header");
         Util.Tests.Assert_Matches (T, ".*text/html.*", Content, "Invalid content type");
      end;
   end Test_Http_Get;

end Util.Http.Clients.Tests;
