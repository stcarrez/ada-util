-----------------------------------------------------------------------
--  util-http-clients-tests -- Unit tests for HTTP client
--  Copyright (C) 2012, 2020, 2022 Stephane Carrez
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
with Ada.Strings.Unbounded;

with Util.Tests;
with Util.Tests.Servers;
with Util.Streams.Texts;
with Util.Streams.Sockets;
package Util.Http.Clients.Tests is

   type Method_Type is (OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, PATCH, CONNECT, UNKNOWN);

   type Test_Server is new Util.Tests.Servers.Server with record
      Method       : Method_Type := UNKNOWN;
      Uri          : Ada.Strings.Unbounded.Unbounded_String;
      Result       : Ada.Strings.Unbounded.Unbounded_String;
      Content_Type : Ada.Strings.Unbounded.Unbounded_String;
      Length       : Natural := 0;
      Test_Timeout : Boolean := False;
   end record;
   type Test_Server_Access is access all Test_Server'Class;

   --  Process the line received by the server.
   overriding
   procedure Process_Line (Into   : in out Test_Server;
                           Line   : in Ada.Strings.Unbounded.Unbounded_String;
                           Stream : in out Util.Streams.Texts.Reader_Stream'Class;
                           Client : in out Util.Streams.Sockets.Socket_Stream'Class);

   type Test is new Util.Tests.Test with record
      Server : Test_Server_Access := null;
   end record;

   --  Test the http Get operation.
   procedure Test_Http_Get (T : in out Test);

   --  Test the http HEAD operation.
   procedure Test_Http_Head (T : in out Test);

   --  Test the http POST operation.
   procedure Test_Http_Post (T : in out Test);

   --  Test the http PUT operation.
   procedure Test_Http_Put (T : in out Test);

   --  Test the http DELETE operation.
   procedure Test_Http_Delete (T : in out Test);

   --  Test the http OPTIONS operation.
   procedure Test_Http_Options (T : in out Test);

   --  Test the http PATCH operation.
   procedure Test_Http_Patch (T : in out Test);

   --  Test the http timeout.
   procedure Test_Http_Timeout (T : in out Test);

   --  Test the http GET with binary content.
   procedure Test_Http_Binary (T : in out Test);

   overriding
   procedure Set_Up (T : in out Test);

   overriding
   procedure Tear_Down (T : in out Test);

   --  Get the test server base URI.
   function Get_Uri (T : in Test) return String;

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
