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
with Util.Strings;
with Util.Log.Loggers;

package body Util.Http.Clients.Tests is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Http.Clients.Tests");

   package body Http_Tests is
      package Caller is new Util.Test_Caller (Http_Test, "Http-" & NAME);

      procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
      begin
         Caller.Add_Test (Suite, "Test Util.Http.Clients." & NAME & ".Get",
                          Test_Http_Get'Access);
         Caller.Add_Test (Suite, "Test Util.Http.Clients." & NAME & ".Post",
                          Test_Http_Post'Access);
      end Add_Tests;

      overriding
      procedure Set_Up (T : in out Http_Test) is
      begin
         Test (T).Set_Up;
         Register;
      end Set_Up;

   end Http_Tests;

   overriding
   procedure Set_Up (T : in out Test) is
   begin
      Log.Info ("Starting test server");
      T.Server := new Test_Server;
      T.Server.Start;
   end Set_Up;

   overriding
   procedure Tear_Down (T : in out Test) is
   begin
      if T.Server /= null then
         Log.Info ("Stopping test server");
         T.Server.Stop;
         T.Server := null;
      end if;
   end Tear_Down;

   --  ------------------------------
   --  Process the line received by the server.
   --  ------------------------------
   overriding
   procedure Process_Line (Into   : in out Test_Server;
                           Line   : in Ada.Strings.Unbounded.Unbounded_String;
                           Stream : in out Util.Streams.Texts.Reader_Stream'Class;
                           Client : in out Util.Streams.Sockets.Socket_Stream'Class) is
      L   : constant String := Ada.Strings.Unbounded.To_String (Line);
      Pos : Natural := Util.Strings.Index (L, ' ');
   begin
      if Pos > 0 and Into.Method = UNKNOWN then
         if L (L'First .. Pos - 1) = "GET" then
            Into.Method := GET;
         elsif L (L'First .. Pos - 1) = "POST" then
            Into.Method := POST;
         else
            Into.Method := UNKNOWN;
         end if;
      end if;

      Pos := Util.Strings.Index (L, ':');
      if Pos > 0 then
         if L (L'First .. Pos) = "Content-Type:" then
            Into.Content_Type
              := Ada.Strings.Unbounded.To_Unbounded_String (L (Pos + 2 .. L'Last - 2));
         elsif L (L'First .. Pos) = "Content-Length:" then
            Into.Length := Natural'Value (L (Pos + 1 .. L'Last - 2));

         end if;
      end if;
      if L'Length = 2 and then Into.Length > 0 then
         for I in 1 .. Into.Length loop
            declare
               C : Character;
            begin
               Stream.Read (C);
               Ada.Strings.Unbounded.Append (Into.Result, C);
            end;
         end loop;
         declare
            Output : Util.Streams.Texts.Print_Stream;
         begin
            Output.Initialize (Client'Unchecked_Access);
            Output.Write ("HTTP/1.1 204 No Content" & ASCII.CR & ASCII.LF);
            Output.Write ("Content-Length: 4" & ASCII.CR & ASCII.LF);
            Output.Write (ASCII.CR & ASCII.LF);
            Output.Write ("OK" & ASCII.CR & ASCII.LF);
            Output.Flush;
         end;
      end if;
      Log.Info ("Received: {0}", L);
   end Process_Line;

   --  ------------------------------
   --  Get the test server base URI.
   --  ------------------------------
   function Get_Uri (T : in Test) return String is

   begin
      return "http://" & T.Server.Get_Host & ":" & Util.Strings.Image (T.Server.Get_Port);
   end Get_Uri;

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

   --  ------------------------------
   --  Test the http POST operation.
   --  ------------------------------
   procedure Test_Http_Post (T : in out Test) is
      Request : Client;
      Reply   : Response;
      Uri     : constant String := T.Get_Uri;
   begin
      Log.Info ("Post on " & Uri);

      T.Server.Method := UNKNOWN;
      Request.Post (Uri & "/post",
                    "p1=1", Reply);

      T.Assert (T.Server.Method = POST, "Invalid method received by server");
      Util.Tests.Assert_Equals (T, "application/x-www-form-urlencoded", T.Server.Content_Type,
                                "Invalid content type received by server");
      Util.Tests.Assert_Equals (T, "OK" & ASCII.CR & ASCII.LF, Reply.Get_Body, "Invalid response");
      Util.Http.Tools.Save_Response (Util.Tests.Get_Test_Path ("http_post.txt"), Reply, True);

   end Test_Http_Post;

end Util.Http.Clients.Tests;
