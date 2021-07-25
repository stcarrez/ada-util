-----------------------------------------------------------------------
--  util-streams-sockets-tests -- Unit tests for socket streams
--  Copyright (C) 2012, 2017, 2018, 2019 Stephane Carrez
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
with Ada.IO_Exceptions;

with Util.Test_Caller;

with Util.Streams.Texts;
with Util.Tests.Servers;
package body Util.Streams.Sockets.Tests is

   package Caller is new Util.Test_Caller (Test, "Streams.Sockets");

   type Test_Server is new Util.Tests.Servers.Server with record
      Count : Natural := 0;
   end record;

   --  Process the line received by the server.
   overriding
   procedure Process_Line (Into   : in out Test_Server;
                           Line   : in Ada.Strings.Unbounded.Unbounded_String;
                           Stream : in out Util.Streams.Texts.Reader_Stream'Class;
                           Client : in out Util.Streams.Sockets.Socket_Stream'Class);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Streams.Initialize,Connect",
                       Test_Socket_Init'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Connect,Read,Write",
                       Test_Socket_Read'Access);
   end Add_Tests;

   --  ------------------------------
   --  Process the line received by the server.
   --  ------------------------------
   overriding
   procedure Process_Line (Into   : in out Test_Server;
                           Line   : in Ada.Strings.Unbounded.Unbounded_String;
                           Stream : in out Util.Streams.Texts.Reader_Stream'Class;
                           Client : in out Util.Streams.Sockets.Socket_Stream'Class) is
      pragma Unreferenced (Stream, Client);
   begin
      if Ada.Strings.Unbounded.Index (Line, "test-" & Natural'Image (Into.Count + 1)) > 0 then
         Into.Count := Into.Count + 1;
      end if;
   end Process_Line;

   --  ------------------------------
   --  Test reading and writing on a socket stream.
   --  ------------------------------
   procedure Test_Socket_Read (T : in out Test) is
      Stream : aliased Sockets.Socket_Stream;
      Writer : Util.Streams.Texts.Print_Stream;
      Server : Test_Server;
      Addr   : GNAT.Sockets.Sock_Addr_Type;
   begin
      Server.Start;
      T.Assert (Server.Get_Port > 0, "The server was not started");

      Addr := (GNAT.Sockets.Family_Inet,
               GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (GNAT.Sockets.Host_Name), 1),
               GNAT.Sockets.Port_Type (Server.Get_Port));

      --  Let the server start.
      delay 0.1;

      --  Get a connection and write 10 lines.
      Stream.Connect (Server => Addr);
      Writer.Initialize (Output => Stream'Unchecked_Access,
                         Size   => 1024);
      for I in 1 .. 10 loop
         Writer.Write ("Sending a line on the socket test-"
                       & Natural'Image (I) & ASCII.CR & ASCII.LF);
         Writer.Flush;
      end loop;
      Writer.Close;

      --  Stop the server and verify that 10 lines were received.
      Server.Stop;
      Util.Tests.Assert_Equals (T, 10, Server.Count, "Invalid number of lines received");
   end Test_Socket_Read;

   --  ------------------------------
   --  Test socket initialization.
   --  ------------------------------
   procedure Test_Socket_Init (T : in out Test) is
      Stream : aliased Sockets.Socket_Stream;
      Fd     : GNAT.Sockets.Socket_Type;
      Addr   : GNAT.Sockets.Sock_Addr_Type;
   begin
      Addr := (GNAT.Sockets.Family_Inet,
               GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (GNAT.Sockets.Host_Name), 1),
               80);

      GNAT.Sockets.Create_Socket (Fd);
      Stream.Open (Fd);
      begin
         Stream.Connect (Addr);
         T.Assert (False, "No exception was raised");

      exception
         when Ada.IO_Exceptions.Use_Error =>
            null;
      end;
   end Test_Socket_Init;

end Util.Streams.Sockets.Tests;
