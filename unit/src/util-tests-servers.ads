-----------------------------------------------------------------------
--  util-tests-server - A small non-compliant-inefficient HTTP server used for unit tests
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Finalization;

with Util.Streams.Sockets;
with Util.Streams.Texts;
package Util.Tests.Servers is

   --  A small TCP/IP server for unit tests.
   type Server is new Ada.Finalization.Limited_Controlled with private;
   type Server_Access is access all Server'Class;

   --  Get the server port.
   function Get_Port (From : in Server) return Natural;

   --  Get the server name.
   function Get_Host (From : in Server) return String;

   --  Process the line received by the server.
   procedure Process_Line (Into   : in out Server;
                           Line   : in Ada.Strings.Unbounded.Unbounded_String;
                           Stream : in out Util.Streams.Texts.Reader_Stream'Class;
                           Client : in out Util.Streams.Sockets.Socket_Stream'Class);

   --  Start the server task.
   procedure Start (S : in out Server);

   --  Stop the server task.
   procedure Stop (S : in out Server);

private

   --  A small server that listens to HTTP requests and replies with fake
   --  responses.  This server is intended to be used by unit tests and not to serve
   --  real pages.
   task type Server_Task is
      entry Start (S : in Server_Access);
      --        entry Stop;
   end Server_Task;

   type Server is new Ada.Finalization.Limited_Controlled with record
      Port          : Natural := 0;
      Need_Shutdown : Boolean := False;
      Server        : Server_Task;
      Client        : aliased Util.Streams.Sockets.Socket_Stream;
   end record;

end Util.Tests.Servers;
