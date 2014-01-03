-----------------------------------------------------------------------
--  util-tests-server - A small non-compliant-inefficient HTTP server used for unit tests
--  Copyright (C) 2012, 2013, 2014 Stephane Carrez
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

with GNAT.Sockets;
with Util.Log.Loggers;
package body Util.Tests.Servers is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Tests.Server");

   --  ------------------------------
   --  Get the server port.
   --  ------------------------------
   function Get_Port (From : in Server) return Natural is
   begin
      return From.Port;
   end Get_Port;

   --  ------------------------------
   --  Get the server name.
   --  ------------------------------
   function Get_Host (From : in Server) return String is
      pragma Unreferenced (From);
   begin
      return GNAT.Sockets.Host_Name;
   end Get_Host;

   --  ------------------------------
   --  Start the server task.
   --  ------------------------------
   procedure Start (S : in out Server) is
   begin
      S.Server.Start (S'Unchecked_Access);
   end Start;

   --  ------------------------------
   --  Stop the server task.
   --  ------------------------------
   procedure Stop (S : in out Server) is
   begin
      S.Need_Shutdown := True;
      for I in 1 .. 10 loop
         delay 0.1;
         if S.Server'Terminated then
            return;
         end if;
      end loop;
      abort S.Server;
   end Stop;

   --  ------------------------------
   --  Process the line received by the server.
   --  ------------------------------
   procedure Process_Line (Into   : in out Server;
                           Line   : in Ada.Strings.Unbounded.Unbounded_String;
                           Stream : in out Util.Streams.Texts.Reader_Stream'Class;
                           Client : in out Util.Streams.Sockets.Socket_Stream'Class) is
      pragma Unreferenced (Into, Line, Stream);
   begin
      null;
   end Process_Line;

   task body Server_Task is
      use GNAT.Sockets;

      Address  : Sock_Addr_Type;
      Server   : Socket_Type;
      Socket   : Socket_Type;
      Instance : Server_Access := null;
      Status   : Selector_Status;
   begin
      Address.Port := 0;
      Create_Socket (Server);
      select
         accept Start (S : in Server_Access) do
            Instance := S;
            Address.Addr := Addresses (Get_Host_By_Name (S.Get_Host), 1);
            Bind_Socket (Server, Address);
            Address := GNAT.Sockets.Get_Socket_Name (Server);
            Listen_Socket (Server);

            Instance.Port := Natural (Address.Port);
         end Start;
      or
         terminate;
      end select;

      Log.Info ("Internal HTTP server started at port {0}", Port_Type'Image (Address.Port));

      while not Instance.Need_Shutdown loop
         Accept_Socket (Server, Socket, Address, 1.0, null, Status);
         if Socket /= No_Socket then
            Log.Info ("Accepted connection");
            declare
               Input : Util.Streams.Texts.Reader_Stream;
            begin
               Instance.Client.Open (Socket);
               Input.Initialize (From => Instance.Client'Access);
               while not Input.Is_Eof loop
                  declare
                     Line     : Ada.Strings.Unbounded.Unbounded_String;
                  begin
                     Input.Read_Line (Into  => Line, Strip => False);
                     exit when Ada.Strings.Unbounded.Length (Line) = 0;
                     Log.Info ("Received: {0}", Line);

                     Instance.Process_Line (Line, Input, Instance.Client);
                  end;
               end loop;

            exception
               when E : others =>
                  Log.Error ("Exception: ", E);
            end;
            Instance.Client.Close;
         end if;
      end loop;

      GNAT.Sockets.Close_Socket (Server);
   exception
      when E : others =>
         Log.Error ("Exception", E);
   end Server_Task;

end Util.Tests.Servers;
