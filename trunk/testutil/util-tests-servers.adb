-----------------------------------------------------------------------
--  util-tests-server - A small non-compliant-inefficient HTTP server used for unit tests
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

with GNAT.Sockets;
with Ada.Streams;
with Util.Streams.Sockets;
with Util.Streams.Texts;
with Util.Log.Loggers;
package body Util.Tests.Servers is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Tests.Server");

   --  Get the server port.
   function Get_Port (From : in Server) return Natural is
   begin
      return From.Port;
   end Get_Port;

   --  Start the server task.
   procedure Start (S : in out Server) is
   begin
   end Start;

   task body Server_Task is
      use GNAT.Sockets;

      Address  : Sock_Addr_Type;
      Server   : Socket_Type;
      Socket   : Socket_Type;
      Channel  : Stream_Access;
   begin
      accept Start;
      Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
      Address.Port := 51432;
      Log.Info ("Internal HTTP server started at port {0}", Port_Type'Image (Address.Port));

      Create_Socket (Server);
      Set_Socket_Option (Server,
                         Socket_Level,
                         (Reuse_Address, True));
      Bind_Socket (Server, Address);
      Listen_Socket (Server);
      loop
         Accept_Socket (Server, Socket, Address);
         Channel := Stream (Socket);

         Log.Info ("Accepted connection");
         declare
            Stream   : aliased Util.Streams.Sockets.Socket_Stream;
            Input    : Util.Streams.Texts.Reader_Stream;
            Line     : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Stream.Open (Socket);
            Input.Initialize (From => Stream'Unchecked_Access);
            loop
               Input.Read_Line (Into  => Line, Strip => True);
               exit when Ada.Strings.Unbounded.Length (Line) = 0;
               Log.Info ("Received: {0}", Line);

            exception
               when E : others =>
                  Log.Error ("Exception: ", E);
            end loop;
            Close_Socket (Socket);
         end;
      end loop;

   exception
      when E : others =>
         Log.Error ("Exception", E);
   end Server_Task;

end Util.Tests.Servers;
