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

with Util.Log.Loggers;
package body Util.Tests.Servers is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Tests.Server");

   function Read_Line (Sock : in GNAT.Sockets.Socket_Type) return String is
      use Ada.Streams;

      Data   : Ada.Streams.Stream_Element_Array (1 .. 256);
      Last   : Ada.Streams.Stream_Element_Offset;
      Result : String (1 .. 256);
   begin
      GNAT.Sockets.Receive_Socket (Sock, Data, Last);
      for I in 1 .. Last loop
         Result (Natural (I)) := Character'Val (Data (I));
      end loop;
      return Result (1 .. Natural (Last));
   end Read_Line;

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
         loop
            begin
               declare
                  Line : constant String := Read_Line (Socket);
               begin
                  exit when Line'Length = 0;
                  Log.Info ("Received: {0}", Line);
               end;

            exception
               when E : others =>
                  Log.Error ("Exception: ", E);
            end;
         end loop;
         Close_Socket (Socket);
      end loop;

   exception
      when E : others =>
         Log.Error ("Exception", E);
   end Server_Task;

end Util.Tests.Servers;
