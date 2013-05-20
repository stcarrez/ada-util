-----------------------------------------------------------------------
--  util-streams-sockets -- Socket streams
--  Copyright (C) 2012, 2013 Stephane Carrez
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

with Ada.IO_Exceptions;
package body Util.Streams.Sockets is

   --  -----------------------
   --  Initialize the socket stream.
   --  -----------------------
   procedure Open (Stream  : in out Socket_Stream;
                   Socket  : in GNAT.Sockets.Socket_Type) is
      use type GNAT.Sockets.Socket_Type;
   begin
      if Stream.Sock /= GNAT.Sockets.No_Socket then
         raise Ada.IO_Exceptions.Use_Error with "Socket stream is already opened";
      end if;
      Stream.Sock := Socket;
   end Open;

   --  -----------------------
   --  Initialize the socket stream by opening a connection to the server defined in <b>Server</b>.
   --  -----------------------
   procedure Connect (Stream : in out Socket_Stream;
                      Server : in GNAT.Sockets.Sock_Addr_Type) is
      use type GNAT.Sockets.Socket_Type;
   begin
      if Stream.Sock /= GNAT.Sockets.No_Socket then
         raise Ada.IO_Exceptions.Use_Error with "Socket stream is already opened";
      end if;
      GNAT.Sockets.Create_Socket (Socket => Stream.Sock,
                                  Family => Server.Family);
      GNAT.Sockets.Connect_Socket (Socket => Stream.Sock,
                                   Server => Server);
   end Connect;

   --  -----------------------
   --  Close the socket stream.
   --  -----------------------
   overriding
   procedure Close (Stream : in out Socket_Stream) is
   begin
      GNAT.Sockets.Close_Socket (Stream.Sock);
      Stream.Sock := GNAT.Sockets.No_Socket;
   end Close;

   --  -----------------------
   --  Write the buffer array to the output stream.
   --  -----------------------
   overriding
   procedure Write (Stream : in out Socket_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
      Last : Ada.Streams.Stream_Element_Offset;
      pragma Unreferenced (Last);
   begin
      GNAT.Sockets.Send_Socket (Stream.Sock, Buffer, Last);
   end Write;

   --  -----------------------
   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   --  -----------------------
   overriding
   procedure Read (Stream : in out Socket_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      GNAT.Sockets.Receive_Socket (Stream.Sock, Into, Last);
   end Read;

   --  -----------------------
   --  Flush the stream and release the buffer.
   --  -----------------------
   overriding
   procedure Finalize (Object : in out Socket_Stream) is
      use type GNAT.Sockets.Socket_Type;
   begin
      if Object.Sock /= GNAT.Sockets.No_Socket then
         Object.Close;
      end if;
   end Finalize;

end Util.Streams.Sockets;
