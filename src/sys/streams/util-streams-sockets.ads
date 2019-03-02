-----------------------------------------------------------------------
--  util-streams-sockets -- Socket streams
--  Copyright (C) 2012, 2013, 2015, 2018 Stephane Carrez
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
with Ada.Finalization;
with GNAT.Sockets;

--  == Sockets ==
--  The <b>Util.Streams.Sockets</b> package defines a socket stream.
package Util.Streams.Sockets is

   --  -----------------------
   --  Socket stream
   --  -----------------------
   --  The <b>Socket_Stream</b> is an output/input stream that reads or writes
   --  to or from a socket.
   type Socket_Stream is limited new Output_Stream and Input_Stream with private;

   --  Initialize the socket stream.
   procedure Open (Stream  : in out Socket_Stream;
                   Socket  : in GNAT.Sockets.Socket_Type);

   --  Initialize the socket stream by opening a connection to the server defined in <b>Server</b>.
   procedure Connect (Stream : in out Socket_Stream;
                      Server : in GNAT.Sockets.Sock_Addr_Type);

   --  Close the socket stream.
   overriding
   procedure Close (Stream : in out Socket_Stream);

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Socket_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   overriding
   procedure Read (Stream : in out Socket_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

private

   type Socket_Stream is limited new Ada.Finalization.Limited_Controlled
     and Output_Stream and Input_Stream with record
      Sock : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
   end record;

   --  Flush the stream and release the buffer.
   overriding
   procedure Finalize (Object : in out Socket_Stream);

end Util.Streams.Sockets;
