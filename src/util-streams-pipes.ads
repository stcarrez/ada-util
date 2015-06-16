-----------------------------------------------------------------------
--  util-streams-pipes -- Pipe stream to or from a process
--  Copyright (C) 2011, 2013, 2015 Stephane Carrez
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

with Util.Processes;

--  The <b>Util.Streams.Pipes</b> package defines a pipe stream to or from a process.
--  The process is created and launched by the <b>Open</b> operation.  The pipe allows
--  to read or write to the process through the <b>Read</b> and <b>Write</b> operation.
package Util.Streams.Pipes is

   use Util.Processes;

   subtype Pipe_Mode is Util.Processes.Pipe_Mode range READ .. READ_WRITE;

   --  -----------------------
   --  Pipe stream
   --  -----------------------
   --  The <b>Pipe_Stream</b> is an output/input stream that reads or writes
   --  to or from a process.
   type Pipe_Stream is limited new Output_Stream and Input_Stream  with private;

   --  Open a pipe to read or write to an external process.  The pipe is created and the
   --  command is executed with the input and output streams redirected through the pipe.
   procedure Open (Stream  : in out Pipe_Stream;
                   Command : in String;
                   Mode    : in Pipe_Mode := READ);

   --  Close the pipe and wait for the external process to terminate.
   overriding
   procedure Close (Stream : in out Pipe_Stream);

   --  Get the process exit status.
   function Get_Exit_Status (Stream : in Pipe_Stream) return Integer;

   --  Returns True if the process is running.
   function Is_Running (Stream : in Pipe_Stream) return Boolean;

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Pipe_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   overriding
   procedure Read (Stream : in out Pipe_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

private

   use Ada.Streams;

   type Pipe_Stream is limited new Ada.Finalization.Limited_Controlled
     and Output_Stream and Input_Stream with record
      Proc   : Util.Processes.Process;
   end record;

   --  Flush the stream and release the buffer.
   overriding
   procedure Finalize (Object : in out Pipe_Stream);

end Util.Streams.Pipes;
