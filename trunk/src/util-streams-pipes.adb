-----------------------------------------------------------------------
--  util-streams-raw -- Raw streams for Unix based systems
--  Copyright (C) 2011, 2013 Stephane Carrez
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

package body Util.Streams.Pipes is

   --  -----------------------
   --  Open a pipe to read or write to an external process.  The pipe is created and the
   --  command is executed with the input and output streams redirected through the pipe.
   --  -----------------------
   procedure Open (Stream  : in out Pipe_Stream;
                   Command : in String;
                   Mode    : in Pipe_Mode := READ) is
   begin
      Util.Processes.Spawn (Stream.Proc, Command, Mode);
   end Open;

   --  -----------------------
   --  Close the pipe and wait for the external process to terminate.
   --  -----------------------
   overriding
   procedure Close (Stream : in out Pipe_Stream) is
   begin
      Util.Processes.Wait (Stream.Proc);
   end Close;

   --  -----------------------
   --  Get the process exit status.
   --  -----------------------
   function Get_Exit_Status (Stream : in Pipe_Stream) return Integer is
   begin
      return Util.Processes.Get_Exit_Status (Stream.Proc);
   end Get_Exit_Status;

   --  -----------------------
   --  Returns True if the process is running.
   --  -----------------------
   function Is_Running (Stream : in Pipe_Stream) return Boolean is
   begin
      return Util.Processes.Is_Running (Stream.Proc);
   end Is_Running;

   --  -----------------------
   --  Write the buffer array to the output stream.
   --  -----------------------
   overriding
   procedure Write (Stream : in out Pipe_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
      Output : constant Streams.Output_Stream_Access := Processes.Get_Input_Stream (Stream.Proc);
   begin
      if Output = null then
         raise Ada.IO_Exceptions.Status_Error with "Process is not launched";
      end if;
      Output.Write (Buffer);
   end Write;

   --  -----------------------
   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   --  -----------------------
   overriding
   procedure Read (Stream : in out Pipe_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is
      Input : constant Streams.Input_Stream_Access := Processes.Get_Output_Stream (Stream.Proc);
   begin
      if Input = null then
         raise Ada.IO_Exceptions.Status_Error with "Process is not launched";
      end if;
      Input.Read (Into, Last);
   end Read;

   --  -----------------------
   --  Flush the stream and release the buffer.
   --  -----------------------
   overriding
   procedure Finalize (Object : in out Pipe_Stream) is
   begin
      null;
   end Finalize;

end Util.Streams.Pipes;
