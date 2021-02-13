-----------------------------------------------------------------------
--  util-streams-pipes -- Pipe stream to or from a process
--  Copyright (C) 2011, 2013, 2016, 2017, 2018, 2021 Stephane Carrez
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
   --  Set the shell executable path to use to launch a command.  The default on Unix is
   --  the /bin/sh command.  Argument splitting is done by the /bin/sh -c command.
   --  When setting an empty shell command, the argument splitting is done by the
   --  <tt>Spawn</tt> procedure.
   --  -----------------------
   procedure Set_Shell (Stream : in out Pipe_Stream;
                        Shell  : in String) is
   begin
      Util.Processes.Set_Shell (Stream.Proc, Shell);
   end Set_Shell;

   --  -----------------------
   --  Before launching the process, redirect the input stream of the process
   --  to the specified file.
   --  Raises <b>Invalid_State</b> if the process is running.
   --  -----------------------
   procedure Set_Input_Stream (Stream : in out Pipe_Stream;
                               File   : in String) is
   begin
      Util.Processes.Set_Input_Stream (Stream.Proc, File);
   end Set_Input_Stream;

   --  -----------------------
   --  Set the output stream of the process.
   --  Raises <b>Invalid_State</b> if the process is running.
   --  -----------------------
   procedure Set_Output_Stream (Stream : in out Pipe_Stream;
                                File   : in String;
                                Append : in Boolean := False) is
   begin
      Util.Processes.Set_Output_Stream (Stream.Proc, File, Append);
   end Set_Output_Stream;

   --  -----------------------
   --  Set the error stream of the process.
   --  Raises <b>Invalid_State</b> if the process is running.
   --  -----------------------
   procedure Set_Error_Stream (Stream : in out Pipe_Stream;
                               File   : in String;
                               Append : in Boolean := False) is
   begin
      Util.Processes.Set_Error_Stream (Stream.Proc, File, Append);
   end Set_Error_Stream;

   --  -----------------------
   --  Closes the given file descriptor in the child process before executing the command.
   --  -----------------------
   procedure Add_Close (Stream : in out Pipe_Stream;
                        Fd     : in Util.Processes.File_Type) is
   begin
      Util.Processes.Add_Close (Stream.Proc, Fd);
   end Add_Close;

   --  -----------------------
   --  Set the working directory that the process will use once it is created.
   --  The directory must exist or the <b>Invalid_Directory</b> exception will be raised.
   --  -----------------------
   procedure Set_Working_Directory (Stream : in out Pipe_Stream;
                                    Path   : in String) is
   begin
      Util.Processes.Set_Working_Directory (Stream.Proc, Path);
   end Set_Working_Directory;

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
      Input : Streams.Input_Stream_Access := Processes.Get_Output_Stream (Stream.Proc);
   begin
      if Input = null then
         Input := Processes.Get_Error_Stream (Stream.Proc);
         if Input = null then
            raise Ada.IO_Exceptions.Status_Error with "Process is not launched";
         end if;
      end if;
      Input.Read (Into, Last);
   end Read;

   --  -----------------------
   --  Terminate the process by sending a signal on Unix and exiting the process on Windows.
   --  This operation is not portable and has a different behavior between Unix and Windows.
   --  Its intent is to stop the process.
   --  -----------------------
   procedure Stop (Stream : in out Pipe_Stream;
                   Signal : in Positive := 15) is
   begin
      Util.Processes.Stop (Stream.Proc, Signal);
   end Stop;

end Util.Streams.Pipes;
