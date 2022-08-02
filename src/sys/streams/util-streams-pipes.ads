-----------------------------------------------------------------------
--  util-streams-pipes -- Pipe stream to or from a process
--  Copyright (C) 2011, 2013, 2015, 2016, 2017, 2018, 2019, 2021, 2022 Stephane Carrez
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

with Util.Processes;

--  == Pipes ==
--  The `Util.Streams.Pipes` package defines a pipe stream to or from a process.
--  It allows to launch an external program while getting the program standard output or
--  providing the program standard input.  The `Pipe_Stream` type represents the input or
--  output stream for the external program.  This is a portable interface that works on
--  Unix and Windows.
--
--  The process is created and launched by the `Open` operation.  The pipe allows
--  to read or write to the process through the `Read` and `Write` operation.
--  It is very close to the *popen* operation provided by the C stdio library.
--  First, create the pipe instance:
--
--    with Util.Streams.Pipes;
--    ...
--       Pipe : aliased Util.Streams.Pipes.Pipe_Stream;
--
--  The pipe instance can be associated with only one process at a time.
--  The process is launched by using the `Open` command and by specifying the command
--  to execute as well as the pipe redirection mode:
--
--  * `READ` to read the process standard output,
--  * `WRITE` to write the process standard input.
--
--  For example to run the `ls -l` command and read its output, we could run it by using:
--
--    Pipe.Open (Command => "ls -l", Mode => Util.Processes.READ);
--
--  The `Pipe_Stream` is not buffered and a buffer can be configured easily by using the
--  `Input_Buffer_Stream` type and connecting the buffer to the pipe so that it reads
--  the pipe to fill the buffer.  The initialization of the buffer is the following:
--
--    with Util.Streams.Buffered;
--    ...
--       Buffer : Util.Streams.Buffered.Input_Buffer_Stream;
--       ...
--       Buffer.Initialize (Input => Pipe'Unchecked_Access, Size => 1024);
--
--  And to read the process output, one can use the following:
--
--     Content : Ada.Strings.Unbounded.Unbounded_String;
--     ...
--     Buffer.Read (Into => Content);
--
--  The pipe object should be closed when reading or writing to it is finished.
--  By closing the pipe, the caller will wait for the termination of the process.
--  The process exit status can be obtained by using the `Get_Exit_Status` function.
--
--     Pipe.Close;
--     if Pipe.Get_Exit_Status /= 0 then
--        Ada.Text_IO.Put_Line ("Command exited with status "
--                              & Integer'Image (Pipe.Get_Exit_Status));
--     end if;
--
--  You will note that the `Pipe_Stream` is a limited type and thus cannot be copied.
--  When leaving the scope of the `Pipe_Stream` instance, the application will wait for
--  the process to terminate.
--
--  Before opening the pipe, it is possible to have some control on the process that
--  will be created to configure:
--
--    * The shell that will be used to launch the process,
--    * The process working directory,
--    * Redirect the process output to a file,
--    * Redirect the process error to a file,
--    * Redirect the process input from a file.
--
--  All these operations must be made before calling the `Open` procedure.
package Util.Streams.Pipes is

   use Util.Processes;

   subtype Pipe_Mode is Util.Processes.Pipe_Mode range READ .. READ_WRITE;

   --  -----------------------
   --  Pipe stream
   --  -----------------------
   --  The <b>Pipe_Stream</b> is an output/input stream that reads or writes
   --  to or from a process.
   type Pipe_Stream is limited new Output_Stream and Input_Stream with private;

   --  Set the shell executable path to use to launch a command.  The default on Unix is
   --  the /bin/sh command.  Argument splitting is done by the /bin/sh -c command.
   --  When setting an empty shell command, the argument splitting is done by the
   --  <tt>Spawn</tt> procedure.
   procedure Set_Shell (Stream : in out Pipe_Stream;
                        Shell  : in String);

   --  Before launching the process, redirect the input stream of the process
   --  to the specified file.
   --  Raises <b>Invalid_State</b> if the process is running.
   procedure Set_Input_Stream (Stream : in out Pipe_Stream;
                               File   : in String);

   --  Set the output stream of the process.
   --  Raises <b>Invalid_State</b> if the process is running.
   procedure Set_Output_Stream (Stream : in out Pipe_Stream;
                                File   : in String;
                                Append : in Boolean := False);

   --  Set the error stream of the process.
   --  Raises <b>Invalid_State</b> if the process is running.
   procedure Set_Error_Stream (Stream : in out Pipe_Stream;
                               File   : in String;
                               Append : in Boolean := False);

   --  Set the working directory that the process will use once it is created.
   --  The directory must exist or the <b>Invalid_Directory</b> exception will be raised.
   procedure Set_Working_Directory (Stream : in out Pipe_Stream;
                                    Path   : in String);

   --  Closes the given file descriptor in the child process before executing the command.
   procedure Add_Close (Stream : in out Pipe_Stream;
                        Fd     : in Util.Processes.File_Type);

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

   --  Terminate the process by sending a signal on Unix and exiting the process on Windows.
   --  This operation is not portable and has a different behavior between Unix and Windows.
   --  Its intent is to stop the process.
   procedure Stop (Stream : in out Pipe_Stream;
                   Signal : in Positive := 15);

private

   type Pipe_Stream is limited new Output_Stream and Input_Stream with record
      Proc   : Util.Processes.Process;
   end record;

end Util.Streams.Pipes;
