-----------------------------------------------------------------------
--  util-processes -- Process creation and control
--  Copyright (C) 2011, 2012, 2016, 2018, 2021, 2022 Stephane Carrez
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
with Util.Streams;
with Util.Systems.Types;
with Util.Systems.Os;
with Util.Strings.Vectors;

with Ada.Finalization;
with Ada.Strings.Unbounded;
package Util.Processes is

   Invalid_State : exception;

   Process_Error : exception;

   --  The optional process pipes:
   --  <dl>
   --    <dt>NONE</dt>
   --    <dd>the process will inherit the standard input, output and error.</dd>
   --    <dt>READ</dt>
   --    <dd>a pipe is created to read the process standard output.</dd>
   --    <dt>READ_ERROR</dt>
   --    <dd>a pipe is created to read the process standard error.  The output and input are
   --    inherited.</dd>
   --    <dt>READ_ALL</dt>
   --    <dd>similar to READ the same pipe is used for the process standard error.</dd>
   --    <dt>WRITE</dt>
   --    <dd>a pipe is created to write on the process standard input.</dd>
   --    <dt>READ_WRITE</dt>
   --    <dd>Combines the <b>READ</b> and <b>WRITE</b> modes.</dd>
   --    <dt>READ_WRITE_ALL</dt>
   --    <dd>Combines the <b>READ_ALL</b> and <b>WRITE</b> modes.</dd>
   --  </dl>
   type Pipe_Mode is (NONE, READ, READ_ERROR, READ_ALL, WRITE, READ_WRITE, READ_WRITE_ALL);

   subtype String_Access is Ada.Strings.Unbounded.String_Access;

   subtype File_Type is Util.Systems.Types.File_Type;

   type Argument_List is array (Positive range <>) of String_Access;

   subtype Process_Identifier is Util.Systems.Os.Process_Identifier;
   use type Util.Systems.Os.Process_Identifier;

   --  ------------------------------
   --  Process
   --  ------------------------------
   type Process is limited private;

   --  Before launching the process, redirect the input stream of the process
   --  to the specified file.
   --  Raises <b>Invalid_State</b> if the process is running.
   procedure Set_Input_Stream (Proc : in out Process;
                               File : in String);

   --  Set the output stream of the process.
   --  Raises <b>Invalid_State</b> if the process is running.
   procedure Set_Output_Stream (Proc   : in out Process;
                                File   : in String;
                                Append : in Boolean := False);

   --  Set the error stream of the process.
   --  Raises <b>Invalid_State</b> if the process is running.
   procedure Set_Error_Stream (Proc   : in out Process;
                               File   : in String;
                               Append : in Boolean := False);

   --  Set the working directory that the process will use once it is created.
   --  The directory must exist or the <b>Invalid_Directory</b> exception will be raised.
   procedure Set_Working_Directory (Proc : in out Process;
                                    Path : in String);

   --  Set the shell executable path to use to launch a command.  The default on Unix is
   --  the /bin/sh command.  Argument splitting is done by the /bin/sh -c command.
   --  When setting an empty shell command, the argument splitting is done by the
   --  <tt>Spawn</tt> procedure.
   procedure Set_Shell (Proc  : in out Process;
                        Shell : in String);

   --  Closes the given file descriptor in the child process before executing the command.
   procedure Add_Close (Proc  : in out Process;
                        Fd    : in File_Type);

   --  Append the argument to the current process argument list.
   --  Raises <b>Invalid_State</b> if the process is running.
   procedure Append_Argument (Proc : in out Process;
                              Arg  : in String);

   --  Set the environment variable to be used by the process before its creation.
   procedure Set_Environment (Proc  : in out Process;
                              Name  : in String;
                              Value : in String);

   procedure Set_Environment (Proc    : in out Process;
                              Iterate : not null access
                                procedure
                                  (Process : not null access procedure
                                     (Name  : in String;
                                      Value : in String)));

   --  Import the default environment variables from the current process.
   procedure Set_Default_Environment (Proc : in out Process);

   --  Spawn a new process with the given command and its arguments.  The standard input, output
   --  and error streams are either redirected to a file or to a stream object.
   procedure Spawn (Proc      : in out Process;
                    Command   : in String;
                    Arguments : in Argument_List;
                    Mode      : in Pipe_Mode := NONE);
   procedure Spawn (Proc      : in out Process;
                    Arguments : in Util.Strings.Vectors.Vector;
                    Mode      : in Pipe_Mode := NONE);
   procedure Spawn (Proc      : in out Process;
                    Command   : in String;
                    Mode      : in Pipe_Mode := NONE);
   procedure Spawn (Proc      : in out Process;
                    Mode      : in Pipe_Mode := NONE);

   --  Wait for the process to terminate.
   procedure Wait (Proc : in out Process);

   --  Terminate the process by sending a signal on Unix and exiting the process on Windows.
   --  This operation is not portable and has a different behavior between Unix and Windows.
   --  Its intent is to stop the process.
   procedure Stop (Proc   : in out Process;
                   Signal : in Positive := 15);

   --  Get the process exit status.
   function Get_Exit_Status (Proc : in Process) return Integer;

   --  Get the process identifier.
   function Get_Pid (Proc : in Process) return Process_Identifier;

   --  Returns True if the process is running.
   function Is_Running (Proc : in Process) return Boolean;

   --  Get the process input stream allowing to write on the process standard input.
   function Get_Input_Stream (Proc : in Process) return Util.Streams.Output_Stream_Access;

   --  Get the process output stream allowing to read the process standard output.
   function Get_Output_Stream (Proc : in Process) return Util.Streams.Input_Stream_Access;

   --  Get the process error stream allowing to read the process standard output.
   function Get_Error_Stream (Proc : in Process) return Util.Streams.Input_Stream_Access;

private

   type File_Type_Array is array (Positive range <>) of File_Type;
   type File_Type_Array_Access is access all File_Type_Array;

   --  The <b>System_Process</b> interface is specific to the system.  On Unix, it holds the
   --  process identifier.  On Windows, more information is necessary, including the process
   --  and thread handles.  It's a little bit overkill to setup an interface for this but
   --  it looks cleaner than having specific system fields here.
   type System_Process is limited interface;
   type System_Process_Access is access all System_Process'Class;

   type Process is new Ada.Finalization.Limited_Controlled with record
      Pid        : Process_Identifier := -1;
      Sys        : System_Process_Access := null;
      Exit_Value : Integer := -1;
      Dir        : Ada.Strings.Unbounded.Unbounded_String;
      In_File    : Ada.Strings.Unbounded.Unbounded_String;
      Out_File   : Ada.Strings.Unbounded.Unbounded_String;
      Err_File   : Ada.Strings.Unbounded.Unbounded_String;
      Shell      : Ada.Strings.Unbounded.Unbounded_String;
      Out_Append : Boolean := False;
      Err_Append : Boolean := False;
      Output     : Util.Streams.Input_Stream_Access := null;
      Input      : Util.Streams.Output_Stream_Access := null;
      Error      : Util.Streams.Input_Stream_Access := null;
      To_Close   : File_Type_Array_Access;
   end record;

   --  Initialize the process instance.
   overriding
   procedure Initialize (Proc : in out Process);

   --  Deletes the process instance.
   overriding
   procedure Finalize (Proc : in out Process);

   --  Wait for the process to exit.
   procedure Wait (Sys     : in out System_Process;
                   Proc    : in out Process'Class;
                   Timeout : in Duration) is abstract;

   --  Terminate the process by sending a signal on Unix and exiting the process on Windows.
   --  This operation is not portable and has a different behavior between Unix and Windows.
   --  Its intent is to stop the process.
   procedure Stop (Sys    : in out System_Process;
                   Proc   : in out Process'Class;
                   Signal : in Positive := 15) is abstract;

   --  Spawn a new process.
   procedure Spawn (Sys  : in out System_Process;
                    Proc : in out Process'Class;
                    Mode : in Pipe_Mode := NONE) is abstract;

   --  Clear the program arguments.
   procedure Clear_Arguments (Sys : in out System_Process) is abstract;

   --  Append the argument to the process argument list.
   procedure Append_Argument (Sys : in out System_Process;
                              Arg : in String) is abstract;

   --  Set the environment variable to be used by the process before its creation.
   procedure Set_Environment (Sys   : in out System_Process;
                              Name  : in String;
                              Value : in String) is abstract;

   --  Set the process input, output and error streams to redirect and use specified files.
   procedure Set_Streams (Sys           : in out System_Process;
                          Input         : in String;
                          Output        : in String;
                          Error         : in String;
                          Append_Output : in Boolean;
                          Append_Error  : in Boolean;
                          To_Close      : in File_Type_Array_Access) is abstract;

   --  Deletes the storage held by the system process.
   procedure Finalize (Sys : in out System_Process) is abstract;

end Util.Processes;
