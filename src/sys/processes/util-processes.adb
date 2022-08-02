-----------------------------------------------------------------------
--  util-processes -- Process creation and control
--  Copyright (C) 2011, 2016, 2018, 2021, 2022 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
with Ada.Environment_Variables;

with Util.Log.Loggers;
with Util.Strings;
with Util.Processes.Os;
package body Util.Processes is

   use Util.Log;
   use Ada.Strings.Unbounded;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("Util.Processes");

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Util.Processes.System_Process'Class,
                                     Name   => Util.Processes.System_Process_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => File_Type_Array,
                                     Name   => File_Type_Array_Access);

   --  ------------------------------
   --  Before launching the process, redirect the input stream of the process
   --  to the specified file.
   --  ------------------------------
   procedure Set_Input_Stream (Proc : in out Process;
                               File : in String) is
   begin
      if Proc.Is_Running then
         Log.Error ("Cannot set input stream to {0} while process is running", File);
         raise Invalid_State with "Process is running";
      end if;
      Proc.In_File := To_Unbounded_String (File);
   end Set_Input_Stream;

   --  ------------------------------
   --  Set the output stream of the process.
   --  ------------------------------
   procedure Set_Output_Stream (Proc   : in out Process;
                                File   : in String;
                                Append : in Boolean := False) is
   begin
      if Proc.Is_Running then
         Log.Error ("Cannot set output stream to {0} while process is running", File);
         raise Invalid_State with "Process is running";
      end if;
      Proc.Out_File   := To_Unbounded_String (File);
      Proc.Out_Append := Append;
   end Set_Output_Stream;

   --  ------------------------------
   --  Set the error stream of the process.
   --  ------------------------------
   procedure Set_Error_Stream (Proc   : in out Process;
                               File   : in String;
                               Append : in Boolean := False) is
   begin
      if Proc.Is_Running then
         Log.Error ("Cannot set error stream to {0} while process is running", File);
         raise Invalid_State with "Process is running";
      end if;
      Proc.Err_File   := To_Unbounded_String (File);
      Proc.Err_Append := Append;
   end Set_Error_Stream;

   --  ------------------------------
   --  Set the working directory that the process will use once it is created.
   --  The directory must exist or the <b>Invalid_Directory</b> exception will be raised.
   --  ------------------------------
   procedure Set_Working_Directory (Proc : in out Process;
                                    Path : in String) is
   begin
      if Proc.Is_Running then
         Log.Error ("Cannot set working directory to {0} while process is running", Path);
         raise Invalid_State with "Process is running";
      end if;
      Proc.Dir := To_Unbounded_String (Path);
   end Set_Working_Directory;

   --  ------------------------------
   --  Set the shell executable path to use to launch a command.  The default on Unix is
   --  the /bin/sh command.  Argument splitting is done by the /bin/sh -c command.
   --  When setting an empty shell command, the argument splitting is done by the
   --  <tt>Spawn</tt> procedure.
   --  ------------------------------
   procedure Set_Shell (Proc  : in out Process;
                        Shell : in String) is
   begin
      if Proc.Is_Running then
         Log.Error ("Cannot set shell to {0} while process is running", Shell);
         raise Invalid_State with "Process is running";
      end if;
      Proc.Shell := To_Unbounded_String (Shell);
   end Set_Shell;

   --  ------------------------------
   --  Closes the given file descriptor in the child process before executing the command.
   --  ------------------------------
   procedure Add_Close (Proc  : in out Process;
                        Fd    : in File_Type) is
      List : File_Type_Array_Access;
   begin
      if Proc.To_Close /= null then
         List := new File_Type_Array (1 .. Proc.To_Close'Last + 1);
         List (1 .. Proc.To_Close'Last) := Proc.To_Close.all;
         List (List'Last) := Fd;
         Free (Proc.To_Close);
      else
         List := new File_Type_Array (1 .. 1);
         List (1) := Fd;
      end if;
      Proc.To_Close := List;
   end Add_Close;

   --  ------------------------------
   --  Append the argument to the current process argument list.
   --  Raises <b>Invalid_State</b> if the process is running.
   --  ------------------------------
   procedure Append_Argument (Proc : in out Process;
                              Arg  : in String) is
   begin
      if Proc.Is_Running then
         Log.Error ("Cannot add argument '{0}' while process is running", Arg);
         raise Invalid_State with "Process is running";
      end if;
      Proc.Sys.Append_Argument (Arg);
   end Append_Argument;

   --  ------------------------------
   --  Set the environment variable to be used by the process before its creation.
   --  ------------------------------
   procedure Set_Environment (Proc  : in out Process;
                              Name  : in String;
                              Value : in String) is
   begin
      if Proc.Is_Running then
         Log.Error ("Cannot set environment '{0}' while process is running", Name);
         raise Invalid_State with "Process is running";
      end if;
      Proc.Sys.Set_Environment (Name, Value);
   end Set_Environment;

   procedure Set_Environment (Proc    : in out Process;
                              Iterate : not null access
                                procedure
                                  (Process : not null access procedure
                                     (Name  : in String;
                                      Value : in String))) is
      procedure Process (Name, Value : in String);

      procedure Process (Name, Value : in String) is
      begin
         Proc.Sys.Set_Environment (Name, Value);
      end Process;

   begin
      if Proc.Is_Running then
         Log.Error ("Cannot set environment while process is running");
         raise Invalid_State with "Process is running";
      end if;
      Iterate (Process'Access);
   end Set_Environment;

   --  ------------------------------
   --  Import the default environment variables from the current process.
   --  ------------------------------
   procedure Set_Default_Environment (Proc : in out Process) is
   begin
      Set_Environment (Proc, Ada.Environment_Variables.Iterate'Access);
   end Set_Default_Environment;

   --  ------------------------------
   --  Spawn a new process with the given command and its arguments.  The standard input, output
   --  and error streams are either redirected to a file or to a stream object.
   --  ------------------------------
   procedure Spawn (Proc      : in out Process;
                    Command   : in String;
                    Arguments : in Argument_List;
                    Mode      : in Pipe_Mode := NONE) is
   begin
      if Is_Running (Proc) then
         raise Invalid_State with "A process is running";
      end if;

      Log.Info ("Starting process {0}", Command);

      Proc.Sys.Clear_Arguments;

      --  Build the argc/argv table, terminated by NULL
      Proc.Sys.Append_Argument (Command);
      for I in Arguments'Range loop
         Proc.Sys.Append_Argument (Arguments (I).all);
      end loop;

      Spawn (Proc, Mode);
   end Spawn;

   procedure Spawn (Proc      : in out Process;
                    Arguments : in Util.Strings.Vectors.Vector;
                    Mode      : in Pipe_Mode := NONE) is
      Command : constant String := Arguments.First_Element;
   begin
      if Is_Running (Proc) then
         raise Invalid_State with "A process is running";
      end if;

      Log.Info ("Starting process {0}", Command);

      Proc.Sys.Clear_Arguments;

      --  Build the argc/argv table, terminated by NULL
      for Argument of Arguments loop
         Proc.Sys.Append_Argument (Argument);
      end loop;

      Spawn (Proc, Mode);
   end Spawn;

   --  ------------------------------
   --  Spawn a new process with the given command and its arguments.  The standard input, output
   --  and error streams are either redirected to a file or to a stream object.
   --  ------------------------------
   procedure Spawn (Proc      : in out Process;
                    Command   : in String;
                    Mode      : in Pipe_Mode := NONE) is
   begin
      if Is_Running (Proc) then
         raise Invalid_State with "A process is running";
      end if;

      Log.Info ("Starting process {0}", Command);

      Proc.Sys.Clear_Arguments;

      if Length (Proc.Shell) > 0 then
         Proc.Sys.Append_Argument (To_String (Proc.Shell));
         Proc.Sys.Append_Argument ("-c");
         Proc.Sys.Append_Argument (Command);
      else
         declare
            Pos  : Natural := Command'First;
            N    : Natural;
         begin
            --  Build the argc/argv table
            while Pos <= Command'Last loop
               N := Util.Strings.Index (Command, ' ', Pos);
               if N = 0 then
                  N := Command'Last + 1;
               end if;
               Proc.Sys.Append_Argument (Command (Pos .. N - 1));
               Pos := N + 1;
            end loop;
         end;
      end if;

      Spawn (Proc, Mode);
   end Spawn;

   --  ------------------------------
   --  Spawn a new process with the given command and its arguments.  The standard input, output
   --  and error streams are either redirected to a file or to a stream object.
   --  ------------------------------
   procedure Spawn (Proc      : in out Process;
                    Mode      : in Pipe_Mode := NONE) is
   begin
      if Is_Running (Proc) then
         raise Invalid_State with "A process is running";
      end if;

      --  Prepare to redirect the input/output/error streams.
      --  The pipe mode takes precedence and will override these redirections.
      Proc.Sys.Set_Streams (Input         => To_String (Proc.In_File),
                            Output        => To_String (Proc.Out_File),
                            Error         => To_String (Proc.Err_File),
                            Append_Output => Proc.Out_Append,
                            Append_Error  => Proc.Err_Append,
                            To_Close      => Proc.To_Close);

      --  System specific spawn
      Proc.Exit_Value := -1;
      Proc.Sys.Spawn (Proc, Mode);
   end Spawn;

   --  ------------------------------
   --  Wait for the process to terminate.
   --  ------------------------------
   procedure Wait (Proc : in out Process) is
   begin
      if not Is_Running (Proc) then
         return;
      end if;

      Log.Info ("Waiting for process {0}", Process_Identifier'Image (Proc.Pid));
      Proc.Sys.Wait (Proc, -1.0);
   end Wait;

   --  ------------------------------
   --  Terminate the process by sending a signal on Unix and exiting the process on Windows.
   --  This operation is not portable and has a different behavior between Unix and Windows.
   --  Its intent is to stop the process.
   --  ------------------------------
   procedure Stop (Proc   : in out Process;
                   Signal : in Positive := 15) is
   begin
      if Is_Running (Proc) then
         Proc.Sys.Stop (Proc, Signal);
      end if;
   end Stop;

   --  ------------------------------
   --  Get the process exit status.
   --  ------------------------------
   function Get_Exit_Status (Proc : in Process) return Integer is
   begin
      return Proc.Exit_Value;
   end Get_Exit_Status;

   --  ------------------------------
   --  Get the process identifier.
   --  ------------------------------
   function Get_Pid (Proc : in Process) return Process_Identifier is
   begin
      return Proc.Pid;
   end Get_Pid;

   --  ------------------------------
   --  Returns True if the process is running.
   --  ------------------------------
   function Is_Running (Proc : in Process) return Boolean is
   begin
      return Proc.Pid > 0 and then Proc.Exit_Value < 0;
   end Is_Running;

   --  ------------------------------
   --  Get the process input stream allowing to write on the process standard input.
   --  ------------------------------
   function Get_Input_Stream (Proc : in Process) return Util.Streams.Output_Stream_Access is
   begin
      return Proc.Input;
   end Get_Input_Stream;

   --  ------------------------------
   --  Get the process output stream allowing to read the process standard output.
   --  ------------------------------
   function Get_Output_Stream (Proc : in Process) return Util.Streams.Input_Stream_Access is
   begin
      return Proc.Output;
   end Get_Output_Stream;

   --  ------------------------------
   --  Get the process error stream allowing to read the process standard output.
   --  ------------------------------
   function Get_Error_Stream (Proc : in Process) return Util.Streams.Input_Stream_Access is
   begin
      return Proc.Error;
   end Get_Error_Stream;

   --  ------------------------------
   --  Initialize the process instance.
   --  ------------------------------
   overriding
   procedure Initialize (Proc : in out Process) is
   begin
      Proc.Sys := new Util.Processes.Os.System_Process;
      Proc.Shell := To_Unbounded_String (Util.Processes.Os.SHELL);
   end Initialize;

   --  ------------------------------
   --  Deletes the process instance.
   --  ------------------------------
   overriding
   procedure Finalize (Proc : in out Process) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Util.Streams.Input_Stream'Class,
                                         Name   => Util.Streams.Input_Stream_Access);
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Util.Streams.Output_Stream'Class,
                                         Name   => Util.Streams.Output_Stream_Access);
   begin
      if Proc.Sys /= null then
         Proc.Sys.Finalize;
         Free (Proc.Sys);
      end if;
      Free (Proc.Input);
      Free (Proc.Output);
      Free (Proc.Error);
      Free (Proc.To_Close);
   end Finalize;

end Util.Processes;
