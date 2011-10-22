-----------------------------------------------------------------------
--  util-processes -- Process creation and control
--  Copyright (C) 2011 Stephane Carrez
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

with Util.Log.Loggers;
with Util.Strings;
with Util.Processes.Os;
package body Util.Processes is

   use Util.Log;
   use Ada.Strings.Unbounded;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("Util.Processes");

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
   --  Set the output stream of the process
   --  ------------------------------
   procedure Set_Output_Stream (Proc : in out Process;
                                File : in String) is
   begin
      if Proc.Is_Running then
         Log.Error ("Cannot set output stream to {0} while process is running", File);
         raise Invalid_State with "Process is running";
      end if;
      Proc.Out_File := To_Unbounded_String (File);
   end Set_Output_Stream;

   --  ------------------------------
   --  Set the error stream of the process
   --  ------------------------------
   procedure Set_Error_Stream (Proc : in out Process;
                               File : in String) is
   begin
      if Proc.Is_Running then
         Log.Error ("Cannot set error stream to {0} while process is running", File);
         raise Invalid_State with "Process is running";
      end if;
      Proc.Err_File := To_Unbounded_String (File);
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
   --  Spawn a new process with the given command and its arguments.  The standard input, output
   --  and error streams are either redirected to a file or to a stream object.
   --  ------------------------------
   procedure Spawn (Proc      : in out Process;
                    Command   : in String;
                    Arguments : in Argument_List) is
      use type Interfaces.C.size_t;

      Argc : Interfaces.C.size_t := 0;
   begin
      if Is_Running (Proc) then
         raise Invalid_State with "A process is running";
      end if;

      Log.Info ("Starting process {0}", Command);
      Free (Proc.Argv);

      --  Build the argc/argv table, terminate by NULL
      Proc.Argv := new Ptr_Array (0 .. Arguments'Length + 2);
      Proc.Argv (0) := Interfaces.C.Strings.New_String (Command);
      for I in Arguments'Range loop
         Argc := Argc + 1;
         Proc.Argv (Argc) := Interfaces.C.Strings.New_String (Arguments (I).all);
      end loop;
      Proc.Argv (Argc + 1) := Interfaces.C.Strings.Null_Ptr;

      --  System specific spawn
      Util.Processes.Os.Spawn (Proc);
   end Spawn;

   procedure Free_Ptr_Ptr_Array is
     new Ada.Unchecked_Deallocation (Name => Ptr_Ptr_Array, Object => Ptr_Array);

   --  ------------------------------
   --  Spawn a new process with the given command and its arguments.  The standard input, output
   --  and error streams are either redirected to a file or to a stream object.
   --  ------------------------------
   procedure Spawn (Proc      : in out Process;
                    Command   : in String;
                    Mode      : in Pipe_Mode := NONE) is
      use type Interfaces.C.size_t;

      Argc : Interfaces.C.size_t := 0;
      Max  : Interfaces.C.size_t := 10;
      Pos  : Natural := Command'First;
      N    : Natural;
   begin
      if Is_Running (Proc) then
         raise Invalid_State with "A process is running";
      end if;

      Log.Info ("Starting process {0}", Command);
      Free (Proc.Argv);

      --  Build the argc/argv table, terminate by NULL
      Proc.Argv := new Ptr_Array (0 .. Max + 1);
      while Pos < Command'Last loop
         N := Util.Strings.Index (Command, ' ', Pos);
         if N = 0 then
            N := Command'Last + 1;
         end if;
         if Argc = Max then
            declare
               Old : Ptr_Ptr_Array := Proc.Argv;
            begin
               Max := Max * 2;
               Proc.Argv := new Ptr_Array (0 .. Max + 1);
               Proc.Argv (Old.all'Range) := Old.all;
               Free_Ptr_Ptr_Array (Old);
            end;
         end if;
         Proc.Argv (Argc) := Interfaces.C.Strings.New_String (Command (Pos .. N - 1));
         Pos := N + 1;
         Argc := Argc + 1;
      end loop;
      Proc.Argv (Argc + 1) := Interfaces.C.Strings.Null_Ptr;

      --  System specific spawn
      Proc.Exit_Value := -1;
      Util.Processes.Os.Spawn (Proc, Mode);
   end Spawn;

   --  ------------------------------
   --  Free the argv table
   --  ------------------------------
   procedure Free (Argv : in out Ptr_Ptr_Array) is
   begin
      if Argv /= null then
         for I in Argv'Range loop
            Interfaces.C.Strings.Free (Argv (I));
         end loop;
         Free_Ptr_Ptr_Array (Argv);
      end if;
   end Free;

   --  ------------------------------
   --  Wait for the process to terminate.
   --  ------------------------------
   procedure Wait (Proc : in out Process) is
   begin
      if not Is_Running (Proc) then
         return;
      end if;

      Log.Info ("Waiting for process {0}", Process_Identifier'Image (Proc.Pid));
      Util.Processes.Os.Waitpid (Proc.Pid, Proc.Exit_Value);
   end Wait;

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
      return Proc.Pid > 0 and Proc.Exit_Value < 0;
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

   overriding
   procedure Finalize (Proc : in out Process) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Util.Streams.Input_Stream'Class,
                                         Name   => Util.Streams.Input_Stream_Access);
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Util.Streams.Output_Stream'Class,
                                         Name   => Util.Streams.Output_Stream_Access);
   begin
      Free (Proc.Input);
      Free (Proc.Output);
      Free (Proc.Error);
      Free (Proc.Argv);
   end Finalize;

end Util.Processes;

