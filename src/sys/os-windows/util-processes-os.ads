-----------------------------------------------------------------------
--  util-processes-os -- Windows specific and low level operations
--  Copyright (C) 2011, 2012, 2016, 2018, 2019, 2021 Stephane Carrez
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
with Util.Streams.Raw;

with Util.Systems.Os;
with Util.Strings.Vectors;
with Interfaces.C;
private package Util.Processes.Os is

   use Util.Systems.Os;

   SHELL : constant String := "";

   subtype Wchar_Ptr is Util.Systems.Os.Wchar_Ptr;

   type System_Process is new Util.Processes.System_Process with record
      Process_Info : aliased Util.Systems.Os.PROCESS_INFORMATION;
      Command      : Wchar_Ptr := null;
      Pos          : Interfaces.C.size_t := 0;
      In_File      : Wchar_Ptr := null;
      Out_File     : Wchar_Ptr := null;
      Err_File     : Wchar_Ptr := null;
      Dir          : Wchar_Ptr := null;
      Env          : Wchar_Ptr := null;
      To_Close     : File_Type_Array_Access;
      Out_Append   : Boolean := False;
      Err_Append   : Boolean := False;
      Environment  : Util.Strings.Vectors.Vector;
   end record;

   --  Wait for the process to exit.
   overriding
   procedure Wait (Sys     : in out System_Process;
                   Proc    : in out Process'Class;
                   Timeout : in Duration);

   --  Spawn a new process.
   overriding
   procedure Spawn (Sys  : in out System_Process;
                    Proc : in out Process'Class;
                    Mode : in Pipe_Mode := NONE);

   --  Terminate the process by sending a signal on Unix and exiting the process on Windows.
   --  This operation is not portable and has a different behavior between Unix and Windows.
   --  Its intent is to stop the process.
   overriding
   procedure Stop (Sys    : in out System_Process;
                   Proc   : in out Process'Class;
                   Signal : in Positive := 15);

   --  Append the argument to the process argument list.
   overriding
   procedure Append_Argument (Sys : in out System_Process;
                              Arg : in String);

   --  Clear the program arguments.
   overriding
   procedure Clear_Arguments (Sys : in out System_Process);

   --  Set the environment variable to be used by the process before its creation.
   overriding
   procedure Set_Environment (Sys   : in out System_Process;
                              Name  : in String;
                              Value : in String);

   --  Set the process input, output and error streams to redirect and use specified files.
   overriding
   procedure Set_Streams (Sys           : in out System_Process;
                          Input         : in String;
                          Output        : in String;
                          Error         : in String;
                          Append_Output : in Boolean;
                          Append_Error  : in Boolean;
                          To_Close      : in File_Type_Array_Access);

   --  Deletes the storage held by the system process.
   overriding
   procedure Finalize (Sys : in out System_Process);

   --  Build the output pipe redirection to read the process output.
   procedure Build_Output_Pipe (Sys  : in out System_Process;
                                Proc : in out Process'Class;
                                Into : in out Startup_Info;
                                Mode : in Pipe_Mode);

   --  Build the input pipe redirection to write the process standard input.
   procedure Build_Input_Pipe (Sys  : in out System_Process;
                               Proc : in out Process'Class;
                               Into : in out Startup_Info);

private

   --  Create the output stream to read/write on the process input/output.
   --  Setup the file to be closed on exec.
   function Create_Stream (File : in Util.Streams.Raw.File_Type)
                           return Util.Streams.Raw.Raw_Stream_Access;

end Util.Processes.Os;



