-----------------------------------------------------------------------
--  util-processes-os -- System specific and low level operations
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

with Util.Streams.Raw;
with Util.Systems.Os;
with Interfaces.C;
with Interfaces.C.Strings;

private package Util.Processes.Os is

   SHELL : constant String := "/bin/sh";

   type System_Process is new Util.Processes.System_Process with record
      Argv       : Util.Systems.Os.Ptr_Ptr_Array := null;
      Envp       : Util.Systems.Os.Ptr_Ptr_Array := null;
      Argc       : Interfaces.C.size_t := 0;
      Envc       : Interfaces.C.size_t := 0;
      In_File    : Util.Systems.Os.Ptr := Interfaces.C.Strings.Null_Ptr;
      Out_File   : Util.Systems.Os.Ptr := Interfaces.C.Strings.Null_Ptr;
      Err_File   : Util.Systems.Os.Ptr := Interfaces.C.Strings.Null_Ptr;
      Dir        : Util.Systems.Os.Ptr := Interfaces.C.Strings.Null_Ptr;
      To_Close   : File_Type_Array_Access;
      Out_Append : Boolean := False;
      Err_Append : Boolean := False;
   end record;

   --  Wait for the process to exit.
   overriding
   procedure Wait (Sys     : in out System_Process;
                   Proc    : in out Process'Class;
                   Timeout : in Duration);

   --  Terminate the process by sending a signal on Unix and exiting the process on Windows.
   --  This operation is not portable and has a different behavior between Unix and Windows.
   --  Its intent is to stop the process.
   overriding
   procedure Stop (Sys    : in out System_Process;
                   Proc   : in out Process'Class;
                   Signal : in Positive := 15);

   --  Spawn a new process.
   overriding
   procedure Spawn (Sys  : in out System_Process;
                    Proc : in out Process'Class;
                    Mode : in Pipe_Mode := NONE);

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

private

   --  Create the output stream to read/write on the process input/output.
   --  Setup the file to be closed on exec.
   function Create_Stream (File : in Util.Systems.Os.File_Type)
                           return Util.Streams.Raw.Raw_Stream_Access;

   procedure Prepare_Working_Directory (Sys : in out System_Process;
                                        Proc : in out Process'Class);

end Util.Processes.Os;
