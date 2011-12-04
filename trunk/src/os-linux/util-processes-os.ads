-----------------------------------------------------------------------
--  util-processes-os -- System specific and low level operations
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

with Util.Streams.Raw;
with Util.Systems.Os;
with Interfaces.C;

private package Util.Processes.Os is

   type System_Process is new Util.Processes.System_Process with record
      Argv  : Util.Systems.Os.Ptr_Ptr_Array := null;
      Argc  : Interfaces.C.size_t := 0;
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

   --  Append the argument to the process argument list.
   overriding
   procedure Append_Argument (Sys : in out System_Process;
                              Arg : in String);

   --  Deletes the storage held by the system process.
   overriding
   procedure Finalize (Sys : in out System_Process);

private

   --  Create the output stream to read/write on the process input/output.
   --  Setup the file to be closed on exec.
   function Create_Stream (File : in Util.Systems.Os.File_Type)
                           return Util.Streams.Raw.Raw_Stream_Access;

end Util.Processes.Os;



