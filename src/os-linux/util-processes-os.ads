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
with System;
with Util.Streams.Raw;

private package Util.Processes.Os is

   --  Wait for the process <b>Pid</b> to finish and return the process exit status.
   procedure Waitpid (Pid    : in Process_Identifier;
                      Status : out Integer);

   --  Spawn a process
   procedure Spawn (Proc : in out Process;
                    Mode : in Pipe_Mode := NONE);

private

   --  System exit without any process cleaning.
   --  (destructors, finalizers, atexit are not called)
   procedure Sys_Exit (Code : in Integer);
   pragma Import (C, Sys_Exit, "_exit");

   --  Fork a new process
   function Sys_Fork return Process_Identifier;
   pragma Import (C, Sys_Fork, "fork");

   --  Fork a new process (vfork implementation)
   function Sys_VFork return Process_Identifier;
   pragma Import (C, Sys_VFork, "fork");

   --  Execute a process with the given arguments.
   function Sys_Execvp (File : in Ptr;
                        Args : in Ptr_Array) return Integer;
   pragma Import (C, Sys_Execvp, "execvp");

   --  Wait for the process <b>Pid</b> to finish and return
   function Sys_Waitpid (Pid     : in Integer;
                         Status  : in System.Address;
                         Options : in Integer) return Integer;
   pragma Import (C, Sys_Waitpid, "waitpid");

   --  Create a bi-directional pipe
   function Sys_Pipe (Fds : in System.Address) return Integer;
   pragma Import (C, Sys_Pipe, "pipe");

   --  Make <b>fd2</b> the copy of <b>fd1</b>
   function Sys_Dup2 (Fd1, Fd2 : in Util.Streams.Raw.File_Type) return Integer;
   pragma Import (C, Sys_Dup2, "dup2");

   --  Close a file
   function Sys_Close (Fd : in Util.Streams.Raw.File_Type) return Integer;
   pragma Import (C, Sys_Close, "close");

   --  Change the file settings
   function Sys_Fcntl (Fd    : in Util.Streams.Raw.File_Type;
                       Cmd   : in Integer;
                       Flags : in Integer) return Integer;
   pragma Import (C, Sys_Fcntl, "fcntl");

   --  Create the output stream to read/write on the process input/output.
   --  Setup the file to be closed on exec.
   function Create_Stream (File : in Util.Streams.Raw.File_Type)
                           return Util.Streams.Raw.Raw_Stream_Access;

end Util.Processes.Os;



