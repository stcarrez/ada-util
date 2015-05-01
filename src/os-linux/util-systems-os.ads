-----------------------------------------------------------------------
--  util-system-os -- Unix system operations
--  Copyright (C) 2011, 2012, 2014, 2015 Stephane Carrez
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
with Interfaces.C;
with Interfaces.C.Strings;
with Util.Processes;
with Util.Systems.Constants;
with Util.Systems.Types;

--  The <b>Util.Systems.Os</b> package defines various types and operations which are specific
--  to the OS (Unix).
package Util.Systems.Os is

   --  The directory separator.
   Directory_Separator : constant Character := '/';

   --  The path separator.
   Path_Separator      : constant Character := ':';

   subtype Ptr is Interfaces.C.Strings.chars_ptr;
   subtype Ptr_Array is Interfaces.C.Strings.chars_ptr_array;
   type Ptr_Ptr_Array is access all Ptr_Array;

   type File_Type is new Integer;

   --  Standard file streams Posix, X/Open standard values.
   STDIN_FILENO  : constant File_Type := 0;
   STDOUT_FILENO : constant File_Type := 1;
   STDERR_FILENO : constant File_Type := 2;

   --  File is not opened
   NO_FILE       : constant File_Type := -1;

   --  The following values should be externalized.  They are valid for GNU/Linux.
   F_SETFL    : constant Interfaces.C.int := Util.Systems.Constants.F_SETFL;
   FD_CLOEXEC : constant Interfaces.C.int := Util.Systems.Constants.FD_CLOEXEC;

   --  These values are specific to Linux.
   O_RDONLY   : constant Interfaces.C.int := Util.Systems.Constants.O_RDONLY;
   O_WRONLY   : constant Interfaces.C.int := Util.Systems.Constants.O_WRONLY;
   O_RDWR     : constant Interfaces.C.int := Util.Systems.Constants.O_RDWR;
   O_CREAT    : constant Interfaces.C.int := Util.Systems.Constants.O_CREAT;
   O_EXCL     : constant Interfaces.C.int := Util.Systems.Constants.O_EXCL;
   O_TRUNC    : constant Interfaces.C.int := Util.Systems.Constants.O_TRUNC;
   O_APPEND   : constant Interfaces.C.int := Util.Systems.Constants.O_APPEND;

   type Size_T is mod 2 ** Standard'Address_Size;

   type Ssize_T is range -(2 ** (Standard'Address_Size - 1))
     .. +(2 ** (Standard'Address_Size - 1)) - 1;

   function Close (Fd : in File_Type) return Integer;
   pragma Import (C, Close, "close");

   function Read (Fd   : in File_Type;
                  Buf  : in System.Address;
                  Size : in Size_T) return Ssize_T;
   pragma Import (C, Read, "read");

   function Write (Fd   : in File_Type;
                   Buf  : in System.Address;
                   Size : in Size_T) return Ssize_T;
   pragma Import (C, Write, "write");

   --  System exit without any process cleaning.
   --  (destructors, finalizers, atexit are not called)
   procedure Sys_Exit (Code : in Integer);
   pragma Import (C, Sys_Exit, "_exit");

   --  Fork a new process
   function Sys_Fork return Util.Processes.Process_Identifier;
   pragma Import (C, Sys_Fork, "fork");

   --  Fork a new process (vfork implementation)
   function Sys_VFork return Util.Processes.Process_Identifier;
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
   function Sys_Dup2 (Fd1, Fd2 : in File_Type) return Integer;
   pragma Import (C, Sys_Dup2, "dup2");

   --  Close a file
   function Sys_Close (Fd : in File_Type) return Integer;
   pragma Import (C, Sys_Close, "close");

   --  Open a file
   function Sys_Open (Path  : in Ptr;
                      Flags : in Interfaces.C.int;
                      Mode  : in Interfaces.C.int) return File_Type;
   pragma Import (C, Sys_Open, "open");

   --  Change the file settings
   function Sys_Fcntl (Fd    : in File_Type;
                       Cmd   : in Interfaces.C.int;
                       Flags : in Interfaces.C.int) return Integer;
   pragma Import (C, Sys_Fcntl, "fcntl");

   function Sys_Kill (Pid : in Integer;
                      Signal : in Integer) return Integer;
   pragma Import (C, Sys_Kill, "kill");

   function Sys_Stat (Path : in Ptr;
                      Stat : access Util.Systems.Types.Stat_Type) return Integer;
   pragma Import (C, Sys_Stat, Util.Systems.Types.STAT_NAME);

   function Sys_Fstat (Fs : in File_Type;
                       Stat : access Util.Systems.Types.Stat_Type) return Integer;
   pragma Import (C, Sys_Fstat, Util.Systems.Types.FSTAT_NAME);

end Util.Systems.Os;
