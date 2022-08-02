-----------------------------------------------------------------------
--  util-systems-os -- Unix system operations
--  Copyright (C) 2011, 2012, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022 Stephane Carrez
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
with Util.Systems.Constants;
with Util.Systems.Types;

--  The <b>Util.Systems.Os</b> package defines various types and operations which are specific
--  to the OS (Unix).
package Util.Systems.Os is

   --  The directory separator.
   Directory_Separator : constant Character := '/';

   --  The path separator.
   Path_Separator      : constant Character := ':';

   --  The line ending separator.
   Line_Separator      : constant String := "" & ASCII.LF;

   use Util.Systems.Constants;

   subtype Ptr is Interfaces.C.Strings.chars_ptr;
   subtype Ptr_Array is Interfaces.C.Strings.chars_ptr_array;
   type Ptr_Ptr_Array is access all Ptr_Array;

   type Process_Identifier is new Integer;

   subtype File_Type is Util.Systems.Types.File_Type;

   --  Standard file streams Posix, X/Open standard values.
   STDIN_FILENO  : constant File_Type := 0;
   STDOUT_FILENO : constant File_Type := 1;
   STDERR_FILENO : constant File_Type := 2;

   --  File is not opened
   use type Util.Systems.Types.File_Type;  --  This use clause is required by GNAT 2018 for the -1!
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

   function Close (Fd : in File_Type) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "close";

   function Read (Fd   : in File_Type;
                  Buf  : in System.Address;
                  Size : in Size_T) return Ssize_T
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "read";

   function Write (Fd   : in File_Type;
                   Buf  : in System.Address;
                   Size : in Size_T) return Ssize_T
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "write";

   --  System exit without any process cleaning.
   --  (destructors, finalizers, atexit are not called)
   procedure Sys_Exit (Code : in Integer)
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "exit";

   --  Fork a new process
   function Sys_Fork return Process_Identifier
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "fork";

   --  Fork a new process (vfork implementation)
   function Sys_VFork return Process_Identifier
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "fork";

   --  Execute a process with the given arguments.
   function Sys_Execvp (File : in Ptr;
                        Args : in Ptr_Array) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "execvp";

   --  Execute a process with the given arguments.
   function Sys_Execve (File : in Ptr;
                        Args : in Ptr_Array;
                        Envp : in Ptr_Array) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "execve";

   --  Wait for the process <b>Pid</b> to finish and return
   function Sys_Waitpid (Pid     : in Integer;
                         Status  : in System.Address;
                         Options : in Integer) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "waitpid";

   --  Get the current process identification.
   function Sys_Getpid return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "getpid";

   --  Create a bi-directional pipe
   function Sys_Pipe (Fds : in System.Address) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "pipe";

   --  Make <b>fd2</b> the copy of <b>fd1</b>
   function Sys_Dup2 (Fd1, Fd2 : in File_Type) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "dup2";

   --  Close a file
   function Sys_Close (Fd : in File_Type) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "close";

   --  Open a file
   function Sys_Open (Path  : in Ptr;
                      Flags : in Interfaces.C.int;
                      Mode  : in Util.Systems.Types.mode_t) return File_Type
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "open";

   --  Change the file settings
   function Sys_Fcntl (Fd    : in File_Type;
                       Cmd   : in Interfaces.C.int;
                       Flags : in Interfaces.C.int) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "fcntl";

   function Sys_Kill (Pid : in Integer;
                      Signal : in Integer) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "kill";

   function Sys_Stat (Path : in Ptr;
                      Stat : access Util.Systems.Types.Stat_Type) return Integer
     with Import => True, Convention => C, Link_Name => Util.Systems.Types.STAT_NAME;

   function Sys_Lstat (Path : in String;
                       Stat : access Util.Systems.Types.Stat_Type) return Integer
     with Import => True, Convention => C, Link_Name => Util.Systems.Types.LSTAT_NAME;

   function Sys_Fstat (Fs : in File_Type;
                       Stat : access Util.Systems.Types.Stat_Type) return Integer
     with Import => True, Convention => C, Link_Name => Util.Systems.Types.FSTAT_NAME;

   function Sys_Lseek (Fs : in File_Type;
                       Offset : in Util.Systems.Types.off_t;
                       Mode   : in Util.Systems.Types.Seek_Mode)
                       return Util.Systems.Types.off_t
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "lseek";

   function Sys_Ftruncate (Fs : in File_Type;
                           Length : in Util.Systems.Types.off_t) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "ftruncate";

   function Sys_Truncate (Path  : in Ptr;
                          Length : in Util.Systems.Types.off_t) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "truncate";

   function Sys_Fchmod (Fd   : in File_Type;
                        Mode : in Util.Systems.Types.mode_t) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "fchmod";

   --  Change permission of a file.
   function Sys_Chmod (Path  : in Ptr;
                       Mode  : in Util.Systems.Types.mode_t) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "chmod";

   --  Change working directory.
   function Sys_Chdir (Path : in Ptr) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "chdir";

   --  Rename a file (the Ada.Directories.Rename does not allow to use
   --  the Unix atomic file rename!)
   function Sys_Rename (Oldpath  : in String;
                        Newpath  : in String) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "rename";

   function Sys_Unlink (Path  : in String) return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "unlink";

   function Sys_Realpath (S : in Ptr;
                          R : in Ptr) return Ptr
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "realpath";

   --  Libc errno.  The __get_errno function is provided by the GNAT runtime.
   function Errno return Integer
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "__get_errno";

   function Strerror (Errno : in Integer) return Interfaces.C.Strings.chars_ptr
     with Import => True, Convention => C, Link_Name => SYMBOL_PREFIX & "strerror";

   type DIR is new System.Address;

   Null_Dir : constant DIR := DIR (System.Null_Address);

   --  Equivalent to Posix opendir (3) but handles some portability issues.
   --  We could use opendir, readdir_r and closedir but the __gnat_* alternative
   --  solves
   function Opendir (Directory : in String) return DIR
      with Import, External_Name => "__gnat_opendir", Convention => C;

   function Readdir (Directory : in DIR;
                     Buffer    : in System.Address;
                     Last      : not null access Integer) return System.Address
      with Import, External_Name => "__gnat_readdir", Convention => C;

   function Closedir (Directory : in DIR) return Integer
      with Import, External_Name => "__gnat_closedir", Convention => C;

end Util.Systems.Os;
