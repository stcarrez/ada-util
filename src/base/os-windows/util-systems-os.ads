-----------------------------------------------------------------------
--  util-system-os -- Windows system operations
--  Copyright (C) 2011, 2012, 2015, 2018, 2019, 2021, 2022 Stephane Carrez
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
with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Util.Systems.Types;
with Util.Systems.Constants;

--  The <b>Util.Systems.Os</b> package defines various types and operations which are specific
--  to the OS (Windows).
package Util.Systems.Os is

   --  The directory separator.
   Directory_Separator : constant Character := '\';

   --  The path separator.
   Path_Separator      : constant Character := ';';

   --  The line ending separator.
   Line_Separator      : constant String := "" & ASCII.CR & ASCII.LF;

   --  Defines several windows specific types.
   type BOOL is mod 8;

   type WORD is new Interfaces.C.short;

   type DWORD is new Interfaces.C.unsigned_long;

   type PDWORD is access all DWORD;
   for PDWORD'Size use Standard'Address_Size;

   type Process_Identifier is new Integer;

   function Get_Last_Error return Integer
     with Import => True, Convention => Stdcall, Link_Name => "GetLastError";

   function Errno return Integer
     with Import => True, Convention => Stdcall, Link_Name => "GetLastError";

   --  Some useful error codes (See Windows document "System Error Codes (0-499)").
   ERROR_BROKEN_PIPE : constant Integer := 109;

   --  ------------------------------
   --  Handle
   --  ------------------------------

   --  The windows HANDLE is defined as a void* in the C API.
   subtype HANDLE is Util.Systems.Types.HANDLE;

   use type Util.Systems.Types.HANDLE;

   INVALID_HANDLE_VALUE : constant HANDLE := -1;

   type PHANDLE is access all HANDLE;
   for PHANDLE'Size use Standard'Address_Size;

   function Wait_For_Single_Object (H : in HANDLE;
                                    Time : in DWORD) return DWORD
     with Import => True, Convention => Stdcall, Link_Name => "WaitForSingleObject";

   type Security_Attributes is record
      Length              : DWORD;
      Security_Descriptor : System.Address;
      Inherit             : Interfaces.C.int := 0;
   end record;
   type LPSECURITY_ATTRIBUTES is access all Security_Attributes;
   for LPSECURITY_ATTRIBUTES'Size use Standard'Address_Size;

   --  ------------------------------
   --  File operations
   --  ------------------------------
   subtype File_Type is Util.Systems.Types.File_Type;

   NO_FILE : constant File_Type := 0;

   STD_INPUT_HANDLE  : constant DWORD := 16#fffffff6#;
   STD_OUTPUT_HANDLE : constant DWORD := 16#fffffff5#;
   STD_ERROR_HANDLE  : constant DWORD := 16#fffffff4#;

   --  These values are specific to Linux.
   O_RDONLY   : constant Interfaces.C.int := Util.Systems.Constants.O_RDONLY;
   O_WRONLY   : constant Interfaces.C.int := Util.Systems.Constants.O_WRONLY;
   O_RDWR     : constant Interfaces.C.int := Util.Systems.Constants.O_RDWR;
   O_CREAT    : constant Interfaces.C.int := Util.Systems.Constants.O_CREAT;
   O_EXCL     : constant Interfaces.C.int := Util.Systems.Constants.O_EXCL;
   O_TRUNC    : constant Interfaces.C.int := Util.Systems.Constants.O_TRUNC;
   O_APPEND   : constant Interfaces.C.int := Util.Systems.Constants.O_APPEND;

   function Get_Std_Handle (Kind : in DWORD) return File_Type
     with Import => True, Convention => Stdcall, Link_Name => "GetStdHandle";

   function STDIN_FILENO return File_Type
     is (Get_Std_Handle (STD_INPUT_HANDLE));

   function STDOUT_FILENO return File_Type
     is (Get_Std_Handle (STD_OUTPUT_HANDLE));

   function STDERR_FILENO return File_Type
     is (Get_Std_Handle (STD_ERROR_HANDLE));

   function Close_Handle (Fd : in File_Type) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "CloseHandle";

   function Duplicate_Handle (SourceProcessHandle : in HANDLE;
                              SourceHandle        : in HANDLE;
                              TargetProcessHandle : in HANDLE;
                              TargetHandle        : in PHANDLE;
                              DesiredAccess       : in DWORD;
                              InheritHandle       : in BOOL;
                              Options             : in DWORD) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "DuplicateHandle";

   function Read_File (Fd      : in File_Type;
                       Buf     : in System.Address;
                       Size    : in DWORD;
                       Result  : in PDWORD;
                       Overlap : in System.Address) return BOOL
   with Import => True, Convention => Stdcall, Link_Name => "ReadFile";

   function Write_File (Fd      : in File_Type;
                        Buf     : in System.Address;
                        Size    : in DWORD;
                        Result  : in PDWORD;
                        Overlap : in System.Address) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "WriteFile";

   function Create_Pipe (Read_Handle :  in PHANDLE;
                         Write_Handle : in PHANDLE;
                         Attributes   : in LPSECURITY_ATTRIBUTES;
                         Buf_Size     : in DWORD) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "CreatePipe";

   subtype LPWSTR is Interfaces.C.Strings.chars_ptr;
   subtype LPCSTR is Interfaces.C.Strings.chars_ptr;
   subtype PBYTE is Interfaces.C.Strings.chars_ptr;
   subtype Ptr is Interfaces.C.Strings.chars_ptr;
   subtype LPCTSTR is System.Address;
   subtype LPTSTR is System.Address;
   subtype LPVOID is System.Address;
   type CommandPtr is access all Interfaces.C.wchar_array;

   NULL_STR : constant LPWSTR := Interfaces.C.Strings.Null_Ptr;

   type FileTime is record
      dwLowDateTime  : DWORD;
      dwHighDateTime : DWORD;
   end record;
   type LPFILETIME is access all FileTime;

   function To_Time (Time : in FileTime) return Util.Systems.Types.Time_Type;

   type Startup_Info is record
      cb              : DWORD := 0;
      lpReserved      : LPWSTR := NULL_STR;
      lpDesktop       : LPWSTR := NULL_STR;
      lpTitle         : LPWSTR := NULL_STR;
      dwX             : DWORD := 0;
      dwY             : DWORD := 0;
      dwXsize         : DWORD := 0;
      dwYsize         : DWORD := 0;
      dwXCountChars   : DWORD := 0;
      dwYCountChars   : DWORD := 0;
      dwFillAttribute : DWORD := 0;
      dwFlags         : DWORD := 0;
      wShowWindow     : WORD := 0;
      cbReserved2     : WORD := 0;
      lpReserved2     : PBYTE := Interfaces.C.Strings.Null_Ptr;
      hStdInput       : HANDLE := 0;
      hStdOutput      : HANDLE := 0;
      hStdError       : HANDLE := 0;
   end record;
   type Startup_Info_Access is access all Startup_Info;

   type PROCESS_INFORMATION is record
      hProcess    : HANDLE := NO_FILE;
      hThread     : HANDLE := NO_FILE;
      dwProcessId : DWORD;
      dwThreadId  : DWORD;
   end record;
   type Process_Information_Access is access all PROCESS_INFORMATION;

   function Get_Current_Process return HANDLE
     with Import => True, Convention => Stdcall, Link_Name => "GetCurrentProcess";

   function Get_Exit_Code_Process (Proc : in HANDLE;
                                   Code : in PDWORD) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "GetExitCodeProcess";

   function Create_Process (Name               : in LPCTSTR;
                            Command            : in System.Address;
                            Process_Attributes : in LPSECURITY_ATTRIBUTES;
                            Thread_Attributes  : in LPSECURITY_ATTRIBUTES;
                            Inherit_Handles    : in BOOL;
                            Creation_Flags     : in DWORD;
                            Environment        : in LPTSTR;
                            Directory          : in LPCTSTR;
                            Startup_Info       : in Startup_Info_Access;
                            Process_Info       : in Process_Information_Access)
                            return Integer
     with Import => True, Convention => Stdcall, Link_Name => "CreateProcessW";

   --  Terminate the windows process and all its threads.
   function Terminate_Process (Proc : in HANDLE;
                               Code : in DWORD) return Integer
     with Import => True, Convention => Stdcall, Link_Name => "TerminateProcess";

   function Sys_Stat (Path : in LPWSTR;
                      Stat : access Util.Systems.Types.Stat_Type) return Integer
     with Import => True, Convention => Stdcall, Link_Name => "_stat64";

   function Sys_Fstat (Fs : in File_Type;
                       Stat : access Util.Systems.Types.Stat_Type) return Integer;

   function Sys_Lstat (Path : in String;
                       Stat : access Util.Systems.Types.Stat_Type) return Integer
     with Import => True, Convention => Stdcall, Link_Name => "_stat64";

   function Sys_Lseek (Fs : in File_Type;
                       Offset : in Util.Systems.Types.off_t;
                       Mode   : in Util.Systems.Types.Seek_Mode)
                       return Util.Systems.Types.off_t;

   FILE_SHARE_WRITE         : constant DWORD := 16#02#;
   FILE_SHARE_READ          : constant DWORD := 16#01#;

   GENERIC_READ             : constant DWORD := 16#80000000#;
   GENERIC_WRITE            : constant DWORD := 16#40000000#;

   CREATE_NEW               : constant DWORD := 1;
   CREATE_ALWAYS            : constant DWORD := 2;
   OPEN_EXISTING            : constant DWORD := 3;
   OPEN_ALWAYS              : constant DWORD := 4;
   TRUNCATE_EXISTING        : constant DWORD := 5;

   FILE_APPEND_DATA         : constant DWORD := 4;

   FILE_ATTRIBUTE_ARCHIVE   : constant DWORD := 16#20#;
   FILE_ATTRIBUTE_HIDDEN    : constant DWORD := 16#02#;
   FILE_ATTRIBUTE_NORMAL    : constant DWORD := 16#80#;
   FILE_ATTRIBUTE_READONLY  : constant DWORD := 16#01#;
   FILE_ATTRIBUTE_TEMPORARY : constant DWORD := 16#100#;

   function Create_File (Name           : in LPCTSTR;
                         Desired_Access : in DWORD;
                         Share_Mode     : in DWORD;
                         Attributes     : in LPSECURITY_ATTRIBUTES;
                         Creation       : in DWORD;
                         Flags          : in DWORD;
                         Template_File  : HANDLE) return HANDLE
     with Import => True, Convention => Stdcall, Link_Name => "CreateFileW";

   function Replace_File (Replaced_File     : in LPCTSTR;
                          Replacement_File  : in LPCTSTR;
                          Backup_File       : in LPCTSTR;
                          Replace_Flags     : in DWORD;
                          Exclude           : in LPCTSTR;
                          Reserved          : in LPCTSTR) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "ReplaceFileW";

   function Move_File (Existing_File : in LPCTSTR;
                       New_File      : in LPCTSTR;
                       Flags         : in DWORD) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "MoveFileExW";

   function Get_Full_Pathname (Path   : in LPCTSTR;
                               Length : in DWORD;
                               Buffer : in LPCTSTR;
                               Ptr    : in LPCSTR) return DWORD
     with Import => True, Convention => Stdcall, Link_Name => "GetFullPathNameW";

   --  Close a file
   function Sys_Close (Fd : in File_Type) return Integer;

   --  Open a file
   function Sys_Open (Path  : in Ptr;
                      Flags : in Interfaces.C.int;
                      Mode  : in Util.Systems.Types.mode_t) return File_Type;

   function Sys_Ftruncate (Fs : in File_Type;
                           Length : in Util.Systems.Types.off_t) return Integer;

   function Sys_Fchmod (Fd   : in File_Type;
                        Mode : in Util.Systems.Types.mode_t) return Integer;

   --  Change permission of a file.
   function Sys_Chmod (Path  : in Ptr;
                       Mode  : in Util.Systems.Types.mode_t) return Integer
     with Import => True, Convention => Stdcall, Link_Name => "_chmod";

   function Sys_Realpath (S : in Ptr;
                          R : in Ptr) return Ptr;

   function Strerror (Errno : in Integer) return Interfaces.C.Strings.chars_ptr
     with Import => True, Convention => Stdcall, Link_Name => "strerror";

   function Sys_GetHandleInformation (Fd : in HANDLE; Flags : access DWORD) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "GetHandleInformation";

   type Wchar_Ptr is access all Interfaces.C.wchar_array;

   function To_WSTR (Value : in String) return Wchar_Ptr;

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => Interfaces.C.wchar_array,
                                      Name   => Wchar_Ptr);

   --  Rename a file (the Ada.Directories.Rename does not allow to use
   --  the Unix atomic file rename!)
   function Sys_Rename (Oldpath  : in String;
                        Newpath  : in String) return Integer;

   function Sys_Unlink (Path  : in String) return Integer
     with Import => True, Convention => C, Link_Name => "unlink";

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

private

   --  kernel32 is used on Windows32 as well as Windows64.
   pragma Linker_Options ("-lkernel32");

end Util.Systems.Os;
