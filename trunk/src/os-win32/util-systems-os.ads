-----------------------------------------------------------------------
--  util-system-os -- Windows system operations
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
with Interfaces.C;
with Interfaces.C.Strings;

--  The <b>Util.Systems.Os</b> package defines various types and operations which are specific
--  to the OS (Windows).
package Util.Systems.Os is

   --  The directory separator.
   Directory_Separator : constant Character := '\';

   --  Defines several windows specific types.
   type BOOL is mod 8;

   type WORD is new Interfaces.C.short;

   type DWORD is new Interfaces.C.unsigned_long;

   type PDWORD is access all DWORD;
   for PDWORD'Size use Standard'Address_Size;

   function Get_Last_Error return Integer;
   pragma Import (Stdcall, Get_Last_Error, "GetLastError");

   --  Some useful error codes (See Windows document "System Error Codes (0-499)").
   ERROR_BROKEN_PIPE : constant Integer := 109;

   --  ------------------------------
   --  Handle
   --  ------------------------------

   --  The windows HANDLE is defined as a void* in the C API.
   subtype HANDLE is System.Address;

   type PHANDLE is access all HANDLE;
   for PHANDLE'Size use Standard'Address_Size;

   function Wait_For_Single_Object (H : in HANDLE;
                                    Time : in DWORD) return DWORD;
   pragma Import (Stdcall, Wait_For_Single_Object, "WaitForSingleObject");

   type Security_Attributes is record
      Length              : DWORD;
      Security_Descriptor : System.Address;
      Inherit             : Boolean;
   end record;
   type LPSECURITY_ATTRIBUTES is access all Security_Attributes;
   for LPSECURITY_ATTRIBUTES'Size use Standard'Address_Size;

   --  ------------------------------
   --  File operations
   --  ------------------------------
   subtype File_Type is HANDLE;

   NO_FILE : constant File_Type := System.Null_Address;

   STD_INPUT_HANDLE  : constant DWORD := -10;
   STD_OUTPUT_HANDLE : constant DWORD := -11;
   STD_ERROR_HANDLE  : constant DWORD := -12;

   function Get_Std_Handle (Kind : in DWORD) return File_Type;
   pragma Import (Stdcall, Get_Std_Handle, "GetStdHandle");

   function Close_Handle (Fd : in File_Type) return BOOL;
   pragma Import (Stdcall, Close_Handle, "CloseHandle");

   function Duplicate_Handle (SourceProcessHandle : in HANDLE;
                              SourceHandle        : in HANDLE;
                              TargetProcessHandle : in HANDLE;
                              TargetHandle        : in PHANDLE;
                              DesiredAccess       : in DWORD;
                              InheritHandle       : in BOOL;
                              Options             : in DWORD) return BOOL;
   pragma Import  (Stdcall, Duplicate_Handle, "DuplicateHandle");

   function Read_File (Fd      : in File_Type;
                       Buf     : in System.Address;
                       Size    : in DWORD;
                       Result  : in PDWORD;
                       Overlap : in System.Address) return BOOL;
   pragma Import (Stdcall, Read_File, "ReadFile");

   function Write_File (Fd      : in File_Type;
                        Buf     : in System.Address;
                        Size    : in DWORD;
                        Result  : in PDWORD;
                        Overlap : in System.Address) return BOOL;
   pragma Import (Stdcall, Write_File, "WriteFile");

   function Create_Pipe (Read_Handle :  in PHANDLE;
                         Write_Handle : in PHANDLE;
                         Attributes   : in LPSECURITY_ATTRIBUTES;
                         Buf_Size     : in DWORD) return BOOL;
   pragma Import (Stdcall, Create_Pipe, "CreatePipe");


--     type Size_T is mod 2 ** Standard'Address_Size;


   subtype LPWSTR is Interfaces.C.Strings.chars_ptr;
   subtype PBYTE is Interfaces.C.Strings.chars_ptr;
   subtype LPCTSTR is System.Address;
   subtype LPTSTR is System.Address;
   type CommandPtr is access all Interfaces.C.wchar_array;

   NULL_STR : constant LPWSTR := Interfaces.C.Strings.Null_Ptr;

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
      hStdInput       : HANDLE := System.Null_Address;
      hStdOutput      : HANDLE := System.Null_Address;
      hStdError       : HANDLE := System.Null_Address;
   end record;
   pragma Pack (Startup_Info);
   type Startup_Info_Access is access all Startup_Info;

   type PROCESS_INFORMATION is record
      hProcess    : HANDLE := NO_FILE;
      hThread     : HANDLE := NO_FILE;
      dwProcessId : DWORD;
      dwThreadId  : DWORD;
   end record;
   type Process_Information_Access is access all PROCESS_INFORMATION;

   function Get_Current_Process return HANDLE;
   pragma Import (Stdcall, Get_Current_Process, "GetCurrentProcess");

   function Get_Exit_Code_Process (Proc : in HANDLE;
                                   Code : in PDWORD) return BOOL;
   pragma Import (Stdcall, Get_Exit_Code_Process, "GetExitCodeProcess");

   function Create_Process (Name               : in LPCTSTR;
                            Command            : in System.Address;
                            Process_Attributes : in LPSECURITY_ATTRIBUTES;
                            Thread_Attributes  : in LPSECURITY_ATTRIBUTES;
                            Inherit_Handlers   : in Boolean;
                            Creation_Flags     : in DWORD;
                            Environment        : in LPTSTR;
                            Directory          : in LPCTSTR;
                            Startup_Info       : in Startup_Info_Access;
                            Process_Info       : in Process_Information_Access) return Integer;
   pragma Import (Stdcall, Create_Process, "CreateProcessW");

private

   --  kernel32 is used on Windows32 as well as Windows64.
   pragma Linker_Options ("-lkernel32");

end Util.Systems.Os;
