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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Conversions;
--  with Interfaces.C.Strings;

with Util.Log.Loggers;
package body Util.Processes.Os is

   use type Interfaces.C.size_t;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Processes.Os");

   --  ------------------------------
   --  Wait for the process to exit.
   --  ------------------------------
   overriding
   procedure Wait (Sys     : in out System_Process;
                   Proc    : in out Process'Class;
                   Timeout : in Duration) is

      use type Util.Streams.Output_Stream_Access;

      Result : DWORD;
      T      : DWORD;
      Code   : aliased DWORD;
      Status : BOOL;
   begin
      --  Close the input stream pipe if there is one.
      if Proc.Input /= null then
         Util.Streams.Raw.Raw_Stream'Class (Proc.Input.all).Close;
      end if;

      if Timeout < 0.0 then
         T := DWORD'Last;
      else
         T := DWORD (Timeout * 1000.0);
      end if;
      Log.Info ("Waiting {0}", DWORD'Image (T));

      Result := Wait_For_Single_Object (H    => Sys.Process_Info.hProcess,
                                        Time => T);

      Log.Info ("Status {0}", DWORD'Image (Result));

      Status := Get_Exit_Code_Process (Proc => Sys.Process_Info.hProcess,
                                       Code => Code'Unchecked_Access);
      if Status = 0 then
         Log.Error ("Process is still running.  Error {0}", Integer'Image (Get_Last_Error));
      end if;
      Proc.Exit_Value := Integer (Code);
      Log.Info ("Process exit is: {0}", Integer'Image (Proc.Exit_Value));
   end Wait;

   --  Spawn a new process.
   overriding
   procedure Spawn (Sys  : in out System_Process;
                    Proc : in out Process'Class;
                    Mode : in Pipe_Mode := NONE) is
      use Util.Streams.Raw;
      use Ada.Characters.Conversions;
      use Interfaces.C;

      Result  : Integer;
      Startup : aliased Startup_Info;
      R       : BOOL;
   begin
      --  Since checks are disabled, verify by hand that the argv table is correct.
      if Sys.Command = null or else Sys.Command'Length < 1 then
         raise Program_Error with "Invalid process argument list";
      end if;

      Startup.hStdInput  := Get_Std_Handle (STD_INPUT_HANDLE);
      Startup.hStdOutput := Get_Std_Handle (STD_OUTPUT_HANDLE);
      Startup.hStdError  := Get_Std_Handle (STD_ERROR_HANDLE);
      if Mode = READ or Mode = READ_WRITE or Mode = READ_ALL then
         Build_Output_Pipe (Proc, Startup);
      end if;
      Startup.cb := Startup'Size / 8;

      --  Start the child process.
      Result := Create_Process (System.Null_Address,
                                Sys.Command.all'Address,
                                null,
                                null,
                                True,
                                16#0#,
                                System.Null_Address,
                                System.Null_Address,
                                Startup'Unchecked_Access,
                                Sys.Process_Info'Unchecked_Access);

      if Result /= 1 then
         Result := Get_Last_Error;
         Log.Error ("Process creation failed: {0}", Integer'Image (Result));
         raise Process_Error with "Cannot create process";
      end if;
      if Mode = READ then
         R := Close_Handle (Startup.hStdOutput);
         if R = 0 then
            Result := Get_Last_Error;
            Log.Info ("Closing stdout handle: {0}", Integer'Image (Result));
         end if;
         R := Close_Handle (Startup.hStdError);
         if R = 0 then
            Result := Get_Last_Error;
            Log.Info ("Closing stderr handle: {0}", Integer'Image (Result));
         end if;
      end if;
      Proc.Pid := Process_Identifier (Sys.Process_Info.dwProcessId);
   end Spawn;

   --  ------------------------------
   --  Create the output stream to read/write on the process input/output.
   --  Setup the file to be closed on exec.
   --  ------------------------------
   function Create_Stream (File : in Util.Streams.Raw.File_Type)
                           return Util.Streams.Raw.Raw_Stream_Access is
      Stream : constant Util.Streams.Raw.Raw_Stream_Access := new Util.Streams.Raw.Raw_Stream;
   begin
      Stream.Initialize (File);
      return Stream;
   end Create_Stream;

   --  ------------------------------
   --  Build the output pipe redirection to read the process output.
   --  ------------------------------
   procedure Build_Output_Pipe (Proc : in out Process'Class;
                                Into : in out Startup_Info) is
      Sec              : aliased Security_Attributes;
      Read_Handle      : aliased HANDLE;
      Write_Handle     : aliased HANDLE;
      Read_Pipe_Handle : aliased HANDLE;
      Error_Handle     : aliased HANDLE;
      Result           : BOOL;
      Current_Proc     : constant HANDLE := Get_Current_Process;
   begin
      Sec.Length  := Sec'Size / 8;
      Sec.Inherit := True;
      Sec.Security_Descriptor := System.Null_Address;

      Result := Create_Pipe (Read_Handle  => Read_Handle'Unchecked_Access,
                             Write_Handle => Write_Handle'Unchecked_Access,
                             Attributes   => Sec'Unchecked_Access,
                             Buf_Size     => 0);
      if Result = 0 then
         Log.Error ("Cannot create pipe: {0}", Integer'Image (Get_Last_Error));
         raise Program_Error with "Cannot create pipe";
      end if;

      Result := Duplicate_Handle (SourceProcessHandle => Current_Proc,
                                  SourceHandle        => Read_Handle,
                                  TargetProcessHandle => Current_Proc,
                                  TargetHandle        => Read_Pipe_Handle'Unchecked_Access,
                                  DesiredAccess       => 0,
                                  InheritHandle       => 0,
                                  Options             => 2);
      if Result = 0 then
         raise Program_Error with "Cannot create pipe";
      end if;
      Result := Close_Handle (Read_Handle);

      Result := Duplicate_Handle (SourceProcessHandle => Current_Proc,
                                  SourceHandle        => Write_Handle,
                                  TargetProcessHandle => Current_Proc,
                                  TargetHandle        => Error_Handle'Unchecked_Access,
                                  DesiredAccess       => 0,
                                  InheritHandle       => 1,
                                  Options             => 2);
      if Result = 0 then
         raise Program_Error with "Cannot create pipe";
      end if;
      Into.dwFlags    := 16#100#;
      Into.hStdOutput := Write_Handle;
      Into.hStdError  := Error_Handle;
      Proc.Output     := Create_Stream (Read_Pipe_Handle).all'Access;
   end Build_Output_Pipe;

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => Interfaces.C.wchar_array,
                                      Name   => Wchar_Ptr);

   --  ------------------------------
   --  Append the argument to the process argument list.
   --  ------------------------------
   overriding
   procedure Append_Argument (Sys : in out System_Process;
                              Arg : in String) is
      Len : Interfaces.C.size_t := Arg'Length;
   begin
      if Sys.Command /= null then
         Len := Len + Sys.Command'Length + 2;
         declare
            S : constant Wchar_Ptr := new Interfaces.C.wchar_array (0 .. Len);
         begin
            S (Sys.Command'Range) := Sys.Command.all;
            Free (Sys.Command);
            Sys.Command := S;
         end;
         Sys.Command (Sys.Pos) := Interfaces.C.To_C (' ');
         Sys.Pos := Sys.Pos + 1;

      else
         Sys.Command := new Interfaces.C.wchar_array (0 .. Len + 1);
         Sys.Pos := 0;
      end if;
      for I in Arg'Range loop
         Sys.Command (Sys.Pos)
           := Interfaces.C.To_C (Ada.Characters.Conversions.To_Wide_Character (Arg (I)));
         Sys.Pos := Sys.Pos + 1;
      end loop;
      Sys.Command (Sys.Pos) := Interfaces.C.wide_nul;
   end Append_Argument;

   --  ------------------------------
   --  Deletes the storage held by the system process.
   --  ------------------------------
   overriding
   procedure Finalize (Sys : in out System_Process) is
   begin
      Free (Sys.Command);
   end Finalize;

end Util.Processes.Os;



