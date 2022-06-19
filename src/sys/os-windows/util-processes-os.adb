-----------------------------------------------------------------------
--  util-processes-os -- System specific and low level operations
--  Copyright (C) 2011, 2012, 2018, 2019, 2021, 2022 Stephane Carrez
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
with Ada.Directories;

with Util.Log.Loggers;
package body Util.Processes.Os is

   use type Interfaces.C.size_t;
   use type Util.Systems.Os.HANDLE;
   use type Ada.Directories.File_Kind;
   use type System.Address;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Processes.Os");

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => Interfaces.C.wchar_array,
                                      Name   => Wchar_Ptr);

   procedure Prepare_Working_Directory (Sys  : in out System_Process;
                                        Proc : in out Process'Class);

   procedure Prepare_Environment (Sys  : in out System_Process);

   procedure Redirect_Output (Path   : in Wchar_Ptr;
                              Append : in Boolean;
                              Output : out HANDLE);

   procedure Redirect_Input (Path  : in Wchar_Ptr;
                             Input : out HANDLE);

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
         Log.Debug ("Waiting {0} ms", DWORD'Image (T));
      end if;

      Result := Wait_For_Single_Object (H    => Sys.Process_Info.hProcess,
                                        Time => T);

      Log.Debug ("Status {0}", DWORD'Image (Result));

      Status := Get_Exit_Code_Process (Proc => Sys.Process_Info.hProcess,
                                       Code => Code'Unchecked_Access);
      if Status = 0 then
         Log.Error ("Process is still running.  Error {0}", Integer'Image (Get_Last_Error));
      end if;
      Proc.Exit_Value := Integer (Code);
      Log.Debug ("Process exit is: {0}", Integer'Image (Proc.Exit_Value));

      --  Release the resources
      if Sys.Process_Info.hProcess /= NO_FILE then
         Status := Close_Handle (Sys.Process_Info.hProcess);
         Sys.Process_Info.hProcess := NO_FILE;
      end if;
      if Sys.Process_Info.hThread /= NO_FILE then
         Status := Close_Handle (Sys.Process_Info.hThread);
         Sys.Process_Info.hThread := NO_FILE;
      end if;

      Free (Sys.Dir);
      Free (Sys.Env);
      Free (Sys.Command);
   end Wait;

   --  ------------------------------
   --  Terminate the process by sending a signal on Unix and exiting the process on Windows.
   --  This operation is not portable and has a different behavior between Unix and Windows.
   --  Its intent is to stop the process.
   --  ------------------------------
   overriding
   procedure Stop (Sys    : in out System_Process;
                   Proc   : in out Process'Class;
                   Signal : in Positive := 15) is
      pragma Unreferenced (Proc);
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      Result := Terminate_Process (Sys.Process_Info.hProcess, DWORD (Signal));
   end Stop;

   procedure Prepare_Working_Directory (Sys  : in out System_Process;
                                        Proc : in out Process'Class) is
      Dir : constant String := Ada.Strings.Unbounded.To_String (Proc.Dir);
   begin
      Free (Sys.Dir);
      if Dir'Length > 0 then
         if not Ada.Directories.Exists (Dir)
           or else Ada.Directories.Kind (Dir) /= Ada.Directories.Directory
         then
            raise Ada.Directories.Name_Error with "Invalid directory: " & Dir;
         end if;
         Sys.Dir := To_WSTR (Dir);
      end if;
   end Prepare_Working_Directory;

   procedure Prepare_Environment (Sys  : in out System_Process) is
      Len    : Interfaces.C.size_t := 2;
      Result : Wchar_Ptr;
      Pos    : Interfaces.C.size_t := 0;
   begin
      Free (Sys.Env);
      if Sys.Environment.Is_Empty then
         return;
      end if;
      for Env of Sys.Environment loop
         Len := Len + Env'Length + 1;
      end loop;
      Result := new Interfaces.C.wchar_array (0 .. Len);
      for Env of Sys.Environment loop
         for C of Env loop
            Result (Pos) := Interfaces.C.To_C (Ada.Characters.Conversions.To_Wide_Character (C));
            Pos := Pos + 1;
         end loop;
         Result (Pos) := Interfaces.C.wide_nul;
         Pos := Pos + 1;
      end loop;
      Result (Pos) := Interfaces.C.wide_nul;
      Result (Pos + 1) := Interfaces.C.wide_nul;
      Sys.Env := Result;
   end Prepare_Environment;

   procedure Redirect_Output (Path   : in Wchar_Ptr;
                              Append : in Boolean;
                              Output : out HANDLE) is
      Sec : aliased Security_Attributes;
   begin
      Sec.Length := Security_Attributes'Size / 8;
      Sec.Security_Descriptor := System.Null_Address;
      Sec.Inherit := 1;

      Output := Create_File (Path.all'Address,
                             (if Append then FILE_APPEND_DATA else GENERIC_WRITE),
                             FILE_SHARE_WRITE + FILE_SHARE_READ,
                             Sec'Unchecked_Access,
                             (if Append then OPEN_ALWAYS else CREATE_ALWAYS),
                             FILE_ATTRIBUTE_NORMAL,
                             NO_FILE);
      if Output = INVALID_HANDLE_VALUE then
         Log.Error ("Cannot create process output file: {0}", Integer'Image (Get_Last_Error));
         raise Process_Error with "Cannot create process output file";
      end if;
   end Redirect_Output;

   procedure Redirect_Input (Path  : in Wchar_Ptr;
                             Input : out HANDLE) is
      Sec : aliased Security_Attributes;
   begin
      Sec.Length := Security_Attributes'Size / 8;
      Sec.Security_Descriptor := System.Null_Address;
      Sec.Inherit := 1;

      Input := Create_File (Path.all'Address,
                            GENERIC_READ,
                            FILE_SHARE_READ,
                            Sec'Unchecked_Access,
                            OPEN_EXISTING,
                            FILE_ATTRIBUTE_NORMAL,
                            NO_FILE);
      if Input = INVALID_HANDLE_VALUE then
         Log.Error ("Cannot open process input file: {0}", Integer'Image (Get_Last_Error));
         raise Process_Error with "Cannot open process input file";
      end if;
   end Redirect_Input;

   --  ------------------------------
   --  Spawn a new process.
   --  ------------------------------
   overriding
   procedure Spawn (Sys  : in out System_Process;
                    Proc : in out Process'Class;
                    Mode : in Pipe_Mode := NONE) is
      use Interfaces.C;

      Result  : Integer;
      Startup : aliased Startup_Info;
      R       : BOOL with Unreferenced;
      Flags   : DWORD;
   begin
      Sys.Prepare_Working_Directory (Proc);
      Sys.Prepare_Environment;

      --  Since checks are disabled, verify by hand that the argv table is correct.
      if Sys.Command = null or else Sys.Command'Length < 1 then
         raise Program_Error with "Invalid process argument list";
      end if;

      Startup.cb         := Startup'Size / 8;
      Startup.hStdInput  := Get_Std_Handle (STD_INPUT_HANDLE);
      Startup.hStdOutput := Get_Std_Handle (STD_OUTPUT_HANDLE);
      Startup.hStdError  := Get_Std_Handle (STD_ERROR_HANDLE);

      if Sys.Out_File /= null then
         Redirect_Output (Sys.Out_File, Sys.Out_Append, Startup.hStdOutput);
         Startup.dwFlags := 16#100#;
      end if;
      if Sys.Err_File /= null then
         Redirect_Output (Sys.Err_File, Sys.Err_Append, Startup.hStdError);
         Startup.dwFlags := 16#100#;
      end if;
      if Sys.In_File /= null then
         Redirect_Input (Sys.In_File, Startup.hStdInput);
         Startup.dwFlags := 16#100#;
      end if;
      if Mode = WRITE or Mode = READ_WRITE or Mode = READ_WRITE_ALL then
         Build_Input_Pipe (Sys, Proc, Startup);
      end if;
      if Mode = READ or Mode = READ_WRITE or Mode = READ_ALL or Mode = READ_WRITE_ALL then
         Build_Output_Pipe (Sys, Proc, Startup, Mode);
      end if;
      if Mode = READ_ERROR then
         Build_Output_Pipe (Sys, Proc, Startup, Mode);
      end if;
      if Sys.Env /= null then
         Flags := 16#400#; --  CREATE_UNICODE_ENVIRONMENT
      else
         Flags := 0;
      end if;

      --  Start the child process.
      Result := Create_Process
        (System.Null_Address,
         Sys.Command.all'Address,
         null,
         null,
         1,
         Flags,
         (if Sys.Env /= null then Sys.Env.all'Address else System.Null_Address),
         (if Sys.Dir /= null then Sys.Dir.all'Address else System.Null_Address),
         Startup'Unchecked_Access,
         Sys.Process_Info'Unchecked_Access);

      --  Close the handles which are not necessary.
      if Startup.hStdInput /= Get_Std_Handle (STD_INPUT_HANDLE) then
         R := Close_Handle (Startup.hStdInput);
      end if;
      if Startup.hStdOutput /= Get_Std_Handle (STD_OUTPUT_HANDLE) then
         R := Close_Handle (Startup.hStdOutput);
      end if;
      if Startup.hStdError /= Get_Std_Handle (STD_ERROR_HANDLE) then
         R := Close_Handle (Startup.hStdError);
      end if;
      if Result /= 1 then
         Result := Get_Last_Error;
         Log.Error ("Process creation failed: {0}", Integer'Image (Result));
         raise Process_Error with "Cannot create process " & Integer'Image (Result);
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
   procedure Build_Output_Pipe (Sys  : in out System_Process;
                                Proc : in out Process'Class;
                                Into : in out Startup_Info;
                                Mode : in Pipe_Mode) is
      Sec              : aliased Security_Attributes;
      Read_Handle      : aliased HANDLE;
      Write_Handle     : aliased HANDLE;
      Read_Pipe_Handle : aliased HANDLE;
      Error_Handle     : aliased HANDLE;
      Result           : BOOL;
      R                : BOOL with Unreferenced;
      Current_Proc     : constant HANDLE := Get_Current_Process;
      Redirect_Error   : constant Boolean := Mode = READ_ALL or Mode = READ_WRITE_ALL;
   begin
      Sec.Length  := Sec'Size / 8;
      Sec.Inherit := 1;
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
         Log.Error ("Cannot create pipe: {0}", Integer'Image (Get_Last_Error));
         raise Program_Error with "Cannot create pipe";
      end if;
      R := Close_Handle (Read_Handle);

      if Redirect_Error and Sys.Out_File = null and Sys.Err_File = null then
         Result := Duplicate_Handle (SourceProcessHandle => Current_Proc,
                                     SourceHandle        => Write_Handle,
                                     TargetProcessHandle => Current_Proc,
                                     TargetHandle        => Error_Handle'Unchecked_Access,
                                     DesiredAccess       => 0,
                                     InheritHandle       => 1,
                                     Options             => 2);
         if Result = 0 then
            Log.Error ("Cannot create pipe: {0}", Integer'Image (Get_Last_Error));
            raise Program_Error with "Cannot create pipe";
         end if;
      elsif Sys.Out_File /= null or Mode = READ_ERROR then
         Error_Handle := Write_Handle;
      end if;
      Into.dwFlags    := 16#100#;
      if Sys.Out_File = null then
         Into.hStdOutput := Write_Handle;
      end if;
      if (Redirect_Error and Sys.Err_File = null) or Mode = READ_ERROR then
         Into.hStdError  := Error_Handle;
      end if;
      Proc.Output     := Create_Stream (Read_Pipe_Handle).all'Access;
   end Build_Output_Pipe;

   --  ------------------------------
   --  Build the input pipe redirection to write the process standard input.
   --  ------------------------------
   procedure Build_Input_Pipe (Sys  : in out System_Process;
                               Proc : in out Process'Class;
                               Into : in out Startup_Info) is
      pragma Unreferenced (Sys);

      Sec               : aliased Security_Attributes;
      Read_Handle       : aliased HANDLE;
      Write_Handle      : aliased HANDLE;
      Write_Pipe_Handle : aliased HANDLE;
      Result            : BOOL;
      R                 : BOOL with Unreferenced;
      Current_Proc      : constant HANDLE := Get_Current_Process;
   begin
      Sec.Length  := Sec'Size / 8;
      Sec.Inherit := 1;
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
                                  SourceHandle        => Write_Handle,
                                  TargetProcessHandle => Current_Proc,
                                  TargetHandle        => Write_Pipe_Handle'Unchecked_Access,
                                  DesiredAccess       => 0,
                                  InheritHandle       => 0,
                                  Options             => 2);
      if Result = 0 then
         Log.Error ("Cannot create pipe: {0}", Integer'Image (Get_Last_Error));
         raise Program_Error with "Cannot create pipe";
      end if;
      R := Close_Handle (Write_Handle);

      Into.dwFlags   := 16#100#;
      Into.hStdInput := Read_Handle;
      Proc.Input     := Create_Stream (Write_Pipe_Handle).all'Access;
   end Build_Input_Pipe;

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
   --  Clear the program arguments.
   --  ------------------------------
   overriding
   procedure Clear_Arguments (Sys : in out System_Process) is
   begin
      Sys.Pos := 0;
   end Clear_Arguments;

   --  ------------------------------
   --  Set the environment variable to be used by the process before its creation.
   --  ------------------------------
   overriding
   procedure Set_Environment (Sys   : in out System_Process;
                              Name  : in String;
                              Value : in String) is
   begin
      if not Sys.Environment.Is_Empty then
         for Iter in Sys.Environment.Iterate loop
            declare
               Env : constant String := Sys.Environment (Iter);
            begin
               if Util.Strings.Starts_With (Env, Name & "=") then
                  Sys.Environment (Iter) := Name & "=" & Value;
                  return;
               end if;
            end;
         end loop;
      end if;
      Sys.Environment.Append (Name & "=" & Value);
   end Set_Environment;

   --  ------------------------------
   --  Set the process input, output and error streams to redirect and use specified files.
   --  ------------------------------
   overriding
   procedure Set_Streams (Sys           : in out System_Process;
                          Input         : in String;
                          Output        : in String;
                          Error         : in String;
                          Append_Output : in Boolean;
                          Append_Error  : in Boolean;
                          To_Close      : in File_Type_Array_Access) is
   begin
      if Input'Length > 0 then
         Log.Info ("Redirect input {0}", Input);
         Sys.In_File := To_WSTR (Input);
      end if;
      if Output'Length > 0 then
         Log.Info ("Redirect output {0}", Output);
         Sys.Out_File   := To_WSTR (Output);
         Sys.Out_Append := Append_Output;
      end if;
      if Error'Length > 0 then
         Sys.Err_File   := To_WSTR (Error);
         Sys.Err_Append := Append_Error;
      end if;
      Sys.To_Close := To_Close;
   end Set_Streams;

   --  ------------------------------
   --  Deletes the storage held by the system process.
   --  ------------------------------
   overriding
   procedure Finalize (Sys : in out System_Process) is
      Result : BOOL;
      pragma Unreferenced (Result);
   begin
      if Sys.Process_Info.hProcess /= NO_FILE then
         Result := Close_Handle (Sys.Process_Info.hProcess);
         Sys.Process_Info.hProcess := NO_FILE;
      end if;
      if Sys.Process_Info.hThread /= NO_FILE then
         Result := Close_Handle (Sys.Process_Info.hThread);
         Sys.Process_Info.hThread := NO_FILE;
      end if;
      Free (Sys.In_File);
      Free (Sys.Out_File);
      Free (Sys.Err_File);
      Free (Sys.Dir);
      Free (Sys.Env);
      Free (Sys.Command);
   end Finalize;

end Util.Processes.Os;
