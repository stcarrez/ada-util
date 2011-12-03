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
with Interfaces.C;

with Ada.Characters.Conversions;

with Util.Log.Loggers;
package body Util.Processes.Os is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Processes.Os");

   --  ------------------------------
   --  Wait for the process to exit.
   --  ------------------------------
   overriding
   procedure Wait (Sys     : in out System_Process;
                   Proc    : in out Process'Class;
                   Timeout : in Duration) is
      Result : DWORD;
      T      : DWORD;
      Code   : aliased DWORD;
      Status : BOOL;
   begin
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
      use Interfaces.C.Strings;
      use Ada.Characters.Conversions;
      use Interfaces.C;

      Result : Integer;

      Startup : aliased Startup_Info;
      Size       : Natural := 0;
      R          : BOOL;
   begin
      --  Since checks are disabled, verify by hand that the argv table is correct.
      if Proc.Argv = null or else Proc.Argv'Length < 1 or else Proc.Argv (0) = Null_Ptr then
         raise Program_Error with "Invalid process argument list";
      end if;

      for I in Proc.Argv.all'Range loop
         exit when Proc.Argv (I) = Interfaces.C.Strings.Null_Ptr;
         Size := Size + Natural (Interfaces.C.Strings.Strlen (Proc.Argv (I))) + 4;
      end loop;

      declare
         S   : Wide_String (1 .. Size);
         Pos : Natural := 1;
         Len : Natural;
         S1  : aliased wchar_array (1 .. size_t (Size));
      begin
         for I in Proc.Argv.all'Range loop
            exit when Proc.Argv (I) = Interfaces.C.Strings.Null_Ptr;
            Len := Natural (Interfaces.C.Strings.Strlen (Proc.Argv (I)));
            if Len = 0 then
               S (Pos .. Pos + 1) := "''";
               Pos := Pos + 2;
            else
               S (Pos .. Pos + Len - 1) := To_Wide_String (Value (Proc.Argv (I)));
               Pos := Pos + Len;
            end if;
            if I /= Proc.Argv.all'Last then
               S (Pos) := ' ';
               Pos := Pos + 1;
            end if;
         end loop;
         for I in S'First .. Pos loop
            S1 (Interfaces.C.size_t (I)) := Interfaces.C.To_C (S (I));
         end loop;
         S1 (Interfaces.C.size_t (Pos)) := Interfaces.C.wide_nul;

         Startup.hStdInput := Get_Std_Handle (STD_INPUT_HANDLE);
         Startup.hStdOutput := Get_Std_Handle (STD_OUTPUT_HANDLE);
         Startup.hStdError := Get_Std_Handle (STD_ERROR_HANDLE);
         if Mode = READ then
            Build_Output_Pipe (Proc, Startup);
         end if;
         Startup.cb := Startup'Size / 8;

         --  Start the child process.
         Result := Create_Process (System.Null_Address,
                                   S1'Address,
                                   null,
                                   null,
                                   True,
                                   16#0#,
                                   System.Null_Address,
                                   System.Null_Address,
                                   Startup'Unchecked_Access,
                                   Sys.Process_Info'Unchecked_Access);

      end;

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
--        Status : constant Integer := Sys_Fcntl (File, F_SETFL, FD_CLOEXEC);
--        pragma Unreferenced (Status);
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

end Util.Processes.Os;



