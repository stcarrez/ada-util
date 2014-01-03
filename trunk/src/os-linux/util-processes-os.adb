-----------------------------------------------------------------------
--  util-processes-os -- System specific and low level operations
--  Copyright (C) 2011, 2012 Stephane Carrez
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

package body Util.Processes.Os is

   use Util.Systems.Os;
   use type Interfaces.C.size_t;

   type Pipe_Type is array (0 .. 1) of File_Type;
   procedure Close (Pipes : in out Pipe_Type);

   --  ------------------------------
   --  Create the output stream to read/write on the process input/output.
   --  Setup the file to be closed on exec.
   --  ------------------------------
   function Create_Stream (File : in File_Type)
                           return Util.Streams.Raw.Raw_Stream_Access is
      Stream : constant Util.Streams.Raw.Raw_Stream_Access := new Util.Streams.Raw.Raw_Stream;
      Status : constant Integer := Sys_Fcntl (File, F_SETFL, FD_CLOEXEC);
      pragma Unreferenced (Status);
   begin
      Stream.Initialize (File);
      return Stream;
   end Create_Stream;

   --  ------------------------------
   --  Wait for the process to exit.
   --  ------------------------------
   overriding
   procedure Wait (Sys     : in out System_Process;
                   Proc    : in out Process'Class;
                   Timeout : in Duration) is
      pragma Unreferenced (Sys, Timeout);

      use type Util.Streams.Output_Stream_Access;

      Result : Integer;
      Wpid   : Integer;
   begin
      --  Close the input stream pipe if there is one.
      if Proc.Input /= null then
         Util.Streams.Raw.Raw_Stream'Class (Proc.Input.all).Close;
      end if;

      Wpid := Sys_Waitpid (Integer (Proc.Pid), Result'Address, 0);
      if Wpid = Integer (Proc.Pid) then
         Proc.Exit_Value := Result / 256;
         if Result mod 256 /= 0 then
            Proc.Exit_Value := (Result mod 256) * 1000;
         end if;
      end if;
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
      pragma Unreferenced (Sys);
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      Result := Sys_Kill (Integer (Proc.Pid), Integer (Signal));
   end Stop;

   --  ------------------------------
   --  Close both ends of the pipe (used to cleanup in case or error).
   --  ------------------------------
   procedure Close (Pipes : in out Pipe_Type) is
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      if Pipes (0) /= NO_FILE then
         Result := Sys_Close (Pipes (0));
         Pipes (0) := NO_FILE;
      end if;
      if Pipes (1) /= NO_FILE then
         Result := Sys_Close (Pipes (1));
         Pipes (1) := NO_FILE;
      end if;
   end Close;

   --  ------------------------------
   --  Spawn a new process.
   --  ------------------------------
   overriding
   procedure Spawn (Sys  : in out System_Process;
                    Proc : in out Process'Class;
                    Mode : in Pipe_Mode := NONE) is
      use Util.Streams.Raw;
      use Interfaces.C.Strings;
      use type Interfaces.C.int;

      procedure Cleanup;

      --  Suppress all checks to make sure the child process will not raise any exception.
      pragma Suppress (All_Checks);

      Result : Integer;
      pragma Unreferenced (Result);

      Stdin_Pipes   : aliased Pipe_Type := (others => NO_FILE);
      Stdout_Pipes  : aliased Pipe_Type := (others => NO_FILE);
      Stderr_Pipes  : aliased Pipe_Type := (others => NO_FILE);

      procedure Cleanup is
      begin
         Close (Stdin_Pipes);
         Close (Stdout_Pipes);
         Close (Stderr_Pipes);
      end Cleanup;

   begin
      --  Since checks are disabled, verify by hand that the argv table is correct.
      if Sys.Argv = null or else Sys.Argc < 1 or else Sys.Argv (0) = Null_Ptr then
         raise Program_Error with "Invalid process argument list";
      end if;

      --  Setup the pipes.
      if Mode = READ or Mode = READ_WRITE or Mode = READ_ALL then
         if Sys_Pipe (Stdout_Pipes'Address) /= 0 then
            raise Process_Error with "Cannot create stdout pipe";
         end if;
      end if;
      if Mode = WRITE or Mode = READ_WRITE or Mode = READ_WRITE_ALL then
         if Sys_Pipe (Stdin_Pipes'Address) /= 0 then
            Cleanup;
            raise Process_Error with "Cannot create stdin pipe";
         end if;
      end if;
      if Mode = READ_ERROR then
         if Sys_Pipe (Stderr_Pipes'Address) /= 0 then
            Cleanup;
            raise Process_Error with "Cannot create stderr pipe";
         end if;
      end if;

      --  Create the new process by using vfork instead of fork.  The parent process is blocked
      --  until the child executes the exec or exits.  The child process uses the same stack
      --  as the parent.
      Proc.Pid := Sys_VFork;
      if Proc.Pid = 0 then

         --  Do not use any Ada type while in the child process.
         if Mode = READ_ALL or Mode = READ_WRITE_ALL then
            Result := Sys_Dup2 (Stdout_Pipes (1), STDERR_FILENO);
         end if;

         if Stderr_Pipes (1) /= NO_FILE then
            if Stderr_Pipes (1) /= STDERR_FILENO then
               Result := Sys_Dup2 (Stderr_Pipes (1), STDERR_FILENO);
               Result := Sys_Close (Stderr_Pipes (1));
            end if;
            Result := Sys_Close (Stderr_Pipes (0));

         elsif Sys.Err_File /= Null_Ptr then
            --  Redirect the process error to a file.
            declare
               Fd : File_Type;
            begin
               if Sys.Err_Append then
                  Fd := Sys_Open (Sys.Err_File, O_CREAT + O_WRONLY + O_APPEND, 8#644#);
               else
                  Fd := Sys_Open (Sys.Err_File, O_CREAT + O_WRONLY + O_TRUNC, 8#644#);
               end if;
               if Fd < 0 then
                  Sys_Exit (254);
               end if;
               Result := Sys_Dup2 (Fd, STDOUT_FILENO);
               Result := Sys_Close (Fd);
            end;
         end if;

         if Stdout_Pipes (1) /= NO_FILE then
            if Stdout_Pipes (1) /= STDOUT_FILENO then
               Result := Sys_Dup2 (Stdout_Pipes (1), STDOUT_FILENO);
               Result := Sys_Close (Stdout_Pipes (1));
            end if;
            Result := Sys_Close (Stdout_Pipes (0));

         elsif Sys.Out_File /= Null_Ptr then
            --  Redirect the process output to a file.
            declare
               Fd : File_Type;
            begin
               if Sys.Out_Append then
                  Fd := Sys_Open (Sys.Out_File, O_CREAT + O_WRONLY + O_APPEND, 8#644#);
               else
                  Fd := Sys_Open (Sys.Out_File, O_CREAT + O_WRONLY + O_TRUNC, 8#644#);
               end if;
               if Fd < 0 then
                  Sys_Exit (254);
               end if;
               Result := Sys_Dup2 (Fd, STDOUT_FILENO);
               Result := Sys_Close (Fd);
            end;
         end if;

         if Stdin_Pipes (0) /= NO_FILE then
            if Stdin_Pipes (0) /= STDIN_FILENO then
               Result := Sys_Dup2 (Stdin_Pipes (0), STDIN_FILENO);
               Result := Sys_Close (Stdin_Pipes (0));
            end if;
            Result := Sys_Close (Stdin_Pipes (1));

         elsif Sys.In_File /= Null_Ptr then
            --  Redirect the process input to a file.
            declare
               Fd : File_Type;
            begin
               Fd := Sys_Open (Sys.Out_File, O_RDONLY, 8#644#);
               if Fd < 0 then
                  Sys_Exit (254);
               end if;
               Result := Sys_Dup2 (Fd, STDIN_FILENO);
               Result := Sys_Close (Fd);
            end;
         end if;

         Result := Sys_Execvp (Sys.Argv (0), Sys.Argv.all);
         Sys_Exit (255);
      end if;

      --  Process creation failed, cleanup and raise an exception.
      if Proc.Pid < 0 then
         Cleanup;
         raise Process_Error with "Cannot create process";
      end if;

      if Stdin_Pipes (1) /= NO_FILE then
         Result := Sys_Close (Stdin_Pipes (0));
         Proc.Input := Create_Stream (Stdin_Pipes (1)).all'Access;
      end if;

      if Stdout_Pipes (0) /= NO_FILE then
         Result := Sys_Close (Stdout_Pipes (1));
         Proc.Output := Create_Stream (Stdout_Pipes (0)).all'Access;
      end if;

      if Stderr_Pipes (0) /= NO_FILE then
         Result := Sys_Close (Stderr_Pipes (1));
         Proc.Error := Create_Stream (Stderr_Pipes (0)).all'Access;
      end if;

   end Spawn;

   procedure Free is
     new Ada.Unchecked_Deallocation (Name => Ptr_Ptr_Array, Object => Ptr_Array);

   --  ------------------------------
   --  Append the argument to the process argument list.
   --  ------------------------------
   overriding
   procedure Append_Argument (Sys : in out System_Process;
                              Arg : in String) is
   begin
      if Sys.Argv = null then
         Sys.Argv := new Ptr_Array (0 .. 10);
      elsif Sys.Argc = Sys.Argv'Last - 1 then
         declare
            N : constant Ptr_Ptr_Array := new Ptr_Array (0 .. Sys.Argc + 32);
         begin
            N (0 .. Sys.Argc) := Sys.Argv (0 .. Sys.Argc);
            Free (Sys.Argv);
            Sys.Argv := N;
         end;
      end if;

      Sys.Argv (Sys.Argc) := Interfaces.C.Strings.New_String (Arg);
      Sys.Argc := Sys.Argc + 1;
      Sys.Argv (Sys.Argc) := Interfaces.C.Strings.Null_Ptr;
   end Append_Argument;

   --  ------------------------------
   --  Set the process input, output and error streams to redirect and use specified files.
   --  ------------------------------
   overriding
   procedure Set_Streams (Sys           : in out System_Process;
                          Input         : in String;
                          Output        : in String;
                          Error         : in String;
                          Append_Output : in Boolean;
                          Append_Error  : in Boolean) is
   begin
      if Input'Length > 0 then
         Sys.In_File := Interfaces.C.Strings.New_String (Input);
      end if;
      if Output'Length > 0 then
         Sys.Out_File   := Interfaces.C.Strings.New_String (Output);
         Sys.Out_Append := Append_Output;
      end if;
      if Error'Length > 0 then
         Sys.Err_File   := Interfaces.C.Strings.New_String (Error);
         Sys.Err_Append := Append_Error;
      end if;
   end Set_Streams;

   --  ------------------------------
   --  Deletes the storage held by the system process.
   --  ------------------------------
   overriding
   procedure Finalize (Sys : in out System_Process) is
   begin
      if Sys.Argv /= null then
         for I in Sys.Argv'Range loop
            Interfaces.C.Strings.Free (Sys.Argv (I));
         end loop;
         Free (Sys.Argv);
      end if;
      Interfaces.C.Strings.Free (Sys.In_File);
      Interfaces.C.Strings.Free (Sys.Out_File);
      Interfaces.C.Strings.Free (Sys.Err_File);
   end Finalize;

end Util.Processes.Os;



