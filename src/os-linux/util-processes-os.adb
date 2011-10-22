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

package body Util.Processes.Os is

   --  The following values should be externalized.  They are valid for GNU/Linux.
   F_SETFL    : constant Integer := 4;
   FD_CLOEXEC : constant Integer := 1;

   --  ------------------------------
   --  Wait for the process <b>Pid</b> to finish and return the process exit status.
   --  ------------------------------
   procedure Waitpid (Pid    : in Process_Identifier;
                      Status : out Integer) is
      Result : Integer;
      Wpid   : Integer;
   begin
      Wpid := Sys_Waitpid (Integer (Pid), Result'Address, 0);
      if Wpid = Integer (Pid) then
         Status := Result;
      end if;
   end Waitpid;

   function Create_Stream (File : in Util.Streams.Raw.File_Type)
                           return Util.Streams.Raw.Raw_Stream_Access is
      Stream : constant Util.Streams.Raw.Raw_Stream_Access := new Util.Streams.Raw.Raw_Stream;
      Status : constant Integer := Sys_Fcntl (File, F_SETFL, FD_CLOEXEC);
   begin
      Stream.Open (File);
      return Stream;
   end Create_Stream;

   --  ------------------------------
   --  Spawn a process
   --  ------------------------------
   procedure Spawn (Proc : in out Process;
                    Mode : in Pipe_Mode := NONE) is
      use Util.Streams.Raw;
      use Interfaces.C.Strings;

      --  Suppress all checks to make sure the child process will not raise any exception.
      pragma Suppress (All_Checks);

      Result : Integer;
      Pipes  : aliased array (0 .. 1) of File_Type := (others => NO_FILE);
   begin
      --  Since checks are disabled, verify by hand that the argv table is correct.
      if Proc.Argv = null or else Proc.Argv'Length < 1 or else Proc.Argv (0) = Null_Ptr then
         raise Program_Error with "Invalid process argument list";
      end if;

      if Mode /= NONE then
         if Sys_Pipe (Pipes'Address) /= 0 then
            raise Process_Error;
         end if;
      end if;

      --  Create the new process by using vfork instead of fork.  The parent process is blocked
      --  until the child executes the exec or exits.  The child process uses the same stack
      --  as the parent.
      Proc.Pid := Sys_VFork;
      if Proc.Pid = 0 then

         --  Do not use any Ada type while in the child process.
         case Mode is
            when READ =>
               if Pipes (1) /= STDOUT_FILENO then
                  Result := Sys_Dup2 (Pipes (1), STDOUT_FILENO);
                  Result := Sys_Close (Pipes (1));
               end if;
               Result := Sys_Close (Pipes (0));

            when WRITE =>
               if Pipes (0) /= STDIN_FILENO then
                  Result := Sys_Dup2 (Pipes (0), STDIN_FILENO);
                  Result := Sys_Close (Pipes (0));
               end if;
               Result := Sys_Close (Pipes (1));

            when others =>
               null;

         end case;

         Result := Sys_Execvp (Proc.Argv (0), Proc.Argv.all);
         Sys_Exit (255);
      end if;

      --  Process creation failed, cleanup and raise an exception.
      if Proc.Pid < 0 then
         if Mode /= NONE then
            Result := Sys_Close (Pipes (0));
            Result := Sys_Close (Pipes (1));
         end if;
         raise Process_Error;
      end if;

      case Mode is
         when READ =>
            Result := Sys_Close (Pipes (1));
            Proc.Output := Create_Stream (Pipes (0)).all'Access;

         when WRITE =>
            Result := Sys_Close (Pipes (0));
            Proc.Input := Create_Stream (Pipes (1)).all'Access;

         when NONE =>
            null;

      end case;
   end Spawn;

end Util.Processes.Os;



