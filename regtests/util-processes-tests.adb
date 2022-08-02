-----------------------------------------------------------------------
--  util-processes-tests - Test for processes
--  Copyright (C) 2011, 2012, 2016, 2018, 2019, 2021, 2022 Stephane Carrez
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

with Util.Log.Loggers;
with Util.Test_Caller;
with Util.Files;
with Util.Strings.Vectors;
with Util.Streams.Pipes;
with Util.Streams.Buffered;
with Util.Streams.Texts;
with Util.Processes.Tools;
package body Util.Processes.Tests is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Processes.Tests");

   Windows  : constant Boolean := Util.Systems.Os.Directory_Separator = '\';

   package Caller is new Util.Test_Caller (Test, "Processes");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Processes.Is_Running",
                       Test_No_Process'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Spawn/Wait/Get_Exit_Status",
                       Test_Spawn'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Spawn(READ pipe)",
                       Test_Output_Pipe'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Spawn(WRITE pipe)",
                       Test_Input_Pipe'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Spawn(ERROR pipe)",
                       Test_Error_Pipe'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Spawn/Shell(WRITE pipe)",
                       Test_Shell_Splitting_Pipe'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Spawn(OUTPUT redirect)",
                       Test_Output_Redirect'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Spawn(INPUT redirect)",
                       Test_Input_Redirect'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Spawn(CHDIR)",
                       Test_Set_Working_Directory'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Spawn(Environment)",
                      Test_Set_Environment'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Spawn(errors)",
                       Test_Errors'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Pipes.Open/Read/Close (Multi spawn)",
                       Test_Multi_Spawn'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Pipes.Set_XXX (errors)",
                       Test_Pipe_Errors'Access);
      Caller.Add_Test (Suite, "Test Util.Streams.Pipes.Stop",
                       Test_Pipe_Stop'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Tools.Execute",
                       Test_Tools_Execute'Access);
      Caller.Add_Test (Suite, "Test Util.Processes.Stop",
                       Test_Stop'Access);

   end Add_Tests;

   --  ------------------------------
   --  Tests when the process is not launched
   --  ------------------------------
   procedure Test_No_Process (T : in out Test) is
      P : Process;
   begin
      T.Assert (not P.Is_Running, "Process should not be running");
      T.Assert (P.Get_Pid < 0, "Invalid process id");
   end Test_No_Process;

   --  ------------------------------
   --  Test executing a process
   --  ------------------------------
   procedure Test_Spawn (T : in out Test) is
      P : Process;
   begin
      --  Launch the test process => exit code 2
      P.Spawn ("bin/util_test_process");
      T.Assert (P.Is_Running, "Process is running");
      P.Wait;
      T.Assert (not P.Is_Running, "Process has stopped");
      T.Assert (P.Get_Pid > 0, "Invalid process id");
      Util.Tests.Assert_Equals (T, 2, P.Get_Exit_Status, "Invalid exit status");

      --  Launch the test process => exit code 0
      P.Spawn ("bin/util_test_process 0 write b c d e f");
      T.Assert (P.Is_Running, "Process is running");
      P.Wait;
      T.Assert (not P.Is_Running, "Process has stopped");
      T.Assert (P.Get_Pid > 0, "Invalid process id");
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Invalid exit status");
   end Test_Spawn;

   --  ------------------------------
   --  Test output pipe redirection: read the process standard output
   --  ------------------------------
   procedure Test_Output_Pipe (T : in out Test) is
      P : aliased Util.Streams.Pipes.Pipe_Stream;
   begin
      P.Set_Working_Directory ("bin");
      --  Note: windows does not change the working dir in the same way as Unix.
      if Windows then
         P.Open ("bin/util_test_process 0 write b c d e f test_marker");
      else
         P.Open ("./util_test_process 0 write b c d e f test_marker");
      end if;
      declare
         Buffer  : Util.Streams.Buffered.Input_Buffer_Stream;
         Content : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Buffer.Initialize (P'Unchecked_Access, 19);
         Buffer.Read (Content);
         P.Close;
         Util.Tests.Assert_Matches (T, "b\s+c\s+d\s+e\s+f\s+test_marker\s+", Content,
                                    "Invalid content");
      end;
      T.Assert (not P.Is_Running, "Process has stopped");
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Invalid exit status");
   end Test_Output_Pipe;

   --  ------------------------------
   --  Test error pipe redirection: read the process standard output
   --  ------------------------------
   procedure Test_Error_Pipe (T : in out Test) is
      P : aliased Util.Streams.Pipes.Pipe_Stream;
   begin
      P.Open ("gprbuild -z", READ_ERROR);
      declare
         Buffer  : Util.Streams.Buffered.Input_Buffer_Stream;
         Content : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Buffer.Initialize (P'Unchecked_Access, 19);
         Buffer.Read (Content);
         P.Close;
         Util.Tests.Assert_Matches (T, ".*gprbuild: .*", Content,
                                    "Invalid content");
      end;
      T.Assert (not P.Is_Running, "Process has stopped");
      --  gprbuild can exit with status 7 for newer version and status 4 for old versions.
      if P.Get_Exit_Status /= 7 then
         Util.Tests.Assert_Equals (T, 4, P.Get_Exit_Status, "Invalid exit status");
      end if;
   end Test_Error_Pipe;

   --  ------------------------------
   --  Test shell splitting.
   --  ------------------------------
   procedure Test_Shell_Splitting_Pipe (T : in out Test) is
      P : aliased Util.Streams.Pipes.Pipe_Stream;
   begin
      P.Open ("bin/util_test_process 0 write ""b c d e f"" test_marker");
      declare
         Buffer  : Util.Streams.Buffered.Input_Buffer_Stream;
         Content : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Buffer.Initialize (P'Unchecked_Access, 19);
         Buffer.Read (Content);
         P.Close;
         Util.Tests.Assert_Matches (T, "b c d e f\s+test_marker\s+", Content,
                                    "Invalid content");
      end;
      T.Assert (not P.Is_Running, "Process has stopped");
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Invalid exit status");
   end Test_Shell_Splitting_Pipe;

   --  ------------------------------
   --  Test input pipe redirection: write the process standard input
   --  At the same time, read the process standard output.
   --  ------------------------------
   procedure Test_Input_Pipe (T : in out Test) is
      P : aliased Util.Streams.Pipes.Pipe_Stream;
   begin
      P.Open ("bin/util_test_process 0 read -", READ_WRITE);
      declare
         Buffer  : Util.Streams.Buffered.Input_Buffer_Stream;
         Content : Ada.Strings.Unbounded.Unbounded_String;
         Print   : Util.Streams.Texts.Print_Stream;
      begin
         --  Write on the process input stream.
         Print.Initialize (P'Unchecked_Access);
         Print.Write ("Write test on the input pipe");
         Print.Close;

         --  Read the output.
         Buffer.Initialize (P'Unchecked_Access, 19);
         Buffer.Read (Content);

         --  Wait for the process to finish.
         P.Close;

         Util.Tests.Assert_Matches (T, "Write test on the input pipe-\s", Content,
                                    "Invalid content");
      end;
      T.Assert (not P.Is_Running, "Process has stopped");
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Invalid exit status");
   end Test_Input_Pipe;

   --  ------------------------------
   --  Test launching several processes through pipes in several threads.
   --  ------------------------------
   procedure Test_Multi_Spawn (T : in out Test) is
      Task_Count    : constant Natural := 8;
      Count_By_Task : constant Natural := 10;

      type State_Array is array (1 .. Task_Count) of Boolean;

      States : State_Array;
   begin
      declare
         task type Worker is
            entry Start (Count : in Natural);
            entry Result (Status : out Boolean);
         end Worker;

         task body Worker is
            Cnt   : Natural;
            State : Boolean := True;
         begin
            accept Start (Count : in Natural) do
               Cnt := Count;
            end Start;
            declare
               type Pipe_Array is array (1 .. Cnt) of aliased Util.Streams.Pipes.Pipe_Stream;

               Pipes : Pipe_Array;
            begin
               --  Launch the processes.
               --  They will print their arguments on stdout, one by one on each line.
               --  The expected exit status is the first argument.
               for I in 1 .. Cnt loop
                  Pipes (I).Open ("bin/util_test_process 0 write b c d e f test_marker");
               end loop;

               --  Read their output
               for I in 1 .. Cnt loop
                  declare
                     Buffer  : Util.Streams.Buffered.Input_Buffer_Stream;
                     Content : Ada.Strings.Unbounded.Unbounded_String;
                  begin
                     Buffer.Initialize (Pipes (I)'Unchecked_Access, 19);
                     Buffer.Read (Content);
                     Pipes (I).Close;

                     --  Check status and output.
                     State := State and then Pipes (I).Get_Exit_Status = 0
                       and then Ada.Strings.Unbounded.Index (Content, "test_marker") > 0;
                  end;
               end loop;

            exception
               when E : others =>
                  Log.Error ("Exception raised", E);
                  State := False;
            end;

            accept Result (Status : out Boolean) do
               Status := State;
            end Result;
         end Worker;

         type Worker_Array is array (1 .. Task_Count) of Worker;

         Tasks : Worker_Array;
      begin
         for I in Tasks'Range loop
            Tasks (I).Start (Count_By_Task);
         end loop;

         --  Get the results (do not raise any assertion here because we have to call
         --  'Result' to ensure the thread terminates.
         for I in Tasks'Range loop
            Tasks (I).Result (States (I));
         end loop;

         --  Leaving the Worker task scope means we are waiting for our tasks to finish.
      end;

      for I in States'Range loop
         T.Assert (States (I), "Task " & Natural'Image (I) & " failed");
      end loop;
   end Test_Multi_Spawn;

   --  ------------------------------
   --  Test output file redirection.
   --  ------------------------------
   procedure Test_Output_Redirect (T : in out Test) is
      P       : Process;
      Path    : constant String := Util.Tests.Get_Test_Path ("proc-output.txt");
      Content : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Util.Processes.Set_Output_Stream (P, Path);
      Util.Processes.Spawn (P, "bin/util_test_process 0 write b c d e f test_marker");
      Util.Processes.Wait (P);

      T.Assert (not P.Is_Running, "Process has stopped");
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Process failed");

      Util.Files.Read_File (Path, Content);
      Util.Tests.Assert_Matches (T, ".*test_marker", Content,
                                 "Invalid content");

      Util.Processes.Set_Output_Stream (P, Path, True);
      Util.Processes.Spawn (P, "bin/util_test_process 0 write appended_text");
      Util.Processes.Wait (P);

      Content := Ada.Strings.Unbounded.Null_Unbounded_String;
      Util.Files.Read_File (Path, Content);
      Util.Tests.Assert_Matches (T, ".*appended_text", Content,
                                 "Invalid content");
      Util.Tests.Assert_Matches (T, ".*test_marker.*", Content,
                                 "Invalid content");

   end Test_Output_Redirect;

   --  ------------------------------
   --  Test input file redirection.
   --  ------------------------------
   procedure Test_Input_Redirect (T : in out Test) is
      P        : Process;
      In_Path  : constant String := Util.Tests.Get_Path ("regtests/files/proc-input.txt");
      Exp_Path : constant String := Util.Tests.Get_Path ("regtests/expect/proc-inres.txt");
      Out_Path : constant String := Util.Tests.Get_Test_Path ("proc-inres.txt");
      Err_Path : constant String := Util.Tests.Get_Test_Path ("proc-errres.txt");
   begin
      Util.Processes.Set_Input_Stream (P, In_Path);
      Util.Processes.Set_Output_Stream (P, Out_Path);
      Util.Processes.Set_Error_Stream (P, Err_Path);
      Util.Processes.Spawn (P, "bin/util_test_process 0 read -");
      Util.Processes.Wait (P);

      T.Assert (not P.Is_Running, "Process has stopped");
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Process failed");

      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Exp_Path,
                                     Test    => Out_Path,
                                     Message => "Process input/output redirection");
   end Test_Input_Redirect;

   --  ------------------------------
   --  Test changing working directory.
   --  ------------------------------
   procedure Test_Set_Working_Directory (T : in out Test) is
      P        : Process;
      Dir_Path : constant String := Util.Tests.Get_Path ("regtests/files");
      In_Path  : constant String := Util.Tests.Get_Path ("regtests/files/proc-empty.txt");
      Exp_Path : constant String := Util.Tests.Get_Path ("regtests/files/proc-input.txt");
      Out_Path : constant String := Util.Tests.Get_Test_Path ("proc-cat.txt");
      Err_Path : constant String := Util.Tests.Get_Test_Path ("proc-errres.txt");
   begin
      Util.Processes.Set_Working_Directory (P, Dir_Path);
      Util.Processes.Set_Input_Stream (P, In_Path);
      Util.Processes.Set_Output_Stream (P, Out_Path);
      Util.Processes.Set_Error_Stream (P, Err_Path);
      Util.Processes.Spawn (P, "cat proc-input.txt");
      Util.Processes.Wait (P);

      T.Assert (not P.Is_Running, "Process has stopped");
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Process failed");

      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Exp_Path,
                                     Test    => Out_Path,
                                     Message => "Process input/output redirection");
   end Test_Set_Working_Directory;

   --  ------------------------------
   --  Test changing working directory.
   --  ------------------------------
   procedure Test_Set_Environment (T : in out Test) is
      P        : Process;
      In_Path  : constant String := Util.Tests.Get_Path ("regtests/files/proc-empty.txt");
      Exp_Path : constant String := Util.Tests.Get_Path ("regtests/expect/proc-env.txt");
      Out_Path : constant String := Util.Tests.Get_Test_Path ("proc-env.txt");
      Err_Path : constant String := Util.Tests.Get_Test_Path ("proc-errres.txt");
   begin
      Util.Processes.Set_Default_Environment (P);
      Util.Processes.Set_Environment (P, "ENV_VAR", "test1");
      Util.Processes.Set_Input_Stream (P, In_Path);
      Util.Processes.Set_Output_Stream (P, Out_Path);
      Util.Processes.Set_Error_Stream (P, Err_Path);
      if Windows then
         Util.Processes.Spawn (P, "env");
      else
         Util.Processes.Spawn (P, "env | grep ENV_VAR");
      end if;
      Util.Processes.Wait (P);

      T.Assert (not P.Is_Running, "Process has stopped");
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Process failed");

      if not Windows then
         Util.Tests.Assert_Equal_Files (T       => T,
                                        Expect  => Exp_Path,
                                        Test    => Out_Path,
                                        Message => "Process input/output redirection");
      end if;
   end Test_Set_Environment;

   --  ------------------------------
   --  Test various errors.
   --  ------------------------------
   procedure Test_Errors (T : in out Test) is
      P        : Process;
   begin
      Util.Processes.Spawn (P, "sleep 1");
      begin
         Util.Processes.Set_Working_Directory (P, "/");
         T.Fail ("Set_Working_Directory: no exception raised");

      exception
         when Invalid_State =>
            null;
      end;
      begin
         Util.Processes.Set_Input_Stream (P, "/");
         T.Fail ("Set_Input_Stream: no exception raised");

      exception
         when Invalid_State =>
            null;
      end;
      begin
         Util.Processes.Set_Output_Stream (P, "/");
         T.Fail ("Set_Output_Stream: no exception raised");

      exception
         when Invalid_State =>
            null;
      end;
      begin
         Util.Processes.Set_Error_Stream (P, ".");
         T.Fail ("Set_Error_Stream: no exception raised");

      exception
         when Invalid_State =>
            null;
      end;
      begin
         Util.Processes.Set_Environment (P, "ENV_VAR", "test1");
         T.Fail ("Set_Environment: no exception raised");

      exception
         when Invalid_State =>
            null;
      end;
      begin
         Util.Processes.Spawn (P, "sleep 1");
         T.Fail ("Spawn: no exception raised");

      exception
         when Invalid_State =>
            null;
      end;

      Util.Processes.Wait (P);

      T.Assert (not P.Is_Running, "Process has stopped");
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Process failed");
   end Test_Errors;

   --  ------------------------------
   --  Test launching and stopping a process.
   --  ------------------------------
   procedure Test_Stop (T : in out Test) is
      P        : Process;
   begin
      Util.Processes.Spawn (P, "sleep 3600");
      T.Assert (P.Is_Running, "Process is running");
      Util.Processes.Stop (P);
      Util.Processes.Wait (P);
      T.Assert (not P.Is_Running, "Process has stopped");
   end Test_Stop;

   --  ------------------------------
   --  Test various errors (pipe streams)).
   --  ------------------------------
   procedure Test_Pipe_Errors (T : in out Test) is
      P : aliased Util.Streams.Pipes.Pipe_Stream;
   begin
      P.Open ("sleep 1");
      begin
         P.Set_Working_Directory ("/");
         T.Fail ("Set_Working_Directory: no exception raised");

      exception
         when Invalid_State =>
            null;
      end;
      begin
         P.Set_Input_Stream ("/");
         T.Fail ("Set_Input_Stream: no exception raised");

      exception
         when Invalid_State =>
            null;
      end;
      begin
         P.Set_Output_Stream ("/");
         T.Fail ("Set_Output_Stream: no exception raised");

      exception
         when Invalid_State =>
            null;
      end;
      begin
         P.Set_Error_Stream (".");
         T.Fail ("Set_Error_Stream: no exception raised");

      exception
         when Invalid_State =>
            null;
      end;

      P.Close;

      T.Assert (not P.Is_Running, "Process has stopped");
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Process failed");
   end Test_Pipe_Errors;

   --  ------------------------------
   --  Test launching and stopping a process (pipe streams).
   --  ------------------------------
   procedure Test_Pipe_Stop (T : in out Test) is
      P : aliased Util.Streams.Pipes.Pipe_Stream;
   begin
      P.Open ("sleep 3600");
      T.Assert (P.Is_Running, "Process is running");
      P.Stop;
      P.Close;
      T.Assert (not P.Is_Running, "Process has stopped");
   end Test_Pipe_Stop;

   --  ------------------------------
   --  Test the Tools.Execute operation.
   --  ------------------------------
   procedure Test_Tools_Execute (T : in out Test) is
      List   : Util.Strings.Vectors.Vector;
      Status : Integer;
   begin
      Tools.Execute (Command => "bin/util_test_process 23 write ""b c d e f"" test_marker",
                     Output  => List,
                     Status  => Status);
      Util.Tests.Assert_Equals (T, 23, Status, "Invalid exit status");
      Util.Tests.Assert_Equals (T, 2, Integer (List.Length),
                                "Invalid output collected by Execute");
      Util.Tests.Assert_Equals (T, "b c d e f", List.Element (1), "");
      Util.Tests.Assert_Equals (T, "test_marker", List.Element (2), "");

      List.Clear;
      Tools.Execute (Command => "grep 'L[2-9]'",
                     Input_Path => Util.Tests.Get_Path ("regtests/files/proc-input.txt"),
                     Output => List,
                     Status => Status);
      Util.Tests.Assert_Equals (T, 0, Status, "Invalid exit status");
      Util.Tests.Assert_Equals (T, 4, Integer (List.Length),
                                "Invalid output collected by Execute");
      Util.Tests.Assert_Equals (T, "L2 ", List.Element (1), "");
      Util.Tests.Assert_Equals (T, "L3", List.Element (2), "");
      Util.Tests.Assert_Equals (T, "L4", List.Element (3), "");
      Util.Tests.Assert_Equals (T, "L5", List.Element (4), "");
   end Test_Tools_Execute;

end Util.Processes.Tests;
