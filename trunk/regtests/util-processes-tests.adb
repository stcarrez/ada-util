-----------------------------------------------------------------------
--  util-processes-tests - Test for processes
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

with Util.Log.Loggers;
with Util.Test_Caller;
with Util.Streams.Pipes;
with Util.Streams.Buffered;
with Util.Streams.Texts;
package body Util.Processes.Tests is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("Util.Processes.Tests");

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
      Caller.Add_Test (Suite, "Test Util.Streams.Pipes.Open/Read/Close (Multi spawn)",
                       Test_Multi_Spawn'Access);

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
      P.Open ("bin/util_test_process 0 write b c d e f test_marker");
      declare
         Buffer  : Util.Streams.Buffered.Buffered_Stream;
         Content : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Buffer.Initialize (null, P'Unchecked_Access, 19);
         Buffer.Read (Content);
         P.Close;
         Util.Tests.Assert_Matches (T, "b\sc\sd\se\sf\stest_marker\s", Content,
                                    "Invalid content");
      end;
      T.Assert (not P.Is_Running, "Process has stopped");
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Invalid exit status");
   end Test_Output_Pipe;

   --  ------------------------------
   --  Test input pipe redirection: write the process standard input
   --  At the same time, read the process standard output.
   --  ------------------------------
   procedure Test_Input_Pipe (T : in out Test) is
      P : aliased Util.Streams.Pipes.Pipe_Stream;
   begin
      P.Open ("bin/util_test_process 0 read -", READ_WRITE);
      declare
         Buffer  : Util.Streams.Buffered.Buffered_Stream;
         Content : Ada.Strings.Unbounded.Unbounded_String;
         Print   : Util.Streams.Texts.Print_Stream;
      begin
         --  Write on the process input stream.
         Print.Initialize (P'Unchecked_Access);
         Print.Write ("Write test on the input pipe");
         Print.Close;

         --  Read the output.
         Buffer.Initialize (null, P'Unchecked_Access, 19);
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
                     Buffer  : Util.Streams.Buffered.Buffered_Stream;
                     Content : Ada.Strings.Unbounded.Unbounded_String;
                  begin
                     Buffer.Initialize (null, Pipes (I)'Unchecked_Access, 19);
                     Buffer.Read (Content);
                     Pipes (I).Close;

                     --  Check status and output.
                     State := State and Pipes (I).Get_Exit_Status = 0;
                     State := State and Ada.Strings.Unbounded.Index (Content, "test_marker") > 0;
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

end Util.Processes.Tests;
