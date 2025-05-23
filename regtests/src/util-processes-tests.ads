-----------------------------------------------------------------------
--  util-processes-tests - Test for processes
--  Copyright (C) 2011, 2016, 2018, 2019, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package Util.Processes.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Tests when the process is not launched
   procedure Test_No_Process (T : in out Test);

   --  Test executing a process
   procedure Test_Spawn (T : in out Test);

   --  Test pseudo TTY pipe redirection: read the process standard output
   procedure Test_TTY_Pipe (T : in out Test);

   --  Test output pipe redirection: read the process standard output
   procedure Test_Output_Pipe (T : in out Test);

   --  Test input pipe redirection: write the process standard input
   procedure Test_Input_Pipe (T : in out Test);

   --  Test error pipe redirection: read the process standard error
   procedure Test_Error_Pipe (T : in out Test);

   --  Test shell splitting.
   procedure Test_Shell_Splitting_Pipe (T : in out Test);

   --  Test launching several processes through pipes in several threads.
   procedure Test_Multi_Spawn (T : in out Test);

   --  Test output file redirection.
   procedure Test_Output_Redirect (T : in out Test);

   --  Test input file redirection.
   procedure Test_Input_Redirect (T : in out Test);

   --  Test changing working directory.
   procedure Test_Set_Working_Directory (T : in out Test);

   --  Test setting specific environment variables.
   procedure Test_Set_Environment (T : in out Test);

   --  Test various errors.
   procedure Test_Errors (T : in out Test);

   --  Test launching and stopping a process.
   procedure Test_Stop (T : in out Test);

   --  Test various errors (pipe streams).
   procedure Test_Pipe_Errors (T : in out Test);

   --  Test launching and stopping a process (pipe streams).
   procedure Test_Pipe_Stop (T : in out Test);

   --  Test the Tools.Execute operation.
   procedure Test_Tools_Execute (T : in out Test);

end Util.Processes.Tests;
