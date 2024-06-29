-----------------------------------------------------------------------
--  util-processes-os -- Dummy system specific and low level operations
--  Copyright (C) 2011, 2012, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

private package Util.Processes.Os is

   SHELL : constant String := "/bin/sh";

   --  The directory separator.
   Directory_Separator : constant Character := '/';

   type System_Process is new Util.Processes.System_Process with null record;

   --  Wait for the process to exit.
   overriding
   procedure Wait (Sys     : in out System_Process;
                   Proc    : in out Process'Class;
                   Timeout : in Duration);

   overriding
   procedure Stop (Sys    : in out System_Process;
                   Proc   : in out Process'Class;
                   Signal : in Positive := 15) is null;

   --  Spawn a new process.
   overriding
   procedure Spawn (Sys  : in out System_Process;
                    Proc : in out Process'Class;
                    Mode : in Pipe_Mode := NONE);

   --  Append the argument to the process argument list.
   overriding
   procedure Append_Argument (Sys : in out System_Process;
                              Arg : in String);

   --  Set the process input, output and error streams to redirect and use specified files.
   procedure Set_Streams (Sys           : in out System_Process;
                          Input         : in String;
                          Output        : in String;
                          Error         : in String;
                          Append_Output : in Boolean;
                          Append_Error  : in Boolean;
                          To_Close      : in File_Type_Array_Access) is null;

   --  Deletes the storage held by the system process.
   overriding
   procedure Finalize (Sys : in out System_Process);

end Util.Processes.Os;



