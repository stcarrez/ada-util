-----------------------------------------------------------------------
--  util-processes-tools -- System specific and low level operations
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Strings.Vectors;
with Util.Streams.Pipes;
package Util.Processes.Tools is

   --  Execute the command and append the output in the vector array.
   --  The program output is read line by line and the standard input is closed.
   --  Return the program exit status.
   procedure Execute (Command : in String;
                      Output  : in out Util.Strings.Vectors.Vector;
                      Status  : out Integer);

   procedure Execute (Command    : in String;
                      Input_Path : in String;
                      Output     : in out Util.Strings.Vectors.Vector;
                      Status     : out Integer);

   procedure Execute (Command : in String;
                      Process : in out Util.Streams.Pipes.Pipe_Stream;
                      Output  : in out Util.Strings.Vectors.Vector;
                      Status  : out Integer);

end Util.Processes.Tools;
