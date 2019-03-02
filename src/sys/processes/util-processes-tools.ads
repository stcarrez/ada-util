-----------------------------------------------------------------------
--  util-processes-tools -- System specific and low level operations
--  Copyright (C) 2018 Stephane Carrez
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
