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

with Util.Systems.Os;
with Util.Streams.Texts;
with Util.Streams.Pipes;
package body Util.Processes.Tools is

   --  ------------------------------
   --  Execute the command and append the output in the vector array.
   --  The program output is read line by line and the standard input is closed.
   --  Return the program exit status.
   --  ------------------------------
   procedure Execute (Command : in String;
                      Output  : in out Util.Strings.Vectors.Vector;
                      Status  : out Integer) is
      Proc    : aliased Util.Streams.Pipes.Pipe_Stream;
      Text    : Util.Streams.Texts.Reader_Stream;
   begin
      Proc.Add_Close (Util.Systems.Os.STDIN_FILENO);
      Text.Initialize (Proc'Unchecked_Access);
      Proc.Open (Command, Util.Processes.READ);
      while not Text.Is_Eof loop
         declare
            Line : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Text.Read_Line (Line, Strip => True);
            exit when Ada.Strings.Unbounded.Length (Line) = 0;
            Output.Append (Ada.Strings.Unbounded.To_String (Line));
         end;
      end loop;
      Proc.Close;
      Status := Proc.Get_Exit_Status;
   end Execute;

end Util.Processes.Tools;
