-----------------------------------------------------------------------
--  util-processes-os -- Dummy system specific and low level operations
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

   --  ------------------------------
   --  Wait for the process <b>Pid</b> to finish and return the process exit status.
   --  ------------------------------
   procedure Waitpid (Pid    : in Process_Identifier;
                      Status : out Integer) is
      pragma Unreferenced (Pid, Status);
   begin
      Status := 0;
   end Waitpid;

   --  ------------------------------
   --  Spawn a process
   --  ------------------------------
   procedure Spawn (Proc : in out Process;
                    Mode : in Pipe_Mode := NONE) is
      pragma Unreferenced (Proc, Mode);
   begin
      raise Program_Error with "Spawn is not supported";
   end Spawn;

end Util.Processes.Os;



