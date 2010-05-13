-----------------------------------------------------------------------
--  Util -- Utilities
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with Ada.Text_IO;
with AUnit.Reporter.Text;
with AUnit.Run;
with Util.Testsuite;
with Util.Measures;

procedure Util_Harness is
   procedure Runner is new AUnit.Run.Test_Runner (Util.Testsuite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   Perf : aliased Util.Measures.Measure_Set;
begin
   Util.Measures.Set_Current (Perf'Unchecked_Access);
   Runner (Reporter);
   Util.Measures.Write (Perf, "Util measures", Ada.Text_IO.Standard_Output);
end Util_Harness;
