-----------------------------------------------------------------------
--  measures -- Example of Runtime Benchmark
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010 Stephane Carrez
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
with Util.Measures;

--
--  This example performs several measures and dumps the result.
--  The result produced by <b>Util.Measures.Write</b> looks like:
--
--  <measures title="Example of measures">
--  <time count="1000" time="406.000000000us" title="Empty"/>
--  <time count="2" time="34.000000000us" title="Ada.Text_IO.Put_Line"/>
--  <time count="1" time="413.000000000us"
--        title="No tracking Empty procedure called 1000 times"/>
--  <time count="1" time="1.960000000ms"
--        title="Tracking Empty procedure called 1000 times"/>
--  </measures>
--
--  'Empty' is called 1000 times for a total time of 406us (or 406ns per call).
procedure Measures is

   procedure Print;
   procedure Empty;

   Perf : Util.Measures.Measure_Set;

   procedure Print is
      S : Util.Measures.Stamp;
   begin
      Ada.Text_IO.Put_Line ("Print test benchmark");
      Util.Measures.Report (Perf, S, "Ada.Text_IO.Put_Line");
   end Print;

   procedure Empty is
      S : Util.Measures.Stamp;
   begin
      Util.Measures.Report (Perf, S, "Empty");
   end Empty;

begin
   Print;
   Print;
   --  Measure time for calling 'Empty' 1000 times
   declare
      S : Util.Measures.Stamp;
   begin
      for I in 1 .. 1_000 loop
         Empty;
      end loop;
      Util.Measures.Report (Perf, S,
                            "Tracking Empty procedure called 1000 times");
   end;

   --  Disable measures (the next calls will not be counted)
   Util.Measures.Disable (Perf);
   Print;
   Print;

   --  Measure time for calling 'Empty' 1000 times with performance tracking OFF.
   declare
      S : Util.Measures.Stamp;
   begin
      for I in 1 .. 1_000 loop
         Empty;
      end loop;

      --  Enable measures again to track
      Util.Measures.Enable (Perf);
      Util.Measures.Report (Perf, S,
                            "No tracking Empty procedure called 1000 times");
   end;

   --  Dump the result
   Util.Measures.Write (Perf, "Example of measures",
                        Ada.Text_IO.Standard_Output);
end Measures;
