-----------------------------------------------------------------------
--  measure -- Benchmark tools
--  Copyright (C) 2008, 2009, 2010, 2011, 2012, 2013, 2018 Stephane Carrez
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
with Ada.Calendar;
with Ada.Containers;
with Ada.Finalization;

with Util.Streams.Texts;

--  = Performance Measurements =
--
--  Performance measurements is often made using profiling tools such as GNU gprof or others.
--  This profiling is however not always appropriate for production or release delivery.
--  The mechanism presented here is a lightweight performance measurement that can be
--  used in production systems.
--
--  The Ada package `Util.Measures` defines the types and operations to make
--  performance measurements.  It is designed to be used for production and multi-threaded
--  environments.
--
--  == Create the measure set ==
--
--  Measures are collected in a `Measure_Set`.  Each measure has a name, a counter and
--  a sum of time spent for all the measure.  The measure set should be declared as some
--  global variable.  The implementation is thread safe meaning that a measure set can
--  be used by several threads at the same time.  It can also be associated with
--  a per-thread data (or task attribute).
--
--  To declare the measure set, use:
--
--     with Util.Measures;
--        ...
--        Perf : Util.Measures.Measure_Set;
--
--  == Measure the implementation ==
--
--  A measure is made by creating a variable of type `Stamp`.  The declaration of
--  this variable marks the beginning of the measure.  The measure ends at the
--  next call to the `Report` procedure.
--
--     with Util.Measures;
--     ...
--       declare
--          Start : Util.Measures.Stamp;
--       begin
--          ...
--          Util.Measures.Report (Perf, Start, "Measure for a block");
--       end;
--
--  When the `Report` procedure is called, the time that elapsed between the creation of
--  the `Start` variable and the procedure call is computed.  This time is
--  then associated with the measure title and the associated counter is incremented.
--  The precision of the measured time depends on the system.  On GNU/Linux, it uses
--  `gettimeofday`.
--
--  If the block code is executed several times, the measure set will report
--  the number of times it was executed.
--
--  == Reporting results ==
--
--  After measures are collected, the results can be saved in a file or in
--  an output stream.  When saving the measures, the measure set is cleared.
--
--     Util.Measures.Write (Perf, "Title of measures",
--                          Ada.Text_IO.Standard_Output);
--
--  == Measure Overhead ==
--
--  The overhead introduced by the measurement is quite small as it does not exceeds 1.5 us
--  on a 2.6 Ghz Core Quad.
--
--  == What must be measured ==
--
--  Defining a lot of measurements for a production system is in general not very useful.
--  Measurements should be relatively high level measurements.  For example:
--
--    * Loading or saving a file
--    * Rendering a page in a web application
--    * Executing a database query
--
package Util.Measures is

   type Unit_Type is (Seconds, Milliseconds, Microseconds, Nanoseconds);

   --  ------------------------------
   --  Measure Set
   --  ------------------------------
   --  The measure set represent a collection of measures each of them being
   --  associated with a same name.  Several measure sets can be created to
   --  collect different kinds of runtime information.  The measure set can be
   --  set on a per-thread data and every call to <b>Report</b> will be
   --  associated with that measure set.
   --
   --  Measure set are thread-safe.
   type Measure_Set is limited private;
   type Measure_Set_Access is access all Measure_Set;

   --  Disable collecting measures on the measure set.
   procedure Disable (Measures : in out Measure_Set);

   --  Enable collecting measures on the measure set.
   procedure Enable (Measures : in out Measure_Set);

   --  Set the per-thread measure set.
   procedure Set_Current (Measures : in Measure_Set_Access);

   --  Get the per-thread measure set.
   function Get_Current return Measure_Set_Access;

   --  Dump an XML result with the measures collected by the measure set.
   --  When writing the measures, the measure set is cleared.  It is safe
   --  to write measures while other measures are being collected.
   procedure Write (Measures : in out Measure_Set;
                    Title    : in String;
                    Stream   : in out Util.Streams.Texts.Print_Stream'Class);

   --  Dump an XML result with the measures collected by the measure set.
   --  When writing the measures, the measure set is cleared.  It is safe
   --  to write measures while other measures are being collected.
   procedure Write (Measures : in out Measure_Set;
                    Title    : in String;
                    Stream   : in Ada.Text_IO.File_Type);

   --  Dump  an XML result with the measures in a file.
   procedure Write (Measures : in out Measure_Set;
                    Title    : in String;
                    Path     : in String);

   --  ------------------------------
   --  Stamp
   --  ------------------------------
   --  The stamp marks the beginning of a measure when the variable of such
   --  type is declared.  The measure represents the time that elapsed between
   --  the stamp creation and when the <b>Report</b> method is called.
   type Stamp is limited private;

   --  Report the time spent between the stamp creation and this method call.
   --  Collect the result in the per-thread measure set under the given measure
   --  title.
   procedure Report (S     : in out Stamp;
                     Title : in String;
                     Count : in Positive := 1);

   --  Report the time spent between the stamp creation and this method call.
   --  Collect the result in the measure set under the given measure title.
   procedure Report (Measures : in out Measure_Set;
                     S        : in out Stamp;
                     Title    : in String;
                     Count    : in Positive := 1);

   --  Report the time spent between the stamp creation and this method call.
   --  The report is written in the file with the given title.  The duration is
   --  expressed in the unit defined in <tt>Unit</tt>.
   procedure Report (S     : in out Stamp;
                     File  : in out Ada.Text_IO.File_Type;
                     Title : in String;
                     Unit  : in Unit_Type := Microseconds);

private

   type String_Access is access String;

   type Stamp is limited record
      Start : Ada.Calendar.Time := Ada.Calendar.Clock;
   end record;

   type Measure;
   type Measure_Access is access Measure;

   type Measure is limited record
      Next  : Measure_Access;
      Time  : Duration;
      Count : Positive;
      Name  : String_Access;
   end record;

   type Buckets_Type is
     array (Ada.Containers.Hash_Type range <>) of Measure_Access;
   type Buckets_Access is access all Buckets_Type;

   --  To reduce contention we only protect insertion and updates of measures.
   --  To write the measure set, we steal the buckets and force the next call
   --  to <b>Add</b> to reallocate the buckets.
   protected type Measure_Data is

      --  Get the measures and clear to start a new set of measures.
      --  Return in <b>Time_Start</b> and <b>Time_End</b> the period of time.
      procedure Steal_Map (Result     : out Buckets_Access;
                           Time_Start : out Ada.Calendar.Time;
                           Time_End   : out Ada.Calendar.Time);

      --  Add the measure
      procedure Add (Title : in String;
                     D     : in Duration;
                     Count : in Positive := 1);

   private
      Start   : Ada.Calendar.Time := Ada.Calendar.Clock;
      Buckets : Buckets_Access;
   end Measure_Data;

   type Measure_Set is new Ada.Finalization.Limited_Controlled with record
      Enabled : Boolean := True;
      pragma Atomic (Enabled);

      Data    : Measure_Data;
   end record;

   --  Finalize the measures and release the storage.
   overriding
   procedure Finalize (Measures : in out Measure_Set);

end Util.Measures;
