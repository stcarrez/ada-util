-----------------------------------------------------------------------
--  util-events-timers-tests -- Unit tests for timers
--  Copyright (C) 2017 Stephane Carrez
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

with Util.Test_Caller;

package body Util.Events.Timers.Tests is

   use Util.Tests;
   use type Ada.Real_Time.Time;

   package Caller is new Util.Test_Caller (Test, "Events.Timers");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Events.Timers.Is_Scheduled",
                       Test_Empty_Timer'Access);
      Caller.Add_Test (Suite, "Test Util.Events.Timers.Process",
                       Test_Timer_Event'Access);
      Caller.Add_Test (Suite, "Test Util.Events.Timers.Repeat",
                       Test_Repeat_Timer'Access);
   end Add_Tests;

   overriding
   procedure Time_Handler (Sub   : in out Test;
                           Event : in out Timer_Ref'Class) is
   begin
      Sub.Count := Sub.Count + 1;
      if Sub.Repeat > 1 then
         Sub.Repeat := Sub.Repeat - 1;
         Event.Repeat (Ada.Real_Time.Milliseconds (1));
      end if;
   end Time_Handler;

   --  -----------------------
   --  Test empty timers.
   --  -----------------------
   procedure Test_Empty_Timer (T : in out Test) is
      M        : Timer_List;
      R        : Timer_Ref;
      Deadline : Ada.Real_Time.Time;
   begin
      T.Assert (not R.Is_Scheduled, "Empty timer should not be scheduled");
      T.Assert (R.Time_Of_Event = Ada.Real_Time.Time_Last, "Time_Of_Event returned invalid value");
      R.Cancel;
      M.Process (Deadline);
      T.Assert (Deadline = Ada.Real_Time.Time_Last,
                "The Process operation returned invalid deadline");
   end Test_Empty_Timer;

   procedure Test_Timer_Event (T : in out Test) is
      M        : Timer_List;
      R        : Timer_Ref;
      Start    : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Deadline : Ada.Real_Time.Time;
      Now      : Ada.Real_Time.Time;
   begin
      for Retry in 1 .. 10 loop
         M.Set_Timer (T'Unchecked_Access, R, Start + Ada.Real_Time.Milliseconds (10));
         M.Process (Deadline);
         Now := Ada.Real_Time.Clock;
         exit when Now < Deadline;
      end loop;
      T.Assert (Now < Deadline, "The timer deadline is not correct");
      delay until Deadline;
      M.Process (Deadline);
      Assert_Equals (T, 1, T.Count, "The timer handler was not called");
   end Test_Timer_Event;

   --  -----------------------
   --  Test repeating timers.
   --  -----------------------
   procedure Test_Repeat_Timer (T : in out Test) is
      M        : Timer_List;
      R        : Timer_Ref;
      Start    : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Deadline : Ada.Real_Time.Time;
      Now      : Ada.Real_Time.Time;
   begin
      T.Count := 0;
      T.Repeat := 5;
      for Retry in 1 .. 10 loop
         M.Set_Timer (T'Unchecked_Access, R, Start + Ada.Real_Time.Milliseconds (10));
         M.Process (Deadline);
         Now := Ada.Real_Time.Clock;
         exit when Now < Deadline;
      end loop;
      T.Assert (Now < Deadline, "The timer deadline is not correct");
      loop
         delay until Deadline;
         M.Process (Deadline);
         exit when Deadline >= Now + Ada.Real_Time.Seconds (1);
      end loop;
      Assert_Equals (T, 5, T.Count, "The timer handler was not repeated");
   end Test_Repeat_Timer;

end Util.Events.Timers.Tests;
