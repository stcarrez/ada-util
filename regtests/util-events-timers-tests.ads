-----------------------------------------------------------------------
--  util-events-timers-tests -- Unit tests for timers
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Events.Timers.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test
     and Util.Events.Timers.Timer with record
      Count  : Natural := 0;
      Repeat : Natural := 0;
   end record;

   overriding
   procedure Time_Handler (Sub   : in out Test;
                           Event : in out Timer_Ref'Class);

   --  Test empty timers.
   procedure Test_Empty_Timer (T : in out Test);

   procedure Test_Timer_Event (T : in out Test);

   --  Test repeating timers.
   procedure Test_Repeat_Timer (T : in out Test);

   --  Test executing several timers.
   procedure Test_Many_Timers (T : in out Test);

end Util.Events.Timers.Tests;
