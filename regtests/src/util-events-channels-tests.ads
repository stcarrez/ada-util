-----------------------------------------------------------------------
--  events.tests -- Unit tests for event channels
--  Copyright (C) 2009, 2010, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Events.Channels.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test
     and Util.Events.Channels.Subscriber with record
      Count : Natural := 0;
   end record;

   overriding
   procedure Receive_Event (Sub  : in out Test;
                            Item : in Event'Class);

   procedure Test_Post_Event (T : in out Test);

end Util.Events.Channels.Tests;
