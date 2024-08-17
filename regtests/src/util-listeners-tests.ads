-----------------------------------------------------------------------
--  util-listeners-tests -- Unit tests for listeners
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Listeners.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the listeners and the publish operation.
   procedure Test_Publish (T : in out Test);

   --  Performance test for the listeners.
   procedure Test_Publish_Perf (T : in out Test);

   --  Test the lifecycles listener.
   procedure Test_Lifecycles (T : in out Test);

end Util.Listeners.Tests;
