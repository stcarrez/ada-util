-----------------------------------------------------------------------
--  Util -- Utilities
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Properties.Bundles.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Bundle (T : in out Test);
   procedure Test_Bundle_Loader (T : in out Test);

   --  Test overloading some bundle definition by having incomplete files.
   procedure Test_Bundle_Overload (T : in out Test);

   --  Test bundle resolution perf.
   procedure Test_Bundle_Perf (T : in out Test);

end Util.Properties.Bundles.Tests;
