-----------------------------------------------------------------------
--  util-streams-sockets-tests -- Unit tests for socket streams
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Streams.Sockets.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test reading and writing on a socket stream.
   procedure Test_Socket_Read (T : in out Test);

   --  Test socket initialization.
   procedure Test_Socket_Init (T : in out Test);

end Util.Streams.Sockets.Tests;
