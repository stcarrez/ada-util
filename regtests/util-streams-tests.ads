-----------------------------------------------------------------------
--  util-streams-tests -- Unit tests for encoding buffered streams
--  Copyright (C) 2017, 2019, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Util.Encoders.AES;

package Util.Streams.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_AES (T     : in out Test;
                       Item  : in String;
                       Count : in Positive;
                       Mode  : in Util.Encoders.AES.AES_Mode;
                       Label : in String);

   procedure Test_Base64_Stream (T : in out Test);

   procedure Test_Copy_Stream (T : in out Test);

end Util.Streams.Tests;
