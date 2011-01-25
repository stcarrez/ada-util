-----------------------------------------------------------------------
--  Util testsuite - Util Testsuite
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

with Util.Properties.Tests;
with Util.Properties.Bundles.Tests;
with Util.Log.Tests;
with Util.Files.Tests;
with Util.Concurrent.Tests;
with Util.Events.Channels.Tests;
with Util.Locales.Tests;
with Util.Strings.Tests;
with Util.Encoders.Tests;
with Util.Streams.Buffered.Tests;
with Util.Streams.Files.Tests;
with Util.Beans.Objects.Discretes;
with Util.Serialize.IO.JSON.Tests;
package body Util.Testsuite is

   Tests : aliased Test_Suite;

   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := Tests'Access;
   begin
      Util.Properties.Tests.Add_Tests (Result);
      Util.Properties.Bundles.Tests.Add_Tests (Result);
      Util.Locales.Tests.Add_Tests (Result);
      Util.Strings.Tests.Add_Tests (Result);
      Util.Log.Tests.Add_Tests (Result);
      Util.Files.Tests.Add_Tests (Result);
      Util.Concurrent.Tests.Add_Tests (Result);
      Util.Events.Channels.Tests.Add_Tests (Result);
      Util.Encoders.Tests.Add_Tests (Result);
      Util.Streams.Buffered.Tests.Add_Tests (Result);
      Util.Streams.Files.Tests.Add_Tests (Result);
      Util.Beans.Objects.Discretes.Add_Tests (Result);
      Util.Serialize.IO.JSON.Tests.Add_Tests (Result);
      return Result;
   end Suite;

end Util.Testsuite;
