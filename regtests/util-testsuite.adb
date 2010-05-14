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
with Util.Log.Tests;
with Util.Files.Tests;
package body Util.Testsuite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Util.Properties.Tests.Add_Tests (Ret);
      Util.Log.Tests.Add_Tests (Ret);
      Util.Files.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end Util.Testsuite;
