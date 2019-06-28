-----------------------------------------------------------------------
--  util-encodes-ecc-tests - ECC function tests
--  Copyright (C) 2019 Stephane Carrez
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

with Util.Tests;
package Util.Encoders.ECC.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test ECC correction on 256 bytes block.
   procedure Test_ECC_Block_256 (T : in out Test);

   --  Test ECC correction on 512 bytes block.
   procedure Test_ECC_Block_512 (T : in out Test);

end Util.Encoders.ECC.Tests;
