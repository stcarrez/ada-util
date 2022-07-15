-----------------------------------------------------------------------
--  util-http-headers-tests - Unit tests for Headers
--  Copyright (C) 2022 Stephane Carrez
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
package body Util.Http.Headers.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Headers");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Http.Headers.Get_Accepted",
                       Test_Get_Accepted'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the Get_Accepted function.
   --  ------------------------------
   procedure Test_Get_Accepted (T : in out Test) is
      use type Mimes.Mime_Access;

      M : Mimes.Mime_Access;
   begin
      M := Get_Accepted ("image/gif", Mimes.Images);
      T.Assert (M /= null, "Null mime returned");
      T.Assert_Equals (M.all, Mimes.Gif, "Bad accept");

      M := Get_Accepted ("image/*", Mimes.Images);
      T.Assert (M /= null, "Null mime returned");
      T.Assert_Equals (M.all, Mimes.Jpg, "Bad accept");

      M := Get_Accepted ("image/* q=0.2, image/svg+xml", Mimes.Images);
      T.Assert (M /= null, "Null mime returned");
      T.Assert_Equals (M.all, Mimes.Svg, "Bad accept");

      M := Get_Accepted ("image/* q=0.2, image/svg+xml", Mimes.Api);
      T.Assert (M = null, "Should not match");
   end Test_Get_Accepted;

end Util.Http.Headers.Tests;
