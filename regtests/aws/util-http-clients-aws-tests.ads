-----------------------------------------------------------------------
--  util-http-clients-curl-tests -- HTTP unit tests for AWS implementation
--  Copyright (C) 2012, 2019, 2022 Stephane Carrez
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

with Util.Http.Clients.Tests;
package Util.Http.Clients.AWS.Tests is
  new Util.Http.Clients.Tests.Http_Tests (Util.Http.Clients.AWS.Register, "aws");
