-----------------------------------------------------------------------
--  util-http-clients-curl-tests -- HTTP unit tests for CURL implementation
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Http.Clients.Tests;
package Util.Http.Clients.Curl.Tests is
   new Util.Http.Clients.Tests.Http_Tests (Util.Http.Clients.Curl.Register, "curl");
