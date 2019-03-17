-----------------------------------------------------------------------
--  util-streams-tests -- Unit tests for encoding buffered streams
--  Copyright (C) 2017, 2018, 2019 Stephane Carrez
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
with Util.Streams.Files;
with Util.Streams.Texts;
with Util.Streams.Base64;
with Ada.Streams.Stream_IO;
package body Util.Streams.Tests is

   use Util.Streams.Files;
   use Ada.Streams.Stream_IO;

   package Caller is new Util.Test_Caller (Test, "Streams");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Streams.Base64.Write, Read",
                       Test_Base64_Stream'Access);
   end Add_Tests;

   procedure Test_Base64_Stream (T : in out Test) is
      Stream  : aliased File_Stream;
      Buffer  : aliased Util.Streams.Base64.Encoding_Stream;
      Print   : Util.Streams.Texts.Print_Stream;
      Path    : constant String := Util.Tests.Get_Test_Path ("regtests/result/test-stream.b64");
      Expect  : constant String := Util.Tests.Get_Path ("regtests/expect/test-stream.b64");
   begin
      Print.Initialize (Output => Buffer'Access, Size => 5);
      Buffer.Initialize (Output => Stream'Access,
                         Size   => 1024);
      Stream.Create (Mode => Out_File, Name => Path);
      for I in 1 .. 32 loop
         Print.Write ("abcd");
         Print.Write (" fghij");
         Print.Write (ASCII.LF);
      end loop;
      Print.Flush;
      Stream.Close;

      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Expect,
                                     Test    => Path,
                                     Message => "Base64 stream");
   end Test_Base64_Stream;

end Util.Streams.Tests;
