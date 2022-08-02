-----------------------------------------------------------------------
--  events.tests -- Unit tests for event channels
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
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

with Ada.Unchecked_Deallocation;
with Util.Test_Caller;
package body Util.Events.Channels.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Events.Channels");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Events.Channels.Post_Event",
                       Test_Post_Event'Access);
   end Add_Tests;

   overriding
   procedure Receive_Event (Sub  : in out Test;
                            Item : in Event'Class) is
      pragma Unreferenced (Item);
   begin
      Sub.Count := Sub.Count + 1;
   end Receive_Event;

   procedure Test_Post_Event (T : in out Test) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Channel'Class,
                                         Name   => Channel_Access);

      C  : Channel_Access := Create ("test", "direct");
      E  : Event;
      T1 : aliased Test;
      T2 : aliased Test;
   begin
      C.Post (E);

      Assert_Equals (T, "test", C.Get_Name, "Invalid channel name");

      C.Subscribe (T1'Unchecked_Access);
      C.Post (E);
      Assert_Equals (T, 1, T1.Count, "Invalid number of received events");
      Assert_Equals (T, 0, T2.Count, "Invalid number of events");

      C.Subscribe (T2'Unchecked_Access);
      C.Post (E);

      C.Unsubscribe (T1'Unchecked_Access);
      C.Post (E);
      Assert_Equals (T, 2, T1.Count, "Invalid number of received events");
      Assert_Equals (T, 2, T2.Count, "Invalid number of events");

      Free (C);
   end Test_Post_Event;

end Util.Events.Channels.Tests;
