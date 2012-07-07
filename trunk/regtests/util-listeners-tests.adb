-----------------------------------------------------------------------
--  util-listeners-tests -- Unit tests for listeners
--  Copyright (C) 2012 Stephane Carrez
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
with Ada.Strings.Unbounded;

with Util.Measures;
with Util.Test_Caller;
package body Util.Listeners.Tests is

   use Util.Tests;

   Test_Error : exception;

   Count     : Natural := 0;

   package String_Publishers is new Util.Listeners.Publishers (String);
   package Integer_Publishers is new Util.Listeners.Publishers (Integer);

   type String_Listener is new String_Publishers.Listener with record
      Expect : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding
   procedure Notify (Listener : in String_Listener;
                     Item     : in String);

   type Integer_Listener is new Integer_Publishers.Listener with record
      Expect : Integer;
   end record;

   overriding
   procedure Notify (Listener : in Integer_Listener;
                     Item     : in Integer);

   package Caller is new Util.Test_Caller (Test, "Listeners");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Listeners.Publish",
                       Test_Publish'Access);
      Caller.Add_Test (Suite, "Test Util.Listeners.Publish_Perf",
                       Test_Publish_Perf'Access);
   end Add_Tests;

   overriding
   procedure Notify (Listener : in String_Listener;
                     Item     : in String) is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Item /= Listener.Expect then
         raise Test_Error;
      end if;
--        Util.Tests.Assert_Equals (T, "Hello", Item);
      Count := Count + 1;
   end Notify;

   overriding
   procedure Notify (Listener : in Integer_Listener;
                     Item     : in Integer) is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Item /= Listener.Expect then
         raise Test_Error;
      end if;
      --        Util.Tests.Assert_Equals (T, "Hello", Item);
      Count := Count + 1;
   end Notify;

   --  ------------------------------
   --  Test the listeners and the publish operation.
   --  ------------------------------
   procedure Test_Publish (T : in out Test) is
      Listeners : Util.Listeners.List;

      L1        : aliased String_Listener;
      L2        : aliased Integer_Listener;
      L3        : aliased Integer_Listener;
   begin
      Listeners.Append (L1'Unchecked_Access);
      Listeners.Append (L2'Unchecked_Access);
      Listeners.Append (L3'Unchecked_Access);

      L1.Expect := Ada.Strings.Unbounded.To_Unbounded_String ("Hello");
      String_Publishers.Publish (Listeners, "Hello");
      Util.Tests.Assert_Equals (T, 1, Count, "Invalid number of calls");

      L2.Expect := 3;
      L3.Expect := 3;
      Integer_Publishers.Publish (Listeners, 3);

   end Test_Publish;

   --  ------------------------------
   --  Performance test for the listeners.
   --  ------------------------------
   procedure Test_Publish_Perf (T : in out Test) is
      Listeners : Util.Listeners.List;
      L1        : aliased Integer_Listener;

      procedure Test_Basic (Item : in Integer) is
      begin
         Util.Tests.Assert_Equals (T, 3, Item);
      end Test_Basic;

   begin
      Listeners.Append (L1'Unchecked_Access);
      L1.Expect := 3;
      declare
         S : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            Integer_Publishers.Publish (Listeners, 3);
         end loop;
         Util.Measures.Report (S, "Published 1000 times");
      end;
      declare
         S : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            Test_Basic (3);
         end loop;
         Util.Measures.Report (S, "Call basic 1000 times");
      end;
   end Test_Publish_Perf;

end Util.Listeners.Tests;
