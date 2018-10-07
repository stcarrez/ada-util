-----------------------------------------------------------------------
--  util-listeners-tests -- Unit tests for listeners
--  Copyright (C) 2012, 2013, 2018 Stephane Carrez
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
with Util.Listeners.Observers;
with Util.Listeners.Lifecycles;
package body Util.Listeners.Tests is

   Test_Error : exception;

   Count     : Natural := 0;

   package String_Observers is new Util.Listeners.Observers (String);
   package Integer_Observers is new Util.Listeners.Observers (Integer);

   type String_Listener is new String_Observers.Observer with record
      Expect : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding
   procedure Update (Listener : in String_Listener;
                     Item     : in String);

   type Integer_Listener is new Integer_Observers.Observer with record
      Expect : Integer;
   end record;

   overriding
   procedure Update (Listener : in Integer_Listener;
                     Item     : in Integer);

   package Caller is new Util.Test_Caller (Test, "Listeners");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Listeners.Publish",
                       Test_Publish'Access);
      Caller.Add_Test (Suite, "Test Util.Listeners.Publish_Perf",
                       Test_Publish_Perf'Access);
      Caller.Add_Test (Suite, "Test Util.Listeners.Lifecycles",
                       Test_Lifecycles'Access);
   end Add_Tests;

   overriding
   procedure Update (Listener : in String_Listener;
                     Item     : in String) is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Item /= Listener.Expect then
         raise Test_Error;
      end if;
      Count := Count + 1;
   end Update;

   overriding
   procedure Update (Listener : in Integer_Listener;
                     Item     : in Integer) is
   begin
      if Item /= Listener.Expect then
         raise Test_Error;
      end if;
      Count := Count + 1;
   end Update;

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
      String_Observers.Notify (Listeners, "Hello");
      Util.Tests.Assert_Equals (T, 1, Count, "Invalid number of calls");

      L2.Expect := 3;
      L3.Expect := 3;
      Integer_Observers.Notify (Listeners, 3);

   end Test_Publish;

   --  ------------------------------
   --  Performance test for the listeners.
   --  ------------------------------
   procedure Test_Publish_Perf (T : in out Test) is
      procedure Test_Basic (Item : in Integer);

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
            Integer_Observers.Notify (Listeners, 3);
         end loop;
         Util.Measures.Report (S, "Observers.Notify", 1000);
      end;
      declare
         S : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            Test_Basic (3);
         end loop;
         Util.Measures.Report (S, "Call basic procedure", 1000);
      end;
   end Test_Publish_Perf;

   --  ------------------------------
   --  Test the lifecycles listener.
   --  ------------------------------
   procedure Test_Lifecycles (T : in out Test) is
      package TL is new Util.Listeners.Lifecycles (Util.Measures.Stamp);

      Create_Count : Natural := 0;
      Update_Count : Natural := 0;
      Delete_Count : Natural := 0;

      type Listener is new TL.Listener with null record;

      --  The `On_Create` procedure is called by `Notify_Create` to notify the creation
      --  of the item.
      overriding
      procedure On_Create (Instance : in Listener;
                           Item     : in Util.Measures.Stamp);

      --  The `On_Update` procedure is called by `Notify_Update` to notify the update of the item.
      overriding
      procedure On_Update (Instance : in Listener;
                           Item     : in Util.Measures.Stamp);

      --  The `On_Delete` procedure is called by `Notify_Delete` to notify the deletion
      --  of the item.
      overriding
      procedure On_Delete (Instance : in Listener;
                           Item     : in Util.Measures.Stamp);

      --  ------------------------------
      --  The `On_Create` procedure is called by `Notify_Create` to notify the creation
      --  of the item.
      --  ------------------------------
      overriding
      procedure On_Create (Instance : in Listener;
                           Item     : in Util.Measures.Stamp) is
         pragma Unreferenced (Instance, Item);
      begin
         Create_Count := Create_Count + 1;
      end On_Create;

      --  ------------------------------
      --  The `On_Update` procedure is called by `Notify_Update` to notify the update of the item.
      --  ------------------------------
      overriding
      procedure On_Update (Instance : in Listener;
                           Item     : in Util.Measures.Stamp) is
         pragma Unreferenced (Instance, Item);
      begin
         Update_Count := Update_Count + 1;
      end On_Update;

      --  ------------------------------
      --  The `On_Delete` procedure is called by `Notify_Delete` to notify the deletion
      --  of the item.
      --  ------------------------------
      overriding
      procedure On_Delete (Instance : in Listener;
                           Item     : in Util.Measures.Stamp) is
         pragma Unreferenced (Instance, Item);
      begin
         Delete_Count := Delete_Count + 1;
      end On_Delete;

      Listeners : Util.Listeners.List;
      L1        : aliased Integer_Listener;
      L2        : aliased Listener;
   begin
      Listeners.Append (L1'Unchecked_Access);
      Listeners.Append (L2'Unchecked_Access);

      declare
         S : Util.Measures.Stamp;
      begin
         TL.Notify_Create (Listeners, S);
         TL.Notify_Update (Listeners, S);
         TL.Notify_Delete (Listeners, S);
         Util.Measures.Report (S, "Notify Create, Update, Delete");
      end;
      Util.Tests.Assert_Equals (T, 1, Create_Count, "On_Create not called");
      Util.Tests.Assert_Equals (T, 1, Update_Count, "On_Update not called");
      Util.Tests.Assert_Equals (T, 1, Delete_Count, "On_Delete not called");
   end Test_Lifecycles;

end Util.Listeners.Tests;
