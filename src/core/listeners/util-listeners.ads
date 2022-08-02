-----------------------------------------------------------------------
--  util-listeners -- Listeners
--  Copyright (C) 2012, 2022 Stephane Carrez
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
with Util.Concurrent.Arrays;

--  == Listeners ==
--  The `Listeners` package implements a simple observer/listener design pattern.
--  A subscriber registers to a list.  When a change is made on an object, the
--  application can notify the subscribers which are then called with the object.
--
--  === Creating the listener list ===
--  The listeners list contains a list of listener interfaces.
--
--    L : Util.Listeners.List;
--
--  The list is heterogeneous meaning that several kinds of listeners could
--  be registered.
--
--  === Creating the observers ===
--  First the `Observers` package must be instantiated with the type being
--  observed.  In the example below, we will observe a string:
--
--    package String_Observers is new Util.Listeners.Observers (String);
--
--  === Implementing the observer ===
--  Now we must implement the string observer:
--
--    type String_Observer is new String_Observer.Observer with null record;
--    procedure Update (List : in String_Observer; Item : in String);
--
--  === Registering the observer ===
--  An instance of the string observer must now be registered in the list.
--
--    O : aliased String_Observer;
--    L.Append (O'Access);
--
--  === Publishing ===
--  Notifying the listeners is done by invoking the `Notify` operation
--  provided by the `String_Observers` package:
--
--    String_Observer.Notify (L, "Hello");
--
package Util.Listeners is

   --  The listener root interface.
   type Listener is limited interface;
   type Listener_Access is access all Listener'Class;

   --  The multi-task safe list of listeners.
   package Listener_Arrays is new Util.Concurrent.Arrays (Listener_Access);

   --  ------------------------------
   --  List of listeners
   --  ------------------------------
   --  The `List` type is a list of listeners that have been registered and must be
   --  called when a notification is sent.  The list uses the concurrent arrays thus
   --  allowing tasks to add or remove listeners while dispatching is also running.
   subtype List is Listener_Arrays.Vector;

   procedure Add_Listener (Into : in out Listener_Arrays.Vector;
                           Item : in Listener_Access) renames Listener_Arrays.Append;

   procedure Remove_Listener (Into : in out Listener_Arrays.Vector;
                              Item : in Listener_Access) renames Listener_Arrays.Remove;

end Util.Listeners;
