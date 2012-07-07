-----------------------------------------------------------------------
--  util-listeners -- Listeners
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
with Util.Concurrent.Arrays;

--  == Introduction ==
--  The `Listeners` package implements a simple observer/listener design pattern.
--  A subscriber registers to a list.  When a change is made on an object, the
--  subscribers are called with the object.
--
--  == Creating the listeners ==
--  The listeners list contains a list of listener interfaces.
--
--    L : Util.Listeners.List;
--
--  == Creating the publishers ==
--  First the `Publishers` package must be instantiated with the type being
--  observed.  In the example below, we will observe a string:
--
--    package String_Publishers is new Util.Listeners.Publishers (String);
--
--  == Implementing the listener ==
--  Now we must implement the string listener:
--
--    type String_Listener is new String_Publishers.Listener with null record;
--    procedure Notify (L : in String_Listener; Item : in String);
--
--  == Registering the listener ==
--  An instance of the string listener must now be registered in the list.
--
--    L.Append (new String_Listener);
--
--  == Publishing ==
--  Notifying the listeners is done by invoking the `Publish` operation
--  provided by the `String_Publishers` package:
--
--    String_Publishers.Publish (L, "Hello");
--
package Util.Listeners is

   type Listener is limited interface;
   type Listener_Access is access all Listener'Class;

   package Listener_Arrays is new Util.Concurrent.Arrays (Listener_Access);

   type List is new Listener_Arrays.Vector with null record;

   generic
      type Element_Type (<>) is limited private;
   package Publishers is
      type Listener is limited interface and Util.Listeners.Listener;

      procedure Notify (Instance : in Listener;
                        Item     : in Element_Type) is abstract;

      procedure Publish (L : in List;
                         Item : in Element_Type);

   end Publishers;

end Util.Listeners;
