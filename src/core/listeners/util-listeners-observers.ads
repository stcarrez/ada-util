-----------------------------------------------------------------------
--  util-listeners-observers -- Observers design pattern
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

--
--  The `Observers` package defines a simple listener interface with a single
--  `Update` operation that is called with an item as parameter.
generic
   type Element_Type (<>) is limited private;
package Util.Listeners.Observers is

   --  ------------------------------
   --  Observer
   --  ------------------------------
   type Observer is limited interface and Util.Listeners.Listener;

   --  The `Update` procedure is called by `Notify` with the given item.
   procedure Update (Instance : in Observer;
                     Item     : in Element_Type) is abstract;

   --  Notify the item passed in `Item` to the list of observers that have been registered
   --  in the `List`.
   procedure Notify (List : in Util.Listeners.List;
                     Item : in Element_Type);

end Util.Listeners.Observers;
