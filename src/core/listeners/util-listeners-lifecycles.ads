-----------------------------------------------------------------------
--  util-listeners-lifecycles -- Listeners
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

--  The `Lifecycles` package provides a listener interface dedicated to
--  track lifecycle managements on objects.  It defines a set of procedures to be
--  notified when an object is created, updated or deleted.
--
--  Notes: another implementation can be made by using three different listener lists
--  that use the simple observer pattern.
generic
   type Element_Type (<>) is limited private;
package Util.Listeners.Lifecycles is

   --  ------------------------------
   --  Lifecycle listener
   --  ------------------------------
   type Listener is limited interface and Util.Listeners.Listener;

   --  The `On_Create` procedure is called by `Notify_Create` to notify the creation of the item.
   procedure On_Create (Instance : in Listener;
                        Item     : in Element_Type) is abstract;

   --  The `On_Update` procedure is called by `Notify_Update` to notify the update of the item.
   procedure On_Update (Instance : in Listener;
                        Item     : in Element_Type) is abstract;

   --  The `On_Delete` procedure is called by `Notify_Delete` to notify the deletion of the item.
   procedure On_Delete (Instance : in Listener;
                        Item     : in Element_Type) is abstract;

   --  Inform the the lifecycle listeners registered in `List` that the item passed in `Item`
   --  has been created (calls `On_Create`).
   procedure Notify_Create (List : in Util.Listeners.List;
                            Item : in Element_Type);

   --  Inform the the lifecycle listeners registered in `List` that the item passed in `Item`
   --  has been updated (calls `On_Update`).
   procedure Notify_Update (List : in Util.Listeners.List;
                            Item : in Element_Type);

   --  Inform the the lifecycle listeners registered in `List` that the item passed in `Item`
   --  has been deleted (calls `On_Delete`).
   procedure Notify_Delete (List : in Util.Listeners.List;
                            Item : in Element_Type);

end Util.Listeners.Lifecycles;
