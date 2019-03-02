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

package body Util.Listeners.Lifecycles is

   --  ------------------------------
   --  Inform the the lifecycle listeners registered in `List` that the item passed in `Item`
   --  has been created (calls `On_Create`).
   --  ------------------------------
   procedure Notify_Create (List : in Util.Listeners.List;
                            Item : in Element_Type) is
      procedure On_Create (Subscriber : in Util.Listeners.Listener_Access);

      procedure On_Create (Subscriber : in Util.Listeners.Listener_Access) is
      begin
         if Subscriber.all in Listener'Class then
            Listener'Class (Subscriber.all).On_Create (Item);
         end if;
      end On_Create;

      R : constant Util.Listeners.Listener_Arrays.Ref := List.Get;
   begin
      R.Iterate (Process => On_Create'Access);
   end Notify_Create;

   --  ------------------------------
   --  Inform the the lifecycle listeners registered in `List` that the item passed in `Item`
   --  has been updated (calls `On_Update`).
   --  ------------------------------
   procedure Notify_Update (List : in Util.Listeners.List;
                            Item : in Element_Type) is
      procedure On_Update (Subscriber : in Util.Listeners.Listener_Access);

      procedure On_Update (Subscriber : in Util.Listeners.Listener_Access) is
      begin
         if Subscriber.all in Listener'Class then
            Listener'Class (Subscriber.all).On_Update (Item);
         end if;
      end On_Update;

      R : constant Util.Listeners.Listener_Arrays.Ref := List.Get;
   begin
      R.Iterate (Process => On_Update'Access);
   end Notify_Update;

   --  ------------------------------
   --  Inform the the lifecycle listeners registered in `List` that the item passed in `Item`
   --  has been deleted (calls `On_Delete`).
   --  ------------------------------
   procedure Notify_Delete (List : in Util.Listeners.List;
                            Item : in Element_Type) is
      procedure On_Delete (Subscriber : in Util.Listeners.Listener_Access);

      procedure On_Delete (Subscriber : in Util.Listeners.Listener_Access) is
      begin
         if Subscriber.all in Listener'Class then
            Listener'Class (Subscriber.all).On_Delete (Item);
         end if;
      end On_Delete;

      R : constant Util.Listeners.Listener_Arrays.Ref := List.Get;
   begin
      R.Iterate (Process => On_Delete'Access);
   end Notify_Delete;

end Util.Listeners.Lifecycles;
