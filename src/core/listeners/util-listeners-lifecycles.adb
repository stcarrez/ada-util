-----------------------------------------------------------------------
--  util-listeners-lifecycles -- Listeners
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
