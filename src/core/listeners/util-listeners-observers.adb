-----------------------------------------------------------------------
--  util-listeners-observers -- Observers design pattern
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Listeners.Observers is

   --  ------------------------------
   --  Notify the item passed in `Item` to the list of observers that have been registered
   --  in the `List`.
   --  ------------------------------
   procedure Notify (List : in Util.Listeners.List;
                     Item : in Element_Type) is
      procedure Update (Subscriber : in Util.Listeners.Listener_Access);

      procedure Update (Subscriber : in Util.Listeners.Listener_Access) is
      begin
         if Subscriber.all in Observer'Class then
            Observer'Class (Subscriber.all).Update (Item);
         end if;
      end Update;

      R : constant Util.Listeners.Listener_Arrays.Ref := List.Get;
   begin
      R.Iterate (Process => Update'Access);
   end Notify;

end Util.Listeners.Observers;
