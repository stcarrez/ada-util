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
