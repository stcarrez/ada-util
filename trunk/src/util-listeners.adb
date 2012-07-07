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

package body Util.Listeners is

   package body Publishers is

      procedure Publish (L : in List;
                         Item : in Element_Type) is
         procedure Notify (Li : in Util.Listeners.Listener_Access) is
         begin
            if Li.all in Listener'Class then
               Listener'Class (Li.all).Notify (Item);
            end if;
         end Notify;

         R : constant Util.Listeners.Listener_Arrays.Ref := L.Get;
      begin
         R.Iterate (Process => Notify'Access);
      end Publish;

   end Publishers;

end Util.Listeners;
