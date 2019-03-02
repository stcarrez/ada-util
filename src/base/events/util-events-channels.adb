-----------------------------------------------------------------------
--  util-events-channel -- Event Channels
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2018 Stephane Carrez
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
with Util.Strings;

package body Util.Events.Channels is

   use Containers;
   use Ada.Strings.Unbounded;

   use Util.Strings;

   type Factory_Line is record
      Name    : Name_Access;
      Creator : Channel_Creator;
   end record;

   type Factory_Table is array (Natural range <>) of Factory_Line;

   DIRECT_NAME : aliased constant String := "direct";

   Factory : constant Factory_Table (1 .. 1) :=
     (1 => (Name    => DIRECT_NAME'Access,
            Creator => Create_Direct_Channel'Access));

   --  -----------------------
   --  Get the name of this event channel.
   --  -----------------------
   function Get_Name (C : Channel) return String is
   begin
      return To_String (C.Name);
   end Get_Name;

   --  -----------------------
   --  Post an event and dispatch it immediately.
   --  -----------------------
   procedure Post (To   : in out Channel;
                   Item : in Event'Class) is
      Iter : Cursor := First (To.Clients);
   begin
      --  Dispatch_One makes the connection between the common implementation
      --  and the generics

      while Has_Element (Iter) loop
         declare
            Client : constant Subscriber_Access := Element (Iter);
         begin
            Client.Receive_Event (Item);
         end;
         Iter := Next (Iter);
      end loop;
   end Post;

   --  -----------------------
   --  Subscribe to events sent on the event channel.
   --  -----------------------
   procedure Subscribe (To     : in out Channel;
                        Client : in Subscriber_Access) is
   begin
      pragma Assert (Client /= null, "Client subscriber must not be null");

      Append (To.Clients, Client);
   end Subscribe;

   --  -----------------------
   --  Unsubscribe to events sent on the event channel.
   --  -----------------------
   procedure Unsubscribe (To     : in out Channel;
                          Client : in Subscriber_Access) is
      Iter : Cursor := First (To.Clients);
   begin
      while Has_Element (Iter) loop
         if Element (Iter) = Client then
            Delete (To.Clients, Iter);
            return;
         end if;
         Iter := Next (Iter);
      end loop;
   end Unsubscribe;

   --  -----------------------
   --  Create an event channel with the given name.  The type of channel
   --  is controlled by <b>Kind</b>.
   --  -----------------------
   function Create (Name : String;
                    Kind : String)
                    return Channel_Access is
   begin
      for I in Factory'Range loop
         if Factory (I).Name.all = Kind then
            return Factory (I).Creator (Name);
         end if;
      end loop;

      return Create_Direct_Channel (Name);
   end Create;

   --  -----------------------
   --  Create an event channel that post the event immediately.
   --  -----------------------
   function Create_Direct_Channel (Name : String) return Channel_Access is
      Result : constant Channel_Access := new Channel;
   begin
      Result.Name := To_Unbounded_String (Name);
      return Result;
   end Create_Direct_Channel;

end Util.Events.Channels;
