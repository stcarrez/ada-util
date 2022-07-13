-----------------------------------------------------------------------
--  util-events-channel -- Event Channels
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2018, 2022 Stephane Carrez
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

with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

--  The <b>Util.Events.Channels</b> package implements a publish/subscribe event
--  channel.  It is inspired from Event pattern and CosEvent service.
package Util.Events.Channels is

   type Subscriber is limited interface;
   type Subscriber_Access is access all Subscriber'Class;

   procedure Receive_Event (Sub  : in out Subscriber;
                            Item : in Event'Class) is abstract;

   ----------------------
   --  Event Channel
   ----------------------
   --  Channel on which events are pushed.
   type Channel is tagged limited private;

   type Channel_Access is access all Channel'Class;

   --  Get the name of this event channel.
   function Get_Name (C : Channel) return String;

   --  Post an event (may be asynchronous)
   procedure Post (To   : in out Channel;
                   Item : in Event'Class);

   --  Subscribe to events sent on the event channel.
   procedure Subscribe (To     : in out Channel;
                        Client : in Subscriber_Access);

   --  Unsubscribe to events sent on the event channel.
   procedure Unsubscribe (To     : in out Channel;
                          Client : in Subscriber_Access);

   type Channel_Creator is access
     function (Name : String) return Channel_Access;

   --  Create an event channel with the given name.  The type of channel
   --  is controlled by <b>Kind</b>.
   function Create (Name : String;
                    Kind : String) return Channel_Access;

   --  Create an event channel that post the event immediately.
   --  This is similar to calling <b>Create</b> with <b>"direct"</b> as kind.
   function Create_Direct_Channel (Name : String) return Channel_Access;

private
   package Containers is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Subscriber_Access);
   subtype List is Containers.List;

   type Channel is tagged limited record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      Clients  : List;
   end record;

end Util.Events.Channels;
