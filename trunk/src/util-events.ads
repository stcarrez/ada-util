-----------------------------------------------------------------------
--  util-events -- Events
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2011 Stephane Carrez
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
with Ada.Calendar;
package Util.Events is

   type Event is tagged limited private;

   --  Get the time identifying when the event was created.
   function Get_Time (Ev : Event) return Ada.Calendar.Time;

   type Event_Listener is limited interface;

private

   type Event is tagged limited record
      Date : Ada.Calendar.Time := Ada.Calendar.Clock;
   end record;

end Util.Events;
