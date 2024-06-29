-----------------------------------------------------------------------
--  util-events -- Events
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
