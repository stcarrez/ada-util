-----------------------------------------------------------------------
--  util-events -- Events
--  Copyright (C) 2001, 2002, 2003, 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package body Util.Events is

   --  ------------------------------
   --  Get the time identifying when the event was created.
   --  ------------------------------
   function Get_Time (Ev : Event) return Ada.Calendar.Time is
   begin
      return Ev.Date;
   end Get_Time;

end Util.Events;
