-----------------------------------------------------------------------
--  util-log-appenders-formatters -- Log appenders
--  Copyright (C) 2001 - 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

procedure Util.Log.Appenders.Formatter (Self    : in Appender'Class;
                                        Content : in Util.Strings.Builders.Builder;
                                        Date    : in Ada.Calendar.Time;
                                        Level   : in Level_Type;
                                        Logger  : in String) is
   procedure Get is new Util.Strings.Builders.Get (Process);
begin
   if Self.Layout /= MESSAGE then
      Process (Format (Self, Date, Level, Logger));
   end if;
   Get (Content);
end Util.Log.Appenders.Formatter;
