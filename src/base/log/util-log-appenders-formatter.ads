-----------------------------------------------------------------------
--  util-log-appenders-formatters -- Log appenders
--  Copyright (C) 2001 - 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Calendar;
with Util.Strings.Builders;

generic
   with procedure Process (Data : in String);
procedure Util.Log.Appenders.Formatter (Self    : in Appender'Class;
                                        Content : in Util.Strings.Builders.Builder;
                                        Date    : in Ada.Calendar.Time;
                                        Level   : in Level_Type;
                                        Logger  : in String);
