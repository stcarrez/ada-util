-----------------------------------------------------------------------
--  date -- Print the date
--  Copyright (C) 2011, 2021, 2022 Stephane Carrez
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
with Ada.Text_IO;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with GNAT.Command_Line;

with Util.Log.Loggers;
with Util.Dates.Formats;
with Util.Properties.Bundles;
procedure Date is

   use Util.Log.Loggers;
   use Ada.Strings.Unbounded;
   use Util.Properties.Bundles;
   use GNAT.Command_Line;

   Log         : constant Logger := Create ("log");

   Factory     : Util.Properties.Bundles.Loader;
   Bundle      : Util.Properties.Bundles.Manager;
   Locale      : Unbounded_String := To_Unbounded_String ("en");
   Date        : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   Use_Default : Boolean := True;
begin
   Util.Log.Loggers.Initialize ("samples/log4j.properties");

   --  Load the bundles from the current directory
   Initialize (Factory, "samples/;bundles");

   loop
      case Getopt ("h l: locale: help") is
         when ASCII.NUL =>
            exit;

         when 'l' =>
            Locale := To_Unbounded_String (Parameter);

         when others =>
            raise GNAT.Command_Line.Invalid_Switch;
      end case;
   end loop;
   begin
      Load_Bundle (Factory, "dates", To_String (Locale), Bundle);

   exception
      when NO_BUNDLE =>
         Log.Error ("There is no bundle: {0}", "dates");
   end;
   loop
      declare
         Pattern : constant String := Get_Argument;
      begin
         exit when Pattern = "";

         Use_Default := False;
         Ada.Text_IO.Put_Line (Util.Dates.Formats.Format (Pattern => Pattern,
                                                          Date    => Date,
                                                          Bundle  => Bundle));
      end;
   end loop;
   if Use_Default then
      Ada.Text_IO.Put_Line (Util.Dates.Formats.Format (Pattern => "%a %b %_d %T %Y",
                                                       Date    => Date,
                                                       Bundle  => Bundle));
   end if;

exception
   when GNAT.Command_Line.Invalid_Switch =>
      Log.Error ("Usage: date -l locale format");
end Date;
