-----------------------------------------------------------------------
--  bundles -- Bundle and translation example
--  Copyright (C) 2010, 2021 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Util.Properties.Bundles;
with Util.Log.Loggers;
with GNAT.Command_Line;

procedure Bundles is
   use GNAT.Command_Line;
   use Ada.Strings.Unbounded;
   use Util.Properties.Bundles;
   use Util.Log.Loggers;

   Log     : constant Logger := Create ("log");

   Factory : Util.Properties.Bundles.Loader;

   Bundle : Util.Properties.Bundles.Manager;

   Locale : Unbounded_String := To_Unbounded_String ("en");
begin
   Util.Log.Loggers.Initialize ("samples/log4j.properties");

   --  Load the bundles from the current directory
   Initialize (Factory, "samples/");
   loop
      case Getopt ("h l: locale: d: directory: help") is
         when ASCII.NUL =>
            exit;

         when 'd' =>
            Initialize (Factory, Parameter);

         when 'l' =>
            Locale := To_Unbounded_String (Parameter);

         when others =>
            Log.Info ("Usage: bundles -d dir -l locale bundle");
            return;
      end case;
   end loop;
   declare
      Name : constant String := Get_Argument;
   begin
      Load_Bundle (Factory, Name, To_String (Locale), Bundle);
   exception
      when NO_BUNDLE =>
         Log.Error ("There is no bundle: {0}", Name);
   end;
   loop
      declare
         Name : constant String := Get_Argument;
      begin
         exit when Name = "";
         Ada.Text_IO.Put_Line (Name & "=" & String '(Bundle.Get (Name)));
      end;
   end loop;

exception
   when Invalid_Switch | Invalid_Parameter =>
      Log.Error ("Usage: bundles -d dir -l locale bundle message message");
      Log.Error ("Example: bundles -d samples -l fr messages welcome");
      Log.Error ("         bundles -d samples -l de messages welcome");
      Log.Error ("         bundles -d samples messages welcome");

   when E : Util.Properties.NO_PROPERTY =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
end Bundles;
