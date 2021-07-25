-----------------------------------------------------------------------
--  util-log-appenders-consoles -- Console log appenders
--  Copyright (C) 2001 - 2019, 2021 Stephane Carrez
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

with Util.Beans.Objects;
with Util.Properties.Basic;
with Util.Log.Appenders.Formatter;
package body Util.Log.Appenders.Consoles is

   use Ada;

   overriding
   procedure Append (Self    : in out Console_Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Level_Type;
                     Logger  : in String) is
      procedure Write_Standard_Output (Data : in String) with Inline_Always;
      procedure Write_Standard_Error (Data : in String) with Inline_Always;

      procedure Write_Standard_Output (Data : in String) is
      begin
         --  Don't use Text_IO.Standard_Output so that we honor the Set_Output definition.
         Text_IO.Put (Data);
      end Write_Standard_Output;

      procedure Write_Standard_Error (Data : in String) is
      begin
         Text_IO.Put (Text_IO.Current_Error, Data);
      end Write_Standard_Error;

      procedure Write_Output is new Formatter (Write_Standard_Output);
      procedure Write_Error is new Formatter (Write_Standard_Error);
   begin
      if Self.Level >= Level then
         if Self.Stderr then
            if not Util.Beans.Objects.Is_Null (Self.Prefix) then
               Text_IO.Put (Text_IO.Current_Error,
                            Util.Beans.Objects.To_String (Self.Prefix));
            end if;
            Write_Error (Self, Message, Date, Level, Logger);
            Text_IO.New_Line (Text_IO.Current_Error);
         else
            Write_Output (Self, Message, Date, Level, Logger);
            Text_IO.New_Line;
         end if;
      end if;
   end Append;

   --  ------------------------------
   --  Flush the log events.
   --  ------------------------------
   overriding
   procedure Flush (Self : in out Console_Appender) is
   begin
      if Self.Stderr then
         Text_IO.Flush (Text_IO.Current_Error);
      else
         Text_IO.Flush;
      end if;
   end Flush;

   --  ------------------------------
   --  Create a console appender and configure it according to the properties
   --  ------------------------------
   function Create (Name       : in String;
                    Properties : in Util.Properties.Manager;
                    Default    : in Level_Type)
                   return Appender_Access is
      use Util.Properties.Basic;

      Result : constant Console_Appender_Access
        := new Console_Appender '(Finalization.Limited_Controlled with Length => Name'Length,
                                  Name => Name,
                                  others => <>);
   begin
      Result.Set_Level (Name, Properties, Default);
      Result.Set_Layout (Name, Properties, FULL);
      Result.Prefix := Properties.Get_Value ("appender." & Name & ".prefix");
      Result.Stderr := Boolean_Property.Get (Properties, "appender." & Name & ".stderr", False);
      return Result.all'Access;
   end Create;

end Util.Log.Appenders.Consoles;
