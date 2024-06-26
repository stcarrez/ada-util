-----------------------------------------------------------------------
--  util-log-appenders-consoles -- Console log appenders
--  Copyright (C) 2001 - 2019, 2021, 2023, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;

with Util.Beans.Objects;
with Util.Properties.Basic;
with Util.Log.Appenders.Formatter;
with Util.Systems.IO;
with Util.Systems.Os;
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
      procedure Write_UTF_8 (Prefix  : in String) with Inline_Always;

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

      procedure Write_UTF_8 (Prefix  : in String) is
         use Util.Systems;

         Fd  : constant IO.File_Type := (if Self.Stderr then
                                            IO.STDERR_FILENO
                                         else
                                            IO.STDOUT_FILENO);
         Msg : constant String := Util.Strings.Builders.To_Array (Message);
      begin
         --  Write on stderr or stdout but call Put_Raw only once with
         --  the final formatted string because Put_Raw does not bufferize.
         if Self.Layout /= Formatters.MESSAGE then
            IO.Put_Raw (Fd, Prefix
                        & Format (Self, Date, Level, Logger)
                        & Msg & Os.Line_Separator);
         else
            IO.Put_Raw (Fd, Prefix & Msg & Os.Line_Separator);
         end if;
      end Write_UTF_8;

   begin
      if Self.Level >= Level then
         if Self.Utf8 then
            if not Util.Beans.Objects.Is_Null (Self.Prefix) then
               Write_UTF_8 (Util.Beans.Objects.To_String (Self.Prefix));
            else
               Write_UTF_8 ("");
            end if;

         elsif Self.Stderr then
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
      if not Self.Utf8 then
         if Self.Stderr then
            Text_IO.Flush (Text_IO.Current_Error);
         else
            Text_IO.Flush;
         end if;
      end if;
   end Flush;

   --  ------------------------------
   --  Create a console appender and configure it according to the properties
   --  ------------------------------
   function Create (Name       : in String;
                    Formatter  : in Formatter_Access;
                    Properties : in Util.Properties.Manager;
                    Default    : in Level_Type)
                   return Appender_Access is
      use Util.Properties.Basic;

      Result : constant Console_Appender_Access
        := new Console_Appender '(Finalization.Limited_Controlled with Length => Name'Length,
                                  Formatter => Formatter,
                                  Name => Name,
                                  others => <>);
   begin
      Result.Set_Level (Name, Properties, Default);
      Result.Set_Layout (Name, Properties, FULL);
      Result.Prefix := Properties.Get_Value ("appender." & Name & ".prefix");
      Result.Stderr := Boolean_Property.Get (Properties, "appender." & Name & ".stderr", False);
      Result.Utf8 := Boolean_Property.Get (Properties, "appender." & Name & ".utf8", False);
      return Result.all'Access;
   end Create;

end Util.Log.Appenders.Consoles;
