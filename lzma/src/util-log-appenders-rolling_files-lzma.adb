-----------------------------------------------------------------------
--  util-log-appenders-rolling_files -- Rolling file log appenders
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Finalization;
with Ada.Text_IO;
with Util.Properties.Basic;
with Util.Log.Appenders.Formatter;
package body Util.Log.Appenders.Rolling_Files.Lzma is

   use Ada;
   use Ada.Finalization;

   package Bool_Prop renames Util.Properties.Basic.Boolean_Property;

   overriding
   procedure Append (Self    : in out File_Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Level_Type;
                     Logger  : in String) is

   begin
      if Self.Level >= Level then
         declare
            File : Util.Files.Rolling.File_Refs.Ref;

            procedure Write_File (Data : in String) with Inline_Always;

            procedure Write_File (Data : in String) is
            begin
               Text_IO.Put (File.Value.Output, Data);
            end Write_File;

            procedure Write is new Formatter (Write_File);
         begin
            Self.File.Openlog (File);
            if not File.Is_Null then
               Write (Self, Message, Date, Level, Logger);
               Text_IO.New_Line (File.Value.Output);
               if Self.Immediate_Flush then
                  Text_IO.Flush (File.Value.Output);
               end if;
            end if;
         end;
      end if;
   end Append;

   --  ------------------------------
   --  Flush the log events.
   --  ------------------------------
   overriding
   procedure Flush (Self   : in out File_Appender) is
      File : Util.Files.Rolling.File_Refs.Ref;
   begin
      Self.File.Flush (File);
      if not File.Is_Null then
         Text_IO.Flush (File.Value.Output);
      end if;
   end Flush;

   --  ------------------------------
   --  Flush and close the file.
   --  ------------------------------
   overriding
   procedure Finalize (Self : in out File_Appender) is
   begin
      Self.File.Closelog;
   end Finalize;

   --  ------------------------------
   --  Create a file appender and configure it according to the properties
   --  ------------------------------
   function Create (Name       : in String;
                    Formatter  : in Formatter_Access;
                    Properties : in Util.Properties.Manager;
                    Default    : in Level_Type)
                   return Appender_Access is
      Base   : constant String := "appender." & Name;
      File   : constant String := Properties.Get (Base & ".fileName", Name & ".log");
      Pat    : constant String := Properties.Get (Base & ".filePattern", Name & "-%i.log");
      Append : constant Boolean := Bool_Prop.Get (Properties, Base & ".append", True);
      Result : constant File_Appender_Access
        := new File_Appender '(Limited_Controlled with Length => Name'Length,
                               Formatter => Formatter,
                               Name => Name,
                               others => <>);
   begin
      Result.Set_Level (Name, Properties, Default);
      Result.Set_Layout (Name, Properties, FULL);
      Result.Immediate_Flush := Bool_Prop.Get (Properties, Base & ".immediateFlush", True);
      Result.File.Initialize (Path     => File,
                              Pattern  => Pat,
                              Policy   => Get_Policy (Base, Properties),
                              Strategy => Get_Strategy (Base, Properties),
                              Mode_Append => Append);
      return Result.all'Access;
   end Create;

end Util.Log.Appenders.Rolling_Files.Lzma;
