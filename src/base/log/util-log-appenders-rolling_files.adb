-----------------------------------------------------------------------
--  util-log-appenders-rolling_files -- Rolling file log appenders
--  Copyright (C) 2001 - 2022 Stephane Carrez
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
with Ada.Finalization;
with Ada.Directories;
with Util.Properties.Basic;
with Util.Log.Appenders.Formatter;
package body Util.Log.Appenders.Rolling_Files is

   use Ada;
   use Ada.Finalization;

   package Bool_Prop renames Util.Properties.Basic.Boolean_Property;
   package Int_Prop renames Util.Properties.Basic.Integer_Property;

   --  ------------------------------
   --  Finalize the referenced object.  This is called before the object is freed.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out File_Entity) is
   begin
      Text_IO.Close (File => Object.Output);
   end Finalize;

   protected body Rolling_File is

      procedure Initialize (Name       : in String;
                            Base       : in String;
                            Properties : in Util.Properties.Manager) is
         function Get_Policy return Util.Files.Rolling.Policy_Type with Inline;
         function Get_Strategy return Util.Files.Rolling.Strategy_Type with Inline;

         File  : constant String := Properties.Get (Base & ".fileName", Name & ".log");
         Pat   : constant String := Properties.Get (Base & ".filePattern", Base & "-%i.log");

         function Get_Policy return Util.Files.Rolling.Policy_Type is
            Str   : constant String := Properties.Get (Base & ".policy", "time");
            Inter : constant Integer := Int_Prop.Get (Properties, Base & ".policyInterval", 1);
            Size  : constant String := Properties.Get (Base & ".minSize", "1000000");
         begin
            if Str = "none" then
               return (Kind => Util.Files.Rolling.No_Policy);
            elsif Str = "time" then
               return (Kind => Util.Files.Rolling.Time_Policy,
                       Size => 0,
                       Interval => Inter);
            elsif Str = "size-time" or else Str = "time-size" then
               return (Kind => Util.Files.Rolling.Size_Time_Policy,
                       Size => Ada.Directories.File_Size'Value (Size),
                       Interval => Inter);
            else
               return (Kind => Util.Files.Rolling.Size_Policy,
                       Size => Ada.Directories.File_Size'Value (Size),
                       Interval => 0);
            end if;

         exception
            when others =>
               return (Kind => Util.Files.Rolling.No_Policy);
         end Get_Policy;

         function Get_Strategy return Util.Files.Rolling.Strategy_Type is
            Str   : constant String := Properties.Get (Base & ".strategy", "ascending");
            Min   : constant Integer := Int_Prop.Get (Properties, Base & ".policyMin", 0);
            Max   : constant Integer := Int_Prop.Get (Properties, Base & ".policyMax", 0);
         begin
            if Str = "direct" then
               return (Kind => Util.Files.Rolling.Direct_Strategy,
                       Max_Files => Max);

            elsif Str = "descending" then
               return (Kind      => Util.Files.Rolling.Descending_Strategy,
                       Min_Index => Min,
                       Max_Index => Max);

            else
               return (Kind      => Util.Files.Rolling.Ascending_Strategy,
                       Min_Index => Min,
                       Max_Index => Max);

            end if;
         end Get_Strategy;
      begin
         Append := Bool_Prop.Get (Properties, Base & ".append", True);
         Manager.Initialize (Path     => File,
                             Pattern  => Pat,
                             Policy   => Get_Policy,
                             Strategy => Get_Strategy);
      end Initialize;

      procedure Openlog (File : out File_Refs.Ref) is
      begin
         if not Current.Is_Null then
            if not Manager.Is_Rollover_Necessary then
               File := Current;
               return;
            end if;

            Closelog;
            Manager.Rollover;
         end if;

         Current := File_Refs.Create;
         declare
            Path : constant String := Manager.Get_Current_Path;
            Dir  : constant String := Ada.Directories.Containing_Directory (Path);
         begin
            if not Ada.Directories.Exists (Dir) then
               Ada.Directories.Create_Path (Dir);
            end if;

            if not Ada.Directories.Exists (Path) then
               Text_IO.Create (File => Current.Value.Output,
                               Name => Path);
            else
               Text_IO.Open (File => Current.Value.Output,
                             Name => Path,
                             Mode => (if Append then Text_IO.Append_File
                                      else Text_IO.Out_File));
            end if;
            File := Current;
         end;
      end Openlog;

      procedure Flush (File : out File_Refs.Ref) is
      begin
         if not Current.Is_Null then
            File := Current;
         end if;
      end Flush;

      procedure Closelog is
         Empty : File_Refs.Ref;
      begin
         --  Close the current log by releasing its reference counter.
         Current := Empty;
      end Closelog;

   end Rolling_File;

   overriding
   procedure Append (Self    : in out File_Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Level_Type;
                     Logger  : in String) is

   begin
      if Self.Level >= Level then
         declare
            File : File_Refs.Ref;

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
      File : File_Refs.Ref;
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
                    Properties : in Util.Properties.Manager;
                    Default    : in Level_Type)
                   return Appender_Access is
      Base    : constant String := "appender." & Name;
      Result  : constant File_Appender_Access
        := new File_Appender '(Limited_Controlled with Length => Name'Length,
                               Name => Name,
                               others => <>);
   begin
      Result.Set_Level (Name, Properties, Default);
      Result.Set_Layout (Name, Properties, FULL);
      Result.Immediate_Flush := Bool_Prop.Get (Properties, Base & ".immediateFlush", True);
      Result.File.Initialize (Name, Base, Properties);
      return Result.all'Access;
   end Create;

end Util.Log.Appenders.Rolling_Files;
