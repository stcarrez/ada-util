-----------------------------------------------------------------------
--  util-files-rolling -- Rolling file manager
--  Copyright (C) 2022 Stephane Carrez
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
with Ada.Calendar;
with Ada.Directories;
with Util.Strings.Vectors;

--  == Rolling file manager ==
--  The `Util.Files.Rolling` package provides a simple support to roll a file
--  based on some rolling policy.  Such rolling is traditionally used for file
--  logs to move files to another place when they reach some size limit or when
--  some date conditions are met (such as a day change).  The file manager uses
--  a file path and a pattern.  The file path is used to define the default
--  or initial file.  The pattern is used when rolling occurs to decide how
--  to reorganize files.
--
--  The file manager defines a triggering policy represented by `Policy_Type`.
--  It controls when the file rolling must be made.
--
--  * `No_Policy`: no policy, the rolling must be triggered manually.
--  * `Size_Policy`: size policy, the rolling is triggered when the file
--     reaches a given size.
--  * `Time_Policy`: time policy, the rolling is made when the date/time pattern
--     no longer applies to the active file; the `Interval` configuration
--     defines the period to check for time changes,
--  * `Size_Time_Policy`: combines the size and time policy, the rolling is
--     triggered when either the file reaches a given size or the date/time
--     pattern no longer applies to the active file.
--
--  To control how the rolling is made, the `Strategy_Type` defines the behavior
--  of the rolling.
--
--  * `Rollover_Strategy`:
--  * `Direct_Strategy`:
--
--  To use the file manager, the first step is to create an instance and configure
--  the default file, pattern, choose the triggering policy and strategy:
--
--    Manager : Util.Files.Rolling.File_Manager;
--
--    Manager.Initialize ("dynamo.log", "dynamo-%i.log",
--                        Policy => (Size_Policy, 100_000),
--                        Strategy => (Rollover_Strategy, 1, 10));
--
--  After the initialization, the current file is retrieved by using the
--  `Get_Current_Path` function and you should call `Is_Rollover_Necessary`
--  before writing content on the file.  When it returns `True`, it means you
--  should call the `Rollover` procedure that will perform roll over according
--  to the rolling strategy.
--
package Util.Files.Rolling is

   type Policy_Kind is (No_Policy, Size_Policy, Time_Policy, Size_Time_Policy);

   type Policy_Type (Kind : Policy_Kind) is record
      case Kind is
         when No_Policy =>
            null;

         when Time_Policy | Size_Policy | Size_Time_Policy =>
            Size     : Ada.Directories.File_Size := 100_000_000;
            Interval : Natural := 0;

      end case;
   end record;

   type Strategy_Kind is (Ascending_Strategy,
                          Descending_Strategy,
                          Direct_Strategy);

   type Strategy_Type (Kind : Strategy_Kind) is record
      case Kind is
         when Ascending_Strategy | Descending_Strategy =>
            Min_Index : Natural;
            Max_Index : Natural;

         when Direct_Strategy =>
            Max_Files : Natural;

      end case;
   end record;

   --  Format the file pattern with the date and index to produce a file path.
   function Format (Pattern : in String;
                    Date    : in Ada.Calendar.Time;
                    Index   : in Natural) return String;

   type File_Manager is tagged limited private;

   --  Initialize the file manager to roll the file referred by `Path` by using
   --  the pattern defined in `Pattern`.
   procedure Initialize (Manager  : in out File_Manager;
                         Path     : in String;
                         Pattern  : in String;
                         Policy   : in Policy_Type;
                         Strategy : in Strategy_Type);

   --  Get the current path (it may or may not exist).
   function Get_Current_Path (Manager : in File_Manager) return String;

   --  Check if a rollover is necessary based on the rolling strategy.
   function Is_Rollover_Necessary (Manager : in out File_Manager) return Boolean;

   --  Perform a rollover according to the strategy that was configured.
   procedure Rollover (Manager : in out File_Manager);

   procedure Rollover_Ascending (Manager : in out File_Manager);
   procedure Rollover_Descending (Manager : in out File_Manager);
   procedure Rollover_Direct (Manager : in out File_Manager);

   --  Get the regex pattern to identify a file that must be purged.
   --  The default is to extract the file pattern part of the file manager pattern.
   function Get_Purge_Pattern (Manager : in File_Manager;
                               Date    : in Ada.Calendar.Time) return String;

private

   --  Find the files that are eligible to purge in the given directory.
   procedure Eligible_Files (Manager     : in out File_Manager;
                             Path        : in String;
                             Date        : in Ada.Calendar.Time;
                             Names       : in out Util.Strings.Vectors.Vector;
                             First_Index : out Natural;
                             Last_Index  : out Natural);

   procedure Rename (Manager : in out File_Manager;
                     Old     : in String);

   type File_Manager is tagged limited record
      Policy    : Policy_Kind := No_Policy;
      Strategy  : Strategy_Kind := Ascending_Strategy;
      File_Path : Unbounded_String;
      Last_Path : Unbounded_String;
      Pattern   : Unbounded_String;
      Interval  : Natural;
      Cur_Index : Natural := 1;
      Min_Index : Natural;
      Max_Index : Natural;
      Max_Files : Natural := 1;
      Deadline  : Ada.Calendar.Time;
      Max_Size  : Ada.Directories.File_Size := Ada.Directories.File_Size'Last;
   end record;

end Util.Files.Rolling;
