-----------------------------------------------------------------------
--  util-log-appenders-rolling_files -- Rolling file log appenders
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
with Ada.Text_IO;
with Ada.Calendar;
with Util.Properties;
private with Util.Refs;
private with Util.Files.Rolling;

--  === Rolling file appender ===
--  The `RollingFile` appender recognises the following configurations:
--
--  | Name           | Description                                                          |
--  | -------------- | --------------------------------------------------------------       |
--  | layout         | Defines the format of the message printed by the appender.           |
--  | level          | Defines the minimum level above which messages are printed.          |
--  | fileName       | The name of the file to write to. If the file, or any of its parent  |
--  |                | directories, do not exist, they will be created.                     |
--  | filePattern    | The pattern of the file name of the archived log file.  The pattern  |
--  |                | can contain '%i' which are replaced by a counter incremented at each |
--  |                | rollover, a '%d' is replaced by a date pattern.                      |
--  | append         | When 'true' or '1', the file is opened in append mode otherwise      |
--  |                | it is truncated (the default is to truncate).                        |
--  | immediateFlush | When 'true' or '1', the file is flushed after each message log.      |
--  |                | Immediate flush is useful in some situations to have the log file    |
--  |                | updated immediately at the expense of slowing down the processing    |
--  |                | of logs.                                                             |
--  | policy         | The triggering policy which drives when a rolling is performed.      |
--  |                | Possible values are: `none`, `size`, `time`, `size-time`             |
--  | strategy       | The strategy to use to determine the name and location of the        |
--  |                | archive file.  Possible values are: `ascending`, `descending`, and   |
--  |                | `direct`.  Default is `ascending`.                                   |
--  | policyInterval | How often a rollover should occur based on the most specific time    |
--  |                | unit in the date pattern.  This indicates the period in seconds      |
--  |                | to check for pattern change in the `time` or `size-time` policy.     |
--  | policyMin      | The minimum value of the counter. The default value is 1.            |
--  | policyMax      | The maximum value of the counter. Once this values is reached older  |
--  |                | archives will be deleted on subsequent rollovers. The default        |
--  |                | value is 7.                                                          |
--  | minSize        | The minimum size the file must have to roll over.                    |
--
--  A typical rolling file configuration would look like:
--
--    log4j.rootCategory=DEBUG,applogger,apperror
--    log4j.appender.applogger=RollingFile
--    log4j.appender.applogger.layout=level-message
--    log4j.appender.applogger.level=DEBUG
--    log4j.appender.applogger.fileName=logs/debug.log
--    log4j.appender.applogger.filePattern=logs/debug-%d{YYYY-MM}/debug-%{dd}-%i.log
--    log4j.appender.applogger.strategy=descending
--    log4j.appender.applogger.policy=time
--    log4j.appender.applogger.policyMax=10
--    log4j.appender.apperror=RollingFile
--    log4j.appender.apperror.layout=level-message
--    log4j.appender.apperror.level=ERROR
--    log4j.appender.apperror.fileName=logs/error.log
--    log4j.appender.apperror.filePattern=logs/error-%d{YYYY-MM}/error-%{dd}.log
--    log4j.appender.apperror.strategy=descending
--    log4j.appender.apperror.policy=time
--
--  With this configuration, the error messages are written in the `error.log` file and
--  they are rotated on a day basis and moved in a directory whose name contains the year
--  and month number.  At the same time, debug messages are written in the `debug.log`
--  file.
package Util.Log.Appenders.Rolling_Files is

   --  ------------------------------
   --  File appender
   --  ------------------------------
   type File_Appender (Length : Positive) is new Appender with private;
   type File_Appender_Access is access all File_Appender'Class;

   overriding
   procedure Append (Self    : in out File_Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Level_Type;
                     Logger  : in String);

   --  Flush the log events.
   overriding
   procedure Flush (Self   : in out File_Appender);

   --  Flush and close the file.
   overriding
   procedure Finalize (Self : in out File_Appender);

   --  Create a file appender and configure it according to the properties
   function Create (Name       : in String;
                    Properties : in Util.Properties.Manager;
                    Default    : in Level_Type)
     return Appender_Access;

private

   type File_Entity is new Util.Refs.Ref_Entity with record
      Output          : Ada.Text_IO.File_Type;
   end record;
   type File_Access is access all File_Entity;

   --  Finalize the referenced object.  This is called before the object is freed.
   overriding
   procedure Finalize (Object : in out File_Entity);

   package File_Refs is new Util.Refs.References (File_Entity, File_Access);

   protected type Rolling_File is

      procedure Initialize (Name       : in String;
                            Base       : in String;
                            Properties : in Util.Properties.Manager);

      procedure Openlog (File : out File_Refs.Ref);

      procedure Flush (File : out File_Refs.Ref);

      procedure Closelog;

   private
      Manager    : Util.Files.Rolling.File_Manager;
      Current    : File_Refs.Ref;
      Append     : Boolean;
   end Rolling_File;

   type File_Appender (Length : Positive) is new Appender (Length) with record
      Immediate_Flush : Boolean := False;
      File            : Rolling_File;
   end record;

end Util.Log.Appenders.Rolling_Files;
