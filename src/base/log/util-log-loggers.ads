-----------------------------------------------------------------------
--  util-log-loggers -- Utility Log Package
--  Copyright (C) 2006 - 2024 Free Software Foundation, Inc.
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Util.Properties;
private with Ada.Finalization;
private with Util.Log.Appenders;
private with Util.Log.Formatters;
package Util.Log.Loggers is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   --  The logger identifies and configures the log produced
   --  by a component that uses it.  The logger has a name
   --  which can be printed in the log outputs.  The logger instance
   --  contains a log level which can be used to control the level of
   --  logs.
   type Logger is tagged limited private;
   type Logger_Access is access constant Logger;

   --  Create a logger with the given name.
   function Create (Name : in String) return Logger;

   --  Create a logger with the given name and use the specified level.
   function Create (Name  : in String;
                    Level : in Level_Type) return Logger;

   --  Change the log level
   procedure Set_Level (Log   : in out Logger;
                        Level : in Level_Type);

   --  Get the log level.
   function Get_Level (Log : in Logger) return Level_Type;

   --  Get the log level name.
   function Get_Level_Name (Log : in Logger) return String;

   --  Check if debug log or info log are enabled for this logger.
   function Is_Debug_Enabled (Log : in Logger) return Boolean;
   function Is_Info_Enabled (Log : in Logger) return Boolean;

   procedure Print (Log     : in Logger;
                    Level   : in Level_Type;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "";
                    Arg4    : in String := "");

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "");

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in String;
                    Arg2    : in String;
                    Arg3    : in String;
                    Arg4    : in String);

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in Unbounded_String;
                    Arg2    : in String := "";
                    Arg3    : in String := "");

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in Unbounded_String;
                    Arg2    : in Unbounded_String;
                    Arg3    : in String := "");

   procedure Info (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "");

   procedure Info (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in String;
                   Arg2    : in String;
                   Arg3    : in String;
                   Arg4    : in String);

   procedure Info (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in Unbounded_String;
                   Arg2    : in String := "";
                   Arg3    : in String := "");

   procedure Warn (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "");

   procedure Error (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "");

   procedure Error (Log     : in Logger'Class;
                    Message : in String;
                    E       : in Exception_Occurrence;
                    Trace   : in Boolean := False);

   DEFAULT_PREFIX : constant String := "log4j.";

   --  Initialize the log environment with the property file.
   procedure Initialize (Name   : in String;
                         Prefix : in String := DEFAULT_PREFIX);

   --  Initialize the log environment with the properties.
   procedure Initialize (Properties : in Util.Properties.Manager;
                         Prefix     : in String := DEFAULT_PREFIX);

   --  Return a printable traceback that correspond to the exception.
   function Traceback (E : in Exception_Occurrence) return String;

private

   type Logger_Info;
   type Logger_Info_Access is access all Logger_Info;

   type Logger_Info (Len : Positive) is limited record
      Next_Logger : Logger_Info_Access;
      Prev_Logger : Logger_Info_Access;
      Appender    : Appenders.Appender_Access;
      Formatter   : Formatters.Formatter_Access;
      Level       : Level_Type := INFO_LEVEL;
      Name        : String (1 .. Len);
   end record;

   type Logger is new Ada.Finalization.Limited_Controlled with record
      Instance : Logger_Info_Access;
   end record;

   --  Finalize the logger and flush the associated appender
   overriding
   procedure Finalize (Log : in out Logger);

end Util.Log.Loggers;
