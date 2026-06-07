-----------------------------------------------------------------------
--  util-log-appenders-rolling_files -- Rolling file log appenders
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Calendar;
with Util.Properties;
with Util.Files.Rolling.Lzma;
with Util.Log.Appenders.Factories;

--  === LZMA Rolling file appender ===
--  The `LzmaRollingFile` appender is similar to the `RollingFile` appender
--  but it compresses the file using LZMA when rotation occurs.
--  To use it, it is necessary to add `utilada_lzma` GNAT project and
--  declare `LzmaRollingFile` factory by using:
--
--
--    with Util.Log.Appenders.Factories;
--    with Util.Log.Appenders.Rolling_Files.Lzma;
--    ...
--    package Lzma_Rolling_Factory is
--       new Util.Log.Appenders.Factories
--         (Name   => "LzmaRollingFile",
--          Create => Util.Log.Appenders.Rolling_Files.Lzma.Create'Access)
--            with Unreferenced;
--
--  Then, you can use the `LzmaRollingFile` appender by using the same
--  definitions as in the *Rolling file appender* but replacing `RollingFile`
--  by the name you have defined, and by adding a `.xz` extension, for example:
--
--    log4j.appender.applogger=RollingFile
--    log4j.appender.applogger.filePattern=logs/debug-%d{YYYY-MM}/debug-%{dd}-%i.log.xz
--
package Util.Log.Appenders.Rolling_Files.Lzma is

   --  ------------------------------
   --  File appender
   --  ------------------------------
   type File_Appender (Length    : Positive;
                       Formatter : Formatter_Access) is new Appender with private;
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
                    Formatter  : in Formatter_Access;
                    Properties : in Util.Properties.Manager;
                    Default    : in Level_Type)
     return Appender_Access;

private

   package Rolling_File_Manager is
      new Util.Files.Rolling.Protected_Manager (Util.Files.Rolling.Lzma.Lzma_File_Manager);

   type File_Appender (Length    : Positive;
                       Formatter : Formatter_Access) is new Appender (Length, Formatter) with
   record
      Immediate_Flush : Boolean := False;
      File            : Rolling_File_Manager.Rolling_File;
   end record;

end Util.Log.Appenders.Rolling_Files.Lzma;
