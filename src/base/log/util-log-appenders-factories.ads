-----------------------------------------------------------------------
--  util-log-appenders-factories -- Factories for log appenders
--  Copyright (C) 2001 - 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  == Custom appender ==
--  It is possible to write a customer log appender and use it in the generation
--  of logs.  This is done in two steps:
--
--  * first by extending the `Util.Log.Appenders.Appender` tagged type and overriding
--    some of the methods to implement the custom log appender and by writing
--    a `Create` function whose role is to create instances of the appender and
--    configure them according to the user configuration.
--  * second by instantiating the `Util.Log.Appenders.Factories` package.
--    The package is instantiated with the appender's name and the `Create` function.
--    It contains an elaboration body that registers automatically the factory.
--
--  For example, the first step could be implemented as follows (methods are
--  not shown):
--
--     type Syslog_Appender (Length : Positive) is
--        new Util.Log.Appenders.Appender (Length) with null record;
--     function Create (Name       : in String;
--                      Properties : in Util.Properties.Manager;
--                      Default    : in Util.Log.Level_Type)
--                   return Util.Log.Appenders.Appender_Access;
--
--  Then, the package is instantiated as follows:
--
--     package Syslog_Factory is
--       new Util.Log.Appenders.Factories (Name   => "syslog",
--                                         Create => Create'Access)
--       with Unreferenced;
--
--  The appender for `syslog` can be configured as follows to report all errors
--  to `syslog`:
--
--     log4j.logger.X.Y=INFO,console,syslog
--     log4j.appender.test=syslog
--     log4j.appender.test.level=ERROR
--
--  See the log.adb and syslog_appender.adb example.
generic
   Name   : String;
   Create : Factory_Access;
package Util.Log.Appenders.Factories is

   --  Make sure the body is elaborated because it links the internal
   --  factory instance to the internal list.
   pragma Elaborate_Body;

   procedure Register is null with Obsolescent;

end Util.Log.Appenders.Factories;
