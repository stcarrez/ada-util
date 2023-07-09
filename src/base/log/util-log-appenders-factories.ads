-----------------------------------------------------------------------
--  util-log-appenders -- Log appenders
--  Copyright (C) 2001 - 2019, 2021, 2023 Stephane Carrez
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

--  == Custom appender ==
--  It is possible to write a customer log appender and use it in the generation
--  of logs.  This is done in three steps:
--
--  * first by extending the `Util.Log.Appenders.Appender` type and overriding
--    some of the methods to implement the custom log appender and by writing
--    a `Create` function whose role is to create instances of the appender and
--    configure them according to the user configuration.
--  * second by instantiating the `Util.Log.Appenders.Factories` package which
--    provides a `Register` procedure.  The package is instantiated with the
--    appender's name and the `Create` function.
--  * third by calling the `Register` procedure before configuring the logs.
--
--  For example, the first step could be implemented as follows (methods are
--  not shown):
--
--     type Syslog_Appender (Length : Positive) is
--        new Util.Log.Appenders.Appender (Length) with null record;
--     function Create (Name       : in String;
--                    Properties : in Util.Properties.Manager;
--                    Default    : in Util.Log.Level_Type)
--                   return Util.Log.Appenders.Appender_Access;
--
--  Then, the package is instantiated as follows:
--
--     package Syslog_Factory is
--       new Util.Log.Appenders.Factories (Name   => "syslog",
--                                         Create => Create'Access);
--
--  The last step should be done before configuring the log appenders:
--
--     Syslog_Appenders.Syslog_Factory.Register;
--     Util.Log.Loggers.Initialize ("samples/log4j.properties");
generic
   Name   : String;
   Create : Factory_Access;
package Util.Log.Appenders.Factories is

   procedure Register;

end Util.Log.Appenders.Factories;
