with Ada.Calendar;
with Util.Properties;
with Util.Strings.Builders;
with Util.Log.Appenders;
with Util.Log.Appenders.Factories;

--  A syslog appender toy example.  Possible configuration:
--
--  log4j.rootCategory=DEBUG,console,result,test
--  log4j.appender.test=syslog
--  log4j.appender.test.level=ERROR
--
package Syslog_Appenders is

   type Syslog_Appender (Length : Positive) is
     new Util.Log.Appenders.Appender (Length) with null record;

   --  Create a syslog appender instance.
   function Create (Name       : in String;
                    Properties : in Util.Properties.Manager;
                    Default    : in Util.Log.Level_Type)
                   return Util.Log.Appenders.Appender_Access;

   --  Append the message to the syslog.
   overriding
   procedure Append (Self    : in out Syslog_Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Util.Log.Level_Type;
                     Logger  : in String);

   overriding
   procedure Flush (Self   : in out Syslog_Appender) is null;

   --  Factory for the syslog appender.
   package Syslog_Factory is
     new Util.Log.Appenders.Factories (Name   => "syslog",
                                       Create => Create'Access);

end Syslog_Appenders;
