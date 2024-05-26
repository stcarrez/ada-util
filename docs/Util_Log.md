# Logging
The `Util.Log` package and children provide a simple logging framework inspired
from the [Java Log4j](https://logging.apache.org/log4j/2.x/) library.  It is intended to provide a subset of logging features
available in other languages, be flexible, extensible, small and efficient.  Having
log messages in large applications is very helpful to understand, track and fix complex
issues, some of them being related to configuration issues or interaction with other
systems.  The overhead of calling a log operation is negligible when the log is disabled
as it is in the order of 30ns and reasonable for a file appender has it is in the order
of 5us.  To use the packages described here, use the following GNAT project:

```Ada
with "utilada_base";
```

## Using the log framework
A bit of terminology:

* A *logger* is the abstraction that provides operations to emit a message.  The message
  is composed of a text, optional formatting parameters, a log level and a timestamp.
* A *formatter* is the abstraction that takes the information about the log and its
  parameters to create the formatted message.
* An *appender* is the abstraction that writes the message either to a console, a file
  or some other final mechanism.  A same log can be sent to several appenders at the
  same time.
* A *layout* describes how the formatted message, log level, date are used to form the
  final message.  Each appender can be configured with its own layout.

## Logger Declaration
Similar to other logging framework such as [Java Log4j](https://logging.apache.org/log4j/2.x/) and [Log4cxx](https://logging.apache.org/log4cxx/latest_stable/index.html), it is necessary to have
an instance of a logger to write a log message.  The logger instance holds the configuration
for the log to enable, disable and control the format and the appender that will receive
the message.  The logger instance is associated with a name that is used for the
configuration.  A good practice is to declare a `Log` instance in the package body or
the package private part to make available the log instance to all the package operations.
The instance is created by using the `Create` function.  The name used for the configuration
is free but using the full package name is helpful to control precisely the logs.

```Ada
with Util.Log.Loggers;
package body X.Y is
  Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("X.Y");
end X.Y;
```

## Logger Messages
A log message is associated with a log level which is used by the logger instance to
decide to emit or drop the log message.  To keep the logging API simple and make it easily
usable in the application, several operations are provided to write a message with different
log level.

A log message is a string that contains optional formatting markers that follow more or
less the Java `MessageFormat` class.  A parameter is represented by a number enclosed by `{}`.
The first parameter is represented by `{0}`, the second by `{1}` and so on.  Parameters are
replaced in the final message only when the message is enabled by the log configuration.
The use of parameters allows to avoid formatting the log message when the log is not used.
The log formatter is responsible for creating the message from the format string and
the parameters.

The example below shows several calls to emit a log message with different levels:

```Ada
 Log.Error ("Cannot open file {0}: {1}", Path, "File does not exist");
 Log.Warn ("The file {0} is empty", Path);
 Log.Info ("Opening file {0}", Path);
 Log.Debug ("Reading line {0}", Line);
```

The logger also provides a special `Error` procedure that accepts an Ada exception
occurrence as parameter.  The exception name and message are printed together with
the error message.  It is also possible to activate a complete traceback of the
exception and report it in the error message.  With this mechanism, an exception
can be handled and reported easily:

```Ada
 begin
    ...
 exception
    when E : others =>
       Log.Error ("Something bad occurred", E, Trace => True);
 end;
```

## Log Configuration
The log configuration uses property files close to the Apache Log4j and to the
Apache [Log4cxx](https://logging.apache.org/log4cxx/latest_stable/index.html) configuration files.
The configuration file contains several parts to configure the logging framework:

* First, the *appender* configuration indicates the appender that exists and can receive
  a log message.
* Second, a root configuration allows to control the default behavior of the logging
  framework.  The root configuration controls the default log level as well as the
  appenders that can be used.
* Last, a logger configuration is defined to control the logging level more precisely
  for each logger.

The log configuration is loaded with `Initialize` either from a file or from a
`Properties` object that has been loaded or populated programatically.  The procedure
takes two arguments.  The first argument must be either a path to the file that must
be loaded or the `Properties` object.  The second argument is a prefix string which
indicates the prefix of configuration properties.  Historically, the default prefix
used is the string `"log4j."`.  Each application can use its own prefix.

```Ada
 Util.Log.Loggers.Initialize ("config.properties", "log4j.");
```

Here is a simple log configuration that creates a file appender where log messages are
written.  The file appender is given the name `result` and is configured to write the
messages in the file `my-log-file.log`.  The file appender will use the `level-message`
format for the layout of messages.  Last is the configuration of the `X.Y` logger
that will enable only messages starting from the `WARN` level.

```Ada
log4j.rootCategory=DEBUG,result
log4j.appender.result=File
log4j.appender.result.File=my-log-file.log
log4j.appender.result.layout=level-message
log4j.logger.X.Y=WARN
```

By default when the `layout` is not set or has an invalid value, the full message is
reported and the generated log messages will look as follows:

```Ada
[2018-02-07 20:39:51] ERROR - X.Y - Cannot open file test.txt: File does not exist
[2018-02-07 20:39:51] WARN  - X.Y - The file test.txt is empty
[2018-02-07 20:39:51] INFO  - X.Y - Opening file test.txt
[2018-02-07 20:39:51] DEBUG - X.Y - Reading line ......
```

When the `layout` configuration is set to `data-level-message`, the message is printed
with the date and message level.

```Ada
[2018-02-07 20:39:51] ERROR: Cannot open file test.txt: File does not exist
[2018-02-07 20:39:51] WARN : The file test.txt is empty
[2018-02-07 20:39:51] INFO : X.Y - Opening file test.txt
[2018-02-07 20:39:51] DEBUG: X.Y - Reading line ......
```

When the `layout` configuration is set to `level-message`, only the message and its
level are reported.

```Ada
ERROR: Cannot open file test.txt: File does not exist
WARN : The file test.txt is empty
INFO : X.Y - Opening file test.txt
DEBUG: X.Y - Reading line ......
```

The last possible configuration for `layout` is `message` which only prints the message.

```Ada
Cannot open file test.txt: File does not exist
The file test.txt is empty
Opening file test.txt
Reading line ......
```

### Console appender
The `Console` appender uses either `Ada.Text_IO` or a direct write on console
to write messages.  The default is to use `Ada.Text_IO` and the appender expects
standard Ada strings encoded in Latin-1 in the configuration.  When the appender
gets UTF-8 strings, it should be configured for a direct write on the console.
The console appender recognises the following configurations:

| Name           | Description                                                          |
| -------------- | --------------------------------------------------------------       |
| layout         | Defines the format of the message printed by the appender.           |
| level          | Defines the minimum level above which messages are printed.          |
| utc            | When 'true' or '1', print the date in UTC instead of local time      |
| stderr         | When 'true' or '1', use the console standard error,                  |
|                | by default the appender uses the standard output.                    |
| utf8           | When 'true', use a direct write on the console and avoid using       |
|                | `Ada.Text_IO`.                                                       |

Example of configuration:

```Ada
log4j.appender.console=Console
log4j.appender.console.level=WARN
log4j.appender.console.layout=level-message
log4j.appender.console.utc=false
```

### File appender
The `File` appender recognises the following configurations:

| Name           | Description                                                          |
| -------------- | --------------------------------------------------------------       |
| layout         | Defines the format of the message printed by the appender.           |
| level          | Defines the minimum level above which messages are printed.          |
| utc            | When 'true' or '1', print the date in UTC instead of local time      |
| File           | The path used by the appender to create the output file.             |
| append         | When 'true' or '1', the file is opened in append mode otherwise      |
|                | it is truncated (the default is to truncate).                        |
| immediateFlush | When 'true' or '1', the file is flushed after each message log.      |
|                | Immediate flush is useful in some situations to have the log file    |
|                | updated immediately at the expense of slowing down the processing    |
|                | of logs.                                                             |

### Rolling file appender
The `RollingFile` appender recognises the following configurations:

| Name           | Description                                                          |
| -------------- | --------------------------------------------------------------       |
| layout         | Defines the format of the message printed by the appender.           |
| level          | Defines the minimum level above which messages are printed.          |
| utc            | When 'true' or '1', print the date in UTC instead of local time      |
| fileName       | The name of the file to write to. If the file, or any of its parent  |
|                | directories, do not exist, they will be created.                     |
| filePattern    | The pattern of the file name of the archived log file.  The pattern  |
|                | can contain '%i' which are replaced by a counter incremented at each |
|                | rollover, a '%d' is replaced by a date pattern.                      |
| append         | When 'true' or '1', the file is opened in append mode otherwise      |
|                | it is truncated (the default is to truncate).                        |
| immediateFlush | When 'true' or '1', the file is flushed after each message log.      |
|                | Immediate flush is useful in some situations to have the log file    |
|                | updated immediately at the expense of slowing down the processing    |
|                | of logs.                                                             |
| policy         | The triggering policy which drives when a rolling is performed.      |
|                | Possible values are: `none`, `size`, `time`, `size-time`             |
| strategy       | The strategy to use to determine the name and location of the        |
|                | archive file.  Possible values are: `ascending`, `descending`, and   |
|                | `direct`.  Default is `ascending`.                                   |
| policyInterval | How often a rollover should occur based on the most specific time    |
|                | unit in the date pattern.  This indicates the period in seconds      |
|                | to check for pattern change in the `time` or `size-time` policy.     |
| policyMin      | The minimum value of the counter. The default value is 1.            |
| policyMax      | The maximum value of the counter. Once this values is reached older  |
|                | archives will be deleted on subsequent rollovers. The default        |
|                | value is 7.                                                          |
| minSize        | The minimum size the file must have to roll over.                    |

A typical rolling file configuration would look like:

```Ada
log4j.rootCategory=DEBUG,applogger,apperror
log4j.appender.applogger=RollingFile
log4j.appender.applogger.layout=level-message
log4j.appender.applogger.level=DEBUG
log4j.appender.applogger.fileName=logs/debug.log
log4j.appender.applogger.filePattern=logs/debug-%d{YYYY-MM}/debug-%{dd}-%i.log
log4j.appender.applogger.strategy=descending
log4j.appender.applogger.policy=time
log4j.appender.applogger.policyMax=10
log4j.appender.apperror=RollingFile
log4j.appender.apperror.layout=level-message
log4j.appender.apperror.level=ERROR
log4j.appender.apperror.fileName=logs/error.log
log4j.appender.apperror.filePattern=logs/error-%d{YYYY-MM}/error-%{dd}.log
log4j.appender.apperror.strategy=descending
log4j.appender.apperror.policy=time
```

With this configuration, the error messages are written in the `error.log` file and
they are rotated on a day basis and moved in a directory whose name contains the year
and month number.  At the same time, debug messages are written in the `debug.log`
file.

## Custom appender
It is possible to write a customer log appender and use it in the generation
of logs.  This is done in two steps:

* first by extending the `Util.Log.Appenders.Appender` tagged type and overriding
  some of the methods to implement the custom log appender and by writing
  a `Create` function whose role is to create instances of the appender and
  configure them according to the user configuration.
* second by instantiating the `Util.Log.Appenders.Factories` package.
  The package is instantiated with the appender's name and the `Create` function.
  It contains an elaboration body that registers automatically the factory.

For example, the first step could be implemented as follows (methods are
not shown):

```Ada
 type Syslog_Appender (Length : Positive) is
    new Util.Log.Appenders.Appender (Length) with null record;
 function Create (Name       : in String;
                  Properties : in Util.Properties.Manager;
                  Default    : in Util.Log.Level_Type)
               return Util.Log.Appenders.Appender_Access;
```

Then, the package is instantiated as follows:

```Ada
 package Syslog_Factory is
   new Util.Log.Appenders.Factories (Name   => "syslog",
                                     Create => Create'Access)
   with Unreferenced;
```

The appender for `syslog` can be configured as follows to report all errors
to `syslog`:

```Ada
 log4j.logger.X.Y=INFO,console,syslog
 log4j.appender.test=syslog
 log4j.appender.test.level=ERROR
```

## Custom formatter
The formatter is responsible for preparing the message to be displayed
by log appenders.  It takes the message string and its arguments and builds
the message.  The same formatted message is given to each log appender.

Using a custom formatter can be useful to change the message before it is
formatter, filter messages to hide sensitive information and so on.
Implementing a custom formatter is made in three steps:

* first by extending the `Util.Log.Formatters.Formatter` tagged type and
  overriding the `Format` procedure.  The procedure gets the log message passed
  to the `Debug`, `Info`, `Warn` or `Error` procedure as well as every parameter
  passed to customize the final message.  It must populate a `Builder`
  object with the formatted message.
* second by writing a `Create` function that allocates an instance of
  the formatter and customizes it with some configuration properties.
* third by instantiating the `Util.Log.Formatters.Factories` generic package.
  It contains an elaboration body that registers automatically the factory.

For example, the two first steps could be implemented as follows (methods are
not shown):

```Ada
 type NLS_Formatter (Length : Positive) is
    new Util.Log.Formatters.Formatter (Length) with null record;
 function Create (Name       : in String;
                  Properties : in Util.Properties.Manager;
                  Default    : in Util.Log.Level_Type)
               return Util.Log.Formatters.Formatter_Access;
```

Then, the package is instantiated as follows:

```Ada
 package NLS_Factory is
   new Util.Log.Appenders.Factories (Name   => "NLS",
                                     Create => Create'Access)
   with Unreferenced;
```

To use the new registered formatter, it is necessary to declare some minimal
configuration.  A `log4j.formatter.<name>` definition must be declared for each
named formatter where `<name>` is the logical name of the formatter.  The
property must indicate the factory name that must be used (example: `NLS`).
The named formatter can have custom properties and they are passed to the
`Create` procedure when it is created.  Such properties can be used to customize
the behavior of the formatter.

```Ada
 log4j.formatter.nlsFormatter=NLS
 log4j.formatter.nslFormatter.prop1=value1
 log4j.formatter.nlsFormatter.prop2=value2
```

Once the named formatter is declared, it can be selected for one or several
logger by appending the string `:<name>` after the log level.  For example:

```Ada
 log4j.logger.X.Y=WARN:nlsFormatter
```

With the above configuration, the `X.Y` and its descendant loggers will use
the formatter identified by `nlsFormatter`.  Note that it is also possible
to specify an appender for the configuration:

```Ada
 log4j.logger.X.Y=INFO:nlsFormatter,console
```

The above configuration will use the `nlsFormatter` formatter and the `console`
appender to write on the console.

