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
* A *formatter* is the abstraction that takes the information about the log to format
  the final message.
* An *appender* is the abstraction that writes the message either to a console, a file
  or some other final mechanism.

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
The `Console` appender recognises the following configurations:

| Name           | Description                                                          |
| -------------- | --------------------------------------------------------------       |
| layout         | Defines the format of the message printed by the appender.           |
| level          | Defines the minimum level above which messages are printed.          |
| stderr         | When 'true' or '1', use the console standard error,                  |
|                | by default the appender uses the standard output                     |

### File appender
The `File` appender recognises the following configurations:

| Name           | Description                                                          |
| -------------- | --------------------------------------------------------------       |
| layout         | Defines the format of the message printed by the appender.           |
| level          | Defines the minimum level above which messages are printed.          |
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

