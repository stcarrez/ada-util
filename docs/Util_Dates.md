
# Date Utilities
The `Util.Dates` package provides various date utilities to help in formatting and parsing
dates in various standard formats.  It completes the standard `Ada.Calendar.Formatting` and
other packages by implementing specific formatting and parsing. To use the packages
described here, use the following GNAT project:

```Ada
with "utilada_base";
```

## Date Operations
Several operations allow to compute from a given date:

  * `Get_Day_Start`: The start of the day (0:00),
  * `Get_Day_End`: The end of the day (23:59:59),
  * `Get_Week_Start`: The start of the week,
  * `Get_Week_End`: The end of the week,
  * `Get_Month_Start`: The start of the month,
  * `Get_Month_End`: The end of the month

The `Date_Record` type represents a date in a split format allowing
to access easily the day, month, hour and other information.

```Ada
Now        : Ada.Calendar.Time := Ada.Calendar.Clock;
Week_Start : Ada.Calendar.Time := Get_Week_Start (Now);
Week_End   : Ada.Calendar.Time := Get_Week_End (Now);
```

## RFC7231 Dates
The [RFC 7231](https://tools.ietf.org/html/rfc7231) defines a standard date format that is used by HTTP headers.
The `Util.Dates.RFC7231` package provides an `Image` function to convert a date into
that target format and a `Value` function to parse such format string and return the date.

```Ada
  Now  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
  S    : constant String := Util.Dates.RFC7231.Image (Now);
  Date : Ada.Calendar.time := Util.Dates.RFC7231.Value (S);
```

A `Constraint_Error` exception is raised when the date string is not in the correct format.

## ISO8601 Dates
The ISO8601 defines a standard date format that is commonly used and easily parsed by programs.
The `Util.Dates.ISO8601` package provides an `Image` function to convert a date into that
target format and a `Value` function to parse such format string and return the date.

```Ada
  Now  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
  S    : constant String := Util.Dates.ISO8601.Image (Now);
  Date : Ada.Calendar.time := Util.Dates.ISO8601.Value (S);
```

A `Constraint_Error` exception is raised when the date string is not in the correct format.

## Localized date formatting
The `Util.Dates.Formats` provides a date formatting and parsing operation similar to the
Unix `strftime`, `strptime` or the `GNAT.Calendar.Time_IO`.  The localization of month
and day labels is however handled through `Util.Properties.Bundle` (similar to
the Java world).  Unlike `strftime` and `strptime`, this allows to have a multi-threaded
application that reports dates in several languages.  The `GNAT.Calendar.Time_IO` only
supports English and this is the reason why it is not used here.

The date pattern recognizes the following formats:

| Format | Description |
| --- | ---------- |
| %a  | The abbreviated weekday name according to the current locale.
| %A  | The full weekday name according to the current locale.
| %b  | The abbreviated month name according to the current locale.
| %h  | Equivalent to %b. (SU)
| %B  | The full month name according to the current locale.
| %c  | The preferred date and time representation for the current locale.
| %C  | The century number (year/100) as a 2-digit integer. (SU)
| %d  | The day of the month as a decimal number (range 01 to 31).
| %D  | Equivalent to %m/%d/%y
| %e  | Like %d, the day of the month as a decimal number,
|     | but a leading zero is replaced by a space. (SU)
| %F  | Equivalent to %Y\-%m\-%d (the ISO 8601 date format). (C99)
| %G  | The ISO 8601 week-based year
| %H  | The hour as a decimal number using a 24-hour clock (range 00 to 23).
| %I  | The hour as a decimal number using a 12-hour clock (range 01 to 12).
| %j  | The day of the year as a decimal number (range 001 to 366).
| %k  | The hour (24 hour clock) as a decimal number (range 0 to 23);
| %l  | The hour (12 hour clock) as a decimal number (range 1 to 12);
| %m  | The month as a decimal number (range 01 to 12).
| %M  | The minute as a decimal number (range 00 to 59).
| %n  | A newline character. (SU)
| %p  | Either "AM" or "PM"
| %P  | Like %p but in lowercase: "am" or "pm"
| %r  | The time in a.m. or p.m. notation.
|     | In the POSIX locale this is equivalent to %I:%M:%S %p. (SU)
| %R  | The time in 24 hour notation (%H:%M).
| %s  | The number of seconds since the Epoch, that is,
|     | since 1970\-01\-01 00:00:00 UTC. (TZ)
| %S  | The second as a decimal number (range 00 to 60).
| %t  | A tab character. (SU)
| %T  | The time in 24 hour notation (%H:%M:%S). (SU)
| %u  | The day of the week as a decimal, range 1 to 7,
|     | Monday being 1. See also %w. (SU)
| %U  | The week number of the current year as a decimal
|     | number, range 00 to 53
| %V  | The ISO 8601 week number
| %w  | The day of the week as a decimal, range 0 to 6,
|     | Sunday being 0. See also %u.
| %W  | The week number of the current year as a decimal number,
|     | range 00 to 53
| %x  | The preferred date representation for the current locale
|     | without the time.
| %X  | The preferred time representation for the current locale
|     | without the date.
| %y  | The year as a decimal number without a century (range 00 to 99).
| %Y  | The year as a decimal number including the century.
| %z  | The timezone as hour offset from GMT.
| %Z  | The timezone or name or abbreviation.

The following strftime flags are ignored:

| Format | Description |
| --- | ---------- |
| %E  |  Modifier: use alternative format, see below. (SU)
| %O  |  Modifier: use alternative format, see below. (SU)

SU:  Single Unix Specification
C99: C99 standard, POSIX.1-2001

See strftime (3) and strptime (3) manual page

To format and use the localize date, it is first necessary to get a bundle
for the `dates` so that date elements are translated into the given locale.

```Ada
 Factory     : Util.Properties.Bundles.Loader;
 Bundle      : Util.Properties.Bundles.Manager;
 ...
    Load_Bundle (Factory, "dates", "fr", Bundle);
```

The date is formatted according to the pattern string described above.
The bundle is used by the formatter to use the day and month names in the
expected locale.

```Ada
 Date : String := Util.Dates.Formats.Format (Pattern => Pattern,
                                             Date    => Ada.Calendar.Clock,
                                             Bundle  => Bundle);
```

To parse a date according to a pattern and a localization, the same pattern string
and bundle can be used and the `Parse` function will return the date in split format.

```Ada
 Result : Date_Record := Util.Dates.Formats.Parse (Date    => Date,
                                                   Pattern => Pattern,
                                                   Bundle  => Bundle);
```

