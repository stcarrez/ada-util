# Files
The `Util.Files` package provides various utility operations around files
to help in reading, writing, searching for files in a path.
To use the operations described here, use the following GNAT project:

```Ada
with "utilada_base";
```

## Reading and writing
To easily get the full content of a file, the `Read_File` procedure can be
used.  A first form exists that populates a `Unbounded_String` or a vector
of strings.  A second form exists with a procedure that is called with each
line while the file is read.  These different forms simplify the reading of
files as it is possible to write:

```Ada
Content : Ada.Strings.Unbounded.Unbounded_String;
Util.Files.Read_File ("config.txt", Content);
```

or

```Ada
List : Util.Strings.Vectors.Vector;
Util.Files.Read_File ("config.txt", List);
```

or

```Ada
procedure Read_Line (Line : in String) is ...
Util.Files.Read_File ("config.txt", Read_Line'Access);
```

Similarly, writing a file when you have a string or an `Unbounded_String`
is easily written by using `Write_File` as follows:

```Ada
Util.Files.Write_File ("config.txt", "full content");
```

## Searching files
Searching for a file in a list of directories can be accomplished by using
the `Iterate_Path`, `Iterate_Files_Path` or `Find_File_Path`.

The `Find_File_Path` function is helpful to find a file in some `PATH`
search list.  The function looks in each search directory for the given
file name and it builds and returns the computed path of the first file
found in the search list.  For example:

```Ada
Path : String := Util.Files.Find_File_Path ("ls",
                                            "/bin:/usr/bin",
                                            ':');
```

This will return `/usr/bin/ls` on most Unix systems.

## Rolling file manager
The `Util.Files.Rolling` package provides a simple support to roll a file
based on some rolling policy.  Such rolling is traditionally used for file
logs to move files to another place when they reach some size limit or when
some date conditions are met (such as a day change).  The file manager uses
a file path and a pattern.  The file path is used to define the default
or initial file.  The pattern is used when rolling occurs to decide how
to reorganize files.

The file manager defines a triggering policy represented by `Policy_Type`.
It controls when the file rolling must be made.

* `No_Policy`: no policy, the rolling must be triggered manually.
* `Size_Policy`: size policy, the rolling is triggered when the file
  reaches a given size.
* `Time_Policy`: time policy, the rolling is made when the date/time pattern
  no longer applies to the active file; the `Interval` configuration
  defines the period to check for time changes,
* `Size_Time_Policy`: combines the size and time policy, the rolling is
  triggered when either the file reaches a given size or the date/time
  pattern no longer applies to the active file.

To control how the rolling is made, the `Strategy_Type` defines the behavior
of the rolling.

* `Rollover_Strategy`:
* `Direct_Strategy`:

To use the file manager, the first step is to create an instance and configure
the default file, pattern, choose the triggering policy and strategy:

```Ada
Manager : Util.Files.Rolling.File_Manager;
Manager.Initialize ("dynamo.log", "dynamo-%i.log",
                    Policy => (Size_Policy, 100_000),
                    Strategy => (Rollover_Strategy, 1, 10));

```

After the initialization, the current file is retrieved by using the
`Get_Current_Path` function and you should call `Is_Rollover_Necessary`
before writing content on the file.  When it returns `True`, it means you
should call the `Rollover` procedure that will perform roll over according
to the rolling strategy.

## Directory tree walk
It is sometimes necessary to walk a directory tree while taking into
account some inclusion or exclusion patterns or more complex ignore lists.
The `Util.Files.Walk` package provides a support to walk such directory
tree while taking into account some possible ignore lists such as the
`.gitignore` file.  The package defines the `Filter_Type` tagged type
to represent and control the exclusion or inclusion filters and a second
tagged type `Walker_Type` to walk the directory tree.

The `Filter_Type` provides two operations to add patterns in the filter
and one operation to check against a path whether it matches a pattern.
A pattern can contain fixed paths, wildcards or regular expressions.
Similar to `.gitignore` rules, a pattern which starts with a `/` will
define a pattern that must match the complete path.  Otherwise, the pattern
is a recursive pattern.  Example of pattern setup:

```Ada
 Filter : Util.Files.Walk.Filter_Type;
 ...
 Filter.Exclude ("*.o");
 Filter.Exclude ("/alire/");
 Filter.Include ("/docs/*");
```

The `Match` function looks in the filter for a match.  The path could be
included, excluded or not found.  For example, the following paths will
match:

| Operation                    | Result         |
| ---------------------------- | -------------- |
| Filter.Match ("test.o")      | Walk.Excluded  |
| Filter.Match ("test.a")      | Walk.Not_Found |
| Filter.Match ("docs/test.o") | Walk.Included  |
| Filter.Match ("alire/")      | Walk.Included  |
| Filter.Match ("test/alire")  | Walk.Not_Found |

To scan a directory tree, the `Walker_Type` must have some of its operations
overriden:

* The `Scan_File` should be overriden to be notified when a file is found
  and handle it.
* The `Scan_Directory` should be overriden to be notified when a directory
  is entered.
* The `Get_Ignore_Path` is called when entering a new directory.  It can
  be overriden to indicate a path of a file which contains some patterns
  to be ignored (ex: the `.gitignore` file).
