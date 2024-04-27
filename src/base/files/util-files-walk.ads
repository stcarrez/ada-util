-- --------------------------------------------------------------------
--  util-files-walk -- Walk directory trees
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Finalization;
with Ada.Directories;
with Util.Files.Filters;

--  == Directory tree walk ==
--  It is sometimes necessary to walk a directory tree while taking into
--  account some inclusion or exclusion patterns or more complex ignore lists.
--  The `Util.Files.Walk` package provides a support to walk such directory
--  tree while taking into account some possible ignore lists such as the
--  `.gitignore` file.  The package defines the `Filter_Type` tagged type
--  to represent and control the exclusion or inclusion filters and a second
--  tagged type `Walker_Type` to walk the directory tree.
--
--  The `Filter_Type` provides two operations to add patterns in the filter
--  and one operation to check against a path whether it matches a pattern.
--  A pattern can contain fixed paths, wildcards or regular expressions.
--  Similar to `.gitignore` rules, a pattern which starts with a `/` will
--  define a pattern that must match the complete path.  Otherwise, the pattern
--  is a recursive pattern.  Example of pattern setup:
--
--     Filter : Util.Files.Walk.Filter_Type;
--     ...
--     Filter.Exclude ("*.o");
--     Filter.Exclude ("/alire/");
--     Filter.Include ("/docs/*");
--
--  The `Match` function looks in the filter for a match.  The path could be
--  included, excluded or not found.  For example, the following paths will
--  match:
--
--  | Operation                    | Result         |
--  | ---------------------------- | -------------- |
--  | Filter.Match ("test.o")      | Walk.Excluded  |
--  | Filter.Match ("test.a")      | Walk.Not_Found |
--  | Filter.Match ("docs/test.o") | Walk.Included  |
--  | Filter.Match ("alire/")      | Walk.Included  |
--  | Filter.Match ("test/alire")  | Walk.Not_Found |
--
--  To scan a directory tree, the `Walker_Type` must have some of its operations
--  overriden:
--
--  * The `Scan_File` should be overriden to be notified when a file is found
--    and handle it.
--  * The `Scan_Directory` should be overriden to be notified when a directory
--    is entered.
--  * The `Get_Ignore_Path` is called when entering a new directory.  It can
--    be overriden to indicate a path of a file which contains some patterns
--    to be ignored (ex: the `.gitignore` file).
package Util.Files.Walk is

   package AF renames Ada.Finalization;

   type Filter_Mode is (Not_Found, Included, Excluded);

   package Path_Filter is
      new Util.Files.Filters (Filter_Mode);

   type Filter_Type is limited new AF.Limited_Controlled with private;

   --  Add a new pattern to include files or directories in the walk.
   procedure Include (Filter  : in out Filter_Type;
                      Pattern : in String) with
     Pre => Pattern'Length > 0;

   --  Add a new pattern to exclude (ignore) files or directories in the walk.
   procedure Exclude (Filter  : in out Filter_Type;
                      Pattern : in String) with
     Pre => Pattern'Length > 0;

   --  Check if a path matches the included or excluded patterns.
   function Match (Filter : in Filter_Type;
                   Path   : in String) return Filter_Mode;

   --  Load a series of lines that contains a list of files to ignore.
   --  The `Reader` procedure is called with a `Process` procedure that is
   --  expected to be called for each line which comes from the ignore
   --  file (such as the .gitignore file).  The `Process` procedure handles
   --  the interpretation of ignore patterns as defined by `.gitignore`
   --  and it updates the `Filter` accordingly.
   procedure Load_Ignore (Filter : in out Filter_Type'Class;
                          Label  : in String;
                          Reader : not null access
                              procedure (Process : not null access
                                   procedure (Line : in String)));
   procedure Load_Ignore (Filter : in out Filter_Type'Class;
                          Path   : String) with
      Pre => Path'Length > 0 and then Ada.Directories.Exists (Path);

   type Walker_Type is limited new AF.Limited_Controlled with private;

   --  Scan the directory tree given by the path for files and sub-directories
   --  matching the filters:
   --  * it calls `Get_Ignore_Path` to get an optional path of files to read
   --    for patterns to ignore,
   --  * if that file exist, it calls `Load_Ignore` in a local filter and
   --    loads the patterns in the filter.
   --  * it scans the directory for files and sub-directories matching the
   --    filters (either defined by the loaded ignored files) or by the main
   --    filter.
   --  * when files are found and are not excluded, it calls `Scan_File`,
   --  * when sub-directories are found and are not excluded, it calls
   --    `Scan_Directory`.
   procedure Scan (Walker : in out Walker_Type;
                   Path   : in String;
                   Filter : in Filter_Type'Class) with
     Pre => Path'Length > 0 and then Ada.Directories.Exists (Path);

   --  Get the path of a file that can be read to get a list of files to ignore
   --  in the given directory (ie, .gitignore).
   function Get_Ignore_Path (Walker : Walker_Type;
                             Path   : String) return String is ("");

   --  Load the file that contains a list of files to ignore.  The default
   --  implementation reads patterns as defined in `.gitignore` files.
   procedure Load_Ignore (Walker : in out Walker_Type;
                          Path   : String;
                          Filter : in out Filter_Type'Class) with
     Pre => Path'Length > 0 and then Ada.Directories.Exists (Path);

   --  Called when a file is found during the directory tree walk.
   procedure Scan_File (Walker : in out Walker_Type;
                        Path   : String) is null with
     Pre'Class => Path'Length > 0 and then Ada.Directories.Exists (Path);

   use Path_Filter;

   subtype Filter_Result is Path_Filter.Filter_Result;
   subtype Filter_Context_Type is Path_Filter.Filter_Context_Type;

   --  Called when a directory is found during a directory tree walk.
   --  The default implementation checks for a configuration file to ignore
   --  files (a .gitignore) and builds a new filter to scan the sub-tree.
   --  Once the filter is created, it calls Scan_Directory to proceed with
   --  the scan.
   procedure Scan_Subdir (Walker : in out Walker_Type;
                          Path   : in String;
                          Filter : in Filter_Context_Type;
                          Match  : in Filter_Result) with
     Pre => Path'Length > 0 and then Ada.Directories.Exists (Path);

   --  Called by Scan_Subdir when a directory was found.
   --  The default implementation scans the directory for files and directories
   --  matching the filter.  It can be overriden to implement specific
   --  behaviors.
   procedure Scan_Directory (Walker : in out Walker_Type;
                             Path   : String;
                             Filter : Filter_Context_Type) with
     Pre => Path'Length > 0 and then Ada.Directories.Exists (Path);

private

   type Filter_Type is limited new Path_Filter.Filter_Type with null record;

   type Walker_Type is limited new AF.Limited_Controlled with null record;

end Util.Files.Walk;
