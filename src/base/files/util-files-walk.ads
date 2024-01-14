-- --------------------------------------------------------------------
--  util-files-walk -- Walk directory trees
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Finalization;
with Ada.Directories;
private with GNAT.Regexp;

--  == Directory tree walk ==
--  It is sometimes necessary to walk a directory tree while taking into
--  account some inclusion or exclusion patterns or more complex ignore lists.
--  The `Util.Files.Walk` package provides a support to walk such directory
--  tree while taking into account some possible ignore lists such as the
--  `.gitignore` file.  The package defines the `Filter_Type` tagged type
--  to represent and control the exclusion or inclusion filters and a second
--  tagged type `Walker_Type` to walk the directory tree.
package Util.Files.Walk is

   package AF renames Ada.Finalization;

   type Filter_Type is limited new AF.Limited_Controlled with private;

   --  Add a new pattern to include files or directories in the walk.
   procedure Include (Filter  : in out Filter_Type;
                      Pattern : String) with
     Pre => Pattern'Length > 0;

   --  Add a new pattern to exclude (ignore) files or directories in the walk.
   procedure Exclude (Filter  : in out Filter_Type;
                      Pattern : String) with
     Pre => Pattern'Length > 0;

   overriding
   procedure Finalize (Filter : in out Filter_Type);

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
                   Path   : String;
                   Filter : Filter_Type'Class) with
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

   type Filter_Context_Type is limited private;

   --  Called when a directory is found during a directory tree walk.
   --  The default implementation checks for a configuration file to ignore
   --  files (a .gitignore) and builds a new filter to scan the sub-tree.
   --  Once the filter is created, it calls Scan_Directory to proceed with
   --  the scan.
   procedure Scan_Subdir (Walker  : in out Walker_Type;
                          Path    : String;
                          Filter  : Filter_Context_Type) with
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

   --  Inclusion and exclusion patterns are represented within a tree where
   --  a node must match a single path component.
   --  * a Wildcard node matches any path component only once,
   --  * a Multi_Wildcard node matches any path component several times,
   --  * the `Name_Pattern_Type` node is used for an exact name match,
   --  * the `Regex_Pattern_Type` node contains a Regexp patern.
   --
   --  With the following ignore lists:
   --
   --  src
   --  bin
   --  lib
   --  obj
   --  src/sys/http
   --  src/sys/os-generated
   --
   --  The pattern tree looks like:
   --
   --  "src" ---[Next]---> "bin" ---[Next]---> "lib" --->[Next]---> "obj"
   --    |
   --   [Child]
   --    |
   --    v
   --  "sys"
   --    |
   --   [Child]
   --    |
   --    v
   --   "http" ---[Next]---> "os-generated"
   type Pattern_Type is tagged;
   type Pattern_Access is access all Pattern_Type'Class;

   type Pattern_Type is tagged limited record
      Child          : Pattern_Access;
      Next           : Pattern_Access;
      Negative       : Boolean := False;
      Dir_Only       : Boolean := False;
      Exclude        : Boolean := True;
      Wildcard       : Boolean := False;
      Multi_Wildcard : Boolean := False;
   end record;

   function Is_Matched (Pattern : Pattern_Type;
                        Name    : String) return Boolean is (Pattern.Wildcard);

   type Name_Pattern_Type (Len : Positive) is new Pattern_Type with record
      Name : String (1 .. Len);
   end record;

   overriding
   function Is_Matched (Pattern : Name_Pattern_Type;
                        Name    : String)
                        return Boolean is (Pattern.Name = Name);

   type Regex_Pattern_Type is new Pattern_Type with record
      Regex : GNAT.Regexp.Regexp;
   end record;

   overriding
   function Is_Matched (Pattern : Regex_Pattern_Type;
                        Name    : String)
                        return Boolean is (GNAT.Regexp.Match (Name,
                                           Pattern.Regex));

   type Filter_Info_Type is limited record
      Previous        : access Filter_Info_Type;
      Recursive       : Pattern_Access;
      Current         : Pattern_Access;
      Local           : Pattern_Access;
      Local_Recursive : Pattern_Access;
   end record;

   function Match (Filter : Filter_Info_Type;
                   Name   : String) return Pattern_Access;
   function Match (Pattern : Pattern_Access;
                   Name    : String) return Pattern_Access;
   function Match_Sibling (Filter : access Filter_Info_Type;
                           Name   : String) return Pattern_Access;

   type Filter_Context_Type is limited record
      Filter  : access Filter_Info_Type;
      Pattern : Pattern_Access;
   end record;

   type Filter_Type is limited new AF.Limited_Controlled with record
      Root      : Pattern_Access;
      Recursive : Pattern_Access;
   end record;

   type Walker_Type is limited new AF.Limited_Controlled with null record;

end Util.Files.Walk;
