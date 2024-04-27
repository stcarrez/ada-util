-- --------------------------------------------------------------------
--  util-files-filters -- Path filters
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Finalization;
private with GNAT.Regexp;

--  == Path filters ==
--  The generic package `Util.Files.Filters` defines the `Filter_Type` tagged
--  type to represent and control a set of path patterns associated with values
--  represented by the `Element_Type` generic type.  The package must be
--  instantiated with the type representing values associated to path patterns.
--
--  The `Filter_Type` provides one operation to add a pattern in the filter
--  and associate it with a value.  A pattern can contain fixed paths, wildcards
--  or regular expressions.
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
generic
   type Element_Type (<>) is private;
package Util.Files.Filters is

   package AF renames Ada.Finalization;

   type Match_Result is (Not_Found, No_Value, Found);

   type Filter_Result (Match : Match_Result) is limited private;

   function Get_Value (Result : in Filter_Result) return Element_Type
     with Pre => Result.Match = Found;

   function Is_Only_Directory (Result : in Filter_Result) return Boolean
     with Pre => Result.Match in No_Value | Found;

   NO_MATCH : constant Filter_Result;

   type Filter_Type is limited new AF.Limited_Controlled with private;

   --  Add a new pattern and associate it with the given value.
   procedure Insert (Filter    : in out Filter_Type;
                     Pattern   : in String;
                     Recursive : in Boolean;
                     Value     : in Element_Type) with
     Pre => Pattern'Length > 0;

   --  Check if a path matches the included or excluded patterns.
   function Match (Filter : in Filter_Type;
                   Path   : in String) return Filter_Result;

   overriding
   procedure Finalize (Filter : in out Filter_Type);

   type Filter_Context_Type is limited private;

   function Create (Filter : in Filter_Type'Class) return Filter_Context_Type;

   function Create (Filter  : in Filter_Type'Class;
                    Context : in Filter_Context_Type) return Filter_Context_Type;

   function Create (Context : in Filter_Context_Type;
                    Match   : in Filter_Result) return Filter_Context_Type;

   --  Check if a path matches the included or excluded patterns.
   function Match (Filter : in Filter_Context_Type;
                   Path   : in String) return Filter_Result;

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
   --  src -> V1
   --  bin -> V2
   --  lib -> V3
   --  obj -> V4
   --  src/sys/http -> V5
   --  src/sys/os-generated -> V6
   --
   --  The pattern tree looks like:
   --
   --  "src" {V1} --[Next]--> "bin" {V2} --[Next]--> "lib" {V3} -->[Next]--> "obj" {V4}
   --    |
   --   [Child]
   --    |
   --    v
   --  "sys"
   --    |
   --   [Child]
   --    |
   --    v
   --   "http" {V5} --[Next]--> "os-generated" {V6}
   type Pattern_Type is tagged;
   type Pattern_Access is access all Pattern_Type'Class;
   type Element_Access is access all Element_Type;

   type Pattern_Type is tagged limited record
      Child          : Pattern_Access;
      Next           : Pattern_Access;
      Negative       : Boolean := False;
      Dir_Only       : Boolean := False;
      Exclude        : Boolean := True;
      Wildcard       : Boolean := False;
      Multi_Wildcard : Boolean := False;
      Value          : Element_Access;
   end record;

   function Is_Matched (Pattern : Pattern_Type;
                        Name    : String) return Boolean is (Pattern.Wildcard);

   function Is_Pattern (Pattern : Pattern_Type;
                        Name    : String) return Boolean is (False);

   function Find_Pattern (Node : in Pattern_Access;
                          Name : in String) return Pattern_Access;

   type Name_Pattern_Type (Len : Natural) is new Pattern_Type with record
      Name : String (1 .. Len);
   end record;

   overriding
   function Is_Matched (Pattern : Name_Pattern_Type;
                        Name    : String)
                        return Boolean is (Pattern.Name = Name);

   overriding
   function Is_Pattern (Pattern : Name_Pattern_Type;
                        Name    : String) return Boolean is (Pattern.Name = Name);

   type Regex_Pattern_Type is new Pattern_Type with record
      Regex : GNAT.Regexp.Regexp;
   end record;

   overriding
   function Is_Matched (Pattern : Regex_Pattern_Type;
                        Name    : String)
                        return Boolean is (GNAT.Regexp.Match (Name,
                                           Pattern.Regex));

   type Filter_Info_Type is limited record
      Previous        : access constant Filter_Info_Type;
      Recursive       : Pattern_Access;
      Current         : Pattern_Access;
      Local           : Pattern_Access;
      Local_Recursive : Pattern_Access;
   end record;

   function Match (Filter : Filter_Info_Type;
                   Name   : String) return Pattern_Access;
   function Match (Pattern : Pattern_Access;
                   Name    : String) return Pattern_Access;
   function Match_Sibling (Filter : access constant Filter_Info_Type;
                           Name   : String) return Pattern_Access;

   type Filter_Context_Type is limited record
      Filter  : aliased Filter_Info_Type;
      Pattern : Pattern_Access;
   end record;

   type Filter_Result (Match : Match_Result) is limited record
      Pattern : Pattern_Access;
   end record;

   type Filter_Type is limited new AF.Limited_Controlled with record
      Root      : Pattern_Access;
      Recursive : Pattern_Access;
   end record;

   procedure Insert (Root    : in out Pattern_Access;
                     Pattern : in String;
                     Value   : in Element_Type);

   NO_MATCH : constant Filter_Result := Filter_Result '(Match   => Not_Found,
                                                        Pattern => null);

end Util.Files.Filters;
