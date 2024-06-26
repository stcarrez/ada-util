-----------------------------------------------------------------------
--  util-strings-transforms -- Various Text Transformation Utilities
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2015, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Texts.Transforms;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
package Util.Strings.Transforms is

   pragma Preelaborate;

   use Ada.Strings.Unbounded;

   package TR is
     new Util.Texts.Transforms (Stream => Unbounded_String,
                                Char   => Character,
                                Input  => String,
                                Put    => Ada.Strings.Unbounded.Append,
                                To_Upper => Ada.Characters.Handling.To_Upper,
                                To_Lower => Ada.Characters.Handling.To_Lower);

   --  Capitalize the string into the result stream.
   procedure Capitalize (Content : in String;
                         Into    : in out Unbounded_String)
                         renames TR.Capitalize;
   function Capitalize (Content : String) return String
                        renames TR.Capitalize;

   --  Translate the input string into an upper case string in the result stream.
   procedure To_Upper_Case (Content : in String;
                            Into    : in out Unbounded_String)
                            renames TR.To_Upper_Case;
   function To_Upper_Case (Content : String) return String
                           renames TR.To_Upper_Case;

   --  Translate the input string into a lower case string in the result stream.
   procedure To_Lower_Case (Content : in String;
                            Into    : in out Unbounded_String)
                         renames TR.To_Lower_Case;
   function To_Lower_Case (Content : String) return String
                           renames TR.To_Lower_Case;

   --  Write in the output stream the value as a \uNNNN encoding form.
   procedure To_Hex (Into  : in out Unbounded_String;
                     Value : in Character) renames TR.To_Hex;

   --  Escape the content into the result stream using the JavaScript
   --  escape rules.
   procedure Escape_Javascript (Content : in String;
                                Into    : in out Unbounded_String)
                                renames TR.Escape_Java_Script;
   function Escape_Javascript (Content : String) return String;

   --  Escape the content into the result stream using the Java
   --  escape rules.
   procedure Escape_Java (Content : in String;
                          Into    : in out Unbounded_String)
                          renames TR.Escape_Java;
   function Escape_Java (Content : String) return String;

   --  Escape the content into the result stream using the XML
   --  escape rules:
   --   '<' -> '&lt;'
   --   '>' -> '&gt;'
   --   ''' -> '&apos;'
   --   '&' -> '&amp;'
   --       -> '&#nnn;' if Character'Pos >= 128
   procedure Escape_Xml (Content : in String;
                         Into    : in out Unbounded_String)
                          renames TR.Escape_Xml;

   function Escape_Xml (Content : String) return String;

   procedure Translate_Xml_Entity (Entity : in String;
                                   Into   : in out Unbounded_String)
                                   renames TR.Translate_Xml_Entity;

   procedure Unescape_Xml (Content    : in String;
                           Translator : not null access
                             procedure (Entity : in String;
                                        Into   : in out Unbounded_String)
                           := Translate_Xml_Entity'Access;
                           Into       : in out Unbounded_String)
   renames TR.Unescape_Xml;

end Util.Strings.Transforms;
