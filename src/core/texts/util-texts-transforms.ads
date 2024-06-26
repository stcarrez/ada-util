-----------------------------------------------------------------------
--  util-texts-transforms -- Various Text Transformation Utilities
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2015, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
generic
   type Stream (<>) is limited private;
   type Char is (<>);
   type Input is array (Positive range <>) of Char;
   with procedure Put (Buffer : in out Stream; C : in Character);
   with function To_Upper (C : Char) return Char;
   with function To_Lower (C : Char) return Char;
package Util.Texts.Transforms is

   pragma Preelaborate;

   --  Capitalize the string into the result stream.
   procedure Capitalize (Content : in Input;
                         Into    : in out Stream);

   --  Capitalize the string
   function Capitalize (Content : Input) return Input;

   --  Translate the input string into an upper case string in the result stream.
   procedure To_Upper_Case (Content : in Input;
                            Into    : in out Stream);

   --  Translate the input string into an upper case string.
   function To_Upper_Case (Content : Input) return Input;

   --  Translate the input string into a lower case string in the result stream.
   procedure To_Lower_Case (Content : in Input;
                            Into    : in out Stream);
   function To_Lower_Case (Content : Input) return Input;

   --  Write in the output stream the value as a \uNNNN encoding form.
   procedure To_Hex (Into  : in out Stream;
                     Value : in Char);
   pragma Inline_Always (To_Hex);

   --  Escape the content into the result stream using the JavaScript
   --  escape rules.
   procedure Escape_Java_Script (Content : in Input;
                                 Into    : in out Stream);

   --  Escape the content into the result stream using the Java
   --  escape rules.
   procedure Escape_Java (Content : in Input;
                          Into    : in out Stream);

   --  Escape the content into the result stream using the XML
   --  escape rules:
   --   '<' -> '&lt;'
   --   '>' -> '&gt;'
   --   ''' -> '&apos;'
   --   '&' -> '&amp;'
   --       -> '&#nnn;' if Character'Pos >= 128
   procedure Escape_Xml (Content : in Input;
                         Into    : in out Stream);

   --  Translate the XML entity represented by <tt>Entity</tt> into an UTF-8 sequence
   --  in the output stream.
   procedure Translate_Xml_Entity (Entity : in Input;
                                   Into   : in out Stream);

   --  Unescape the XML entities from the content into the result stream.
   --  For each XML entity found, call the <tt>Translator</tt> procedure who is responsible
   --  for writing the result in the stream.  The XML entity starts with '&' and ends with ';'.
   --  The '&' and ';' are part of the entity when given to the translator.  If the trailing
   --  ';' is not part of the entity, it means the entity was truncated or the end of input
   --  stream is reached.
   procedure Unescape_Xml (Content    : in Input;
                           Translator : not null access
                             procedure (Entity : in Input;
                                        Into   : in out Stream) := Translate_Xml_Entity'Access;
                           Into       : in out Stream);

private
   procedure Put (Into  : in out Stream;
                  Value : in String);

   procedure Escape_Java (Content             : in Input;
                          Escape_Single_Quote : in Boolean;
                          Into                : in out Stream);
end Util.Texts.Transforms;
