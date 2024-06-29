-----------------------------------------------------------------------
--  util-strings-transforms -- Various Text Transformation Utilities
--  Copyright (C) 2015, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Strings.Transforms is

   --  ------------------------------
   --  Escape the content into the result stream using the JavaScript
   --  escape rules.
   --  ------------------------------
   function Escape_Javascript (Content : String) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      TR.Escape_Java_Script (Content, Result);
      return Ada.Strings.Unbounded.To_String (Result);
   end Escape_Javascript;

   --  ------------------------------
   --  Escape the content into the result stream using the Java
   --  escape rules.
   --  ------------------------------
   function Escape_Java (Content : String) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      TR.Escape_Java (Content, Result);
      return Ada.Strings.Unbounded.To_String (Result);
   end Escape_Java;

   --  ------------------------------
   --  Escape the content into the result stream using the XML
   --  escape rules:
   --   '<' -> '&lt;'
   --   '>' -> '&gt;'
   --   ''' -> '&apos;'
   --   '&' -> '&amp;'
   --       -> '&#nnn;' if Character'Pos >= 128
   --  ------------------------------
   function Escape_Xml (Content : String) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      TR.Escape_Xml (Content, Result);
      return Ada.Strings.Unbounded.To_String (Result);
   end Escape_Xml;

end Util.Strings.Transforms;
