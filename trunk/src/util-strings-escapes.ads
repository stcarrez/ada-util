-----------------------------------------------------------------------
--  Util-strings -- Various String Utility
--  Copyright (C) 2001, 2002, 2003, 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
package Util.Strings.Escapes is

   use Ada.Strings.Unbounded;

   package Escape_String is
     new Escape (Stream => Unbounded_String,
                 Char   => Character,
                 Input  => String,
                 Put    => Ada.Strings.Unbounded.Append);

   package Escape_Wide_Wide_String is
     new Escape (Stream => Unbounded_String,
                 Char   => Wide_Wide_Character,
                 Input  => Wide_Wide_String,
                 Put    => Ada.Strings.Unbounded.Append);

   procedure Escape_Javascript (Content : in String;
                                Into    : in out Unbounded_String)
     renames Escape_String.Escape_Java_Script;

   procedure Escape_Javascript (Content : in Wide_Wide_String;
                                Into    : in out Unbounded_String)
                                renames Escape_Wide_Wide_String.Escape_Java_Script;

   procedure Escape_Java (Content : in String;
                          Into    : in out Unbounded_String)
     renames Escape_String.Escape_Java;

   procedure Escape_Java (Content : in Wide_Wide_String;
                          Into    : in out Unbounded_String)
                          renames Escape_Wide_Wide_String.Escape_Java;

end Util.Strings.Escapes;
