-----------------------------------------------------------------------
--  util-streams-texts-tr -- Text translation utilities on streams
--  Copyright (C) 2010, 2011, 2012, 2015, 2016, 2017 Stephane Carrez
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
with Util.Texts.Transforms;
with Ada.Wide_Wide_Characters.Handling;
package Util.Streams.Texts.WTR is
  new Util.Texts.Transforms (Stream => Print_Stream'Class,
                             Char   => Wide_Wide_Character,
                             Input  => Wide_Wide_String,
                             Put    => Write_Char,
                             To_Upper => Ada.Wide_Wide_Characters.Handling.To_Upper,
                             To_Lower => Ada.Wide_Wide_Characters.Handling.To_Lower);
