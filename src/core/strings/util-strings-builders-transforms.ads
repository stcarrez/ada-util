-----------------------------------------------------------------------
--  util-strings-builders-transforms --  String transformations on Builder types
--  Copyright (C) 2015 Stephane Carrez
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
with Ada.Characters.Handling;
with Util.Texts.Transforms;

--  The <b>Util.Strings.Builders.Transforms</b> package provides an instantiation
--  of the text transformation operations for the Builder type.
package Util.Strings.Builders.Transforms is
  new Util.Texts.Transforms (Stream => Builder,
                             Char   => Character,
                             Input  => String,
                             Put    => Append,
                             To_Upper => Ada.Characters.Handling.To_Upper,
                             To_Lower => Ada.Characters.Handling.To_Lower);
