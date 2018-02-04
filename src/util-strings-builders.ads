-----------------------------------------------------------------------
--  util-strings-builders --  Set of strings
--  Copyright (C) 2013, 2018 Stephane Carrez
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
with Util.Texts.Builders;

--  The <b>Util.Strings.Builders</b> package provides an instantiation
--  of a text builders for <tt>Character</tt> and <tt>String</tt> types.
package Util.Strings.Builders is new Util.Texts.Builders
  (Element_Type        => Character,
   Input               => String);
