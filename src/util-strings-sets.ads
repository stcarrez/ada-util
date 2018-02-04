-----------------------------------------------------------------------
--  util-strings-sets --  Set of strings
--  Copyright (C) 2011, 2018 Stephane Carrez
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
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Sets;

--  The <b>Util.Strings.Sets</b> package provides an instantiation
--  of a hashed set with Strings.
package Util.Strings.Sets is new Ada.Containers.Indefinite_Hashed_Sets
  (Element_Type        => String,
   Hash                => Ada.Strings.Hash,
   Equivalent_Elements => "=");
