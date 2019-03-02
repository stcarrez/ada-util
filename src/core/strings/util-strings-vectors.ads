-----------------------------------------------------------------------
--  util-strings-vectors --  Vector of strings
--  Copyright (C) 2011, 2017 Stephane Carrez
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
with Ada.Containers.Indefinite_Vectors;

--  The <b>Util.Strings.Vectors</b> package provides an instantiation
--  of a vector container with Strings.
package Util.Strings.Vectors is new Ada.Containers.Indefinite_Vectors
  (Element_Type => String,
   Index_Type   => Positive);
