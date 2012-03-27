-----------------------------------------------------------------------
--  Util.Beans.Objects.Maps -- Object maps
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
package Util.Beans.Objects.Maps is
  new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                             Element_Type    => Object,
                                             Hash            => Ada.Strings.Hash,
                                             Equivalent_Keys => "=");
