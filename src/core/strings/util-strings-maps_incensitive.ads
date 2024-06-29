-----------------------------------------------------------------------
--  util-strings-maps_incensitive --  Map of strings incensitive keys
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Containers.Indefinite_Hashed_Maps;

--  The <b>Util.Strings.Maps</b> package provides an instantiation
--  of a hashed map with Strings.
package Util.Strings.Maps_Incensitive is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
