-----------------------------------------------------------------------
--  util-strings-maps --  Map of strings
--  Copyright (C) 2010, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

--  The <b>Util.Strings.Maps</b> package provides an instantiation
--  of a hashed map with Strings.
package Util.Strings.Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
