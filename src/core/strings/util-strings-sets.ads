-----------------------------------------------------------------------
--  util-strings-sets --  Set of strings
--  Copyright (C) 2011, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Sets;

--  The <b>Util.Strings.Sets</b> package provides an instantiation
--  of a hashed set with Strings.
package Util.Strings.Sets is new Ada.Containers.Indefinite_Hashed_Sets
  (Element_Type        => String,
   Hash                => Ada.Strings.Hash,
   Equivalent_Elements => "=");
