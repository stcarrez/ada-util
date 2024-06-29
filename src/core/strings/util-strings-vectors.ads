-----------------------------------------------------------------------
--  util-strings-vectors --  Vector of strings
--  Copyright (C) 2011, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Containers.Indefinite_Vectors;

--  The <b>Util.Strings.Vectors</b> package provides an instantiation
--  of a vector container with Strings.
package Util.Strings.Vectors is new Ada.Containers.Indefinite_Vectors
  (Element_Type => String,
   Index_Type   => Positive);
