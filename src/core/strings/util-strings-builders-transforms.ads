-----------------------------------------------------------------------
--  util-strings-builders-transforms --  String transformations on Builder types
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
