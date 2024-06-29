-----------------------------------------------------------------------
--  util-strings-builders --  Set of strings
--  Copyright (C) 2013, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Texts.Builders;

--  The <b>Util.Strings.Builders</b> package provides an instantiation
--  of a text builders for <tt>Character</tt> and <tt>String</tt> types.
package Util.Strings.Builders is new Util.Texts.Builders
  (Element_Type        => Character,
   Input               => String);
