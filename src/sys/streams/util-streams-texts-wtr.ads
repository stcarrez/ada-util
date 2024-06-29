-----------------------------------------------------------------------
--  util-streams-texts-tr -- Text translation utilities on streams
--  Copyright (C) 2010, 2011, 2012, 2015, 2016, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Texts.Transforms;
with Ada.Wide_Wide_Characters.Handling;
package Util.Streams.Texts.WTR is
  new Util.Texts.Transforms (Stream => Print_Stream'Class,
                             Char   => Wide_Wide_Character,
                             Input  => Wide_Wide_String,
                             Put    => Write_Char,
                             To_Upper => Ada.Wide_Wide_Characters.Handling.To_Upper,
                             To_Lower => Ada.Wide_Wide_Characters.Handling.To_Lower);
