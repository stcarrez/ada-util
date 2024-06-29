-----------------------------------------------------------------------
--  util-strings-tokenizers --  Split strings into tokens
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Texts.Tokenizers;

--  The <b>Util.Strings.Tokenizers</b> package provides an instantiation of the text
--  tokenizer.  The package provides operations to easily separate a string into tokens.
package Util.Strings.Tokenizers is new Util.Texts.Tokenizers
  (Char     => Character,
   Input    => String,
   Index    => Util.Strings.Index);
