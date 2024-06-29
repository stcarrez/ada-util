-----------------------------------------------------------------------
--  util-serialize-io-xml-get_location -- Get_Location for XML Ada 3.2
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Sax.Exceptions;
with Sax.Locators;

separate (Util.Serialize.IO.XML)

--  ------------------------------
--  Return the location where the exception was raised.
--  ------------------------------
function Get_Location (Except : Sax.Exceptions.Sax_Parse_Exception'Class)
                      return String is
begin
   return To_String (Sax.Exceptions.Get_Locator (Except));
end Get_Location;
