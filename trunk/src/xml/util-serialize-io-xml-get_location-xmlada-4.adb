-----------------------------------------------------------------------
--  util-serialize-io-xml-get_location -- Get_Location for XML Ada 4.1
--  Copyright (C) 2011 Stephane Carrez
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

with Sax.Exceptions;
with Sax.Locators;

separate (Util.Serialize.IO.XML)

--  ------------------------------
--  Return the location where the exception was raised.
--  ------------------------------
function Get_Location (Except : Sax.Exceptions.Sax_Parse_Exception'Class)
                       return String is
begin
   return To_String (Sax.Exceptions.Get_Location (Except));
end Get_Location;
