-----------------------------------------------------------------------
--  util-serialize-tools -- Tools to Serialize objects in various formats
--  Copyright (C) 2012 Stephane Carrez
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
with Util.Beans.Objects.Maps;
package Util.Serialize.Tools is

   --  Serialize the objects defined in the object map <b>Map</b> into an JSON stream.
   --  Returns the JSON string that contains a serialization of the object maps.
   function To_JSON (Map : in Util.Beans.Objects.Maps.Map) return String;

   --  Deserializes the XML content passed in <b>Content</b> and restore the object map
   --  which their values.
   --  Returns the object map that was restored.
   function From_JSON (Content : in String) return Util.Beans.Objects.Maps.Map;

end Util.Serialize.Tools;
