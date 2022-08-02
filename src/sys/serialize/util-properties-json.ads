-----------------------------------------------------------------------
--  util-properties-json -- read json files into properties
--  Copyright (C) 2013, 2018, 2022 Stephane Carrez
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

--  == Reading JSON property files ==
--  The `Util.Properties.JSON` package provides operations to read a JSON
--  content and put the result in a property manager.  The JSON content is flattened
--  into a set of name/value pairs.  The JSON structure is reflected in the name.
--  Example:
--
--    { "id": "1",                                 id         -> 1
--      "info": { "name": "search",                info.name  -> search
--                "count": "12",                   info.count -> 12
--                "data": { "value": "empty" }},   info.data.value  -> empty
--      "count": 1                                 count      -> 1
--    }
--
--  To get the value of a JSON property, the user can use the flatten name.  For example:
--
--     Value : constant String := Props.Get ("info.data.value");
--
--  The default separator to construct a flatten name is the dot (`.`) but this can be
--  changed easily when loading the JSON file by specifying the desired separator:
--
--     Util.Properties.JSON.Read_JSON (Props, "config.json", "|");
--
--  Then, the property will be fetch by using:
--
--     Value : constant String := Props.Get ("info|data|value");
--
package Util.Properties.JSON is

   --  Parse the JSON content and put the flattened content in the property manager.
   procedure Parse_JSON (Manager : in out Util.Properties.Manager'Class;
                         Content : in String;
                         Flatten_Separator : in String := ".");

   --  Read the JSON file into the property manager.
   --  The JSON content is flatten into Flatten the JSON content and add the properties.
   procedure Read_JSON (Manager : in out Util.Properties.Manager'Class;
                        Path    : in String;
                        Flatten_Separator : in String := ".");

end Util.Properties.JSON;
