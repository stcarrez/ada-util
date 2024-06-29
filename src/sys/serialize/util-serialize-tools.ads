-----------------------------------------------------------------------
--  util-serialize-tools -- Tools to Serialize objects in various formats
--  Copyright (C) 2012, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Beans.Objects.Maps;
with Util.Serialize.IO.JSON;
package Util.Serialize.Tools is

   --  Serialize the objects defined in the object map <b>Map</b> into the <b>Output</b>
   --  JSON stream.  Use the <b>Name</b> as the name of the JSON object.
   procedure To_JSON (Output : in out Util.Serialize.IO.JSON.Output_Stream'Class;
                      Name   : in String;
                      Map    : in Util.Beans.Objects.Maps.Map);

   --  Deserializes the JSON content passed in <b>Content</b> and restore the object map
   --  with their values.  The object map passed in <b>Map</b> can contain existing values.
   --  They will be overridden by the JSON values.
   procedure From_JSON (Content : in String;
                        Map     : in out Util.Beans.Objects.Maps.Map);

   --  Serialize the objects defined in the object map <b>Map</b> into an JSON stream.
   --  Returns the JSON string that contains a serialization of the object maps.
   function To_JSON (Map : in Util.Beans.Objects.Maps.Map) return String;

   --  Deserializes the XML content passed in <b>Content</b> and restore the object map
   --  with their values.
   --  Returns the object map that was restored.
   function From_JSON (Content : in String) return Util.Beans.Objects.Maps.Map;

end Util.Serialize.Tools;
