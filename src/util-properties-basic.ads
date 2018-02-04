-----------------------------------------------------------------------
--  util-properties-basic -- Basic types properties
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2018 Stephane Carrez
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
with Util.Properties.Discrete;
package Util.Properties.Basic is

   pragma Elaborate_Body;

   --  Get/Set boolean properties.
   package Boolean_Property is new Util.Properties.Discrete (Boolean);

   --  Get/Set integer properties.
   package Integer_Property is new Util.Properties.Discrete (Integer);

end Util.Properties.Basic;
