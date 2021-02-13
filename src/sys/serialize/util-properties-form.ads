-----------------------------------------------------------------------
--  util-properties-form -- read application/form content into properties
--  Copyright (C) 2020 Stephane Carrez
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

package Util.Properties.Form is

   --  Parse the application/form content and put the flattened content in the property manager.
   procedure Parse_Form (Manager : in out Util.Properties.Manager'Class;
                         Content : in String;
                         Flatten_Separator : in String := ".");

end Util.Properties.Form;
