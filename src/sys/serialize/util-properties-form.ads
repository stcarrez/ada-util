-----------------------------------------------------------------------
--  util-properties-form -- read application/form content into properties
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package Util.Properties.Form is

   --  Parse the application/form content and put the flattened content in the property manager.
   procedure Parse_Form (Manager : in out Util.Properties.Manager'Class;
                         Content : in String;
                         Flatten_Separator : in String := ".");

end Util.Properties.Form;
