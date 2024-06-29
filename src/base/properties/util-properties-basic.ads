-----------------------------------------------------------------------
--  util-properties-basic -- Basic types properties
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Properties.Discrete;
package Util.Properties.Basic is

   pragma Elaborate_Body;

   --  Get/Set boolean properties.
   package Boolean_Property is new Util.Properties.Discrete (Boolean);

   --  Get/Set integer properties.
   package Integer_Property is new Util.Properties.Discrete (Integer);

end Util.Properties.Basic;
