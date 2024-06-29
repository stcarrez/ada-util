-----------------------------------------------------------------------
--  util-beans-ranges -- Interface Definition with Getter and Setters
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Objects;
with Util.Beans.Basic.Ranges;
package Util.Beans.Ranges is

   package Integer_Ranges is new Util.Beans.Basic.Ranges (Integer, Util.Beans.Objects.To_Object);

end Util.Beans.Ranges;
