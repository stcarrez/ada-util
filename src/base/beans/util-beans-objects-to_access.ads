-----------------------------------------------------------------------
--  Util.Beans.Objects.To_Access -- Conversion utility
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  Convert the object to the corresponding access type.
--  Returns null if the object is not a <b>TYPE_BEAN</b> or not of the given bean type.
with Util.Beans.Basic;
generic
   type T is limited new Util.Beans.Basic.Readonly_Bean with private;
   type T_Access is access all T'Class;
function Util.Beans.Objects.To_Access (Value : in Object) return T_Access;
