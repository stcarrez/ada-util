-----------------------------------------------------------------------
--  Util.Beans.Objects.To_Access -- Conversion utility
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  ------------------------------
--  Convert the object to the corresponding access type.
--  Returns null if the object is not a <b>TYPE_BEAN</b> or not of the given bean type.
--  ------------------------------
function Util.Beans.Objects.To_Access (Value : in Object) return T_Access is
begin
   if Value.V.Of_Type /= TYPE_BEAN then
      return null;
   end if;
   if Value.V.Proxy = null then
      return null;
   end if;
   if Bean_Proxy (Value.V.Proxy.all).Bean = null then
      return null;
   end if;
   if not (Bean_Proxy (Value.V.Proxy.all).Bean.all in T'Class) then
      return null;
   end if;
   return T'Class (Bean_Proxy (Value.V.Proxy.all).Bean.all)'Unchecked_Access;
end Util.Beans.Objects.To_Access;
