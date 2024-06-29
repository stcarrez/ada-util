-----------------------------------------------------------------------
--  util-log-loggers-traceback-none -- Dummy traceback
--  Copyright (C) 2012, 2013, 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

separate (Util.Log.Loggers)

--  ------------------------------
--  Return a (dummy) printable traceback that correspond to the exception.
--  ------------------------------
function Traceback (E : in Exception_Occurrence) return String is
   pragma Unreferenced (E);
begin
   return "";
end Traceback;
