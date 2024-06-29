-----------------------------------------------------------------------
--  util-log-loggers-traceback-gnat -- GNAT symbolic traceback
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with GNAT.Traceback.Symbolic;

separate (Util.Log.Loggers)

--  ------------------------------
--  Return a printable traceback that correspond to the exception.
--  ------------------------------
function Traceback (E : in Exception_Occurrence) return String is
begin
   return GNAT.Traceback.Symbolic.Symbolic_Traceback (E);
end Traceback;
