-----------------------------------------------------------------------
--  util-processes-os__ptsname_r.adb -- Ptsname using ptsname_r(3)
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

separate (Util.Processes.Os) function Ptsname (Fd     : in File_Type;
                                               Buf    : in Ptr;
                                               Buflen : in Size_T) return Integer is
begin
   return Sys_Ptsname_r (Fd, Buf, Buflen);
end Ptsname;
