-----------------------------------------------------------------------
--  util-processes-os__ptsname_r.adb -- Ptsname using ptsname_r(3)
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

separate (Util.Processes.Os) function Ptsname (Fd     : in File_Type;
                                               Buf    : out Ptr) return Integer is
   Name : constant Interfaces.C.char_array (1 .. 64)
        := (64 => Interfaces.C.nul, others => ' ');
begin
   Buf := Interfaces.C.Strings.New_Char_Array (Name);
   return Sys_Ptsname_R (Fd, Buf, 64);
end Ptsname;
