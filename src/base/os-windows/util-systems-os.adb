-----------------------------------------------------------------------
--  util-system-os -- Windows system operations
--  Copyright (C) 2011, 2012, 2015, 2018, 2019, 2022 Stephane Carrez
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
with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Wide_Strings;
package body Util.Systems.Os is

   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type Interfaces.C.size_t;

   function To_WSTR (Value : in String) return Wchar_Ptr is
      Result : constant Wchar_Ptr := new Interfaces.C.wchar_array (0 .. Value'Length + 1);
      Pos    : Interfaces.C.size_t := 0;
   begin
      for C of Value loop
         Result (Pos)
           := Interfaces.C.To_C (Ada.Characters.Conversions.To_Wide_Character (C));
         Pos := Pos + 1;
      end loop;
      Result (Pos) := Interfaces.C.wide_nul;
      return Result;
   end To_WSTR;

   function Sys_SetFilePointerEx (Fs : in File_Type;
                                  Offset : in Util.Systems.Types.off_t;
                                  Result : access Util.Systems.Types.off_t;
                                  Mode   : in Util.Systems.Types.Seek_Mode) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "SetFilePointerEx";

   function Sys_Lseek (Fs : in File_Type;
                       Offset : in Util.Systems.Types.off_t;
                       Mode   : in Util.Systems.Types.Seek_Mode)
                       return Util.Systems.Types.off_t is
      Result : aliased Util.Systems.Types.off_t;
   begin
      if Sys_SetFilePointerEx (Fs, Offset, Result'Access, Mode) /= 0 then
         return Result;
      else
         return -1;
      end if;
   end Sys_Lseek;

   function Sys_GetFileSizeEx (Fs : in File_Type;
                               Result : access Util.Systems.Types.off_t) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "GetFileSizeEx";

   function Sys_GetFileTime (Fs : in File_Type;
                             Create     : access FileTime;
                             AccessTime : access FileTime;
                             ModifyTime : access FileTime) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "GetFileTime";

   TICKS_PER_SECOND : constant := 10000000;
   EPOCH_DIFFERENCE : constant := 11644473600;

   function To_Time (Time : in FileTime) return Types.Time_Type is
      Value : Interfaces.Unsigned_64;
   begin
      Value := Interfaces.Shift_Left (Interfaces.Unsigned_64 (Time.dwHighDateTime), 32);
      Value := Value + Interfaces.Unsigned_64 (Time.dwLowDateTime);
      Value := Value / TICKS_PER_SECOND;
      Value := Value - EPOCH_DIFFERENCE;
      return Types.Time_Type (Value);
   end To_Time;

   function Sys_Fstat (Fs : in File_Type;
                       Stat : access Util.Systems.Types.Stat_Type) return Integer is
      Size          : aliased Util.Systems.Types.off_t;
      Creation_Time : aliased FileTime;
      Access_Time   : aliased FileTime;
      Write_Time    : aliased FileTime;
   begin
      Stat.st_dev := 0;
      Stat.st_ino := 0;
      Stat.st_mode := 0;
      Stat.st_nlink := 0;
      Stat.st_uid := 0;
      Stat.st_gid := 0;
      Stat.st_rdev := 0;
      Stat.st_atime := 0;
      Stat.st_mtime := 0;
      Stat.st_ctime := 0;
      if Sys_GetFileSizeEx (Fs, Size'Access) = 0 then
         return -1;
      end if;
      if Sys_GetFileTime (Fs, Creation_Time'Access, Access_Time'Access, Write_Time'Access) = 0 then
         return -1;
      end if;
      Stat.st_size := Size;
      Stat.st_ctime := To_Time (Creation_Time);
      Stat.st_mtime := To_Time (Write_Time);
      Stat.st_atime := To_Time (Access_Time);
      return 0;
   end Sys_Fstat;

   --  Open a file
   function Sys_Open (Path  : in Ptr;
                      Flags : in Interfaces.C.int;
                      Mode  : in Util.Systems.Types.mode_t) return File_Type is
      pragma Unreferenced (Mode);

      function Has_Flag (M : in Interfaces.C.int;
                         F : in Interfaces.C.int) return Boolean
        is ((Interfaces.Unsigned_32 (M) and Interfaces.Unsigned_32 (F)) /= 0);

      Sec            : aliased Security_Attributes;
      Result         : File_Type;
      Desired_Access : DWORD;
      Share_Mode     : DWORD := FILE_SHARE_READ + FILE_SHARE_WRITE;
      Creation       : DWORD;
      WPath          : Wchar_Ptr;
   begin
      WPath := To_WSTR (Interfaces.C.Strings.Value (Path));
      Sec.Length := Security_Attributes'Size / 8;
      Sec.Security_Descriptor := System.Null_Address;
      Sec.Inherit := (if Has_Flag (Flags, Util.Systems.Constants.O_CLOEXEC) then 0 else 1);

      if Has_Flag (Flags, O_WRONLY) then
         Desired_Access := GENERIC_WRITE;
      elsif Has_Flag (Flags, O_RDWR) then
         Desired_Access := GENERIC_READ + GENERIC_WRITE;
      else
         Desired_Access := GENERIC_READ;
      end if;
      if Has_Flag (Flags, O_CREAT) then
         if Has_Flag (Flags, O_EXCL) then
            Creation := CREATE_NEW;
         else
            Creation := CREATE_ALWAYS;
         end if;
      else
         Creation := OPEN_EXISTING;
      end if;
      if Has_Flag (Flags, O_APPEND) then
         Desired_Access := FILE_APPEND_DATA;
      end if;
      if Has_Flag (Flags, O_EXCL) then
         Share_Mode := 0;
      end if;
      Result := Create_File (WPath.all'Address,
                             Desired_Access,
                             Share_Mode,
                             Sec'Unchecked_Access,
                             Creation,
                             FILE_ATTRIBUTE_NORMAL,
                             NO_FILE);

      Free (WPath);

      return (if Result = INVALID_HANDLE_VALUE then NO_FILE else Result);
   end Sys_Open;

   function Sys_SetEndOfFile (Fs : in File_Type) return BOOL
     with Import => True, Convention => Stdcall, Link_Name => "SetEndOfFile";

   function Sys_Ftruncate (Fs : in File_Type;
                           Length : in Util.Systems.Types.off_t) return Integer is
   begin
      if Sys_Lseek (Fs, Length, Util.Systems.Types.SEEK_SET) < 0 then
         return -1;
      end if;
      if Sys_SetEndOfFile (Fs) = 0 then
         return -1;
      end if;
      return 0;
   end Sys_Ftruncate;

   function Sys_Fchmod (Fd   : in File_Type;
                        Mode : in Util.Systems.Types.mode_t) return Integer is
      pragma Unreferenced (Fd, Mode);
   begin
      return 0;
   end Sys_Fchmod;

   --  Close a file
   function Sys_Close (Fd : in File_Type) return Integer is
   begin
      if Close_Handle (Fd) = 0 then
         return -1;
      else
         return 0;
      end if;
   end Sys_Close;

   --  ------------------------------
   --  Rename a file (the Ada.Directories.Rename does not allow to use
   --  the Unix atomic file rename!)
   --  ------------------------------
   function Sys_Rename (Oldpath  : in String;
                        Newpath  : in String) return Integer is
      Old_WPath : Wchar_Ptr;
      New_WPath : Wchar_Ptr;
      Result    : BOOL;
   begin
      Old_WPath := To_WSTR (Oldpath (Oldpath'First .. Oldpath'Last - 1));
      New_WPath := To_WSTR (Newpath (Newpath'First .. Newpath'Last - 1));
      Result := Move_File (Old_WPath.all'Address,
                           New_WPath.all'Address,
                           3 + 8);

      Free (Old_WPath);
      Free (New_WPath);

      return (if Result = 0 then -1 else 0);
   end Sys_Rename;

   function Sys_Realpath (S : in Ptr;
                          R : in Ptr) return Ptr is
      pragma Unreferenced (R);

      use Interfaces.C;

      WPath  : Wchar_Ptr := To_WSTR (Interfaces.C.Strings.Value (S));
      Length : constant := 1024;
      Buffer : Wchar_Ptr := new Interfaces.C.wchar_array (0 .. Length);
      Result : DWORD;
   begin
      Result := Get_Full_Pathname (WPath.all'Address, Length, Buffer.all'Address, NULL_STR);
      Free (WPath);
      if Result = 0 then
         Free (Buffer);
         return Interfaces.C.Strings.Null_Ptr;
      end if;
      declare
         S : constant Wide_String (1 .. Natural (Result))
           := Interfaces.C.To_Ada (Buffer (0 .. size_t (Result)));
      begin
         Free (Buffer);
         return Strings.New_String (Ada.Strings.UTF_Encoding.Wide_Strings.Encode (S));
      end;
   end Sys_Realpath;

end Util.Systems.Os;
