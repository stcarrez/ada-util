-----------------------------------------------------------------------
--  util-systems-dlls -- Windows shared library support
--  Copyright (C) 2013, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Interfaces.C.Strings;
with Util.Systems.Os;
package body Util.Systems.DLLs is

   function Sys_Load_Library (Path  : in Interfaces.C.Strings.chars_ptr) return Handle
     with Import => True, Convention => Stdcall, Link_Name => "LoadLibraryA";

   function Sys_Free_Library (Lib : in Handle) return Interfaces.C.int
     with Import => True, Convention => Stdcall, Link_Name => "FreeLibrary";

   function Sys_Get_Proc_Address (Lib    : in Handle;
                                  Symbol : in Interfaces.C.Strings.chars_ptr)
                                  return System.Address
     with Import => True, Convention => Stdcall, Link_Name => "GetProcAddress";

   function Error_Message return String;

   function Error_Message return String is
      Err : constant Integer := Util.Systems.Os.Get_Last_Error;
   begin
      return Integer'Image (Err);
   end Error_Message;

   --  -----------------------
   --  Load the shared library with the given name or path and return a library handle.
   --  Raises the <tt>Load_Error</tt> exception if the library cannot be loaded.
   --  -----------------------
   function Load (Path : in String;
                  Mode : in Flags := 0) return Handle is
      pragma Unreferenced (Mode);

      Lib    : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Path);
      Result : constant Handle := Sys_Load_Library (Lib);
   begin
      Interfaces.C.Strings.Free (Lib);
      if Result = Null_Handle then
         raise Load_Error with Error_Message;
      else
         return Result;
      end if;
   end Load;

   --  -----------------------
   --  Unload the shared library.
   --  -----------------------
   procedure Unload (Lib : in Handle) is
      Result : Interfaces.C.int with Unreferenced;
   begin
      if Lib /= Null_Handle then
         Result := Sys_Free_Library (Lib);
      end if;
   end Unload;

   --  -----------------------
   --  Get a global symbol with the given name in the library.
   --  Raises the <tt>Not_Found</tt> exception if the symbol does not exist.
   --  -----------------------
   function Get_Symbol (Lib  : in Handle;
                        Name : in String) return System.Address is
      use type System.Address;

      Symbol : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Name);
      Result : constant System.Address := Sys_Get_Proc_Address (Lib, Symbol);
   begin
      Interfaces.C.Strings.Free (Symbol);
      if Result = System.Null_Address then
         raise Not_Found with Error_Message;
      else
         return Result;
      end if;
   end Get_Symbol;

end Util.Systems.DLLs;
