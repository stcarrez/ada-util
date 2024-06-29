-----------------------------------------------------------------------
--  util-systems-dlls -- Windows shared library support
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with System;

with Interfaces.C;

package Util.Systems.DLLs is

   --  The shared library handle.
   type Handle is private;

   Null_Handle : constant Handle;

   --  Exception raised when there is a problem loading a shared library.
   Load_Error : exception;

   --  Exception raised when a symbol cannot be found in a shared library.
   Not_Found  : exception;

   Extension  : constant String := ".dll";

   subtype Flags is Interfaces.C.int;

   --  Load the shared library with the given name or path and return a library handle.
   --  Raises the <tt>Load_Error</tt> exception if the library cannot be loaded.
   function Load (Path : in String;
                  Mode : in Flags := 0) return Handle;

   --  Unload the shared library.
   procedure Unload (Lib : in Handle);

   --  Get a global symbol with the given name in the library.
   --  Raises the <tt>Not_Found</tt> exception if the symbol does not exist.
   function Get_Symbol (Lib  : in Handle;
                        Name : in String) return System.Address;

private

   type Handle is new System.Address;

   Null_Handle : constant Handle := Handle (System.Null_Address);

end Util.Systems.DLLs;
