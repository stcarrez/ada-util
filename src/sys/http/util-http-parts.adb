-----------------------------------------------------------------------
--  util-http-parts -- HTTP Parts
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Directories;
with Ada.IO_Exceptions;

with Util.Log.Loggers;

package body Util.Http.Parts is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Http.Parts");

   --  ------------------------------
   --  Write the part data item to the file.  This method is not guaranteed to succeed
   --  if called more than once for the same part. This allows a particular implementation
   --  to use, for example, file renaming, where possible, rather than copying all of
   --  the underlying data, thus gaining a significant performance benefit.
   --  ------------------------------
   procedure Save (Data : in Part;
                   Path : in String) is
      Old_Path : constant String := Part'Class (Data).Get_Local_Filename;
   begin
      Log.Info ("Saving uploaded file {0} to {1}", Old_Path, Path);

      Ada.Directories.Rename (Old_Name => Old_Path,
                              New_Name => Path);

   exception
      when Ada.IO_Exceptions.Use_Error =>
         Log.Error ("Cannot save uploaded file");
   end Save;

   --  ------------------------------
   --  Deletes the underlying storage for a file item, including deleting any associated
   --  temporary disk file.
   --  ------------------------------
   procedure Delete (Data : in out Part) is
      Path : constant String := Part'Class (Data).Get_Local_Filename;
   begin
      Ada.Directories.Delete_File (Path);
   end Delete;

end Util.Http.Parts;
