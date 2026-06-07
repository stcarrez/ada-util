-----------------------------------------------------------------------
--  util-files-rolling-lzma -- Rolling file manager with LZMA compression
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package Util.Files.Rolling.Lzma is

   type Lzma_File_Manager is new File_Manager with null record;

private

   overriding
   procedure Rename (Manager  : in out Lzma_File_Manager;
                     Old_Name : in String;
                     New_Name : in String);

end Util.Files.Rolling.Lzma;
