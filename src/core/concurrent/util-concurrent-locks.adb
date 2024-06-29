-----------------------------------------------------------------------
--  Util.Concurrent.Locks -- Concurrent Tools
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package body Util.Concurrent.Locks is

   --  Lock for accessing the shared cache
   protected body RW_Lock is
      --  ------------------------------
      --  Lock the resource for reading.
      --  ------------------------------
      entry Read when Readable is
      begin
         Reader_Count := Reader_Count + 1;
      end Read;

      --  ------------------------------
      --  Release the read lock.
      --  ------------------------------
      procedure Release_Read is
      begin
         Reader_Count := Reader_Count - 1;
      end Release_Read;

      --  ------------------------------
      --  Lock the resource for writing.
      --  ------------------------------
      entry Write when Reader_Count = 0 and then Readable is
      begin
         Readable := False;
      end Write;

      --  ------------------------------
      --  Release the write lock.
      --  ------------------------------
      procedure Release_Write is
      begin
         Readable := True;
      end Release_Write;

   end RW_Lock;

end Util.Concurrent.Locks;
