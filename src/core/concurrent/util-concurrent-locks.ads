-----------------------------------------------------------------------
--  Util.Concurrent.Locks -- Concurrent Tools
--  Copyright (C) 2001, 2002, 2003, 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package Util.Concurrent.Locks is

   pragma Preelaborate;

   --  Lock for accessing the shared cache
   protected type RW_Lock is
      --  Lock the resource for reading.
      entry Read;

      --  Release the read lock.
      procedure Release_Read;

      --  Lock the resource for writing.
      entry Write;

      --  Release the write lock.
      procedure Release_Write;

   private
      Readable     : Boolean := True;
      Reader_Count : Natural := 0;
   end RW_Lock;

end Util.Concurrent.Locks;
