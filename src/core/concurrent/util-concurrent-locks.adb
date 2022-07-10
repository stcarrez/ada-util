-----------------------------------------------------------------------
--  Util.Concurrent.Locks -- Concurrent Tools
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2022 Stephane Carrez
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
