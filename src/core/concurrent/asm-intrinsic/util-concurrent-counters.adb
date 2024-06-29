-----------------------------------------------------------------------
--  util-concurrent -- Concurrent Counters
--  Copyright (C) 2009, 2010, 2015, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Concurrent.Counters is

   function Sync_Sub_And_Fetch (Ptr   : access Interfaces.Unsigned_32;
                                Value : in Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   pragma Import (Intrinsic, Sync_Sub_And_Fetch,
                  External_Name => "__sync_sub_and_fetch_4");

   function Sync_Fetch_And_Add (Ptr   : access Interfaces.Unsigned_32;
                                Value : in Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   pragma Import (Intrinsic, Sync_Fetch_And_Add,
                  External_Name => "__sync_fetch_and_add_4");

   procedure Sync_Add (Ptr   : access Interfaces.Unsigned_32;
                       Value : in Interfaces.Unsigned_32);
   pragma Import (Intrinsic, Sync_Add,
                  External_Name => "__sync_add_and_fetch_4");

   procedure Sync_Sub (Ptr   : access Interfaces.Unsigned_32;
                       Value : in Interfaces.Unsigned_32);
   pragma Import (Intrinsic, Sync_Sub,
                  External_Name => "__sync_sub_and_fetch_4");

   --  ------------------------------
   --  Increment the counter atomically.
   --  ------------------------------
   procedure Increment (C : in out Counter) is
   begin
      Sync_Add (C.Value'Unrestricted_Access, 1);
   end Increment;

   --  ------------------------------
   --  Increment the counter atomically and return the value before increment.
   --  ------------------------------
   procedure Increment (C     : in out Counter;
                        Value : out Integer) is
   begin
      Value := Integer (Sync_Fetch_And_Add (C.Value'Unrestricted_Access, 1));
   end Increment;

   --  ------------------------------
   --  Decrement the counter atomically.
   --  ------------------------------
   procedure Decrement (C : in out Counter) is
   begin
      Sync_Sub (C.Value'Unrestricted_Access, 1);
   end Decrement;

   --  ------------------------------
   --  Decrement the counter atomically and return a status.
   --  ------------------------------
   procedure Decrement (C : in out Counter;
                        Is_Zero : out Boolean) is
      use type Interfaces.Unsigned_32;

      Value : Interfaces.Unsigned_32;
   begin
      Value := Sync_Sub_And_Fetch (C.Value'Unrestricted_Access, 1);
      Is_Zero := Value = 0;
   end Decrement;

   --  ------------------------------
   --  Get the counter value
   --  ------------------------------
   function Value (C : in Counter) return Integer is
   begin
      return Integer (C.Value);
   end Value;

end Util.Concurrent.Counters;
