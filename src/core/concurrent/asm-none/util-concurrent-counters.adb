-----------------------------------------------------------------------
--  Util.Concurrent -- Concurrent Counters
--  Copyright (C) 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  This implementation of atomic counters is the portable Ada05 implementation.
--  It uses a protected type to implement the increment/decrement operations,
--  thus providing the thread-safe capability.
package body Util.Concurrent.Counters is

   function ONE return Counter is
   begin
      return C : Counter do
         C.Value.Increment;
      end return;
   end ONE;

   --  ------------------------------
   --  Increment the counter atomically.
   --  ------------------------------
   procedure Increment (C : in out Counter) is
   begin
      C.Value.Increment;
   end Increment;

   --  ------------------------------
   --  Increment the counter atomically and return the value before increment.
   --  ------------------------------
   procedure Increment (C     : in out Counter;
                        Value : out Integer) is
   begin
      C.Value.Increment (Value);
   end Increment;

   --  ------------------------------
   --  Decrement the counter atomically.
   --  ------------------------------
   procedure Decrement (C : in out Counter) is
      Is_Zero : Boolean;
   begin
      C.Value.Decrement (Is_Zero);
   end Decrement;

   --  ------------------------------
   --  Decrement the counter atomically and return a status.
   --  ------------------------------
   procedure Decrement (C : in out Counter;
                        Is_Zero : out Boolean) is
   begin
      C.Value.Decrement (Is_Zero);
   end Decrement;

   --  ------------------------------
   --  Get the counter value
   --  ------------------------------
   function Value (C : in Counter) return Integer is
   begin
      return C.Value.Get;
   end Value;

   protected body Cnt is

      procedure Increment is
      begin
         N := N + 1;
      end Increment;

      procedure Increment (Value : out Integer) is
      begin
         Value := N;
         N := N + 1;
      end Increment;

      procedure Decrement (Is_Zero : out Boolean) is
      begin
         N := N - 1;
         Is_Zero := N = 0;
      end Decrement;

      function Get return Natural is
      begin
         return N;
      end Get;

   end Cnt;

end Util.Concurrent.Counters;
