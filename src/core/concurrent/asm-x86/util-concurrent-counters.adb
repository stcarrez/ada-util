-----------------------------------------------------------------------
--  Util.Concurrent -- Concurrent Counters
--  Copyright (C) 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  This implementation of atomic counters works only for Intel x86 based
--  platforms.  It uses the <b>lock</b> instruction followed by an <b>incl</b>
--  or a <b>decl</b> instruction to implement the atomic operations.
--  (See GNU/Linux atomic.h headers).
with System.Machine_Code;
package body Util.Concurrent.Counters is

   use System.Machine_Code;
   use Interfaces;

   --  ------------------------------
   --  Increment the counter atomically.
   --  ------------------------------
   procedure Increment (C : in out Counter) is
   begin
      Asm (Template => "lock incl %0",
           Volatile => True,
           Outputs  => Unsigned_32'Asm_Output ("+m", C.Value),
           Clobber  => "memory");
   end Increment;

   --  ------------------------------
   --  Increment the counter atomically and return the value before increment.
   --  ------------------------------
   procedure Increment (C     : in out Counter;
                        Value : out Integer) is
      Val : Unsigned_32 := 1;
   begin
      Asm (Template => "lock xaddl %1,%0",
           Volatile => True,
           Outputs  => (Unsigned_32'Asm_Output ("+m", C.Value),
                        Unsigned_32'Asm_Output ("=r", Val)),
           Inputs   => Unsigned_32'Asm_Input ("1", Val),
           Clobber  => "memory");
      Value := Integer (Val);
   end Increment;

   --  ------------------------------
   --  Decrement the counter atomically.
   --  ------------------------------
   procedure Decrement (C : in out Counter) is
   begin
      Asm (Template => "lock decl %0",
           Volatile => True,
           Outputs  => Unsigned_32'Asm_Output ("+m", C.Value),
           Clobber  => "memory");
   end Decrement;

   --  ------------------------------
   --  Decrement the counter atomically and return a status.
   --  ------------------------------
   procedure Decrement (C : in out Counter;
                        Is_Zero : out Boolean) is
      Result : Unsigned_8;
   begin
      Asm ("lock decl %0; sete %1",
           Volatile => True,
           Outputs => (Unsigned_32'Asm_Output ("+m", C.Value),
                       Unsigned_8'Asm_Output ("=qm", Result)));
      Is_Zero := Result /= 0;
   end Decrement;

   --  ------------------------------
   --  Get the counter value
   --  ------------------------------
   function Value (C : in Counter) return Integer is
   begin
      --  On x86, reading the counter is atomic.
      return Integer (C.Value);
   end Value;

end Util.Concurrent.Counters;
