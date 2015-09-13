-----------------------------------------------------------------------
--  Util.Concurrent -- Concurrent Counters
--  Copyright (C) 2009, 2010, 2015 Stephane Carrez
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

--  The <b>Counters</b> package defines the <b>Counter</b> type which provides
--  atomic increment and decrement operations.  It is intended to be used to
--  implement reference counting in a multi-threaded environment.
--
--    type Ref is record
--        Cnt  : Counter;
--        Data : ...;
--    end record;
--
--    Object  : access Ref;
--    Is_Last : Boolean;
--  begin
--    Decrement (Object.Cnt, Is_Last);  -- Multi-task safe operation
--    if Is_Last then
--        Free (Object);
--    end if;
--
--  Unlike the Ada portable implementation based on protected type, this implementation
--  does not require that <b>Counter</b> be a limited type.
private with Interfaces;
package Util.Concurrent.Counters is

   pragma Preelaborate;

   --  ------------------------------
   --  Atomic Counter
   --  ------------------------------
   --  The atomic <b>Counter</b> implements a simple counter that can be
   --  incremented or decremented atomically.
   type Counter is private;
   type Counter_Access is access all Counter;

   --  Increment the counter atomically.
   procedure Increment (C : in out Counter);
   pragma Inline_Always (Increment);

   --  Increment the counter atomically and return the value before increment.
   procedure Increment (C     : in out Counter;
                        Value : out Integer);
   pragma Inline_Always (Increment);

   --  Decrement the counter atomically.
   procedure Decrement (C : in out Counter);
   pragma Inline_Always (Decrement);

   --  Decrement the counter atomically and return a status.
   procedure Decrement (C : in out Counter;
                        Is_Zero : out Boolean);
   pragma Inline_Always (Decrement);

   --  Get the counter value
   function Value (C : in Counter) return Integer;
   pragma Inline_Always (Value);

   ONE : constant Counter;

private

   --  This implementation works without an Ada protected type:
   --  o The size of the target object is 10 times smaller.
   --  o Increment and Decrement operations are 5 times faster.
   --  o It works by using special instructions
   --  o The counter is Atomic to make sure the compiler will use atomic read/write instructions
   --    and it prevents optimization (Atomic implies Volatile).  The Atomic does not mean
   --    that atomic instructions are used.
   type Counter is record
      Value : Interfaces.Unsigned_32 := 0;
      pragma Atomic (Value);
   end record;

   ONE : constant Counter := Counter '(Value => 1);

end Util.Concurrent.Counters;
