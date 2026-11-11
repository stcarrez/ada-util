-----------------------------------------------------------------------
--  util-encoders-random-system -- System random generator (/dev/urandom)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;
with Interfaces;
private with Util.Encoders.Random.Xorshiftr;

--  === System random generator ===
--  The `Util.Encoders.Random.System` package provides the `System_Generator`
--  type which reads the operating system random source (<tt>/dev/urandom</tt>).
--  When it is not available, the Xorshiftr pseudo-random generator seeded
--  with the clock is used.
package Util.Encoders.Random.System is

   --  The system random generator.  It is safe to share it between several tasks.
   type System_Generator is limited new Generator with private;

   --  Fill the array with random bytes read from the operating system random source.
   overriding
   procedure Generate (Gen  : in out System_Generator;
                       Into : out Ada.Streams.Stream_Element_Array);

private

   type System_Generator is limited new Generator with record
      Rand           : Util.Encoders.Random.Xorshiftr.Xorshiftr_Generator;
      Use_Dev_Random : Boolean := True;
   end record;

end Util.Encoders.Random.System;
