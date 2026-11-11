-----------------------------------------------------------------------
--  util-encoders-random-synchronized_generator -- Synchronized random generator
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;

--  === Synchronized random generator ===
--  The `Util.Encoders.Random.Synchronized_Generator` generic package wraps a
--  random generator in a protected type so that the resulting `Generator` is
--  safe to share it between several tasks:
--
--    package Sync_Generator is
--      new Util.Encoders.Random.Xorshiftr
--        (Random_Generator => Util.Encoders.Random.Xorshiftr.Xorshiftr_Generator);
--
--    Gen : Sync_Generator.Generator;
--
--  It is possible to access the generator itself from a procedure by calling
--  the `Configure` procedure:
--
--    procedure Setup (Gen : in out Xorshiftr_Generator) is
--    begin
--       Gen.Set_Seed (42);
--    end Setup;
--
--    Gen.Configure (Setup'Access);
generic
   type Random_Generator is limited new Generator with private;
package Util.Encoders.Random.Synchronized_Generator with Preelaborate is

   --  The synchronized random generator.  It is safe to share it between
   --  several tasks.
   type Generator is limited new Util.Encoders.Random.Generator with private;

   overriding
   procedure Generate (Gen  : in out Generator;
                       Into : out Ada.Streams.Stream_Element_Array);

   overriding
   procedure Reseed (Gen : in out Generator);

   procedure Configure (Gen : in out Generator;
                        Process : not null access
                          procedure (G : in out Random_Generator));

private

   --  Protected type to allow using the random generator by several tasks.
   protected type MT_Generator is

      procedure Generate (Into : out Ada.Streams.Stream_Element_Array);

      procedure Reseed;

      procedure Configure (Process : not null access
                             procedure (G : in out Random_Generator));

   private
      Rand : Random_Generator;
   end MT_Generator;

   type Generator is limited new Util.Encoders.Random.Generator with record
      Rand : MT_Generator;
   end record;

end Util.Encoders.Random.Synchronized_Generator;
