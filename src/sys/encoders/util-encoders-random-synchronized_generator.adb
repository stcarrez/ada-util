-----------------------------------------------------------------------
--  util-encoders-random-synchronized_generator -- Synchronized random generator
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Encoders.Random.Synchronized_Generator is

   overriding
   procedure Generate (Gen  : in out Generator;
                       Into : out Ada.Streams.Stream_Element_Array) is
   begin
      Gen.Rand.Generate (Into);
   end Generate;

   overriding
   procedure Reseed (Gen : in out Generator) is
   begin
      Gen.Rand.Reseed;
   end Reseed;

   procedure Configure (Gen : in out Generator;
                        Process : not null access
                          procedure (G : in out Random_Generator)) is
   begin
      Gen.Rand.Configure (Process);
   end Configure;

   protected body MT_Generator is

      procedure Generate (Into : out Ada.Streams.Stream_Element_Array) is
      begin
         Rand.Generate (Into);
      end Generate;

      procedure Reseed is
      begin
         Rand.Reseed;
      end Reseed;

      procedure Configure (Process : not null access
                             procedure (G : in out Random_Generator)) is
      begin
         Process (Rand);
      end Configure;

   end MT_Generator;

end Util.Encoders.Random.Synchronized_Generator;
