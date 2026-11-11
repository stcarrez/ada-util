-----------------------------------------------------------------------
--  util-encoders-random -- Random number generator interface
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Finalization;
private with Ada.Numerics.Discrete_Random;

package Util.Encoders.Random.Pseudo is

   type Pseudo_Generator is limited new Ada.Finalization.Limited_Controlled
     and Generator with private;

   overriding
   procedure Generate (Gen  : in out Pseudo_Generator;
                       Into : out Ada.Streams.Stream_Element_Array);

private

   package Id_Random is new Ada.Numerics.Discrete_Random (Interfaces.Unsigned_32);

   type Pseudo_Generator is limited new Ada.Finalization.Limited_Controlled
     and Generator with record
      Rand : Id_Random.Generator;
   end record;

end Util.Encoders.Random.Pseudo;
