-----------------------------------------------------------------------
--  util-encoders-ed25519-tests - Ed25519 function tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Test_Caller;
with Util.Encoders.Random.Pseudo;
with Util.Encoders.Random.System;

package body Util.Encoders.Ed25519.Tests is

   package Caller is new Util.Test_Caller (Test, "Encoders.Ed25519");

   Hex_Dec : constant Util.Encoders.Decoder := Util.Encoders.Create ("hex");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Encoders.Ed25519.Create_Private_Key",
                       Test_Create_Private_Key'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the creation of a random Ed25519 private key.
   --  ------------------------------
   procedure Test_Create_Private_Key (T : in out Test) is
      Msg   : constant Stream_Element_Array := Hex_Dec.Decode_Binary ("48656c6c6f");  --  "Hello"
      Seed1 : Private_Key;
      Seed2 : Private_Key;
      All_Zero : Boolean;
      Rand  : Util.Encoders.Random.Pseudo.Pseudo_Generator;
   begin
      Create_Private_Key (Rand, Seed1);
      Create_Private_Key (Rand, Seed2);
      T.Assert (Seed1.Secret /= Seed2.Secret, "Two created private keys are identical");
      All_Zero := True;
      for B of Seed1.Secret loop
         if B /= 0 then
            All_Zero := False;
         end if;
      end loop;
      T.Assert (not All_Zero, "Created private key is all zeros");

      --  The created key can be used to sign and verify.
      declare
         Pub : constant Public_Key := Get_Public_Key (Seed1);
      begin
         T.Assert (Verify (Pub, Msg, Sign (Seed1, Msg)),
                   "Signature with a created key not verified");
      end;

      --  The generator can be provided explicitly.
      declare
         Sys : Util.Encoders.Random.System.System_Generator;
         S3  : Private_Key;
         S4  : Private_Key;
      begin
         Create_Private_Key (Sys, S3);
         Create_Private_Key (Rand, S4);
         T.Assert (S3.Secret /= Seed1.Secret
                   and then S4.Secret /= Seed1.Secret
                   and then S3.Secret /= S4.Secret,
                   "Created private keys are not independent");
      end;
   end Test_Create_Private_Key;

end Util.Encoders.Ed25519.Tests;
