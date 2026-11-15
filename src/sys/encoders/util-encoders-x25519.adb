-----------------------------------------------------------------------
--  util-encoders-x25519 -- X25519 key exchange (RFC 7748)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Interfaces;
with Util.Encoders.Curve25519;

package body Util.Encoders.X25519 is

   use type Interfaces.Unsigned_64;

   use Util.Encoders.Curve25519;

   function Clamp (K : in Key) return Key;
   function X25519_Ladder (K : in Key;
                           U : in Fe) return Fe;
   procedure CSwap (Do_Swap : in Boolean;
                    A, B : in out Fe);

   --  ====================== X25519 (RFC 7748) ======================
   --  Montgomery curve y^2 = x^3 + 486662 x^2 + x over GF(2^255 - 19),
   --  x-only arithmetic.  a24 = (486662 - 2) / 4 = 121665.

   A24 : constant Fe := Fe_From_U64 (121_665);

   --  The standard base point u-coordinate: 9.
   Base_U : constant Ada.Streams.Stream_Element_Array (1 .. Key_Size)
     := [1 => 9, others => 0];

   --  ------------------------------
   --  Clamp a 32-byte scalar as specified by RFC 7748 section 5.
   --  ------------------------------
   function Clamp (K : in Key) return Key is
   begin
      return R : Key do
         R.Secret := K.Secret;
         R.Secret (1) := R.Secret (1) and 248;
         R.Secret (32) := (R.Secret (32) and 127) or 64;
      end return;
   end Clamp;

   function Bit_Of (K : in Key;
                    T : in Natural) return Boolean is
     (Integer (K.Secret (Stream_Element_Offset (T / 8 + 1))) / (2 ** (T mod 8)) mod 2 = 1);

   procedure CSwap (Do_Swap : in Boolean;
                    A, B : in out Fe) is
      T : Fe;
   begin
      if Do_Swap then
         T := A; A := B; B := T;
      end if;
   end CSwap;

   --  ------------------------------
   --  Montgomery ladder of RFC 7748 section 5, computing [k]u in x-only
   --  form and returning the x-coordinate as a field element.
   --  ------------------------------
   function X25519_Ladder (K : in Key;
                           U : in Fe) return Fe is
      X1 : constant Fe := U;
      X2 : Fe := Fe_One;
      Z2 : Fe := Fe_Zero;
      X3 : Fe := U;
      Z3 : Fe := Fe_One;
      Swap : Boolean := False;
   begin
      for T in reverse 0 .. 254 loop
         declare
            Kt : constant Boolean := Bit_Of (K, T);
         begin
            CSwap (Swap xor Kt, X2, X3);
            CSwap (Swap xor Kt, Z2, Z3);
            Swap := Kt;
            declare
               A  : constant Fe := Fe_Add (X2, Z2);
               AA : constant Fe := Fe_Mul (A, A);
               B  : constant Fe := Fe_Sub (X2, Z2);
               BB : constant Fe := Fe_Mul (B, B);
               E  : constant Fe := Fe_Sub (AA, BB);
               C  : constant Fe := Fe_Add (X3, Z3);
               D  : constant Fe := Fe_Sub (X3, Z3);
               DA : constant Fe := Fe_Mul (D, A);
               CB : constant Fe := Fe_Mul (C, B);
               T1 : constant Fe := Fe_Add (DA, CB);
               T2 : constant Fe := Fe_Sub (DA, CB);
            begin
               X3 := Fe_Mul (T1, T1);
               Z3 := Fe_Mul (X1, Fe_Mul (T2, T2));
               X2 := Fe_Mul (AA, BB);
               Z2 := Fe_Mul (E, Fe_Add (AA, Fe_Mul (A24, E)));
            end;
         end;
      end loop;
      CSwap (Swap, X2, X3);
      CSwap (Swap, Z2, Z3);
      return Fe_Mul (X2, Fe_Inv (Z2));
   end X25519_Ladder;

   --  ------------------------------
   --  The raw X25519 function of RFC 7748.
   --  ------------------------------
   procedure X25519 (Scalar : in Key;
                     U      : in Public_Key;
                     Secret : out Shared_Key) is
      K : constant Key := Clamp (Scalar);
      X : constant Fe := X25519_Ladder (K, Fe_From_Bytes (U, U'First));
   begin
      Fe_To_Bytes (X, Secret.Secret);
   end X25519;

   --  ------------------------------
   --  Create a new random X25519 private key using the given random generator.
   --  ------------------------------
   procedure Create_Private_Key (Gen : in out Util.Encoders.Random.Generator'Class;
                                 Key : out Private_Key) is
   begin
      Util.Encoders.Random.Generate (Gen, Key.Secret);
   end Create_Private_Key;

   function Get_Public_Key (Priv : in Private_Key) return Public_Key is
      R : Shared_Key;
   begin
      X25519 (Priv, Base_U, R);
      return R.Secret;
   end Get_Public_Key;

   --  ------------------------------
   --  Compute the Diffie-Hellman shared secret with the peer public key.
   --  ------------------------------
   procedure Shared_Secret (Priv        : in Private_Key;
                            Peer_Public : in Public_Key;
                            Secret      : out Shared_Key) is
   begin
      X25519 (Priv, Peer_Public, Secret);
   end Shared_Secret;

end Util.Encoders.X25519;
