-----------------------------------------------------------------------
--  util-encoders-ed25519 -- Ed25519 digital signature (RFC 8032) in pure Ada
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Interfaces;
with Util.Encoders.Curve25519;
with Util.Encoders.SHA512;

package body Util.Encoders.Ed25519 is

   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;

   use Util.Encoders.Curve25519;

   --  A SHA-512 digest is a 64-byte array.
   subtype Digest is Util.Encoders.SHA512.Hash_Array;

   --  Fixed exponents used by the point decoding (square root computation).
   Exp_P_Plus_3_Div_8  : constant Fe := [0 => 16#FFFFFFFE#,
                                         1 .. 6 => 16#FFFFFFFF#,
                                         7 => 16#0FFFFFFF#];
   Exp_P_Minus_1_Div_4 : constant Fe := [0 => 16#FFFFFFFB#,
                                         1 .. 6 => 16#FFFFFFFF#,
                                         7 => 16#1FFFFFFF#];

   --  ================ edwards25519 point arithmetic ================
   --  Points are held in extended twisted Edwards coordinates (X:Y:Z:T)
   --  with x = X/Z, y = Y/Z, T = XY/Z on the curve -x^2 + y^2 = 1 + d x^2 y^2.

   type Point is record
      X, Y, Z, T : Fe;
   end record;
   function Point_Add (P1, P2 : Point) return Point;
   function Compute_Base_Point return Point;
   function Scalar_Mult (S : Limbs8; P : Point) return Point;
   function Recover_X (Y : in Fe;
                       Sign : in Interfaces.Unsigned_32;
                       X : out Fe) return Boolean;
   function Point_Decode (B  : in Ada.Streams.Stream_Element_Array;
                          Pt : out Point) return Boolean;
   function Point_Encode (P : in Point) return Public_Key;

   --  d = -121665/121666 mod p and 2d.
   Curve_D : constant Fe
     := Fe_Mul (Fe_Sub (Prime, Fe_From_U64 (121_665)), Fe_Inv (Fe_From_U64 (121_666)));
   Two_D   : constant Fe := Fe_Add (Curve_D, Curve_D);

   --  sqrt(-1) = 2^((p-1)/4) mod p.
   Sqrt_Minus_1 : constant Fe := Fe_Pow (Fe_From_U64 (2), Exp_P_Minus_1_Div_4);

   function Point_Identity return Point is
     (X => Fe_Zero, Y => Fe_One, Z => Fe_One, T => Fe_Zero);

   --  ------------------------------
   --  Unified point addition (valid for all inputs on a curve with a = -1,
   --  including P = Q and P = -Q), from add-2008-hwcd-3.
   --  ------------------------------
   function Point_Add (P1, P2 : Point) return Point is
      AA : constant Fe := Fe_Mul (Fe_Sub (P1.Y, P1.X), Fe_Sub (P2.Y, P2.X));
      BB : constant Fe := Fe_Mul (Fe_Add (P1.Y, P1.X), Fe_Add (P2.Y, P2.X));
      CC : constant Fe := Fe_Mul (Fe_Mul (P1.T, Two_D), P2.T);
      DD : constant Fe := Fe_Mul (Fe_Add (P1.Z, P1.Z), P2.Z);
      EE : constant Fe := Fe_Sub (BB, AA);
      FF : constant Fe := Fe_Sub (DD, CC);
      GG : constant Fe := Fe_Add (DD, CC);
      HH : constant Fe := Fe_Add (BB, AA);
   begin
      return (X => Fe_Mul (EE, FF),
              Y => Fe_Mul (GG, HH),
              T => Fe_Mul (EE, HH),
              Z => Fe_Mul (FF, GG));
   end Point_Add;

   --  ------------------------------
   --  Scalar multiplication using a Montgomery ladder over the 256 bits of
   --  the scalar (R0 = m*P, R1 = (m+1)*P invariant).
   --  ------------------------------
   function Scalar_Mult (S : Limbs8; P : Point) return Point is
      R0 : Point := Point_Identity;
      R1 : Point := P;
   begin
      for I in reverse 0 .. 255 loop
         if (S (I / 32) and Interfaces.Shift_Left (Interfaces.Unsigned_32 (1), I mod 32)) /= 0 then
            R0 := Point_Add (R0, R1);
            R1 := Point_Add (R1, R1);
         else
            R1 := Point_Add (R0, R1);
            R0 := Point_Add (R0, R0);
         end if;
      end loop;
      return R0;
   end Scalar_Mult;

   --  ------------------------------
   --  Recover the x coordinate of a point from its y coordinate and the
   --  requested sign bit.  Returns False when y has no square root on the
   --  curve or when the encoding is invalid (x = 0 with sign bit set).
   --  ------------------------------
   function Recover_X (Y : in Fe;
                       Sign : in Interfaces.Unsigned_32;
                       X : out Fe) return Boolean is
      YY  : constant Fe := Fe_Mul (Y, Y);
      U   : constant Fe := Fe_Sub (YY, Fe_One);
      V   : constant Fe := Fe_Add (Fe_Mul (Curve_D, YY), Fe_One);
      XX  : constant Fe := Fe_Mul (U, Fe_Inv (V));
      Chk : Fe;
   begin
      X := Fe_Pow (XX, Exp_P_Plus_3_Div_8);
      Chk := Fe_Sub (Fe_Mul (X, X), XX);
      if not Fe_Is_Zero (Chk) then
         X := Fe_Mul (X, Sqrt_Minus_1);
         Chk := Fe_Sub (Fe_Mul (X, X), XX);
         if not Fe_Is_Zero (Chk) then
            return False;
         end if;
      end if;
      if Fe_Is_Zero (X) and then Sign = 1 then
         return False;
      end if;
      if ((X (0) and 1) /= 0) /= (Sign = 1) then
         X := Fe_Sub (Prime, X);
      end if;
      return True;
   end Recover_X;

   --  ------------------------------
   --  Decode a 32-byte compressed point.  Returns False when the encoding
   --  is not a valid curve point.
   --  ------------------------------
   function Point_Decode (B  : in Ada.Streams.Stream_Element_Array;
                          Pt : out Point) return Boolean is
      Off   : constant Ada.Streams.Stream_Element_Offset := B'First;
      Sign : constant Interfaces.Unsigned_32
        := Interfaces.Shift_Right (Interfaces.Unsigned_32 (B (Off + 31)), 7);
      Y    : constant Fe := Fe_From_Bytes (B, Off);
      X    : Fe;
   begin
      if Fe_Ge (Y, Prime) then
         return False;
      end if;
      if not Recover_X (Y, Sign, X) then
         return False;
      end if;
      Pt := (X => X, Y => Y, Z => Fe_One, T => Fe_Mul (X, Y));
      return True;
   end Point_Decode;

   --  ------------------------------
   --  Encode a point as 32 bytes: the little-endian y coordinate with the
   --  sign of x in the top bit.
   --  ------------------------------
   function Point_Encode (P : in Point) return Public_Key is
      Zinv : constant Fe := Fe_Inv (P.Z);
      X    : constant Fe := Fe_Mul (P.X, Zinv);
      Y    : constant Fe := Fe_Mul (P.Y, Zinv);
      B    : Public_Key;
   begin
      Fe_To_Bytes (Y, B);
      if (X (0) and 1) /= 0 then
         B (32) := B (32) or 16#80#;
      end if;
      return B;
   end Point_Encode;

   --  ------------------------------
   --  The standard Ed25519 base point: y = 4/5, x = recover_x(y, 0).
   --  ------------------------------
   function Compute_Base_Point return Point is
      Y : constant Fe := Fe_Mul (Fe_From_U64 (4), Fe_Inv (Fe_From_U64 (5)));
      X : Fe;
   begin
      if not Recover_X (Y, 0, X) then
         raise Program_Error with "ed25519 base point is invalid";
      end if;
      return (X => X, Y => Y, Z => Fe_One, T => Fe_Mul (X, Y));
   end Compute_Base_Point;

   Base : constant Point := Compute_Base_Point;

   --  ================ Scalar arithmetic modulo L ================
   --  L is the order of the base point:
   --  L = 2^252 + 27742317777372353535851937790883648493.

   type Scalar is new Limbs8;
   function Clamp_Scalar (H : in Digest) return Scalar;
   function Scalar_Ge (A, B : Scalar) return Boolean;
   function Scalar_Sub (A, B : Scalar) return Scalar;
   function Scalar_Add_Mod (A, B : Scalar) return Scalar;
   function Scalar_Load (B  : in Ada.Streams.Stream_Element_Array;
                         Off : Ada.Streams.Stream_Element_Offset) return Scalar;
   function Scalar_Reduce_Wide (W : Wide17) return Scalar;
   function Scalar_From_Digest (D : in Digest) return Scalar;

   Group_Order : constant Scalar
     := [0 => 16#5CF5D3ED#, 1 => 16#5812631A#, 2 => 16#A2F79CD6#, 3 => 16#14DEF9DE#,
         4 => 0, 5 => 0, 6 => 0, 7 => 16#10000000#];

   --  ------------------------------
   --  Returns True when A >= B.
   --  ------------------------------
   function Scalar_Ge (A, B : Scalar) return Boolean is
   begin
      for I in reverse A'Range loop
         if A (I) /= B (I) then
            return A (I) > B (I);
         end if;
      end loop;
      return True;
   end Scalar_Ge;

   --  ------------------------------
   --  A - B, assuming A >= B.
   --  ------------------------------
   function Scalar_Sub (A, B : Scalar) return Scalar is
      R      : Scalar;
      Borrow : Interfaces.Unsigned_64 := 0;
   begin
      for I in A'Range loop
         declare
            Ai : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (A (I));
            Bi : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (B (I));
            V  : Interfaces.Unsigned_64;
         begin
            if Ai >= Bi + Borrow then
               V := Ai - Bi - Borrow;
               Borrow := 0;
            else
               V := Ai + 16#1_0000_0000# - Bi - Borrow;
               Borrow := 1;
            end if;
            R (I) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
         end;
      end loop;
      return R;
   end Scalar_Sub;

   --  ------------------------------
   --  (A + B) mod L.  A, B < L so A + B < 2L < 2^254.
   --  ------------------------------
   function Scalar_Add_Mod (A, B : Scalar) return Scalar is
      R     : Scalar;
      Carry : Interfaces.Unsigned_64 := 0;
   begin
      for I in A'Range loop
         declare
            V : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64 (A (I)) + Interfaces.Unsigned_64 (B (I)) + Carry;
         begin
            R (I) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
            Carry := Interfaces.Shift_Right (V, 32);
         end;
      end loop;
      if Scalar_Ge (R, Group_Order) then
         R := Scalar_Sub (R, Group_Order);
      end if;
      return R;
   end Scalar_Add_Mod;

   --  ------------------------------
   --  Load a 32-byte little-endian scalar (not reduced modulo L).
   --  ------------------------------
   function Scalar_Load (B  : in Ada.Streams.Stream_Element_Array;
                         Off : Ada.Streams.Stream_Element_Offset) return Scalar is
      R : Limbs8;
   begin
      for I in 0 .. 7 loop
         R (I) := Load_LE32 (B, Off + 4 * Ada.Streams.Stream_Element_Offset (I));
      end loop;
      return Scalar (R);
   end Scalar_Load;

   --  ------------------------------
   --  Reduce a 512-bit number modulo L, one bit at a time.  Because L < 2^253,
   --  doubling a reduced value never overflows 256 bits.
   --  ------------------------------
   function Scalar_Reduce_Wide (W : Wide17) return Scalar is
      R : Scalar := [others => 0];
   begin
      for Bit_Index in reverse 0 .. 511 loop
         --  R := 2R mod L
         for I in reverse 1 .. 7 loop
            R (I) := Interfaces.Shift_Left (R (I), 1)
              or Interfaces.Shift_Right (R (I - 1), 31);
         end loop;
         R (0) := Interfaces.Shift_Left (R (0), 1);
         if Scalar_Ge (R, Group_Order) then
            R := Scalar_Sub (R, Group_Order);
         end if;
         --  R := R + bit mod L
         if (W (Bit_Index / 32)
             and Interfaces.Shift_Left (Interfaces.Unsigned_32 (1), Bit_Index mod 32)) /= 0
         then
            declare
               Carry : Interfaces.Unsigned_64 := 1;
            begin
               for I in 0 .. 7 loop
                  exit when Carry = 0;
                  declare
                     V : constant Interfaces.Unsigned_64
                       := Interfaces.Unsigned_64 (R (I)) + Carry;
                  begin
                     R (I) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
                     Carry := Interfaces.Shift_Right (V, 32);
                  end;
               end loop;
            end;
            if Scalar_Ge (R, Group_Order) then
               R := Scalar_Sub (R, Group_Order);
            end if;
         end if;
      end loop;
      return R;
   end Scalar_Reduce_Wide;

   --  ------------------------------
   --  Load a 64-byte little-endian value (a SHA-512 digest) as a 512-bit
   --  number and reduce it modulo L.
   --  ------------------------------
   function Scalar_From_Digest (D : in Digest) return Scalar is
      W : Wide17 := [others => 0];
   begin
      for I in 0 .. 15 loop
         W (I) := Load_LE32 (D, D'First + 4 * Ada.Streams.Stream_Element_Offset (I));
      end loop;
      return Scalar_Reduce_Wide (W);
   end Scalar_From_Digest;

   --  ------------------------------
   --  (A * B) mod L.
   --  ------------------------------
   function Scalar_Mul_Mod (A, B : Scalar) return Scalar is
     (Scalar_Reduce_Wide (Mul_Wide (Limbs8 (A), Limbs8 (B))));

   --  ------------------------------
   --  Clamp the first 32 bytes of the SHA-512 hash of the seed into the
   --  secret scalar a (RFC 8032, section 5.1.5).
   --  ------------------------------
   function Clamp_Scalar (H : in Digest) return Scalar is
      S : Scalar := Scalar_Load (H, H'First);
   begin
      S (0) := S (0) and 16#FFFFFFF8#;
      S (7) := (S (7) and 16#7FFFFFFF#) or 16#40000000#;
      return S;
   end Clamp_Scalar;

   --  ------------------------------
   --  Create a new random Ed25519 private key (a random 32-byte seed).
   --  ------------------------------
   procedure Create_Private_Key (Gen  : in out Util.Encoders.Random.Generator'Class;
                                 Seed : out Private_Key) is
   begin
      Util.Encoders.Random.Generate (Gen, Seed.Secret);
   end Create_Private_Key;

   --  ------------------------------
   --  Derive the public key associated with the private key seed.
   --  ------------------------------
   function Get_Public_Key (Seed : in Private_Key) return Public_Key is
      H : Digest;
   begin
      SHA512.Hash (Seed.Secret, H);
      return Point_Encode (Scalar_Mult (Limbs8 (Clamp_Scalar (H)), Base));
   end Get_Public_Key;

   --  ------------------------------
   --  Sign the message with the Ed25519 private key (RFC 8032, section 5.1.6).
   --  ------------------------------
   function Sign (Seed : in Private_Key;
                  Message : in Ada.Streams.Stream_Element_Array) return Signature is
      H : Digest;
   begin
      SHA512.Hash (Seed.Secret, H);
      declare
         A_Scalar : constant Scalar := Clamp_Scalar (H);
         Pub_Key  : constant Public_Key := Point_Encode (Scalar_Mult (Limbs8 (A_Scalar), Base));
         Ctx      : Util.Encoders.SHA512.Context;
         R_Scalar : Scalar;
         R_Bytes  : Public_Key;
         K_Scalar : Scalar;
         S        : Scalar;
         Sig      : Signature;
      begin
         --  r = SHA-512(prefix || M) mod L where prefix = H(seed)[32..64]
         Util.Encoders.SHA512.Update (Ctx, H (32 .. 63));
         Util.Encoders.SHA512.Update (Ctx, Message);
         declare
            Dg : Digest;
         begin
            Util.Encoders.SHA512.Finish (Ctx, Dg);
            R_Scalar := Scalar_From_Digest (Dg);
         end;
         --  R = [r]B
         R_Bytes := Point_Encode (Scalar_Mult (Limbs8 (R_Scalar), Base));
         --  k = SHA-512(R || A || M) mod L
         Util.Encoders.SHA512.Update (Ctx, R_Bytes);
         Util.Encoders.SHA512.Update (Ctx, Pub_Key);
         Util.Encoders.SHA512.Update (Ctx, Message);
         declare
            Dg : Digest;
         begin
            Util.Encoders.SHA512.Finish (Ctx, Dg);
            K_Scalar := Scalar_From_Digest (Dg);
         end;
         --  S = (r + k*a) mod L
         S := Scalar_Add_Mod (R_Scalar, Scalar_Mul_Mod (K_Scalar, A_Scalar));
         Sig (1 .. 32) := R_Bytes;
         for I in 0 .. 7 loop
            Store_LE32 (Limbs8 (S) (I), Sig,
                        Sig'First + 32 + 4 * Ada.Streams.Stream_Element_Offset (I));
         end loop;
         return Sig;
      end;
   end Sign;

   --  ------------------------------
   --  Verify the Ed25519 signature of the message with the public key
   --  (RFC 8032, section 5.1.7).
   --  ------------------------------
   function Verify (Public    : in Public_Key;
                    Message   : in Ada.Streams.Stream_Element_Array;
                    Signature : in Ada.Streams.Stream_Element_Array) return Boolean is
   begin
      if Signature'Length /= Signature_Size then
         return False;
      end if;
      declare
         S       : constant Scalar := Scalar_Load (Signature, Signature'First + 32);
         A_Point : Point;
         R_Point : Point;
      begin
         --  Reject non-canonical scalars (S >= L).
         if Scalar_Ge (S, Group_Order) then
            return False;
         end if;
         if not Point_Decode (Public, A_Point)
           or else not Point_Decode (Signature (Signature'First .. Signature'First + 31), R_Point)
         then
            return False;
         end if;
         declare
            Ctx : Util.Encoders.SHA512.Context;
            K   : Scalar;
            Dg  : Digest;
         begin
            --  k = SHA-512(R || A || M) mod L
            Util.Encoders.SHA512.Update (Ctx, Signature (Signature'First .. Signature'First + 31));
            Util.Encoders.SHA512.Update (Ctx, Public);
            Util.Encoders.SHA512.Update (Ctx, Message);
            Util.Encoders.SHA512.Finish (Ctx, Dg);
            K := Scalar_From_Digest (Dg);
            --  Check the group equation [S]B = R + [k]A.
            return Point_Encode (Scalar_Mult (Limbs8 (S), Base))
              = Point_Encode (Point_Add (R_Point, Scalar_Mult (Limbs8 (K), A_Point)));
         end;
      end;
   end Verify;

end Util.Encoders.Ed25519;
