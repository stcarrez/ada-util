-----------------------------------------------------------------------
--  util-encoders-kdf-hkdf -- HMAC-based Extract-and-Expand KDF, RFC 5869
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Encoders.KDF.HKDF is

   --  ------------------------------
   --  HKDF-Extract (RFC 5869 section 2.2): PRK = HMAC-Hash (salt, IKM).
   --  ------------------------------
   procedure Extract (Salt : in Stream_Element_Array;
                      IKM  : in Stream_Element_Array;
                      PRK  : out Stream_Element_Array) is
   begin
      HMAC (Key => Salt, Data => IKM, Into => PRK);
   end Extract;

   --  ------------------------------
   --  HKDF-Extract without salt: the salt is a string of HashLen zeros.
   --  ------------------------------
   procedure Extract (IKM : in Stream_Element_Array;
                      PRK : out Stream_Element_Array) is
      Salt : constant Stream_Element_Array (1 .. Length) := [others => 0];
   begin
      Extract (Salt, IKM, PRK);
   end Extract;

   --  ------------------------------
   --  HKDF-Expand (RFC 5869 section 2.3):
   --    T(0) = empty
   --    T(i) = HMAC-Hash (PRK, T(i-1) || info || i)
   --    OKM  = first L bytes of T(1) || T(2) || ... || T(N)
   --  ------------------------------
   procedure Expand (PRK  : in Stream_Element_Array;
                     Info : in Stream_Element_Array;
                     OKM  : out Stream_Element_Array) is
      Block_Count : constant Natural
        := (Natural (OKM'Length) + Natural (Length) - 1) / Natural (Length);
      T     : Stream_Element_Array (1 .. Length);  --  T(i-1)
      Buf   : Stream_Element_Array (1 .. Length + Info'Length + 1);
      T_Len : Stream_Element_Offset := 0;         --  T(0) is empty
      Pos   : Stream_Element_Offset := OKM'First;
   begin
      T := [others => 0];
      for I in 1 .. Block_Count loop
         if T_Len > 0 then
            Buf (1 .. T_Len) := T (1 .. T_Len);
         end if;
         Buf (T_Len + 1 .. T_Len + Info'Length) := Info;
         Buf (T_Len + Info'Length + 1) := Stream_Element (I);
         HMAC (Key  => PRK,
               Data => Buf (1 .. T_Len + Info'Length + 1),
               Into => T);
         T_Len := Length;
         declare
            Take : constant Stream_Element_Offset
              := Stream_Element_Offset'Min (Length, OKM'Last - Pos + 1);
         begin
            OKM (Pos .. Pos + Take - 1) := T (1 .. Take);
            Pos := Pos + Take;
         end;
      end loop;
   end Expand;

   --  ------------------------------
   --  HKDF extract then expand: derive the secret key.
   --  ------------------------------
   procedure Derive_Key (Salt   : in Stream_Element_Array;
                         IKM    : in Stream_Element_Array;
                         Info   : in Stream_Element_Array;
                         Result : out Secret_Key) is
      PRK : Stream_Element_Array (1 .. Length);
   begin
      Extract (Salt, IKM, PRK);
      Expand (PRK, Info, Result.Secret);
   end Derive_Key;

end Util.Encoders.KDF.HKDF;
