-----------------------------------------------------------------------
--  util-encoders-quoted_printable -- Encode/Decode a stream in quoted-printable
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package Util.Encoders.Quoted_Printable is

   pragma Preelaborate;

   --  Decode the Quoted-Printable string and return the result.
   --  When Strict is true, raises the Encoding_Error exception if the
   --  format is invalid.  Otherwise, ignore invalid encoding.
   function Decode (Content : in String;
                    Strict  : in Boolean := True) return String;

   --  Decode the "Q" encoding, similar to Quoted-Printable but with
   --  spaces that can be replaced by '_'.
   --  See RFC 2047.
   function Q_Decode (Content : in String) return String;

end Util.Encoders.Quoted_Printable;
