-----------------------------------------------------------------------
--  util-strings-formats --  String formatting helper
--  Copyright (C) 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Strings.Formats is

   procedure Append (Result : in out Util.Strings.Builders.Builder;
                     Item : in String_Access) is
   begin
      Util.Strings.Builders.Append (Result, Item.all);
   end Append;

   procedure Format (Into    : in out Util.Strings.Builders.Builder;
                     Message : in String;
                     Arg1    : in String) is
      V : constant String_Array_Access (1 .. 1) := (1 => Arg1'Unrestricted_Access);
   begin
      Format (Into, Message, V);
   end Format;

   procedure Format (Into    : in out Util.Strings.Builders.Builder;
                     Message : in String;
                     Arg1    : in String;
                     Arg2    : in String;
                     Arg3    : in String;
                     Arg4    : in String) is
      V : constant String_Array_Access (1 .. 4)
        := (1 => Arg1'Unrestricted_Access,
            2 => Arg2'Unrestricted_Access,
            3 => Arg3'Unrestricted_Access,
            4 => Arg4'Unrestricted_Access);
   begin
      Format (Into, Message, V);
   end Format;

   function Format (Message : in String;
                    Arg1    : in String) return String is
      Buffer : Util.Strings.Builders.Builder (256);
   begin
      Format (Buffer, Message, Arg1);
      return Util.Strings.Builders.To_Array (Buffer);
   end Format;

   function Format (Message : in String;
                    Arg1    : in String;
                    Arg2    : in String;
                    Arg3    : in String;
                    Arg4    : in String) return String is
      Buffer : Util.Strings.Builders.Builder (256);
   begin
      Format (Buffer, Message, Arg1, Arg2, Arg3, Arg4);
      return Util.Strings.Builders.To_Array (Buffer);
   end Format;

end Util.Strings.Formats;
