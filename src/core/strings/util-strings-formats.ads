-----------------------------------------------------------------------
--  util-strings-formats --  String formatting helper
--  Copyright (C) 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Strings.Builders;

package Util.Strings.Formats is

   type String_Access is access constant String;
   type String_Array_Access is array (Positive range <>) of String_Access;

   procedure Append (Result : in out Util.Strings.Builders.Builder;
                     Item : in String_Access);

   procedure Format is
      new Util.Strings.Builders.Format (Value      => String_Access,
                                        Value_List => String_Array_Access,
                                        Append     => Append);

   --  Format the message and append it to the string builder.
   procedure Format (Into    : in out Util.Strings.Builders.Builder;
                     Message : in String;
                     Arg1    : in String);
   procedure Format (Into    : in out Util.Strings.Builders.Builder;
                     Message : in String;
                     Arg1    : in String;
                     Arg2    : in String;
                     Arg3    : in String;
                     Arg4    : in String);

   --  Format the message and return the formatted message.
   function Format (Message : in String;
                    Arg1    : in String) return String;
   function Format (Message : in String;
                    Arg1    : in String;
                    Arg2    : in String;
                    Arg3    : in String;
                    Arg4    : in String) return String;

end Util.Strings.Formats;
