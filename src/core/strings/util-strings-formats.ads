-----------------------------------------------------------------------
--  util-strings-formats --  String formatting helper
--  Copyright (C) 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
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
