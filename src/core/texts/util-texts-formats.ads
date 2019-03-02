-----------------------------------------------------------------------
--  Util-texts-formats -- Text Format ala Java MessageFormat
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2011, 2012, 2013 Stephane Carrez
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
generic
   type Stream is limited private;
   type Char is (<>);
   type Input is array (Positive range <>) of Char;
   type Value is limited private;
   type Value_List is array (Positive range <>) of Value;
   with procedure Put (Buffer : in out Stream; C : in Character);
   with function To_Input (Arg : in Value) return Input;
package Util.Texts.Formats is
   pragma Preelaborate;

   --  Format the message and replace occurrences of argument patterns by
   --  their associated value.
   --  Returns the formatted message in the stream
   procedure Format (Message   : in Input;
                     Arguments : in Value_List;
                     Into      : in out Stream);

private
   procedure Format (Argument : in Value;
                     Into     : in out Stream);

end Util.Texts.Formats;
