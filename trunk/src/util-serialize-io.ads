-----------------------------------------------------------------------
--  util-serialize-io -- IO Drivers for serialization
--  Copyright (C) 2010 Stephane Carrez
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
with Util.Beans.Objects;
with Util.Streams.Buffered;
package Util.Serialize.IO is

   type Parser is abstract tagged private;

   --  Parse the stream using the JSON parser.
   procedure Parse (Handler : in out Parser;
                    Stream  : in out Util.Streams.Buffered.Buffered_Stream'Class);

   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   procedure Start_Object (Handler : in out Parser;
                           Name    : in String) is abstract;

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   procedure Finish_Object (Handler : in out Parser;
                            Name    : in String) is abstract;

   --  Set the name/value pair on the current object.
   procedure Set_Member (Handler : in out Parser;
                         Name    : in String;
                         Value   : in Util.Beans.Objects.Object) is abstract;

   --  Report an error while parsing the JSON stream.
   procedure Error (Handler  : in out Parser;
                    Message : in String);
private

   type Parser is abstract tagged record
      N : Natural;
   end record;

end Util.Serialize.IO;
