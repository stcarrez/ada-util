-----------------------------------------------------------------------
--  util-beans-objects-readers -- Datasets
--  Copyright (C) 2013 Stephane Carrez
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
with Util.Beans.Objects.Maps;
with Util.Beans.Objects.Vectors;
with Util.Serialize.IO;
with Util.Stacks;
package Util.Beans.Objects.Readers is

   type Reader is limited new Util.Serialize.IO.Reader with private;

   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   overriding
   procedure Start_Object (Handler : in out Reader;
                           Name    : in String);

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   overriding
   procedure Finish_Object (Handler : in out Reader;
                            Name    : in String);

   overriding
   procedure Start_Array (Handler : in out Reader;
                          Name    : in String);

   overriding
   procedure Finish_Array (Handler : in out Reader;
                           Name    : in String;
                           Count   : in Natural);

   --  Set the name/value pair on the current object.  For each active mapping,
   --  find whether a rule matches our name and execute it.
   overriding
   procedure Set_Member (Handler   : in out Reader;
                         Name      : in String;
                         Value     : in Util.Beans.Objects.Object;
                         Attribute : in Boolean := False);

   --  Report an error while parsing the input stream.  The error message will be reported
   --  on the logger associated with the parser.  The parser will be set as in error so that
   --  the <b>Has_Error</b> function will return True after parsing the whole file.
   procedure Error (Handler : in out Reader;
                    Message : in String);

private

   type Reader is limited new Util.Serialize.IO.Reader with record
      Current          : Util.Beans.Objects.Maps.Map_Bean_Access;
      List             : Util.Beans.Objects.Vectors.Vector;
   end record;

end Util.Beans.Objects.Readers;
