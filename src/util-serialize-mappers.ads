-----------------------------------------------------------------------
--  util-serialize -- Serialize objects in various formats
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
package Util.Serialize.Mappers is

   type Mapper is abstract tagged limited private;

   procedure Error (Stream  : in out Mapper;
                    Message : in String);

   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   procedure Create_Object (Stream : in out Mapper;
                            Name   : in String) is abstract;

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   procedure Finish_Object (Stream : in out Mapper;
                            Name   : in String) is abstract;

   --  Set the name/value pair on the current object.
   procedure Set_Member (Stream  : in out Mapper;
                         Name    : in String;
                         Value   : in Util.Beans.Objects.Object) is abstract;

private

   type Mapper is abstract tagged limited record
      N : Natural;
   end record;

end Util.Serialize.Mappers;
