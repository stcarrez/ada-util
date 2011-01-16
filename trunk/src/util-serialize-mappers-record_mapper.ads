-----------------------------------------------------------------------
--  Util.Readers.Object_Reader -- Object Reader
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
with Util.Serialize.Streamers;
generic
   type Element_Type is limited private;
   type Fields is (<>);
   type Element_Type_Access is access all Element_Type;
   with procedure Set_Member (Into   : in out Element_Type;
                              Field  : in Fields;
                              Value  : in Util.Beans.Objects.Object);
--     with procedure Set_Object (Obj   : in out T;
--                                Name  : in String;
--                                Parser: in Reader'Class) is null;
package Util.Serialize.Mappers.Record_Mapper is

   type Mapper is new Util.Serialize.Mappers.Mapper with private;

   --  Create a new object associated with the given name.
   --  The reader must be associated with the new object so
   --  that calls to <b>Set_Member</b> will apply on the new object.
--     procedure Create_Object (Parser : in out Mapper;
--                              Name   : in String);

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
--     procedure Finish_Object (Parser : in out Mapper;
--                              Name   : in String);

   --  Set the name/value pair on the current object.  The value is an
   --  object held by another parser.
--     procedure Set_Object (Parser : in out Mapper;
--                           Name   : in String;
--                           Value  : in Reader'Class);

   procedure Add_Mapping (Into  : in out Mapper;
                          Path  : in String;
                          Field : in Fields);
--
--     function Create_Context (Map : in T_Mapper) return Context_Access is
--        Result : Element_Context_Access := new Element_Context;
--     begin
--        return Result;
--     end Create_Context;

private

   type Mapper is new Util.Serialize.Mappers.Mapper with record
      Key : Util.Serialize.Contexts.Data_Key;
   end record;
--
--     procedure Push_Context (Mapper : in T_Mapper;
--                             Context : in out Util.Serialize.Streamers.Reader'Class) is
--  --      Ctx :
--     begin
--     end Push_Context;

end Util.Serialize.Mappers.Record_Mapper;