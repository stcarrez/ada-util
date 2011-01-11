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
with Util.Serialize.Mappers;
package Util.Serialize.Streamers is

   type Streamer is limited private;

private

   type Streamer is limited record
      N : Natural;
   end record;
--
--     Pm : aliased Person_Mapper;
--     Am : aliased Address_Mapper;
--     S  : Streamer;
--
--     S.Add_Member ("person", Pm'Unchecked_Access);
--     Pm.Add_Member ("name");
--     Pm.Add_Member ("age");
--     Pm.Add_Member ("address", Am'Unchecked_Access);
--
--     Am.Add_Member ("street");
--     Am.Add_Member ("country");
--     Am.Add_Member ("city");
--
--     S.Add_Member ("addr", Am'Unchecked_Access);
--     S.Add_Member ("asd", X'Unchecked_Access);
--
--     S.Add_Mapper ("person", new Person_Mapper);
--     S.Add_Mapping ("person", "name");
--     S.Add_Mapping ("person", "age");
--     S.Add_Mapping ("person", "address", new Address_Mapper);
--     S.Add_Mapping ("address", "street");
--
--

end Util.Serialize.Streamers;
