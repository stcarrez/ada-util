-----------------------------------------------------------------------
--  proplist -- List the properties
--  Copyright (C) 2017 Stephane Carrez
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
with Ada.Text_IO;
with Ada.Command_Line;
with Util.Properties;

procedure Proplist is

   procedure Print (Name  : in String;
                    Value : in Util.Properties.Value);

   procedure Print (Name  : in String;
                    Value : in Util.Properties.Value) is
   begin
      if Util.Properties.Is_Manager (Value) then
         Ada.Text_IO.Put_Line ("[" & Name & "]");
         Util.Properties.To_Manager (Value).Iterate (Print'Access);
         Ada.Text_IO.New_Line;
      else
         Ada.Text_IO.Put (Name);
         Ada.Text_IO.Put ("=");
         Ada.Text_IO.Put_Line (Util.Properties.To_String (Value));
      end if;
   end Print;

   Properties : Util.Properties.Manager;

begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      Properties.Load_Properties (Path => Ada.Command_Line.Argument (I));

      Properties.Iterate (Print'Access);
   end loop;
end Proplist;
