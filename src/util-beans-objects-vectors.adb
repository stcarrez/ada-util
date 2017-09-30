-----------------------------------------------------------------------
--  util-beans-vectors -- Object vectors
--  Copyright (C) 2011, 2017 Stephane Carrez
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

package body Util.Beans.Objects.Vectors is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : in Vector_Bean;
                       Name : in String) return Object is
   begin
      return Null_Object;
   end Get_Value;

   --  Set the value identified by the name.
   --  If the map contains the given name, the value changed.
   --  Otherwise name is added to the map and the value associated with it.
   procedure Set_Value (From  : in out Vector_Bean;
                        Name  : in String;
                        Value : in Object) is
   begin
      null;
   end Set_Value;

   --  Create an object that contains a <tt>Vector_Bean</tt> instance.
   function Create return Object is
      M : constant access Vector_Bean'Class := new Vector_Bean;
   begin
      return To_Object (Value => M, Storage => DYNAMIC);
   end Create;

end Util.Beans.Objects.Vectors;
