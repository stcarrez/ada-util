-----------------------------------------------------------------------
--  util-properties-discrete -- Generic package for get/set of discrete properties
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2018, 2021 Stephane Carrez
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
package body Util.Properties.Discrete is

   --  ------------------------------
   --  Get the property value
   --  ------------------------------
   function Get (Self : in Manager'Class;
                 Name : in String) return Property_Type is
      Val : constant String := -Get (Self, Name);
   begin
      return Property_Type'Value (Val);
   end Get;

   --  ------------------------------
   --  Get the property value.
   --  Return the default if the property does not exist.
   --  ------------------------------
   function Get (Self    : in Manager'Class;
                 Name    : in String;
                 Default : in Property_Type) return Property_Type is
      Value : constant Util.Beans.Objects.Object := Self.Get_Value (Name);
   begin
      if Util.Beans.Objects.Is_Null (Value) then
         return Default;
      end if;

      return Property_Type'Value (Util.Beans.Objects.To_String (Value));
   exception
      when others =>
         return Default;
   end Get;

   --  ------------------------------
   --  Set the property value
   --  ------------------------------
   procedure Set (Self : in out Manager'Class; Name : in String;
                  Value : in Property_Type) is
   begin
      Set (Self, Name, Property_Type'Image (Value));
   end Set;

end Util.Properties.Discrete;
