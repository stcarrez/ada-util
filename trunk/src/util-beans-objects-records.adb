-----------------------------------------------------------------------
--  Util.Beans.Objects.Records -- Generic Typed Data Representation
--  Copyright (C) 2011 Stephane Carrez
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

package body Util.Beans.Objects.Records is

   use Util.Concurrent.Counters;

   --  ------------------------------
   --  Bean Type
   --  ------------------------------
   type Record_Bean_Type is new Bean_Type with null record;

   --  Get the type name
   function Get_Name (Type_Def : in Record_Bean_Type) return String;

   --  Convert the value into a boolean.
   function To_Boolean (Type_Def : in Record_Bean_Type;
                        Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   function Get_Name (Type_Def : in Record_Bean_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Bean_Record";
   end Get_Name;

   --  ------------------------------
   --  Convert the value into a boolean.
   --  ------------------------------
   function To_Boolean (Type_Def : in Record_Bean_Type;
                        Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
   begin
      return Value.Proxy /= null;
   end To_Boolean;

   Bn_Type : aliased Record_Bean_Type := Record_Bean_Type '(others => <>);

   --  ------------------------------
   --  Create an object which holds a record of the type <b>Element_Type</b>.
   --  ------------------------------
   function Create return Object is
   begin
      return Object '(Controlled with
        V => Object_Value '(Of_Type => TYPE_BEAN,
                            Proxy   => new Element_Proxy '(Ref_Counter => ONE,
                                                           others => <>)),
        Type_Def   => Bn_Type'Access);
   end Create;

   --  ------------------------------
   --  Create an object which is initialized with the given value.
   --  ------------------------------
   function To_Object (Value : in Element_Type) return Object is
   begin
      return Object '(Controlled with
        V => Object_Value '(Of_Type => TYPE_BEAN,
                            Proxy   => new Element_Proxy '(Ref_Counter => ONE,
                                                           Value       => Value)),
        Type_Def   => Bn_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Returns the element
   --  ------------------------------
   function To_Element (Value : in Object) return Element_Type is
   begin
      if Value.V.Of_Type /= TYPE_BEAN then
         raise Conversion_Error with "Object is not a bean";
      end if;
      declare
         Proxy : constant Bean_Proxy_Access := Value.V.Proxy;
      begin
         if Proxy = null then
            raise Conversion_Error with "Object is null";
         end if;
         if not (Proxy.all in Element_Proxy'Class) then
            raise Conversion_Error with "Object is not of the good type";
         end if;
         return Element_Proxy'Class (Proxy.all).Value;
      end;
   end To_Element;

   --  ------------------------------
   --  Returns an access to the element.
   --  ------------------------------
   function To_Element_Access (Value : in Object) return Element_Type_Access is
   begin
      if Value.V.Of_Type /= TYPE_BEAN then
         return null;
      end if;
      declare
         Proxy : constant Bean_Proxy_Access := Value.V.Proxy;
      begin
         if Proxy = null then
            return null;
         end if;
         if not (Proxy.all in Element_Proxy'Class) then
            return null;
         end if;
         return Element_Proxy'Class (Proxy.all).Value'Access;
      end;
   end To_Element_Access;

end Util.Beans.Objects.Records;
