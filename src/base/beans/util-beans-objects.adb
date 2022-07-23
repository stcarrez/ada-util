-----------------------------------------------------------------------
--  util-beans-objects -- Generic Typed Data Representation
--  Copyright (C) 2009, 2010, 2011, 2013, 2016, 2017, 2018, 2020, 2022 Stephane Carrez
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

with Ada.Characters.Conversions;
with Ada.Tags;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Util.Beans.Basic;
package body Util.Beans.Objects is

   use Util.Concurrent.Counters;
   use Ada.Characters.Conversions;

   function UTF8_Decode (S : in String) return Wide_Wide_String
      renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode;

   --  Find the data type to be used for an arithmetic operation between two objects.
   function Get_Arithmetic_Type (Left, Right : Object) return Data_Type;

   --  Find the data type to be used for a composition operation between two objects.
   function Get_Compose_Type (Left, Right : Object) return Data_Type;

   --  Find the best type to be used to compare two operands.
   function Get_Compare_Type (Left, Right : Object) return Data_Type;

   Integer_Type  : aliased constant Int_Type         := Int_Type '(null record);
   Bool_Type     : aliased constant Boolean_Type     := Boolean_Type '(null record);
   Str_Type      : aliased constant String_Type      := String_Type '(null record);
   WString_Type  : aliased constant Wide_String_Type := Wide_String_Type '(null record);
   Flt_Type      : aliased constant Float_Type       := Float_Type '(null record);
   Duration_Type : aliased constant Duration_Type_Def := Duration_Type_Def '(null record);
   Bn_Type       : aliased constant Bean_Type        := Bean_Type '(null record);
   Ar_Type       : aliased constant Array_Type       := Array_Type '(null record);
   Blob_Def      : aliased constant Blob_Type        := Blob_Type '(null record);

   --  ------------------------------
   --  Convert the value into a wide string.
   --  ------------------------------
   overriding
   function To_Wide_Wide_String (Type_Def : in Basic_Type;
                                 Value    : in Object_Value) return Wide_Wide_String is
   begin
      return UTF8_Decode (Object_Type'Class (Type_Def).To_String (Value));
   end To_Wide_Wide_String;

   --  ------------------------------
   --  Convert the value into a float.
   --  ------------------------------
   overriding
   function To_Long_Float (Type_Def : in Basic_Type;
                           Value    : in Object_Value) return Long_Long_Float is
      pragma Unreferenced (Type_Def, Value);
   begin
      return 0.0;
   end To_Long_Float;

   --  ------------------------------
   --  Convert the value into a boolean.
   --  ------------------------------
   overriding
   function To_Boolean (Type_Def : in Basic_Type;
                        Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def, Value);
   begin
      return False;
   end To_Boolean;

   --  ------------------------------
   --  Convert the value into a duration.
   --  ------------------------------
   overriding
   function To_Duration (Type_Def : in Basic_Type;
                         Value    : in Object_Value) return Duration is
      pragma Unreferenced (Type_Def, Value);
   begin
      return 0.0;
   end To_Duration;

   --  ------------------------------
   --  Returns False
   --  ------------------------------
   overriding
   function Is_Empty (Type_Def : in Basic_Type;
                      Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def, Value);
   begin
      return False;
   end Is_Empty;

   --  ------------------------------
   --  Null Type
   --  ------------------------------

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : Null_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Null";
   end Get_Name;

   --  ------------------------------
   --  Get the base data type.
   --  ------------------------------
   overriding
   function Get_Data_Type (Type_Def : Null_Type) return Data_Type is
      pragma Unreferenced (Type_Def);
   begin
      return TYPE_NULL;
   end Get_Data_Type;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in Null_Type;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def, Value);
   begin
      return "null";
   end To_String;

   --  ------------------------------
   --  Returns True
   --  ------------------------------
   overriding
   function Is_Empty (Type_Def : in Null_Type;
                      Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def, Value);
   begin
      return True;
   end Is_Empty;

   --  ------------------------------
   --  Integer Type
   --  ------------------------------

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : Int_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Integer";
   end Get_Name;

   --  ------------------------------
   --  Get the base data type.
   --  ------------------------------
   overriding
   function Get_Data_Type (Type_Def : Int_Type) return Data_Type is
      pragma Unreferenced (Type_Def);
   begin
      return TYPE_INTEGER;
   end Get_Data_Type;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in Int_Type;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);

      S : constant String := Long_Long_Integer'Image (Value.Int_Value);
   begin
      if Value.Int_Value >= 0 then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end To_String;

   --  ------------------------------
   --  Convert the value into an integer.
   --  ------------------------------
   overriding
   function To_Long_Long (Type_Def : in Int_Type;
                          Value    : in Object_Value) return Long_Long_Integer is
      pragma Unreferenced (Type_Def);
   begin
      return Value.Int_Value;
   end To_Long_Long;

   --  ------------------------------
   --  Convert the value into a float.
   --  ------------------------------
   overriding
   function To_Long_Float (Type_Def : in Int_Type;
                           Value    : in Object_Value) return Long_Long_Float is
      pragma Unreferenced (Type_Def);
   begin
      return Long_Long_Float (Value.Int_Value);
   end To_Long_Float;

   --  ------------------------------
   --  Convert the value into a boolean.
   --  ------------------------------
   overriding
   function To_Boolean (Type_Def : in Int_Type;
                        Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
   begin
      return Value.Int_Value /= 0;
   end To_Boolean;

   --  ------------------------------
   --  Convert the value into a duration.
   --  ------------------------------
   overriding
   function To_Duration (Type_Def : in Int_Type;
                         Value    : in Object_Value) return Duration is
      pragma Unreferenced (Type_Def);
   begin
      return Duration (Value.Int_Value);
   end To_Duration;

   --  ------------------------------
   --  Float Type
   --  ------------------------------

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : in Float_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Float";
   end Get_Name;

   --  ------------------------------
   --  Get the base data type.
   --  ------------------------------
   overriding
   function Get_Data_Type (Type_Def : in Float_Type) return Data_Type is
      pragma Unreferenced (Type_Def);
   begin
      return TYPE_FLOAT;
   end Get_Data_Type;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in Float_Type;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);
   begin
      return Long_Long_Float'Image (Value.Float_Value);
   end To_String;

   --  ------------------------------
   --  Convert the value into an integer.
   --  ------------------------------
   overriding
   function To_Long_Long (Type_Def : in Float_Type;
                          Value    : in Object_Value) return Long_Long_Integer is
      pragma Unreferenced (Type_Def);
   begin
      return Long_Long_Integer (Value.Float_Value);
   end To_Long_Long;

   --  ------------------------------
   --  Convert the value into a float.
   --  ------------------------------
   overriding
   function To_Long_Float (Type_Def : in Float_Type;
                           Value    : in Object_Value) return Long_Long_Float is
      pragma Unreferenced (Type_Def);
   begin
      return Value.Float_Value;
   end To_Long_Float;

   --  ------------------------------
   --  Convert the value into a boolean.
   --  ------------------------------
   overriding
   function To_Boolean (Type_Def : in Float_Type;
                        Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
   begin
      return Value.Float_Value /= 0.0;
   end To_Boolean;

   --  ------------------------------
   --  Convert the value into a duration.
   --  ------------------------------
   overriding
   function To_Duration (Type_Def : in Float_Type;
                         Value    : in Object_Value) return Duration is
      pragma Unreferenced (Type_Def);
   begin
      return Duration (Value.Float_Value);
   end To_Duration;

   --  ------------------------------
   --  String Type
   --  ------------------------------

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : in String_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "String";
   end Get_Name;

   --  ------------------------------
   --  Get the base data type.
   --  ------------------------------
   overriding
   function Get_Data_Type (Type_Def : in String_Type) return Data_Type is
      pragma Unreferenced (Type_Def);
   begin
      return TYPE_STRING;
   end Get_Data_Type;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in String_Type;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);
      Proxy : constant String_Proxy_Access := Value.String_Proxy;
   begin
      if Proxy = null then
         return "null";
      else
         return Proxy.Value;
      end if;
   end To_String;

   --  ------------------------------
   --  Convert the value into an integer.
   --  ------------------------------
   overriding
   function To_Long_Long (Type_Def : in String_Type;
                          Value    : in Object_Value) return Long_Long_Integer is
      pragma Unreferenced (Type_Def);
      Proxy : constant String_Proxy_Access := Value.String_Proxy;
   begin
      if Proxy = null then
         return 0;
      else
         return Long_Long_Integer'Value (Proxy.Value);
      end if;
   end To_Long_Long;

   --  ------------------------------
   --  Convert the value into a float.
   --  ------------------------------
   overriding
   function To_Long_Float (Type_Def : in String_Type;
                           Value    : in Object_Value) return Long_Long_Float is
      pragma Unreferenced (Type_Def);
      Proxy : constant String_Proxy_Access := Value.String_Proxy;
   begin
      if Proxy = null then
         return 0.0;
      else
         return Long_Long_Float'Value (Proxy.Value);
      end if;
   end To_Long_Float;

   --  ------------------------------
   --  Convert the value into a boolean.
   --  ------------------------------
   overriding
   function To_Boolean (Type_Def : in String_Type;
                        Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
      Proxy : constant String_Proxy_Access := Value.String_Proxy;
   begin
      return Proxy /= null
        and then (Proxy.Value = "true"
                  or else Proxy.Value = "TRUE"
                  or else Proxy.Value = "1");
   end To_Boolean;

   --  ------------------------------
   --  Returns True if the value is empty.
   --  ------------------------------
   overriding
   function Is_Empty (Type_Def : in String_Type;
                      Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
      Proxy : constant String_Proxy_Access := Value.String_Proxy;
   begin
      return Proxy = null or else Proxy.Value = "";
   end Is_Empty;

   --  ------------------------------
   --  Convert the value into a duration.
   --  ------------------------------
   overriding
   function To_Duration (Type_Def : in String_Type;
                         Value    : in Object_Value) return Duration is
      pragma Unreferenced (Type_Def);
      Proxy : constant String_Proxy_Access := Value.String_Proxy;
   begin
      if Proxy = null then
         return 0.0;
      else
         return Duration'Value (Proxy.Value);
      end if;
   end To_Duration;

   --  ------------------------------
   --  Wide String Type
   --  ------------------------------

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : in Wide_String_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "WideString";
   end Get_Name;

   --  ------------------------------
   --  Get the base data type.
   --  ------------------------------
   overriding
   function Get_Data_Type (Type_Def : in Wide_String_Type) return Data_Type is
      pragma Unreferenced (Type_Def);
   begin
      return TYPE_WIDE_STRING;
   end Get_Data_Type;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in Wide_String_Type;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);
      Proxy : constant Wide_String_Proxy_Access := Value.Wide_Proxy;
   begin
      if Proxy = null then
         return "null";
      else
         return To_String (Proxy.Value);
      end if;
   end To_String;

   --  ------------------------------
   --  Convert the value into a wide string.
   --  ------------------------------
   overriding
   function To_Wide_Wide_String (Type_Def : in Wide_String_Type;
                                 Value    : in Object_Value) return Wide_Wide_String is
      pragma Unreferenced (Type_Def);
      Proxy : constant Wide_String_Proxy_Access := Value.Wide_Proxy;
   begin
      if Proxy = null then
         return "null";
      else
         return Proxy.Value;
      end if;
   end To_Wide_Wide_String;

   --  ------------------------------
   --  Convert the value into an integer.
   --  ------------------------------
   overriding
   function To_Long_Long (Type_Def : in Wide_String_Type;
                          Value    : in Object_Value) return Long_Long_Integer is
      pragma Unreferenced (Type_Def);
      Proxy : constant Wide_String_Proxy_Access := Value.Wide_Proxy;
   begin
      if Proxy = null then
         return 0;
      else
         return Long_Long_Integer'Value (To_String (Proxy.Value));
      end if;
   end To_Long_Long;

   --  ------------------------------
   --  Convert the value into a float.
   --  ------------------------------
   overriding
   function To_Long_Float (Type_Def : in Wide_String_Type;
                           Value    : in Object_Value) return Long_Long_Float is
      pragma Unreferenced (Type_Def);
      Proxy : constant Wide_String_Proxy_Access := Value.Wide_Proxy;
   begin
      if Proxy = null then
         return 0.0;
      else
         return Long_Long_Float'Value (To_String (Proxy.Value));
      end if;
   end To_Long_Float;

   --  ------------------------------
   --  Convert the value into a boolean.
   --  ------------------------------
   overriding
   function To_Boolean (Type_Def : in Wide_String_Type;
                        Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
      Proxy : constant Wide_String_Proxy_Access := Value.Wide_Proxy;
   begin
      return Proxy /= null
        and then (Proxy.Value = "true"
                  or else Proxy.Value = "TRUE"
                  or else Proxy.Value = "1");
   end To_Boolean;

   --  ------------------------------
   --  Convert the value into a duration.
   --  ------------------------------
   overriding
   function To_Duration (Type_Def : in Wide_String_Type;
                         Value    : in Object_Value) return Duration is
      pragma Unreferenced (Type_Def);
      Proxy : constant Wide_String_Proxy_Access := Value.Wide_Proxy;
   begin
      if Proxy = null then
         return 0.0;
      else
         return Duration'Value (To_String (Proxy.Value));
      end if;
   end To_Duration;

   --  ------------------------------
   --  Returns True if the value is empty.
   --  ------------------------------
   overriding
   function Is_Empty (Type_Def : in Wide_String_Type;
                      Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
      Proxy : constant Wide_String_Proxy_Access := Value.Wide_Proxy;
   begin
      return Proxy = null or else Proxy.Value = "";
   end Is_Empty;

   --  ------------------------------
   --  Boolean Type
   --  ------------------------------

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : in Boolean_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Boolean";
   end Get_Name;

   --  ------------------------------
   --  Get the base data type.
   --  ------------------------------
   overriding
   function Get_Data_Type (Type_Def : in Boolean_Type) return Data_Type is
      pragma Unreferenced (Type_Def);
   begin
      return TYPE_BOOLEAN;
   end Get_Data_Type;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in Boolean_Type;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);
   begin
      if Value.Bool_Value then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end To_String;

   --  ------------------------------
   --  Convert the value into an integer.
   --  ------------------------------
   overriding
   function To_Long_Long (Type_Def : in Boolean_Type;
                          Value    : in Object_Value) return Long_Long_Integer is
      pragma Unreferenced (Type_Def);
   begin
      if Value.Bool_Value then
         return 1;
      else
         return 0;
      end if;
   end To_Long_Long;

   --  ------------------------------
   --  Convert the value into a float.
   --  ------------------------------
   overriding
   function To_Long_Float (Type_Def : in Boolean_Type;
                           Value    : in Object_Value) return Long_Long_Float is
      pragma Unreferenced (Type_Def);
   begin
      if Value.Bool_Value then
         return 1.0;
      else
         return 0.0;
      end if;
   end To_Long_Float;

   --  ------------------------------
   --  Convert the value into a boolean.
   --  ------------------------------
   overriding
   function To_Boolean (Type_Def : in Boolean_Type;
                        Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
   begin
      return Value.Bool_Value;
   end To_Boolean;

   --  ------------------------------
   --  Duration Type
   --  ------------------------------

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : in Duration_Type_Def) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Duration";
   end Get_Name;

   --  ------------------------------
   --  Get the base data type.
   --  ------------------------------
   overriding
   function Get_Data_Type (Type_Def : in Duration_Type_Def) return Data_Type is
      pragma Unreferenced (Type_Def);
   begin
      return TYPE_TIME;
   end Get_Data_Type;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in Duration_Type_Def;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);
   begin
      return Duration'Image (Value.Time_Value);
   end To_String;

   --  ------------------------------
   --  Convert the value into an integer.
   --  ------------------------------
   overriding
   function To_Long_Long (Type_Def : in Duration_Type_Def;
                          Value    : in Object_Value) return Long_Long_Integer is
      pragma Unreferenced (Type_Def);
   begin
      return Long_Long_Integer (Value.Time_Value);
   end To_Long_Long;

   --  ------------------------------
   --  Convert the value into a float.
   --  ------------------------------
   overriding
   function To_Long_Float (Type_Def : in Duration_Type_Def;
                           Value    : in Object_Value) return Long_Long_Float is
      pragma Unreferenced (Type_Def);
   begin
      return Long_Long_Float (Value.Time_Value);
   end To_Long_Float;

   --  ------------------------------
   --  Convert the value into a boolean.
   --  ------------------------------
   overriding
   function To_Boolean (Type_Def : in Duration_Type_Def;
                        Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
   begin
      return Value.Time_Value > 0.0;
   end To_Boolean;

   --  ------------------------------
   --  Convert the value into a duration.
   --  ------------------------------
   overriding
   function To_Duration (Type_Def : in Duration_Type_Def;
                         Value    : in Object_Value) return Duration is
      pragma Unreferenced (Type_Def);
   begin
      return Value.Time_Value;
   end To_Duration;

   --  ------------------------------
   --  Bean Type
   --  ------------------------------

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : in Bean_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Bean";
   end Get_Name;

   --  ------------------------------
   --  Get the base data type.
   --  ------------------------------
   overriding
   function Get_Data_Type (Type_Def : in Bean_Type) return Data_Type is
      pragma Unreferenced (Type_Def);
   begin
      return TYPE_BEAN;
   end Get_Data_Type;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in Bean_Type;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);
   begin
      if Value.Proxy = null or else Value.Proxy.Bean = null then
         return "<null bean>";
      else
         return "<" & Ada.Tags.Expanded_Name (Value.Proxy.Bean'Tag) & ">";
      end if;
   end To_String;

   --  ------------------------------
   --  Convert the value into an integer.
   --  ------------------------------
   overriding
   function To_Long_Long (Type_Def : in Bean_Type;
                          Value    : in Object_Value) return Long_Long_Integer is
      pragma Unreferenced (Type_Def, Value);
   begin
      return 0;
   end To_Long_Long;

   --  ------------------------------
   --  Convert the value into a float.
   --  ------------------------------
   overriding
   function To_Long_Float (Type_Def : in Bean_Type;
                           Value    : in Object_Value) return Long_Long_Float is
      pragma Unreferenced (Type_Def, Value);
   begin
      return 0.0;
   end To_Long_Float;

   --  ------------------------------
   --  Convert the value into a boolean.
   --  ------------------------------
   overriding
   function To_Boolean (Type_Def : in Bean_Type;
                        Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
      Proxy : constant Bean_Proxy_Access := Value.Proxy;
   begin
      return Proxy /= null;
   end To_Boolean;

   --  ------------------------------
   --  Returns True if the value is empty.
   --  ------------------------------
   overriding
   function Is_Empty (Type_Def : in Bean_Type;
                      Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
      Proxy : constant Bean_Proxy_Access := Value.Proxy;
   begin
      if Proxy = null then
         return True;
      end if;
      if not (Proxy.all in Bean_Proxy'Class) then
         return False;
      end if;
      if not (Proxy.Bean.all in Util.Beans.Basic.List_Bean'Class) then
         return False;
      end if;
      declare
         L : constant Util.Beans.Basic.List_Bean_Access :=
           Beans.Basic.List_Bean'Class (Proxy.Bean.all)'Unchecked_Access;
      begin
         return L.Get_Count = 0;
      end;
   end Is_Empty;

   --  ------------------------------
   --  Blob Type
   --  ------------------------------

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : in Blob_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Blob";
   end Get_Name;

   --  ------------------------------
   --  Get the base data type.
   --  ------------------------------
   overriding
   function Get_Data_Type (Type_Def : in Blob_Type) return Data_Type is
      pragma Unreferenced (Type_Def);
   begin
      return TYPE_BLOB;
   end Get_Data_Type;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in Blob_Type;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);
   begin
      if Value.Blob_Proxy = null then
         return "<null array>";
      else
         return "<array>";
      end if;
   end To_String;

   --  ------------------------------
   --  Convert the value into an integer.
   --  ------------------------------
   overriding
   function To_Long_Long (Type_Def : in Blob_Type;
                          Value    : in Object_Value) return Long_Long_Integer is
      pragma Unreferenced (Type_Def, Value);
   begin
      return 0;
   end To_Long_Long;

   --  ------------------------------
   --  Convert the value into a float.
   --  ------------------------------
   overriding
   function To_Long_Float (Type_Def : in Blob_Type;
                           Value    : in Object_Value) return Long_Long_Float is
      pragma Unreferenced (Type_Def, Value);
   begin
      return 0.0;
   end To_Long_Float;

   --  ------------------------------
   --  Convert the value into a boolean.
   --  ------------------------------
   overriding
   function To_Boolean (Type_Def : in Blob_Type;
                        Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
      Proxy : constant Blob_Proxy_Access := Value.Blob_Proxy;
   begin
      return Proxy /= null;
   end To_Boolean;

   --  ------------------------------
   --  Returns True if the value is empty.
   --  ------------------------------
   overriding
   function Is_Empty (Type_Def : in Blob_Type;
                      Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
   begin
      if Value.Blob_Proxy = null then
         return True;
      else
         return Value.Blob_Proxy.Blob.Is_Null;
      end if;
   end Is_Empty;

   --  ------------------------------
   --  Array Type
   --  ------------------------------

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : in Array_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Array";
   end Get_Name;

   --  ------------------------------
   --  Get the base data type.
   --  ------------------------------
   overriding
   function Get_Data_Type (Type_Def : in Array_Type) return Data_Type is
      pragma Unreferenced (Type_Def);
   begin
      return TYPE_ARRAY;
   end Get_Data_Type;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in Array_Type;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);
   begin
      if Value.Array_Proxy = null then
         return "<null array>";
      else
         return "<array>";
      end if;
   end To_String;

   --  ------------------------------
   --  Convert the value into an integer.
   --  ------------------------------
   overriding
   function To_Long_Long (Type_Def : in Array_Type;
                          Value    : in Object_Value) return Long_Long_Integer is
      pragma Unreferenced (Type_Def, Value);
   begin
      return 0;
   end To_Long_Long;

   --  ------------------------------
   --  Convert the value into a float.
   --  ------------------------------
   overriding
   function To_Long_Float (Type_Def : in Array_Type;
                           Value    : in Object_Value) return Long_Long_Float is
      pragma Unreferenced (Type_Def, Value);
   begin
      return 0.0;
   end To_Long_Float;

   --  ------------------------------
   --  Convert the value into a boolean.
   --  ------------------------------
   overriding
   function To_Boolean (Type_Def : in Array_Type;
                        Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
      Proxy : constant Array_Proxy_Access := Value.Array_Proxy;
   begin
      return Proxy /= null;
   end To_Boolean;

   --  ------------------------------
   --  Returns True if the value is empty.
   --  ------------------------------
   overriding
   function Is_Empty (Type_Def : in Array_Type;
                      Value    : in Object_Value) return Boolean is
      pragma Unreferenced (Type_Def);
   begin
      if Value.Array_Proxy = null then
         return True;
      else
         return Value.Array_Proxy.Len = 0;
      end if;
   end Is_Empty;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_Long_Long (Type_Def : in Basic_Type;
                          Value    : in Object_Value) return Long_Long_Integer is
      pragma Unreferenced (Type_Def, Value);
   begin
      return 0;
   end To_Long_Long;

   --  ------------------------------
   --  Check whether the object contains a value.
   --  Returns true if the object does not contain a value.
   --  ------------------------------
   function Is_Null (Value : in Object) return Boolean is
   begin
      return Value.V.Of_Type = TYPE_NULL;
   end Is_Null;

   --  ------------------------------
   --  Check whether the object is empty.
   --  If the object is null, returns true.
   --  If the object is the empty string, returns true.
   --  If the object is a list bean whose Get_Count is 0, returns true.
   --  Otherwise returns false.
   --  ------------------------------
   function Is_Empty (Value : in Object) return Boolean is
   begin
      return Value.Type_Def.Is_Empty (Value.V);
   end Is_Empty;

   function Get_Array_Bean (Value : in Object) return access Util.Beans.Basic.Array_Bean'Class is
      Bean : constant access Util.Beans.Basic.Readonly_Bean'Class := To_Bean (Value);
   begin
      if Bean = null or else not (Bean.all in Util.Beans.Basic.Array_Bean'Class) then
         return null;
      else
         return Util.Beans.Basic.Array_Bean'Class (Bean.all)'Unchecked_Access;
      end if;
   end Get_Array_Bean;

   --  ------------------------------
   --  Returns True if the object is an array.
   --  ------------------------------
   function Is_Array (Value : in Object) return Boolean is
   begin
      if Value.V.Of_Type = TYPE_ARRAY then
         return True;
      elsif Value.V.Of_Type /= TYPE_BEAN then
         return False;
      else
         return Get_Array_Bean (Value) /= null;
      end if;
   end Is_Array;

   --  ------------------------------
   --  Generic Object holding a value
   --  ------------------------------

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   function Get_Type_Name (Value : in Object) return String is
   begin
      return Value.Type_Def.Get_Name;
   end Get_Type_Name;

   --  ------------------------------
   --  Get a type identification for the object value.
   --  ------------------------------
   function Get_Type (Value : in Object) return Data_Type is
   begin
      return Value.V.Of_Type;
   end Get_Type;

   --  ------------------------------
   --  Get the type definition of the object value.
   --  ------------------------------
   function Get_Type (Value : Object) return Object_Type_Access is
   begin
      return Value.Type_Def;
   end Get_Type;

   --  ------------------------------
   --  Get the value identified by the name in the bean object.
   --  If the value object is not a bean, returns the null object.
   --  ------------------------------
   function Get_Value (Value : in Object;
                       Name  : in String) return Object is
      Bean : constant access Util.Beans.Basic.Readonly_Bean'Class := To_Bean (Value);
   begin
      if Bean = null then
         return Null_Object;
      else
         return Bean.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set into the target object a value identified by the name.
   --  The target object must be a <tt>Bean</tt> instance that implements the <tt>Set_Value</tt>
   --  procedure.  The operation does nothing if this is not the case.
   --  ------------------------------
   procedure Set_Value (Into  : in Object;
                        Name  : in String;
                        Value : in Object) is
      Bean : constant access Util.Beans.Basic.Readonly_Bean'Class := To_Bean (Into);
   begin
      if Bean /= null and then Bean.all in Util.Beans.Basic.Bean'Class then
         Util.Beans.Basic.Bean'Class (Bean.all).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Get the number of elements in the array object.
   --  Returns 0 if the object is not an array.
   --  ------------------------------
   function Get_Count (From : in Object) return Natural is
      use type Util.Beans.Basic.Array_Bean_Access;
      Bean : Util.Beans.Basic.Array_Bean_Access;
   begin
      if From.V.Of_Type = TYPE_ARRAY then
         return From.V.Array_Proxy.Len;
      elsif From.V.Of_Type /= TYPE_BEAN or else From.V.Proxy = null then
         return 0;
      else
         Bean := Get_Array_Bean (From);
         if Bean = null then
            return 0;
         else
            return Bean.Get_Count;
         end if;
      end if;
   end Get_Count;

   --  ------------------------------
   --  Get the array element at the given position.
   --  ------------------------------
   function Get_Value (From     : in Object;
                       Position : in Positive) return Object is
      use type Util.Beans.Basic.Array_Bean_Access;
      Bean : Util.Beans.Basic.Array_Bean_Access;
   begin
      if From.V.Of_Type = TYPE_BEAN then
         Bean := Get_Array_Bean (From);
         if Bean /= null then
            return Bean.Get_Row (Position);
         else
            return Null_Object;
         end if;
      elsif From.V.Of_Type /= TYPE_ARRAY then
         return Null_Object;
      elsif From.V.Array_Proxy.Len < Position then
         return Null_Object;
      else
         return From.V.Array_Proxy.Values (Position);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Convert the object to the corresponding type.
   --  ------------------------------
   function To_String (Value : Object) return String is
   begin
      return Value.Type_Def.To_String (Value.V);
   end To_String;

   --  ------------------------------
   --  Convert the object to a wide string.
   --  ------------------------------
   function To_Wide_Wide_String (Value : Object) return Wide_Wide_String is
   begin
      return Value.Type_Def.To_Wide_Wide_String (Value.V);
   end To_Wide_Wide_String;

   --  ------------------------------
   --  Convert the object to an unbounded string.
   --  ------------------------------
   function To_Unbounded_String (Value : Object) return Unbounded_String is
   begin
      case Value.V.Of_Type is
         when TYPE_STRING =>
            if Value.V.String_Proxy = null then
               return To_Unbounded_String ("null");
            end if;
            return To_Unbounded_String (Value.V.String_Proxy.Value);

         when others =>
            return To_Unbounded_String (To_String (Value));

      end case;
   end To_Unbounded_String;

   --  ------------------------------
   --  Convert the object to an unbounded wide string.
   --  ------------------------------
   function To_Unbounded_Wide_Wide_String (Value : Object) return Unbounded_Wide_Wide_String is
   begin
      case Value.V.Of_Type is
         when TYPE_WIDE_STRING =>
            if Value.V.Wide_Proxy = null then
               return To_Unbounded_Wide_Wide_String ("null");
            end if;
            return To_Unbounded_Wide_Wide_String (Value.V.Wide_Proxy.Value);

         when TYPE_STRING =>
            if Value.V.String_Proxy = null then
               return To_Unbounded_Wide_Wide_String ("null");
            end if;
            return To_Unbounded_Wide_Wide_String
              (UTF8_Decode (Value.V.String_Proxy.Value));

         when others =>
            return To_Unbounded_Wide_Wide_String (To_Wide_Wide_String (To_String (Value)));

      end case;
   end To_Unbounded_Wide_Wide_String;

   --  ------------------------------
   --  Convert the object to an integer.
   --  ------------------------------
   function To_Integer (Value : Object) return Integer is
   begin
      return Integer (Value.Type_Def.To_Long_Long (Value.V));
   end To_Integer;

   --  ------------------------------
   --  Convert the object to an integer.
   --  ------------------------------
   function To_Long_Integer (Value : Object) return Long_Integer is
   begin
      return Long_Integer (Value.Type_Def.To_Long_Long (Value.V));
   end To_Long_Integer;

   --  ------------------------------
   --  Convert the object to a long integer.
   --  ------------------------------
   function To_Long_Long_Integer (Value : Object) return Long_Long_Integer is
   begin
      return Value.Type_Def.To_Long_Long (Value.V);
   end To_Long_Long_Integer;

   --  ------------------------------
   --  Convert the object to a duration.
   --  ------------------------------
   function To_Duration (Value : in Object) return Duration is
   begin
      return Value.Type_Def.To_Duration (Value.V);
   end To_Duration;

   function To_Bean (Value : in Object) return access Util.Beans.Basic.Readonly_Bean'Class is
   begin
      if Value.V.Of_Type = TYPE_BEAN and then Value.V.Proxy /= null then
         return Value.V.Proxy.Bean;
      else
         return null;
      end if;
   end To_Bean;

   --  ------------------------------
   --  Convert the object to a boolean.
   --  ------------------------------
   function To_Boolean (Value : Object) return Boolean is
   begin
      return Value.Type_Def.To_Boolean (Value.V);
   end To_Boolean;

   --  ------------------------------
   --  Convert the object to a float.
   --  ------------------------------
   function To_Float (Value : Object) return Float is
   begin
      return Float (Value.Type_Def.To_Long_Float (Value.V));
   end To_Float;

   --  ------------------------------
   --  Convert the object to a long float.
   --  ------------------------------
   function To_Long_Float (Value : Object) return Long_Float is
   begin
      return Long_Float (Value.Type_Def.To_Long_Float (Value.V));
   end To_Long_Float;

   --  ------------------------------
   --  Convert the object to a long float.
   --  ------------------------------
   function To_Long_Long_Float (Value : Object) return Long_Long_Float is
   begin
      return Value.Type_Def.To_Long_Float (Value.V);
   end To_Long_Long_Float;

   --  ------------------------------
   --  Convert the object to a long float.
   --  ------------------------------
   function To_Blob (Value : Object) return Util.Blobs.Blob_Ref is
   begin
      if Value.V.Of_Type = TYPE_BLOB and then Value.V.Blob_Proxy /= null then
         return Value.V.Blob_Proxy.Blob;
      else
         return Util.Blobs.Null_Blob;
      end if;
   end To_Blob;

   --  ------------------------------
   --  Convert an integer into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Integer) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type   => TYPE_INTEGER,
                                          Int_Value => Long_Long_Integer (Value)),
                      Type_Def  => Integer_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert an integer into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Integer) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type   => TYPE_INTEGER,
                                          Int_Value => Long_Long_Integer (Value)),
                      Type_Def  => Integer_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert an integer into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Long_Integer) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type   => TYPE_INTEGER,
                                          Int_Value => Value),
                      Type_Def  => Integer_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a boolean into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Boolean) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type    => TYPE_BOOLEAN,
                                          Bool_Value => Value),
                      Type_Def   => Bool_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a float into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Float) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type     => TYPE_FLOAT,
                                          Float_Value => Long_Long_Float (Value)),
                      Type_Def    => Flt_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a long float into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Float) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type     => TYPE_FLOAT,
                                          Float_Value => Long_Long_Float (Value)),
                      Type_Def    => Flt_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a long long float into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Long_Long_Float) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type     => TYPE_FLOAT,
                                          Float_Value => Value),
                      Type_Def    => Flt_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a duration into a generic typed object.
   --  ------------------------------
   function To_Object (Value : in Duration) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type    => TYPE_TIME,
                                          Time_Value => Value),
                      Type_Def    => Duration_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : String) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type => TYPE_STRING,
                                          String_Proxy  => new String_Proxy '(Ref_Counter  => ONE,
                                                                        Len  => Value'Length,
                                                                        Value => Value)),
                      Type_Def     => Str_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a wide string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Wide_Wide_String) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type => TYPE_WIDE_STRING,
                                          Wide_Proxy => new Wide_String_Proxy
                                            '(Ref_Counter => ONE,
                                              Len => Value'Length,
                                              Value => Value)),
                      Type_Def          => WString_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert an unbounded string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Unbounded_String) return Object is
      Len : constant Natural := Length (Value);
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type => TYPE_STRING,
                                          String_Proxy => new String_Proxy
                                            '(Ref_Counter  => ONE,
                                              Len          => Len,
                                              Value        => To_String (Value))),
                      Type_Def => Str_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert a unbounded wide string into a generic typed object.
   --  ------------------------------
   function To_Object (Value : Unbounded_Wide_Wide_String) return Object is
      Len : constant Natural := Length (Value);
   begin
      return Object '(Controlled with
        V => Object_Value '(Of_Type => TYPE_WIDE_STRING,
                            Wide_Proxy   => new Wide_String_Proxy
                              '(Ref_Counter => ONE,
                                Len => Len,
                                Value => To_Wide_Wide_String (Value))),
        Type_Def          => WString_Type'Access);
   end To_Object;

   function To_Object (Value   : access Util.Beans.Basic.Readonly_Bean'Class;
                       Storage : in Storage_Type := DYNAMIC) return Object is
   begin
      if Value = null then
         return Object '(Controlled with
                         V => Object_Value '(Of_Type    => TYPE_BEAN,
                                             Proxy      => null),
                         Type_Def   => Bn_Type'Access);
      else
         return Object '(Controlled with
                         V => Object_Value '(Of_Type => TYPE_BEAN,
                                             Proxy   => new Bean_Proxy '(Ref_Counter => ONE,
                                                                         Bean    => Value,
                                                                         Storage => Storage)),
                         Type_Def   => Bn_Type'Access);
      end if;
   end To_Object;

   function To_Object (Value : in Object_Array) return Object is
   begin
      return Object '(Controlled with
                        V => Object_Value '(Of_Type => TYPE_ARRAY,
                                            Array_Proxy =>
                                               new Array_Proxy '(Ref_Counter => ONE,
                                                                 Len    => Value'Length,
                                                                 Count  => Value'Length,
                                                                 Values => Value)),
                      Type_Def => Ar_Type'Access);
   end To_Object;

   function To_Object (Value : in Util.Blobs.Blob_Ref) return Object is
   begin
      return Object '(Controlled with
                        V => Object_Value '(Of_Type => TYPE_BLOB,
                                            Blob_Proxy =>
                                               new Blob_Proxy '(Ref_Counter => ONE,
                                                                Blob => Value)),
                      Type_Def => Blob_Def'Access);
   end To_Object;

   --  ------------------------------
   --  Convert the object to an object of another time.
   --  Force the object to be an integer.
   --  ------------------------------
   function Cast_Integer (Value : Object) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type   => TYPE_INTEGER,
                                          Int_Value => Value.Type_Def.To_Long_Long (Value.V)),
                      Type_Def  => Integer_Type'Access);
   end Cast_Integer;

   --  ------------------------------
   --  Force the object to be a float.
   --  ------------------------------
   function Cast_Float (Value : Object) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type     => TYPE_FLOAT,
                                          Float_Value => Value.Type_Def.To_Long_Float (Value.V)),
                      Type_Def    => Flt_Type'Access);
   end Cast_Float;

   --  ------------------------------
   --  Convert the object to an object of another time.
   --  Force the object to be a duration.
   --  ------------------------------
   function Cast_Duration (Value : Object) return Object is
   begin
      return Object '(Controlled with
                      V => Object_Value '(Of_Type   => TYPE_TIME,
                                          Time_Value => Value.Type_Def.To_Duration (Value.V)),
                      Type_Def  => Duration_Type'Access);
   end Cast_Duration;

   --  ------------------------------
   --  Force the object to be a string.
   --  ------------------------------
   function Cast_String (Value : Object) return Object is
   begin
      if Value.V.Of_Type = TYPE_STRING or else Value.V.Of_Type = TYPE_WIDE_STRING then
         return Value;
      else
         return To_Object (To_Wide_Wide_String (Value));
      end if;
   end Cast_String;

   --  ------------------------------
   --  Find the best type to be used to compare two operands.
   --
   --  ------------------------------
   function Get_Compare_Type (Left, Right : Object) return Data_Type is
   begin
      --  Operands are of the same type.
      if Left.V.Of_Type = Right.V.Of_Type then
         return Left.V.Of_Type;
      end if;

      --  12 >= "23"
      --  if Left.Of_Type = TYPE_STRING or
      case Left.V.Of_Type is
         when TYPE_BOOLEAN =>
            case Right.V.Of_Type is
               when TYPE_INTEGER | TYPE_BOOLEAN | TYPE_TIME =>
                  return TYPE_INTEGER;

               when TYPE_FLOAT | TYPE_STRING | TYPE_WIDE_STRING =>
                  return Right.V.Of_Type;

               when others =>
                  null;
            end case;

         when TYPE_INTEGER =>
            case Right.V.Of_Type is
               when TYPE_BOOLEAN | TYPE_TIME =>
                  return TYPE_INTEGER;

               when TYPE_FLOAT =>
                  return TYPE_FLOAT;

               when others =>
                  null;
            end case;

         when TYPE_TIME =>
            case Right.V.Of_Type is
               when TYPE_INTEGER | TYPE_BOOLEAN | TYPE_FLOAT =>
                  return TYPE_INTEGER;

               when others =>
                  null;

            end case;

         when TYPE_FLOAT =>
            case Right.V.Of_Type is
               when TYPE_INTEGER | TYPE_BOOLEAN =>
                  return TYPE_FLOAT;

               when TYPE_TIME =>
                  return TYPE_INTEGER;

               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
      return TYPE_STRING;
   end Get_Compare_Type;

   --  ------------------------------
   --  Find the data type to be used for an arithmetic operation between two objects.
   --  ------------------------------
   function Get_Arithmetic_Type (Left, Right : Object) return Data_Type is
   begin
      if Left.V.Of_Type = TYPE_FLOAT or else Right.V.Of_Type = TYPE_FLOAT then
         return TYPE_FLOAT;
      end if;
      if Left.V.Of_Type = TYPE_INTEGER or else Right.V.Of_Type = TYPE_INTEGER then
         return TYPE_INTEGER;
      end if;
      if Left.V.Of_Type = TYPE_BOOLEAN and then Right.V.Of_Type = TYPE_BOOLEAN then
         return TYPE_BOOLEAN;
      end if;
      return TYPE_FLOAT;
   end Get_Arithmetic_Type;

   --  ------------------------------
   --  Find the data type to be used for a composition operation between two objects.
   --  ------------------------------
   function Get_Compose_Type (Left, Right : Object) return Data_Type is
   begin
      if Left.V.Of_Type = Right.V.Of_Type then
         return Left.V.Of_Type;
      end if;
      if Left.V.Of_Type = TYPE_FLOAT or else Right.V.Of_Type = TYPE_FLOAT then
         return TYPE_FLOAT;
      end if;
      if Left.V.Of_Type = TYPE_INTEGER or else Right.V.Of_Type = TYPE_INTEGER then
         return TYPE_INTEGER;
      end if;
      if Left.V.Of_Type = TYPE_TIME or else Right.V.Of_Type = TYPE_TIME then
         return TYPE_TIME;
      end if;
      if Left.V.Of_Type = TYPE_BOOLEAN and then Right.V.Of_Type = TYPE_BOOLEAN then
         return TYPE_BOOLEAN;
      end if;
      return TYPE_FLOAT;
   end Get_Compose_Type;

   --  ------------------------------
   --  Comparison of objects
   --  ------------------------------
   generic
      with function Int_Comparator (Left, Right : Long_Long_Integer) return Boolean;
      with function Time_Comparator (Left, Right : Duration) return Boolean;
      with function Boolean_Comparator (Left, Right : Boolean) return Boolean;
      with function Float_Comparator (Left, Right : Long_Long_Float) return Boolean;
      with function String_Comparator (Left, Right : String) return Boolean;
      with function Wide_String_Comparator (Left, Right : Wide_Wide_String)
                                    return Boolean;
   function Compare (Left, Right : Object) return Boolean;

   --  ------------------------------
   --  Comparison of objects
   --  ------------------------------
   function Compare (Left, Right : Object) return Boolean is
      T : constant Data_Type := Get_Compare_Type (Left, Right);
   begin
      case T is
         when TYPE_BOOLEAN =>
            return Boolean_Comparator (Left.Type_Def.To_Boolean (Left.V),
                                       Right.Type_Def.To_Boolean (Right.V));

         when TYPE_INTEGER =>
            return Int_Comparator (Left.Type_Def.To_Long_Long (Left.V),
                                   Right.Type_Def.To_Long_Long (Right.V));

         when TYPE_TIME =>
            return Time_Comparator (Left.Type_Def.To_Duration (Left.V),
                                    Right.Type_Def.To_Duration (Right.V));

         when TYPE_FLOAT =>
            return Float_Comparator (Left.Type_Def.To_Long_Float (Left.V),
                                     Right.Type_Def.To_Long_Float (Right.V));

         when TYPE_STRING =>
            return String_Comparator (To_String (Left), To_String (Right));

         when TYPE_WIDE_STRING =>
            return Wide_String_Comparator (To_Wide_Wide_String (Left),
                                           To_Wide_Wide_String (Right));

         when others =>
            return False;
      end case;
   end Compare;

   function ">" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => ">",
                                   Time_Comparator => ">",
                                   Boolean_Comparator => ">",
                                   Float_Comparator => ">",
                                   String_Comparator => ">",
                                   Wide_String_Comparator => ">");
   begin
      return Cmp (Left, Right);
   end ">";

   function "<" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => "<",
                                   Time_Comparator => "<",
                                   Boolean_Comparator => "<",
                                   Float_Comparator => "<",
                                   String_Comparator => "<",
                                   Wide_String_Comparator => "<");
   begin
      return Cmp (Left, Right);
   end "<";

   function "<=" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => "<=",
                                   Time_Comparator => "<=",
                                   Boolean_Comparator => "<=",
                                   Float_Comparator => "<=",
                                   String_Comparator => "<=",
                                   Wide_String_Comparator => "<=");
   begin
      return Cmp (Left, Right);
   end "<=";

   function ">=" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => ">=",
                                   Time_Comparator => ">=",
                                   Boolean_Comparator => ">=",
                                   Float_Comparator => ">=",
                                   String_Comparator => ">=",
                                   Wide_String_Comparator => ">=");
   begin
      return Cmp (Left, Right);
   end ">=";

   overriding
   function "=" (Left, Right : Object) return Boolean is
      function Cmp is new Compare (Int_Comparator => "=",
                                   Time_Comparator => "=",
                                   Boolean_Comparator => "=",
                                   Float_Comparator => "=",
                                   String_Comparator => "=",
                                   Wide_String_Comparator => "=");
   begin
      return Cmp (Left, Right);
   end "=";

   --  ------------------------------
   --  Arithmetic operations of objects
   --  ------------------------------
   generic
      with function Int_Operation (Left, Right : Long_Long_Integer)
                                    return Long_Long_Integer;
      with function Duration_Operation (Left, Right : Duration)
                                    return Duration;
      with function Float_Operation (Left, Right : Long_Long_Float)
                                    return Long_Long_Float;
   function Arith (Left, Right : Object) return Object;

   --  Comparison of objects
   function Arith (Left, Right : Object) return Object is
   begin
      --  If we have a time object, keep the time definition.
      if Left.V.Of_Type = TYPE_TIME then
         return Result : Object do
            Result.Type_Def := Left.Type_Def;
            Result.V := Object_Value
              '(Of_Type    => TYPE_TIME,
                Time_Value => Duration_Operation
                  (Left.Type_Def.To_Duration (Left.V),
                   Right.Type_Def.To_Duration (Right.V)));
         end return;
      end if;
      if Right.V.Of_Type = TYPE_TIME then
         return Result : Object do
            Result.Type_Def := Right.Type_Def;
            Result.V := Object_Value
              '(Of_Type    => TYPE_TIME,
                Time_Value => Duration_Operation (Left.Type_Def.To_Duration (Left.V),
                  Right.Type_Def.To_Duration (Right.V)));
         end return;
      end if;
      declare
         T : constant Data_Type := Get_Arithmetic_Type (Left, Right);
      begin
         case T is
         when TYPE_INTEGER =>
            return To_Object (Int_Operation (Left.Type_Def.To_Long_Long (Left.V),
              Right.Type_Def.To_Long_Long (Right.V)));

         when TYPE_FLOAT =>
            return To_Object (Float_Operation (Left.Type_Def.To_Long_Float (Left.V),
              Right.Type_Def.To_Long_Float (Right.V)));

         when others =>
            return Left;
         end case;
      end;
   end Arith;

   --  Arithmetic operations on objects
   function "+" (Left, Right : Object) return Object is
      function Operation is new Arith (Int_Operation => "+",
                                       Duration_Operation => "+",
                                       Float_Operation => "+");
   begin
      return Operation (Left, Right);
   end "+";

   function "-" (Left, Right : Object) return Object is
      function Operation is new Arith (Int_Operation => "-",
                                       Duration_Operation => "-",
                                       Float_Operation => "-");
   begin
      return Operation (Left, Right);
   end "-";

   function "-" (Left : Object) return Object is
   begin
      case Left.V.Of_Type is
         when TYPE_INTEGER =>
            return To_Object (-Left.Type_Def.To_Long_Long (Left.V));

         when TYPE_TIME =>
            return To_Object (-Left.Type_Def.To_Duration (Left.V));

         when TYPE_FLOAT =>
            return To_Object (-(Left.Type_Def.To_Long_Float (Left.V)));

         when others =>
            return Left;

      end case;
   end "-";

   function "*" (Left, Right : Object) return Object is
      function Operation is new Arith (Int_Operation => "*",
                                       Duration_Operation => "+",
                                       Float_Operation => "*");
   begin
      return Operation (Left, Right);
   end "*";

   function "/" (Left, Right : Object) return Object is
      function Operation is new Arith (Int_Operation => "/",
                                       Duration_Operation => "-",
                                       Float_Operation => "/");
   begin
      return Operation (Left, Right);
   end "/";

   function "mod" (Left, Right : Object) return Object is
      function "mod" (Left, Right : Long_Long_Float) return Long_Long_Float;

      function "mod" (Left, Right : Long_Long_Float) return Long_Long_Float is
         L : constant Long_Long_Integer := Long_Long_Integer (Left);
         R : constant Long_Long_Integer := Long_Long_Integer (Right);
      begin
         return Long_Long_Float (L mod R);
      end "mod";

      function Operation is new Arith (Int_Operation => "mod",
                                       Duration_Operation => "-",
                                       Float_Operation => "mod");
   begin
      return Operation (Left, Right);
   end "mod";

   function "&" (Left, Right : Object) return Object is
      T : constant Data_Type := Get_Compose_Type (Left, Right);
   begin
      case T is
         when TYPE_BOOLEAN =>
            return To_Object (To_Boolean (Left) and then To_Boolean (Right));

         when others =>
            return To_Object (To_String (Left) & To_String (Right));

      end case;
   end "&";

   overriding
   procedure Adjust (Obj : in out Object) is
   begin
      case Obj.V.Of_Type is
         when TYPE_BEAN =>
            if Obj.V.Proxy /= null then
               Util.Concurrent.Counters.Increment (Obj.V.Proxy.Ref_Counter);
            end if;

         when TYPE_ARRAY =>
            if Obj.V.Array_Proxy /= null then
               Util.Concurrent.Counters.Increment (Obj.V.Array_Proxy.Ref_Counter);
            end if;

         when TYPE_STRING =>
            if Obj.V.String_Proxy /= null then
               Util.Concurrent.Counters.Increment (Obj.V.String_Proxy.Ref_Counter);
            end if;

         when TYPE_WIDE_STRING =>
            if Obj.V.Wide_Proxy /= null then
               Util.Concurrent.Counters.Increment (Obj.V.Wide_Proxy.Ref_Counter);
            end if;

         when TYPE_BLOB =>
            if Obj.V.Blob_Proxy /= null then
               Util.Concurrent.Counters.Increment (Obj.V.Blob_Proxy.Ref_Counter);
            end if;

         when others =>
            null;

      end case;
   end Adjust;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Basic.Readonly_Bean'Class,
                                     Name   => Basic.Readonly_Bean_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Array_Proxy,
                                     Name   => Array_Proxy_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => String_Proxy,
                                     Name   => String_Proxy_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Wide_String_Proxy,
                                     Name   => Wide_String_Proxy_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Blob_Proxy,
                                     Name   => Blob_Proxy_Access);

   overriding
   procedure Finalize (Obj : in out Object) is
      Release : Boolean;
   begin
      case Obj.V.Of_Type is
         when TYPE_STRING =>
            if Obj.V.String_Proxy /= null then
               Util.Concurrent.Counters.Decrement (Obj.V.String_Proxy.Ref_Counter, Release);
               if Release then
                  Free (Obj.V.String_Proxy);
               else
                  Obj.V.String_Proxy := null;
               end if;
            end if;

         when TYPE_WIDE_STRING =>
            if Obj.V.Wide_Proxy /= null then
               Util.Concurrent.Counters.Decrement (Obj.V.Wide_Proxy.Ref_Counter, Release);
               if Release then
                  Free (Obj.V.Wide_Proxy);
               else
                  Obj.V.Wide_Proxy := null;
               end if;
            end if;

         when TYPE_BEAN =>
            if Obj.V.Proxy /= null then
               Util.Concurrent.Counters.Decrement (Obj.V.Proxy.Ref_Counter, Release);
               if Release then
                  Obj.V.Proxy.all.Release;
                  Free (Obj.V.Proxy);
               else
                  Obj.V.Proxy := null;
               end if;
            end if;

         when TYPE_ARRAY =>
            if Obj.V.Array_Proxy /= null then
               Util.Concurrent.Counters.Decrement (Obj.V.Array_Proxy.Ref_Counter, Release);
               if Release then
                  Free (Obj.V.Array_Proxy);
               else
                  Obj.V.Array_Proxy := null;
               end if;
            end if;

         when TYPE_BLOB =>
            if Obj.V.Blob_Proxy /= null then
               Util.Concurrent.Counters.Decrement (Obj.V.Blob_Proxy.Ref_Counter, Release);
               if Release then
                  Free (Obj.V.Blob_Proxy);
               else
                  Obj.V.Blob_Proxy := null;
               end if;
            end if;

         when others =>
            null;

      end case;
   end Finalize;

   --  ------------------------------
   --  Release the object pointed to by the proxy (if necessary).
   --  ------------------------------
   overriding
   procedure Release (P : in out Bean_Proxy) is
   begin
      if P.Storage = DYNAMIC and then P.Bean /= null then
         declare
            Bean : Basic.Readonly_Bean_Access := P.Bean.all'Access;
         begin
            P.Bean := null;
            Free (Bean);
         end;
      end if;
   end Release;

   overriding
   procedure Finalize (Proxy : in out Proxy_Iterator) is
      Release : Boolean;
   begin
      if Proxy.Proxy /= null then
         Util.Concurrent.Counters.Decrement (Proxy.Proxy.Ref_Counter, Release);
         if Release then
            Free (Proxy.Proxy);
         else
            Proxy.Proxy := null;
         end if;
      end if;
   end Finalize;

end Util.Beans.Objects;
