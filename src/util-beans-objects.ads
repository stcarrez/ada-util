-----------------------------------------------------------------------
--  Util.Beans.Objects -- Generic Typed Data Representation
--  Copyright (C) 2009, 2010, 2011, 2013 Stephane Carrez
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

--  Provides a data type to manage entities of different types using the
--  same abstraction.
--
--  An ''Object' can hold one of the following values:
--   o a boolean
--   o a long long integer
--   o a date
--   o a string
--   o a wide wide string
--   o a generic data
--
--
--  Value : Object := To_Object ("something");
--  Value := Value + To_Object ("12");
--
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Finalization;
private with Util.Concurrent.Counters;
limited with Util.Beans.Basic;
package Util.Beans.Objects is

   pragma Preelaborate;

   use Ada.Strings.Unbounded;
   use Ada.Strings.Wide_Wide_Unbounded;

   --  Exception raised when an object cannot be converted to a given type.
   Conversion_Error : exception;

   type Data_Type is (TYPE_NULL,
                      --  The object holds a boolean value.
                      TYPE_BOOLEAN,
                      --  The object holds an integer value (64-bits).
                      TYPE_INTEGER,
                      --  The object holds a floating point value.
                      TYPE_FLOAT,
                      --  The object holds a date and time
                      TYPE_TIME,
                      --  The object holds a string
                      TYPE_STRING,
                      --  The object holds a wide wide string
                      TYPE_WIDE_STRING,
                      --  The object holds a generic bean
                      TYPE_BEAN);

   type Storage_Type is (STATIC, DYNAMIC);

   --  Exception raised when the value identified by a name is not
   --  recognized.
   No_Value : exception;

   --  ------------------------------
   --  Generic Object holding a value
   --  ------------------------------
   --  The object has a type represented by 'Object_Type'.
   --  It can hold any value while being tightly coupled with a type.
   --  The object can be converted to standard Ada types.
   type Object is private;
   type Object_Value is private;

   --  The null object.
   Null_Object : constant Object;

   --  ------------------------------
   --  Type definition
   --  ------------------------------
   --  The Object_Type describes a type.  It serves as a basis
   --  for type conversion.
   type Object_Type is limited interface;
   type Object_Type_Access is not null access constant Object_Type'Class;

   --  Get the type name
   function Get_Name (Type_Def : Object_Type) return String is abstract;

   --  Get the base data type.
   function Get_Data_Type (Type_Def : Object_Type) return Data_Type is abstract;

   --  Convert the value into a string.
   function To_String (Type_Def : in Object_Type;
                       Value    : in Object_Value) return String is abstract;

   --  Convert the value into a wide string.
   function To_Wide_Wide_String (Type_Def : in Object_Type;
                                 Value    : in Object_Value) return Wide_Wide_String is abstract;

   --  Convert the value into an integer.
   function To_Long_Long (Type_Def : in Object_Type;
                          Value    : in Object_Value) return Long_Long_Integer is abstract;

   --  Convert the value into a float.
   function To_Long_Float (Type_Def : in Object_Type;
                           Value    : in Object_Value) return Long_Long_Float is abstract;

   --  Convert the value into a boolean.
   function To_Boolean (Type_Def : in Object_Type;
                        Value    : in Object_Value) return Boolean is abstract;

   --  Convert the value into a duration.
   function To_Duration (Type_Def : in Object_Type;
                         Value    : in Object_Value) return Duration is abstract;

   --  Returns True if the value is empty.
   function Is_Empty (Type_Def : in Object_Type;
                      Value    : in Object_Value) return Boolean is abstract;

   --  ------------------------------
   --  Generic Object holding a value
   --  ------------------------------

   --  Check whether the object contains a value.
   --  Returns true if the object does not contain a value.
   function Is_Null (Value : in Object) return Boolean;

   --  Check whether the object is empty.
   --  If the object is null, returns true.
   --  If the object is the empty string, returns true.
   --  If the object is a list bean whose Get_Count is 0, returns true.
   --  Otherwise returns false.
   function Is_Empty (Value : in Object) return Boolean;

--     function Is_Constant (Value : in Object) return Boolean;
   --  Get a type identification for the object value.
   function Get_Type (Value : in Object) return Data_Type;

   --  Get the type definition of the object value.
   function Get_Type (Value : in Object) return Object_Type_Access;

   --  Get the type name of this object.
   function Get_Type_Name (Value : Object) return String;

   --  Get the value identified by the name in the bean object.
   --  If the value object is not a bean, returns the null object.
   function Get_Value (Value : in Object;
                       Name  : in String) return Object;

   --  Convert the object to the corresponding type.
   function To_String (Value : in Object) return String;
   function To_Wide_Wide_String (Value : in Object) return Wide_Wide_String;
   function To_Unbounded_String (Value : in Object) return Unbounded_String;
   function To_Unbounded_Wide_Wide_String (Value : in Object) return Unbounded_Wide_Wide_String;
   function To_Integer (Value : in Object) return Integer;
   function To_Boolean (Value : in Object) return Boolean;
   function To_Long_Integer (Value : in Object) return Long_Integer;
   function To_Long_Long_Integer (Value : in Object) return Long_Long_Integer;
   function To_Float (Value : in Object) return Float;
   function To_Long_Float (Value : in Object) return Long_Float;
   function To_Long_Long_Float (Value : in Object) return Long_Long_Float;
   function To_Duration (Value : in Object) return Duration;

   function To_Bean (Value : in Object) return access Util.Beans.Basic.Readonly_Bean'Class;

   --  Convert the object to an object of another time.
   --  Force the object to be an integer.
   function Cast_Integer (Value : Object) return Object;

   --  Force the object to be a float.
   function Cast_Float (Value : Object) return Object;

   --  Force the object to be a duration.
   function Cast_Duration (Value : Object) return Object;

   --  Force the object to be a string.
   function Cast_String (Value : Object) return Object;

   --  Convert a value to a generic typed object.
   function To_Object (Value : in Integer) return Object;
   function To_Object (Value : in Long_Integer) return Object;
   function To_Object (Value : in Long_Long_Integer) return Object;
   function To_Object (Value : in Float) return Object;
   function To_Object (Value : in Long_Float) return Object;
   function To_Object (Value : in Long_Long_Float) return Object;
   function To_Object (Value : in String) return Object;
   function To_Object (Value : in Wide_Wide_String) return Object;
   function To_Object (Value : in Unbounded_String) return Object;
   function To_Object (Value : in Unbounded_Wide_Wide_String) return Object;
   function To_Object (Value : in Boolean) return Object;
   function To_Object (Value : in Duration) return Object;

   --  Create an object that refers to the bean object.  With the storage type
   --  <b>DYNAMIC</b>, the default, the bean object will be freed when there is
   --  no <b>Object</b> that refers to that bean.  With <b>STATIC</b>, the bean
   --  is a static bean and it will not be freed automaticaly.
   function To_Object (Value   : access Util.Beans.Basic.Readonly_Bean'Class;
                       Storage : in Storage_Type := DYNAMIC) return Object;

   --  Comparison of objects
   function "<" (Left, Right : Object) return Boolean;
   function "<=" (Left, Right : Object) return Boolean;
   function ">" (Left, Right : Object) return Boolean;
   function ">=" (Left, Right : Object) return Boolean;
   function "=" (Left, Right : Object) return Boolean;

   --  Arithmetic operations on objects
   function "+" (Left, Right : Object) return Object;
   function "-" (Left, Right : Object) return Object;
   function "*" (Left, Right : Object) return Object;
   function "/" (Left, Right : Object) return Object;
   function "&" (Left, Right : Object) return Object;
   function "mod" (Left, Right : Object) return Object;
   function "-" (Left : Object) return Object;

private

   use Ada.Finalization;

   type Name_Access is access constant String;

   type Basic_Type is abstract limited new Object_Type with null record;

   --  Convert the value into a wide string.
   function To_Wide_Wide_String (Type_Def : in Basic_Type;
                                 Value    : in Object_Value) return Wide_Wide_String;

   --  Convert the value into an integer.
   function To_Long_Long (Type_Def : in Basic_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   function To_Long_Float (Type_Def : in Basic_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   function To_Boolean (Type_Def : in Basic_Type;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   function To_Duration (Type_Def : in Basic_Type;
                         Value    : in Object_Value) return Duration;

   --  Returns False
   function Is_Empty (Type_Def : in Basic_Type;
                      Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Null Type
   --  ------------------------------
   type Null_Type is new Basic_Type with null record;

   --  Get the type name
   function Get_Name (Type_Def : Null_Type) return String;

   --  Get the base data type.
   function Get_Data_Type (Type_Def : Null_Type) return Data_Type;

   --  Convert the value into a string.
   function To_String (Type_Def : in Null_Type;
                       Value    : in Object_Value) return String;

   --  Returns True
   function Is_Empty (Type_Def : in Null_Type;
                      Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Integer Type
   --  ------------------------------
   type Int_Type is new Basic_Type with null record;

   --  Get the type name
   function Get_Name (Type_Def : Int_Type) return String;

   --  Get the base data type.
   function Get_Data_Type (Type_Def : Int_Type) return Data_Type;

   function To_String (Type_Def : in Int_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   function To_Long_Long (Type_Def : in Int_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   function To_Long_Float (Type_Def : in Int_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   function To_Boolean (Type_Def : in Int_Type;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   function To_Duration (Type_Def : in Int_Type;
                         Value    : in Object_Value) return Duration;

   --  ------------------------------
   --  Float Type
   --  ------------------------------
   type Float_Type is new Basic_Type with null record;

   --  Get the type name
   function Get_Name (Type_Def : in Float_Type) return String;

   --  Get the base data type.
   function Get_Data_Type (Type_Def : in Float_Type) return Data_Type;

   --  Convert the value into a string.
   function To_String (Type_Def : in Float_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   function To_Long_Long (Type_Def : in Float_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   function To_Long_Float (Type_Def : in Float_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   function To_Boolean (Type_Def : in Float_Type;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   function To_Duration (Type_Def : in Float_Type;
                         Value    : in Object_Value) return Duration;

   --  ------------------------------
   --  String Type
   --  ------------------------------
   type String_Type is new Basic_Type with null record;

   --  Get the type name
   function Get_Name (Type_Def : in String_Type) return String;

   --  Get the base data type.
   function Get_Data_Type (Type_Def : in String_Type) return Data_Type;

   --  Convert the value into a string.
   function To_String (Type_Def : in String_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   function To_Long_Long (Type_Def : in String_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   function To_Long_Float (Type_Def : in String_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   function To_Boolean (Type_Def : in String_Type;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   function To_Duration (Type_Def : in String_Type;
                         Value    : in Object_Value) return Duration;

   --  Returns True if the value is empty.
   function Is_Empty (Type_Def : in String_Type;
                      Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Wide String Type
   --  ------------------------------
   type Wide_String_Type is new Basic_Type with null record;

   --  Get the type name
   function Get_Name (Type_Def : in Wide_String_Type) return String;

   --  Get the base data type.
   function Get_Data_Type (Type_Def : in Wide_String_Type) return Data_Type;

   --  Convert the value into a string.
   function To_String (Type_Def : in Wide_String_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into a wide string.
   function To_Wide_Wide_String (Type_Def : in Wide_String_Type;
                                 Value    : in Object_Value) return Wide_Wide_String;

   --  Convert the value into an integer.
   function To_Long_Long (Type_Def : in Wide_String_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   function To_Long_Float (Type_Def : in Wide_String_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   function To_Boolean (Type_Def : in Wide_String_Type;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   function To_Duration (Type_Def : in Wide_String_Type;
                         Value    : in Object_Value) return Duration;

   --  Returns True if the value is empty.
   function Is_Empty (Type_Def : in Wide_String_Type;
                      Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Boolean Type
   --  ------------------------------
   type Boolean_Type is new Basic_Type with null record;

   --  Get the type name
   function Get_Name (Type_Def : in Boolean_Type) return String;

   --  Get the base data type.
   function Get_Data_Type (Type_Def : in Boolean_Type) return Data_Type;

   --  Convert the value into a string.
   function To_String (Type_Def : in Boolean_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   function To_Long_Long (Type_Def : in Boolean_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   function To_Long_Float (Type_Def : in Boolean_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   function To_Boolean (Type_Def : in Boolean_Type;
                        Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Duration Type
   --  ------------------------------
   type Duration_Type_Def is new Basic_Type with null record;

   --  Get the type name
   function Get_Name (Type_Def : in Duration_Type_Def) return String;

   --  Get the base data type.
   function Get_Data_Type (Type_Def : in Duration_Type_Def) return Data_Type;

   --  Convert the value into a string.
   function To_String (Type_Def : in Duration_Type_Def;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   function To_Long_Long (Type_Def : in Duration_Type_Def;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   function To_Long_Float (Type_Def : in Duration_Type_Def;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   function To_Boolean (Type_Def : in Duration_Type_Def;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   function To_Duration (Type_Def : in Duration_Type_Def;
                         Value    : in Object_Value) return Duration;

   --  ------------------------------
   --  Bean Type
   --  ------------------------------
   type Bean_Type is new Basic_Type with null record;

   --  Get the type name
   function Get_Name (Type_Def : in Bean_Type) return String;

   --  Get the base data type.
   function Get_Data_Type (Type_Def : in Bean_Type) return Data_Type;

   --  Convert the value into a string.
   function To_String (Type_Def : in Bean_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   function To_Long_Long (Type_Def : in Bean_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   function To_Long_Float (Type_Def : in Bean_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   function To_Boolean (Type_Def : in Bean_Type;
                        Value    : in Object_Value) return Boolean;

   --  Returns True if the value is empty.
   function Is_Empty (Type_Def : in Bean_Type;
                      Value    : in Object_Value) return Boolean;

   subtype Proxy_Data_Type is Data_Type range TYPE_STRING .. TYPE_BEAN;

   type Proxy is tagged limited record
      Ref_Counter : Util.Concurrent.Counters.Counter;
   end record;

   --  Release the object pointed to by the proxy (if necessary).
   procedure Release (P : in out Proxy) is null;

   type Bean_Proxy_Access is access all Proxy'Class;

   type String_Proxy (Len : Natural) is new Proxy with record
      Value : String (1 .. Len);
   end record;
   type String_Proxy_Access is access all String_Proxy;

   type Wide_String_Proxy (Len : Natural) is new Proxy with record
      Value : Wide_Wide_String (1 .. Len);
   end record;
   type Wide_String_Proxy_Access is access all Wide_String_Proxy;

   type Bean_Proxy is new Proxy with record
      Bean    : access Util.Beans.Basic.Readonly_Bean'Class;
      Storage : Storage_Type;
   end record;

   --  Release the object pointed to by the proxy (if necessary).
   overriding
   procedure Release (P : in out Bean_Proxy);

   type Object_Value (Of_Type : Data_Type := TYPE_NULL) is record
      case Of_Type is
         when TYPE_NULL =>
            null;

            --  Integers and enums are stored as 64-bit integer.
         when TYPE_INTEGER =>
            Int_Value : Long_Long_Integer;

         when TYPE_BOOLEAN =>
            Bool_Value : Boolean;

         when TYPE_FLOAT =>
            Float_Value : Long_Long_Float;

         when TYPE_TIME =>
            Time_Value  : Duration;

         when TYPE_STRING =>
            String_Proxy : String_Proxy_Access;

         when TYPE_WIDE_STRING =>
            Wide_Proxy : Wide_String_Proxy_Access;

         when TYPE_BEAN =>
            Proxy : Bean_Proxy_Access;

      end case;
   end record;

   No_Type     : aliased constant Null_Type := Null_Type '(others => <>);

   Null_Value  : constant Object_Value := Object_Value '(Of_Type => TYPE_NULL);

   type Object is new Controlled with record
      Type_Def : Object_Type_Access := No_Type'Access;
      V        : Object_Value := Null_Value;
   end record;

   overriding
   procedure Adjust (Obj : in out Object);

   overriding
   procedure Finalize (Obj : in out Object);

   Null_Object : constant Object := Object '(Controlled with
                                             V        => Object_Value '(Of_Type => TYPE_NULL),
                                             Type_Def => No_Type'Access);

end Util.Beans.Objects;
