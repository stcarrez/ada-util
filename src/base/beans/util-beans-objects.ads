-----------------------------------------------------------------------
--  util-beans-objects -- Generic Typed Data Representation
--  Copyright (C) 2009, 2010, 2011, 2013, 2017, 2018, 2022 Stephane Carrez
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

--  == Objects ==
--  The `Util.Beans.Objects` package provides a data type to manage entities of different types
--  by using the same abstraction.  The `Object` type allows to hold various values of different
--  types.
--
--  An `Object` can hold one of the following values:
--
--    * a boolean,
--    * a long long integer,
--    * a date,
--    * a string,
--    * a wide wide string,
--    * an array of objects,
--    * a generic data bean,
--    * a map of objects,
--    * a vector of object
--
--  Several operations are provided to convert a value into an `Object`.
--
--    with Util.Beans.Objects;
--
--      Value : Util.Beans.Objects.Object
--         := Util.Beans.Objects.To_Object (String '("something"));
--      Value := Value + To_Object (String '("12"));
--      Value := Value - To_Object (Integer (3));
--
--  The package provides various operations to check, convert and use the `Object`
--  type.
--
--  | Name      | Description                              |
--  | --------- | ---------------------------------------- |
--  | Is_Empty  | Returns true if the object is the empty string or empty list |
--  | Is_Null   | Returns true if the object does not contain any value |
--  | Is_Array  | Returns true if the object is an array |
--  | Get_Type  | Get the type of the object |
--  | To_String | Converts the object to a string |
--  | To_Wide_Wide_String | Convert to a wide wide string |
--  | To_Unbounded_String | Convert to an unbounded string |
--  | To_Boolean | Convert to a boolean |
--  | To_Integer | Convert to an integer |
--  | To_Long_Integer | Convert to a long integer |
--  | To_Long_Long_Integer | Convert to a long long integer |
--  | To_Float   | Convert to a float |
--  | To_Long_Float | Convert to a long float |
--  | To_Long_Long_Float | Convert to a long long float |
--  | To_Duration  | Convert to a duration |
--  | To_Bean | Convert to an access to the Read_Only_Bean'Class |
--
--  Conversion to a time or enumeration is provided by specific packages.
--
--  The support for enumeration is made by the generic package
--  `Util.Beans.Objects.Enums` which must be instantiated with the enumeration
--  type.  Example of instantiation:
--
--     with Util.Beans.Objects.Enum;
--     ...
--        type Color_Type is (GREEN, BLUE, RED, BROWN);
--        package Color_Enum is
--           new Util.Beans.Objects.Enum (Color_Type);
--
--  Then, two functions are available to convert the enum value into an `Object`
--  or convert back the `Object` in the enum value:
--
--     Color : Object := Color_Enum.To_Object (BLUE);
--     C : Color_Type := Color_Enum.To_Value (Color);
--
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Finalization;
with Util.Blobs;
private with Ada.Unchecked_Deallocation;
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
                      --  The object holds a date and time.
                      TYPE_TIME,
                      --  The object holds some record object.
                      TYPE_RECORD,
                      --  The object holds a string.
                      TYPE_STRING,
                      --  The object holds a wide wide string.
                      TYPE_WIDE_STRING,
                      --  The object holds an array of objects.
                      TYPE_ARRAY,
                      --  The object holds an binary blob.
                      TYPE_BLOB,
                      --  The object holds a generic bean.
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

   --  An array of objects.
   type Object_Array is array (Positive range <>) of Object;

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

   --  Returns True if the object is an array.
   function Is_Array (Value : in Object) return Boolean;

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

   --  Set into the target object a value identified by the name.
   --  The target object must be a <tt>Bean</tt> instance that implements the <tt>Set_Value</tt>
   --  procedure.  The operation does nothing if this is not the case.
   procedure Set_Value (Into  : in Object;
                        Name  : in String;
                        Value : in Object);

   --  Get the number of elements in the array object.
   --  Returns 0 if the object is not an array.
   function Get_Count (From : in Object) return Natural;

   --  Get the array element at the given position.
   function Get_Value (From     : in Object;
                       Position : in Positive) return Object;

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
   function To_Blob (Value : in Object) return Util.Blobs.Blob_Ref;

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
   function To_Object (Value : in Object_Array) return Object;
   function To_Object (Value : in Util.Blobs.Blob_Ref) return Object;

   --  Create an object that refers to the bean object.  With the storage type
   --  <b>DYNAMIC</b>, the default, the bean object will be freed when there is
   --  no <b>Object</b> that refers to that bean.  With <b>STATIC</b>, the bean
   --  is a static bean and it will not be freed automatically.
   function To_Object (Value   : access Util.Beans.Basic.Readonly_Bean'Class;
                       Storage : in Storage_Type := DYNAMIC) return Object;

   --  Comparison of objects
   function "<" (Left, Right : Object) return Boolean;
   function "<=" (Left, Right : Object) return Boolean;
   function ">" (Left, Right : Object) return Boolean;
   function ">=" (Left, Right : Object) return Boolean;
   overriding
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
   overriding
   function To_Wide_Wide_String (Type_Def : in Basic_Type;
                                 Value    : in Object_Value) return Wide_Wide_String;

   --  Convert the value into an integer.
   overriding
   function To_Long_Long (Type_Def : in Basic_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   overriding
   function To_Long_Float (Type_Def : in Basic_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   overriding
   function To_Boolean (Type_Def : in Basic_Type;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   overriding
   function To_Duration (Type_Def : in Basic_Type;
                         Value    : in Object_Value) return Duration;

   --  Returns False
   overriding
   function Is_Empty (Type_Def : in Basic_Type;
                      Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Null Type
   --  ------------------------------
   type Null_Type is new Basic_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : Null_Type) return String;

   --  Get the base data type.
   overriding
   function Get_Data_Type (Type_Def : Null_Type) return Data_Type;

   --  Convert the value into a string.
   overriding
   function To_String (Type_Def : in Null_Type;
                       Value    : in Object_Value) return String;

   --  Returns True
   overriding
   function Is_Empty (Type_Def : in Null_Type;
                      Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Integer Type
   --  ------------------------------
   type Int_Type is new Basic_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : Int_Type) return String;

   --  Get the base data type.
   overriding
   function Get_Data_Type (Type_Def : Int_Type) return Data_Type;

   overriding
   function To_String (Type_Def : in Int_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   overriding
   function To_Long_Long (Type_Def : in Int_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   overriding
   function To_Long_Float (Type_Def : in Int_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   overriding
   function To_Boolean (Type_Def : in Int_Type;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   overriding
   function To_Duration (Type_Def : in Int_Type;
                         Value    : in Object_Value) return Duration;

   --  ------------------------------
   --  Float Type
   --  ------------------------------
   type Float_Type is new Basic_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : in Float_Type) return String;

   --  Get the base data type.
   overriding
   function Get_Data_Type (Type_Def : in Float_Type) return Data_Type;

   --  Convert the value into a string.
   overriding
   function To_String (Type_Def : in Float_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   overriding
   function To_Long_Long (Type_Def : in Float_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   overriding
   function To_Long_Float (Type_Def : in Float_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   overriding
   function To_Boolean (Type_Def : in Float_Type;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   overriding
   function To_Duration (Type_Def : in Float_Type;
                         Value    : in Object_Value) return Duration;

   --  ------------------------------
   --  String Type
   --  ------------------------------
   type String_Type is new Basic_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : in String_Type) return String;

   --  Get the base data type.
   overriding
   function Get_Data_Type (Type_Def : in String_Type) return Data_Type;

   --  Convert the value into a string.
   overriding
   function To_String (Type_Def : in String_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   overriding
   function To_Long_Long (Type_Def : in String_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   overriding
   function To_Long_Float (Type_Def : in String_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   overriding
   function To_Boolean (Type_Def : in String_Type;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   overriding
   function To_Duration (Type_Def : in String_Type;
                         Value    : in Object_Value) return Duration;

   --  Returns True if the value is empty.
   overriding
   function Is_Empty (Type_Def : in String_Type;
                      Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Wide String Type
   --  ------------------------------
   type Wide_String_Type is new Basic_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : in Wide_String_Type) return String;

   --  Get the base data type.
   overriding
   function Get_Data_Type (Type_Def : in Wide_String_Type) return Data_Type;

   --  Convert the value into a string.
   overriding
   function To_String (Type_Def : in Wide_String_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into a wide string.
   overriding
   function To_Wide_Wide_String (Type_Def : in Wide_String_Type;
                                 Value    : in Object_Value) return Wide_Wide_String;

   --  Convert the value into an integer.
   overriding
   function To_Long_Long (Type_Def : in Wide_String_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   overriding
   function To_Long_Float (Type_Def : in Wide_String_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   overriding
   function To_Boolean (Type_Def : in Wide_String_Type;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   overriding
   function To_Duration (Type_Def : in Wide_String_Type;
                         Value    : in Object_Value) return Duration;

   --  Returns True if the value is empty.
   overriding
   function Is_Empty (Type_Def : in Wide_String_Type;
                      Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Boolean Type
   --  ------------------------------
   type Boolean_Type is new Basic_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : in Boolean_Type) return String;

   --  Get the base data type.
   overriding
   function Get_Data_Type (Type_Def : in Boolean_Type) return Data_Type;

   --  Convert the value into a string.
   overriding
   function To_String (Type_Def : in Boolean_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   overriding
   function To_Long_Long (Type_Def : in Boolean_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   overriding
   function To_Long_Float (Type_Def : in Boolean_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   overriding
   function To_Boolean (Type_Def : in Boolean_Type;
                        Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Duration Type
   --  ------------------------------
   type Duration_Type_Def is new Basic_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : in Duration_Type_Def) return String;

   --  Get the base data type.
   overriding
   function Get_Data_Type (Type_Def : in Duration_Type_Def) return Data_Type;

   --  Convert the value into a string.
   overriding
   function To_String (Type_Def : in Duration_Type_Def;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   overriding
   function To_Long_Long (Type_Def : in Duration_Type_Def;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   overriding
   function To_Long_Float (Type_Def : in Duration_Type_Def;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   overriding
   function To_Boolean (Type_Def : in Duration_Type_Def;
                        Value    : in Object_Value) return Boolean;

   --  Convert the value into a duration.
   overriding
   function To_Duration (Type_Def : in Duration_Type_Def;
                         Value    : in Object_Value) return Duration;

   --  ------------------------------
   --  Bean Type
   --  ------------------------------
   type Bean_Type is new Basic_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : in Bean_Type) return String;

   --  Get the base data type.
   overriding
   function Get_Data_Type (Type_Def : in Bean_Type) return Data_Type;

   --  Convert the value into a string.
   overriding
   function To_String (Type_Def : in Bean_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   overriding
   function To_Long_Long (Type_Def : in Bean_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   overriding
   function To_Long_Float (Type_Def : in Bean_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   overriding
   function To_Boolean (Type_Def : in Bean_Type;
                        Value    : in Object_Value) return Boolean;

   --  Returns True if the value is empty.
   overriding
   function Is_Empty (Type_Def : in Bean_Type;
                      Value    : in Object_Value) return Boolean;

   --  ------------------------------
   --  Array Type
   --  ------------------------------
   type Array_Type is new Basic_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : in Array_Type) return String;

   --  Get the base data type.
   overriding
   function Get_Data_Type (Type_Def : in Array_Type) return Data_Type;

   --  Convert the value into a string.
   overriding
   function To_String (Type_Def : in Array_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   overriding
   function To_Long_Long (Type_Def : in Array_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   overriding
   function To_Long_Float (Type_Def : in Array_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   overriding
   function To_Boolean (Type_Def : in Array_Type;
                        Value    : in Object_Value) return Boolean;

   --  Returns True if the value is empty.
   overriding
   function Is_Empty (Type_Def : in Array_Type;
                      Value    : in Object_Value) return Boolean;

   function Get_Array_Bean (Value : in Object) return access Util.Beans.Basic.Array_Bean'Class;

   --  ------------------------------
   --  Blob Type
   --  ------------------------------
   type Blob_Type is new Basic_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : in Blob_Type) return String;

   --  Get the base data type.
   overriding
   function Get_Data_Type (Type_Def : in Blob_Type) return Data_Type;

   --  Convert the value into a string.
   overriding
   function To_String (Type_Def : in Blob_Type;
                       Value    : in Object_Value) return String;

   --  Convert the value into an integer.
   overriding
   function To_Long_Long (Type_Def : in Blob_Type;
                          Value    : in Object_Value) return Long_Long_Integer;

   --  Convert the value into a float.
   overriding
   function To_Long_Float (Type_Def : in Blob_Type;
                           Value    : in Object_Value) return Long_Long_Float;

   --  Convert the value into a boolean.
   overriding
   function To_Boolean (Type_Def : in Blob_Type;
                        Value    : in Object_Value) return Boolean;

   --  Returns True if the value is empty.
   overriding
   function Is_Empty (Type_Def : in Blob_Type;
                      Value    : in Object_Value) return Boolean;

   subtype Proxy_Data_Type is Data_Type range TYPE_STRING .. TYPE_BEAN;

   type Proxy is tagged limited record
      Ref_Counter : Util.Concurrent.Counters.Counter;
   end record;

   --  Release the object pointed to by the proxy (if necessary).
   procedure Release (P : in out Proxy) is null;

   type Proxy_Access is access all Proxy'Class;

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
   type Bean_Proxy_Access is access all Bean_Proxy'Class;

   type Array_Proxy (Len : Natural) is new Proxy with record
      Count  : Natural := 0;
      Values : Object_Array (1 .. Len);
   end record;
   type Array_Proxy_Access is access all Array_Proxy;

   type Blob_Proxy is new Proxy with record
      Blob : Util.Blobs.Blob_Ref;
   end record;
   type Blob_Proxy_Access is access all Blob_Proxy;

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

         when TYPE_ARRAY =>
            Array_Proxy : Array_Proxy_Access;

         when TYPE_BLOB =>
            Blob_Proxy : Blob_Proxy_Access;

         when TYPE_RECORD =>
            Record_Proxy : Proxy_Access;

         when TYPE_BEAN =>
            Proxy : Bean_Proxy_Access;

      end case;
   end record;

   No_Type     : aliased constant Null_Type := Null_Type '(null record);

   Null_Value  : constant Object_Value := Object_Value '(Of_Type => TYPE_NULL);

   type Object is new Controlled with record
      Type_Def : Object_Type_Access := No_Type'Access;
      V        : Object_Value := Null_Value;
   end record;

   overriding
   procedure Adjust (Obj : in out Object);

   overriding
   procedure Finalize (Obj : in out Object);

   type Proxy_Iterator is abstract new Limited_Controlled with record
      Ref_Counter : Util.Concurrent.Counters.Counter;
      Proxy       : Proxy_Access;
   end record;
   type Proxy_Iterator_Access is access all Proxy_Iterator'Class;

   overriding
   procedure Finalize (Proxy : in out Proxy_Iterator);

   function Is_Empty (Iter : in Proxy_Iterator) return Boolean is abstract;

   procedure Next (Iter : in out Proxy_Iterator) is abstract;

   procedure Previous (Iter : in out Proxy_Iterator) is abstract;

   function Element (Iter : in Proxy_Iterator) return Object'Class is abstract;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Bean_Proxy'Class,
                                     Name   => Bean_Proxy_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Proxy'Class,
                                     Name   => Proxy_Access);

   Null_Object : constant Object := Object '(Controlled with
                                             V        => Object_Value '(Of_Type => TYPE_NULL),
                                             Type_Def => No_Type'Access);

end Util.Beans.Objects;
