-----------------------------------------------------------------------
--  util-beans-objects-enums -- Helper conversion for discrete types
--  Copyright (C) 2010, 2016, 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Characters.Conversions;
package body Util.Beans.Objects.Enums is

   use Ada.Characters.Conversions;

   Value_Range : constant Long_Long_Integer := T'Pos (T'Last) - T'Pos (T'First) + 1;

   --  ------------------------------
   --  Integer Type
   --  ------------------------------
   type Enum_Type is new Int_Type with null record;

   --  Get the type name
   overriding
   function Get_Name (Type_Def : in Enum_Type) return String;

   overriding
   function To_String (Type_Def : in Enum_Type;
                       Value    : in Object_Value) return String;

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : Enum_Type) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Enum";
   end Get_Name;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
   function To_String (Type_Def : in Enum_Type;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);
   begin
      return T'Image (T'Val (Value.Int_Value));
   end To_String;

   Value_Type  : aliased constant Enum_Type := Enum_Type '(null record);

   --  ------------------------------
   --  Create an object from the given value.
   --  ------------------------------
   function To_Object (Value : in T) return Object is
   begin
      return Object '(Controlled with
        V => Object_Value '(Of_Type   => TYPE_INTEGER,
                            Int_Value => Long_Long_Integer (T'Pos (Value))),
        Type_Def  => Value_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert the object into a value.
   --  Raises Constraint_Error if the object cannot be converter to the target type.
   --  ------------------------------
   function To_Value (Value : in Util.Beans.Objects.Object) return T is
   begin
      case Value.V.Of_Type is
         when TYPE_INTEGER =>
            if ROUND_VALUE then
               return T'Val (Value.V.Int_Value mod Value_Range);
            else
               return T'Val (Value.V.Int_Value);
            end if;

         when TYPE_BOOLEAN =>
            return T'Val (Boolean'Pos (Value.V.Bool_Value));

         when TYPE_FLOAT =>
            if ROUND_VALUE then
               return T'Val (To_Long_Long_Integer (Value) mod Value_Range);
            else
               return T'Val (To_Long_Long_Integer (Value));
            end if;

         when TYPE_STRING =>
            if Value.V.String_Proxy = null then
               raise Constraint_Error with "The object value is null";
            end if;
            return T'Value (Value.V.String_Proxy.Value);

         when TYPE_WIDE_STRING =>
            if Value.V.Wide_Proxy = null then
               raise Constraint_Error with "The object value is null";
            end if;
            return T'Value (To_String (Value.V.Wide_Proxy.Value));

         when TYPE_NULL =>
            raise Constraint_Error with "The object value is null";

         when TYPE_TIME =>
            raise Constraint_Error with "Cannot convert a date into a discrete type";

         when TYPE_RECORD =>
            raise Constraint_Error with "Cannot convert a record into a discrete type";

         when TYPE_BEAN =>
            raise Constraint_Error with "Cannot convert a bean into a discrete type";

         when TYPE_ARRAY =>
            raise Constraint_Error with "Cannot convert an array into a discrete type";

         when TYPE_BLOB =>
            raise Constraint_Error with "Cannot convert a blob into a discrete type";

      end case;
   end To_Value;

end Util.Beans.Objects.Enums;
