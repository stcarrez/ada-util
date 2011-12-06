-----------------------------------------------------------------------
--  serialize-mappers-tests -- Unit tests for serialization
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

package body Util.Serialize.Mappers.Tests is

   procedure Set_Member (P     : in out Map_Test;
                         Field : in Map_Test_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_VALUE =>
            P.Value := Natural (Util.Beans.Objects.To_Integer (Value));

         when FIELD_BOOL =>
            P.Bool := Util.Beans.Objects.To_Boolean (Value);

         when FIELD_NAME =>
            P.Name := Util.Beans.Objects.To_Unbounded_String (Value);
            if P.Name = "raise-field-error" then
               raise Util.Serialize.Mappers.Field_Error with "Testing Field_Error exception";
            end if;
            if P.Name = "raise-field-fatal-error" then
               raise Util.Serialize.Mappers.Field_Fatal_Error with "Testing Fatal_Error exception";
            end if;

         when FIELD_NODE =>
            P.Node := Value;
      end case;
   end Set_Member;

   function Get_Member (P : in Map_Test;
                        Field : in Map_Test_Fields) return Util.Beans.Objects.Object is
   begin
      case Field is
         when FIELD_VALUE =>
            return Util.Beans.Objects.To_Object (P.Value);

         when FIELD_BOOL =>
            return Util.Beans.Objects.To_Object (P.Bool);

         when FIELD_NAME =>
            return Util.Beans.Objects.To_Object (P.Name);

         when FIELD_NODE =>
            return P.Node;

      end case;
   end Get_Member;

end Util.Serialize.Mappers.Tests;
