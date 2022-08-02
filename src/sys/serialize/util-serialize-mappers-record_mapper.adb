-----------------------------------------------------------------------
--  Util.Serialize.Mappers.Record_Mapper -- Mapper for record types
--  Copyright (C) 2010, 2011, 2022 Stephane Carrez
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
with Ada.Unchecked_Deallocation;

with Util.Serialize.Contexts;
with Util.Strings.Transforms;
with Util.Log.Loggers;
package body Util.Serialize.Mappers.Record_Mapper is

   use Util.Log;

   Key : Util.Serialize.Contexts.Data_Key;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("Util.Serialize.Mappers.Record_Mapper",
                                                    Util.Log.WARN_LEVEL);

   --  -----------------------
   --  Get the element object.
   --  -----------------------
   function Get_Element (Data : in Element_Data) return Element_Type_Access is
   begin
      return Data.Element;
   end Get_Element;

   --  -----------------------
   --  Set the element object.  When <b>Release</b> is set, the element <b>Element</b>
   --  will be freed when the reader context is deleted (by <b>Finalize</b>).
   --  -----------------------
   procedure Set_Element (Data    : in out Element_Data;
                          Element : in Element_Type_Access;
                          Release : in Boolean := False) is
   begin
      Data.Element := Element;
      Data.Release := Release;
   end Set_Element;

   --  -----------------------
   --  Finalize the object when it is removed from the reader context.
   --  If the
   --  -----------------------
   overriding
   procedure Finalize (Data : in out Element_Data) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Element_Type,
                                         Name   => Element_Type_Access);
   begin
      if Data.Release then
         Free (Data.Element);
      end if;
   end Finalize;

   --  -----------------------
   --  Execute the mapping operation on the object associated with the current context.
   --  The object is extracted from the context and the <b>Execute</b> operation is called.
   --  -----------------------
   overriding
   procedure Execute (Handler : in Mapper;
                      Map     : in Mapping'Class;
                      Ctx     : in out Util.Serialize.Contexts.Context'Class;
                      Value   : in Util.Beans.Objects.Object) is
      D : constant Contexts.Data_Access := Ctx.Get_Data (Key);
   begin
      if not (D.all in Element_Data'Class) then
         raise Util.Serialize.Contexts.No_Data;
      end if;
      declare
         DE : constant Element_Data_Access := Element_Data'Class (D.all)'Access;
      begin
         if DE.Element = null then
            raise Util.Serialize.Contexts.No_Data;
         end if;
         Handler.Execute (Map, DE.Element.all, Value);
      end;
   end Execute;

   --  -----------------------
   --  Add a mapping for setting a member.  When the attribute rule defined by <b>Path</b>
   --  is matched, the <b>Set_Member</b> procedure will be called with the value and the
   --  <b>Field</b> identification.
   --  -----------------------
   procedure Add_Mapping (Into  : in out Mapper;
                          Path  : in String;
                          Field : in Fields) is
      Map : constant Attribute_Mapping_Access := new Attribute_Mapping;
   begin
      Map.Index   := Field;
      Into.Add_Mapping (Path, Map.all'Unchecked_Access);
   end Add_Mapping;

   --  -----------------------
   --  Add a mapping associated with the path and described by a mapper object.
   --  The <b>Proxy</b> procedure is in charge of giving access to the target
   --  object used by the <b>Map</b> mapper.
   --  -----------------------
   procedure Add_Mapping (Into  : in out Mapper;
                          Path  : in String;
                          Map   : in Util.Serialize.Mappers.Mapper_Access;
                          Proxy : in Proxy_Object) is
      M : constant Proxy_Mapper_Access := new Proxy_Mapper;
   begin
      M.Mapper  := Map;
      M.Execute := Proxy;
      M.Is_Proxy_Mapper := True;
      Into.Add_Mapping (Path, M.all'Unchecked_Access);
   end Add_Mapping;

   --
   procedure Bind (Into    : in out Mapper;
                   Getter  : in Get_Member_Access) is
   begin
      Into.Get_Member := Getter;
   end Bind;

   procedure Bind (Into    : in out Mapper;
                   From    : in Mapper_Access) is
   begin
      Into.Get_Member := From.Get_Member;
   end Bind;

   function Get_Getter (From : in Mapper) return Get_Member_Access is
   begin
      return From.Get_Member;
   end Get_Getter;

   procedure Set_Member (Attr    : in Attribute_Mapping;
                         Element : in out Element_Type;
                         Value   : in Util.Beans.Objects.Object) is
   begin
      Set_Member (Element, Attr.Index, Value);
   end Set_Member;

   --  -----------------------
   --  Set the attribute member described by the <b>Attr</b> mapping
   --  into the value passed in <b>Element</b>.  This operation will call
   --  the package parameter function of the same name.
   --  -----------------------
   procedure Set_Member (Attr    : in Mapping'Class;
                         Element : in out Element_Type;
                         Value   : in Util.Beans.Objects.Object) is
   begin
      if not (Attr in Attribute_Mapping) then
         Log.Error ("Mapping is not an Attribute_Mapping");
         raise Mapping_Error;
      end if;
      Attribute_Mapping (Attr).Set_Member (Element, Value);
   end Set_Member;

   --  -----------------------
   --  Set the element in the context.  When <b>Release</b> is set, the element <b>Element</b>
   --  will be freed when the reader context is deleted (by <b>Finalize</b>).
   --  -----------------------
   procedure Set_Context (Ctx     : in out Util.Serialize.Contexts.Context'Class;
                          Element : in Element_Type_Access;
                          Release : in Boolean := False) is
      Data_Context : constant Element_Data_Access := new Element_Data;
   begin
      Data_Context.Element := Element;
      Data_Context.Release := Release;
      Ctx.Set_Data (Key => Key, Content => Data_Context.all'Unchecked_Access);
   end Set_Context;

   --  -----------------------
   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   --  -----------------------
   overriding
   function Find_Mapper (Controller : in Proxy_Mapper;
                         Name       : in String;
                         Attribute  : in Boolean := False) return Mappers.Mapper_Access is
      Result : constant Mappers.Mapper_Access := Controller.Mapper.Find_Mapper (Name, Attribute);
   begin
      if Result /= null then
         return Result;
      else
         return Util.Serialize.Mappers.Mapper (Controller).Find_Mapper (Name, Attribute);
      end if;
   end Find_Mapper;

   --  -----------------------
   --  Build a default mapping based on the <b>Fields</b> enumeration.
   --  The enumeration name is used for the mapping name with the optional <b>FIELD_</b>
   --  prefix stripped.
   --  -----------------------
   procedure Add_Default_Mapping (Into : in out Mapper) is
      use Util.Strings.Transforms;
   begin
      for Field in Fields'Range loop
         declare
            Name : constant String := To_Lower_Case (Fields'Image (Field));
         begin
            if Name (Name'First .. Name'First + 5) = "field_" then
               Into.Add_Mapping (Name (Name'First + 6 .. Name'Last), Field);
            else
               Into.Add_Mapping (Name, Field);
            end if;
         end;
      end loop;
   end Add_Default_Mapping;

   --  -----------------------
   --  Write the element on the stream using the mapper description.
   --  -----------------------
   procedure Write (Handler : in Mapper;
                    Stream  : in out Util.Serialize.IO.Output_Stream'Class;
                    Element : in Element_Type) is
   begin
      if Handler.Get_Member = null then
         Log.Error ("The mapper has a null Get_Member function");
         raise Mapping_Error with "The mapper has a null Get_Member function";
      end if;
      Write (Handler, Handler.Get_Member, Stream, Element);
   end Write;

   --  -----------------------
   --  Write the element on the stream using the mapper description.
   --  -----------------------
   procedure Write (Handler : in Util.Serialize.Mappers.Mapper'Class;
                    Getter  : in Get_Member_Access;
                    Stream  : in out Util.Serialize.IO.Output_Stream'Class;
                    Element : in Element_Type) is
      use Ada.Strings.Unbounded;

      procedure Write (Map : in Util.Serialize.Mappers.Mapper'Class);

      procedure Write (Map : in Util.Serialize.Mappers.Mapper'Class) is
         Name : constant String := To_String (Map.Name);
      begin
         if Map.Mapping /= null then
            declare
               M   : constant Attribute_Mapping_Access
                 := Attribute_Mapping'Class (Map.Mapping.all)'Access;
               Val : constant Util.Beans.Objects.Object := Getter (Element, M.Index);
            begin
               if M.Is_Attribute then
                  Stream.Write_Attribute (Name => Name, Value => Val);
               else
                  Stream.Write_Entity (Name => Name, Value => Val);
               end if;
            end;
         else
            Stream.Start_Entity (Name);
            Map.Iterate (Write'Access);
            Stream.End_Entity (Name);
         end if;
      end Write;

   begin
      Handler.Iterate (Write'Access);
   end Write;

   --  -----------------------
   --  Clone the <b>Handler</b> instance and get a copy of that single object.
   --  -----------------------
   overriding
   function Clone (Handler : in Mapper) return Util.Serialize.Mappers.Mapper_Access is
      Result : constant Mapper_Access := new Mapper;
   begin
      Result.Name             := Handler.Name;
      Result.Mapper           := Handler.Mapper;
      Result.Mapping          := Handler.Mapping;
      Result.Is_Proxy_Mapper  := Handler.Is_Proxy_Mapper;
      Result.Is_Wildcard      := Handler.Is_Wildcard;
      Result.Is_Deep_Wildcard := Handler.Is_Deep_Wildcard;
      Result.Get_Member       := Handler.Get_Member;
      Result.Execute          := Handler.Execute;
      return Result.all'Unchecked_Access;
   end Clone;

begin
   --  Allocate the unique data key.
   Util.Serialize.Contexts.Allocate (Key);
end Util.Serialize.Mappers.Record_Mapper;
