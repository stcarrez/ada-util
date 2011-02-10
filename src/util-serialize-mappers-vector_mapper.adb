-----------------------------------------------------------------------
--  Util.Serialize.Mappers.Vector_Mapper -- Mapper for vector types
--  Copyright (C) 2010, 2011 Stephane Carrez
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

package body Util.Serialize.Mappers.Vector_Mapper is

   use Vectors;

   Key : Util.Serialize.Contexts.Data_Key;

   --  -----------------------
   --  Data context
   --  -----------------------
   --  Data context to get access to the target element.

   --  -----------------------
   --  Get the vector object.
   --  -----------------------
   function Get_Vector (Data : in Vector_Data) return Vector_Type_Access is
   begin
      return Data.Vector;
   end Get_Vector;

   --  -----------------------
   --  Set the vector object.
   --  -----------------------
   procedure Set_Vector (Data   : in out Vector_Data;
                         Vector : in Vector_Type_Access) is
   begin
      Data.Vector := Vector;
   end Set_Vector;

   procedure Start_Array (Ctx : in out Util.Serialize.Contexts.Context'Class) is
      D : constant Contexts.Data_Access := Ctx.Get_Data (Key);
   begin
      if not (D.all in Vector_Data'Class) then
         raise Util.Serialize.Contexts.No_Data;
      end if;
      declare
         DE : constant Vector_Data_Access := Vector_Data'Class (D.all)'Access;
      begin
         DE.Position := Index_Type'First;
      end;
   end Start_Array;

   --  -----------------------
   --  Record mapper
   --  -----------------------

   --  -----------------------
   --  Set the <b>Data</b> vector in the context.
   --  -----------------------
   procedure Set_Context (Ctx  : in out Util.Serialize.Contexts.Context'Class;
                          Data : in Vector_Type_Access) is
      Data_Context : Vector_Data_Access := new Vector_Data;
   begin
      Data_Context.Vector   := Data;
      Data_Context.Position := Index_Type'First;
      Ctx.Set_Data (Key => Key, Content => Data_Context.all'Access);
   end Set_Context;

   --  Execute the mapping operation on the object associated with the current context.
   --  The object is extracted from the context and the <b>Execute</b> operation is called.
   procedure Execute (Handler : in Mapper;
                      Map     : in Mapping'Class;
                      Ctx     : in out Util.Serialize.Contexts.Context'Class;
                      Value   : in Util.Beans.Objects.Object) is

      procedure Process (Element : in out Element_Type) is
      begin
         Element_Mapper.Set_Member (Map, Element, Value);
      end Process;

      D : constant Contexts.Data_Access := Ctx.Get_Data (Key);
   begin
      if not (D.all in Vector_Data'Class) then
         raise Util.Serialize.Contexts.No_Data;
      end if;
      declare
         DE : constant Vector_Data_Access := Vector_Data'Class (D.all)'Access;
      begin
         if DE.Vector = null then
            raise Util.Serialize.Contexts.No_Data;
         end if;
         --  Update the element through the generic procedure
         Update_Element (DE.Vector.all, DE.Position - 1, Process'Access);
      end;
   end Execute;

   procedure Set_Mapping (Into  : in out Mapper;
                          Path  : in String;
                          Inner : in Element_Mapper.Mapper_Access) is
   begin
      Into.Add_Mapping (Path, Inner.all'Access);
      null; -- Element_Mapper.Copy (Into.Map, Inner, Execute_Object'Access);
   end Set_Mapping;

   --  -----------------------
   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   --  -----------------------
   function Find_Mapping (Controller : in Mapper;
                          Name       : in String) return Mapping_Access is
   begin
      return Controller.Map.Find_Mapping (Name);
   end Find_Mapping;

   --  -----------------------
   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   --  -----------------------
   function Find_Mapper (Controller : in Mapper;
                         Name       : in String) return Util.Serialize.Mappers.Mapper_Access is
   begin
      return Controller.Element_Map.all'Unchecked_Access;
   end Find_Mapper;

   overriding
   procedure Initialize (Controller : in out Mapper) is
   begin
      Controller.Element_Map := Controller.Map'Unchecked_Access;
   end Initialize;

   procedure Start_Object (Handler : in Mapper;
                           Context : in out Util.Serialize.Contexts.Context'Class;
                           Name    : in String) is
      D : constant Contexts.Data_Access := Context.Get_Data (Key);
   begin
      if not (D.all in Vector_Data'Class) then
         raise Util.Serialize.Contexts.No_Data;
      end if;
      declare
         DE : constant Vector_Data_Access := Vector_Data'Class (D.all)'Access;
      begin
         if DE.Vector = null then
            raise Util.Serialize.Contexts.No_Data;
         end if;
         Insert_Space (DE.Vector.all, DE.Position);
         DE.Position := DE.Position + 1;
      end;
   end Start_Object;

   procedure Finish_Object (Handler : in Mapper;
                            Context : in out Util.Serialize.Contexts.Context'Class;
                            Name    : in String) is
   begin
      null;
   end Finish_Object;

   --  -----------------------
   --  Write the element on the stream using the mapper description.
   --  -----------------------
   procedure Write (Handler : in Mapper;
                    Stream  : in out Util.Serialize.IO.Output_Stream'Class;
                    Element : in Vectors.Vector) is
      Pos : Vectors.Cursor := Element.First;
   begin
      Stream.Start_Array (Element.Length);
      while Vectors.Has_Element (Pos) loop
         Handler.Map.Write (Stream, Vectors.Element (Pos));
         Vectors.Next (Pos);
      end loop;
      Stream.End_Array;
   end Write;

begin
   --  Allocate the unique data key.
   Util.Serialize.Contexts.Allocate (Key);
end Util.Serialize.Mappers.Vector_Mapper;