-----------------------------------------------------------------------
--  Util.Serialize.Mappers.Vector_Mapper -- Mapper for vector types
--  Copyright (C) 2010, 2011, 2014, 2016, 2022 Stephane Carrez
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
with Util.Log.Loggers;

package body Util.Serialize.Mappers.Vector_Mapper is

   use Vectors;
   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("Util.Serialize.Mappers.Vector_Mapper",
                                                    Util.Log.WARN_LEVEL);

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

   --  -----------------------
   --  Record mapper
   --  -----------------------

   --  -----------------------
   --  Set the <b>Data</b> vector in the context.
   --  -----------------------
   procedure Set_Context (Ctx  : in out Util.Serialize.Contexts.Context'Class;
                          Data : in Vector_Type_Access) is
      Data_Context : constant Vector_Data_Access := new Vector_Data;
   begin
      Data_Context.Vector   := Data;
      Data_Context.Position := Index_Type'First;
      Ctx.Set_Data (Key => Key, Content => Data_Context.all'Unchecked_Access);
   end Set_Context;

   --  -----------------------
   --  Execute the mapping operation on the object associated with the current context.
   --  The object is extracted from the context and the <b>Execute</b> operation is called.
   --  -----------------------
   overriding
   procedure Execute (Handler : in Mapper;
                      Map     : in Mapping'Class;
                      Ctx     : in out Util.Serialize.Contexts.Context'Class;
                      Value   : in Util.Beans.Objects.Object) is
      pragma Unreferenced (Handler);

      procedure Process (Element : in out Element_Type);

      procedure Process (Element : in out Element_Type) is
      begin
         Element_Mapper.Set_Member (Map, Element, Value);
      end Process;

      D : constant Contexts.Data_Access := Ctx.Get_Data (Key);
   begin
      Log.Debug ("Updating vector element");

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
                          Inner : in Element_Mapper.Mapper_Access) is
   begin
      Into.Mapper := Inner.all'Unchecked_Access;
      Into.Map.Bind (Inner);
   end Set_Mapping;

   --  -----------------------
   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   --  -----------------------
   function Find_Mapper (Controller : in Mapper;
                         Name       : in String) return Util.Serialize.Mappers.Mapper_Access is
   begin
      return Controller.Mapper.Find_Mapper (Name);
   end Find_Mapper;

   overriding
   procedure Initialize (Controller : in out Mapper) is
   begin
      Controller.Mapper := Controller.Map'Unchecked_Access;
   end Initialize;

   overriding
   procedure Start_Object (Handler : in Mapper;
                           Context : in out Util.Serialize.Contexts.Context'Class;
                           Name    : in String) is
      pragma Unreferenced (Handler);

      procedure Set_Context (Item : in out Element_Type);

      D : constant Contexts.Data_Access := Context.Get_Data (Key);

      procedure Set_Context (Item : in out Element_Type) is
      begin
         Element_Mapper.Set_Context (Ctx => Context, Element => Item'Unrestricted_Access);
      end Set_Context;

   begin
      Log.Debug ("Creating vector element {0}", Name);

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
         DE.Vector.Update_Element (Index => DE.Position, Process => Set_Context'Access);
         DE.Position := DE.Position + 1;
      end;
   end Start_Object;

   overriding
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
      Stream.Start_Array (Ada.Strings.Unbounded.To_String (Handler.Name));
      while Vectors.Has_Element (Pos) loop
         Element_Mapper.Write (Handler.Mapper.all, Handler.Map.Get_Getter,
                               Stream, Vectors.Element (Pos));
         Vectors.Next (Pos);
      end loop;
      Stream.End_Array (Ada.Strings.Unbounded.To_String (Handler.Name));
   end Write;

begin
   --  Allocate the unique data key.
   Util.Serialize.Contexts.Allocate (Key);
end Util.Serialize.Mappers.Vector_Mapper;
