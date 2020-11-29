-----------------------------------------------------------------------
--  util-beans-objects-readers -- Datasets
--  Copyright (C) 2017, 2020 Stephane Carrez
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
package body Util.Beans.Objects.Readers is

   use type Maps.Map_Bean_Access;
   use type Vectors.Vector_Bean_Access;

   --  Start a document.
   overriding
   procedure Start_Document (Handler : in out Reader) is
   begin
      Object_Stack.Clear (Handler.Context);
   end Start_Document;

   --  -----------------------
   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   --  -----------------------
   overriding
   procedure Start_Object (Handler : in out Reader;
                           Name    : in String;
                           Logger  : in out Util.Log.Logging'Class) is
      pragma Unreferenced (Logger);
      Current : constant Object_Context_Access := Object_Stack.Current (Handler.Context);
      Next    : Object_Context_Access;
   begin
      Object_Stack.Push (Handler.Context);
      Next := Object_Stack.Current (Handler.Context);
      Next.Map  := new Maps.Map_Bean;
      Next.List := null;
      if Current = null then
         Handler.Root := To_Object (Next.Map, DYNAMIC);
      elsif Current.Map /= null then
         Current.Map.Include (Name, To_Object (Next.Map, DYNAMIC));
      else
         Current.List.Append (To_Object (Next.Map, DYNAMIC));
      end if;
   end Start_Object;

   --  -----------------------
   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   --  -----------------------
   overriding
   procedure Finish_Object (Handler : in out Reader;
                            Name    : in String;
                            Logger  : in out Util.Log.Logging'Class) is
      pragma Unreferenced (Name, Logger);
   begin
      Object_Stack.Pop (Handler.Context);
   end Finish_Object;

   overriding
   procedure Start_Array (Handler : in out Reader;
                          Name    : in String;
                          Logger  : in out Util.Log.Logging'Class) is
      pragma Unreferenced (Logger);
      Current : constant Object_Context_Access := Object_Stack.Current (Handler.Context);
      Next    : Object_Context_Access;
   begin
      Object_Stack.Push (Handler.Context);
      Next := Object_Stack.Current (Handler.Context);
      Next.List := new Vectors.Vector_Bean;
      Next.Map  := null;
      if Current = null then
         Handler.Root := To_Object (Next.List, DYNAMIC);
      elsif Current.Map /= null then
         Current.Map.Include (Name, To_Object (Next.List, DYNAMIC));
      else
         Current.List.Append (To_Object (Next.List, DYNAMIC));
      end if;
   end Start_Array;

   overriding
   procedure Finish_Array (Handler : in out Reader;
                           Name    : in String;
                           Count   : in Natural;
                           Logger  : in out Util.Log.Logging'Class) is
      pragma Unreferenced (Name, Count, Logger);
   begin
      Object_Stack.Pop (Handler.Context);
   end Finish_Array;

   --  -----------------------
   --  Set the name/value pair on the current object.  For each active mapping,
   --  find whether a rule matches our name and execute it.
   --  -----------------------
   overriding
   procedure Set_Member (Handler   : in out Reader;
                         Name      : in String;
                         Value     : in Util.Beans.Objects.Object;
                         Logger    : in out Util.Log.Logging'Class;
                         Attribute : in Boolean := False) is
      pragma Unreferenced (Logger, Attribute);
      Current : constant Object_Context_Access := Object_Stack.Current (Handler.Context);
   begin
      if Current = null then
         Handler.Root := Value;
      elsif Current.Map /= null then
         Current.Map.Set_Value (Name, Value);
      else
         Current.List.Append (Value);
      end if;
   end Set_Member;

   --  -----------------------
   --  Get the root object.
   --  -----------------------
   function Get_Root (Handler : in Reader) return Object is
   begin
      return Handler.Root;
   end Get_Root;

end Util.Beans.Objects.Readers;
