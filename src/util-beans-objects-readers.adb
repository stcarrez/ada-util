-----------------------------------------------------------------------
--  util-beans-objects-readers -- Datasets
--  Copyright (C) 2013 Stephane Carrez
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
with Util.Serialize.IO;
package body Util.Beans.Objects.Readers is

   use type Maps.Map_Bean_Access;
   use type Vectors.Vector_Bean_Access;

   --  Start a document.
   overriding
   procedure Start_Document (Handler : in out Reader) is
   begin
      Object_Stack.Clear (Handler.Context);
      Object_Stack.Push (Handler.Context);
      Object_Stack.Current (Handler.Context).Map := new Maps.Map_Bean;
   end Start_Document;

   --  -----------------------
   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   --  -----------------------
   overriding
   procedure Start_Object (Handler : in out Reader;
                           Name    : in String) is
      Current : constant Object_Context_Access := Object_Stack.Current (Handler.Context);
      Next    : Object_Context_Access;
   begin
      Object_Stack.Push (Handler.Context);
      Next := Object_Stack.Current (Handler.Context);
      Next.Map := new Maps.Map_Bean;
      if Current.Map /= null then
         Current.Map.Include (Name, To_Object (Next.List, DYNAMIC));
      else
         Current.List.Append (To_Object (Next.List, DYNAMIC));
      end if;
   end Start_Object;

   --  -----------------------
   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   --  -----------------------
   overriding
   procedure Finish_Object (Handler : in out Reader;
                            Name    : in String) is
   begin
      Object_Stack.Pop (Handler.Context);
   end Finish_Object;

   overriding
   procedure Start_Array (Handler : in out Reader;
                          Name    : in String) is
      Current : constant Object_Context_Access := Object_Stack.Current (Handler.Context);
      Next    : Object_Context_Access;
   begin
      Object_Stack.Push (Handler.Context);
      Next := Object_Stack.Current (Handler.Context);
      Next.List := new Vectors.Vector_Bean;
      if Current.Map /= null then
         Current.Map.Include (Name, To_Object (Next.List, DYNAMIC));
      else
         Current.List.Append (To_Object (Next.List, DYNAMIC));
      end if;
   end Start_Array;

   overriding
   procedure Finish_Array (Handler : in out Reader;
                           Name    : in String;
                           Count   : in Natural) is
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
                         Attribute : in Boolean := False) is
      pragma Unreferenced (Attribute);
      Current : constant Object_Context_Access := Object_Stack.Current (Handler.Context);
   begin
      if Current.Map /= null then
         Current.Map.Set_Value (Name, Value);
      else
         Current.List.Append (Value);
      end if;
   end Set_Member;

   --  Report an error while parsing the input stream.  The error message will be reported
   --  on the logger associated with the parser.  The parser will be set as in error so that
   --  the <b>Has_Error</b> function will return True after parsing the whole file.
   procedure Error (Handler : in out Reader;
                    Message : in String) is
   begin
      null;
   end Error;

end Util.Beans.Objects.Readers;
