-----------------------------------------------------------------------
--  util-serialize-io -- IO Drivers for serialization
--  Copyright (C) 2010 Stephane Carrez
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
with Util.Streams.Files;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
package body Util.Serialize.IO is

   procedure Free is
     new Ada.Unchecked_Deallocation (Element_Context_Array,
                                     Element_Context_Array_Access);

   --  Read the file and parse it using the JSON parser.
   procedure Parse (Handler : in out Parser;
                    File    : in String) is
      Stream     : aliased Util.Streams.Files.File_Stream;
      Buffer     : Util.Streams.Buffered.Buffered_Stream;
   begin
      Buffer.Initialize (Output => null,
                         Input  => Stream'Unchecked_Access,
                         Size   => 1024);
      Stream.Open (Mode => Ada.Streams.Stream_IO.In_File, Name => File);
      Parser'Class (Handler).Parse (Buffer);
   end Parse;

   --  ------------------------------
   --  Push the current context when entering in an element.
   --  ------------------------------
   procedure Push (Handler : in out Parser;
                   Mapper  : in Util.Serialize.Mappers.Mapper_Access) is
   begin
      if Handler.Stack = null then
         Handler.Stack := new Element_Context_Array (1 .. 100);
         Handler.Stack_Pos := Handler.Stack'First;
      elsif Handler.Stack_Pos = Handler.Stack'Last then
         declare
            Old : Element_Context_Array_Access := Handler.Stack;
         begin
            Handler.Stack := new Element_Context_Array (1 .. Old'Last + 100);
            Handler.Stack (1 .. Old'Last) := Old (1 .. Old'Last);
            Free (Old);
         end;
      end if;
      if Handler.Stack_Pos /= Handler.Stack'First then
         Handler.Stack (Handler.Stack_Pos + 1) := Handler.Stack (Handler.Stack_Pos);
      end if;
      Handler.Stack_Pos := Handler.Stack_Pos + 1;
      Handler.Current := Handler.Stack (Handler.Stack_Pos)'Access;
      Handler.Current.Mapper := Mapper;
   end Push;

   --  ------------------------------
   --  Pop the context and restore the previous context when leaving an element
   --  ------------------------------
   procedure Pop (Handler  : in out Parser) is
   begin
      Handler.Stack_Pos := Handler.Stack_Pos - 1;
      Handler.Current   := Handler.Stack (Handler.Stack_Pos)'Access;
   end Pop;

   function Find_Mapper (Handler : in Parser;
                         Name    : in String) return Util.Serialize.Mappers.Mapper_Access is
      use type Util.Serialize.Mappers.Mapper_Access;

      Pos : Util.Serialize.Mappers.Mapper_Map.Cursor;
      Map : Util.Serialize.Mappers.Mapper_Access;
   begin
      if Handler.Current /= null and then Handler.Current.Mapper /= null then
         Map := Handler.Current.Mapper.Find_Mapper (Name);
         if Map /= null then
            return Map;
         end if;
      end if;
      Pos := Handler.Mappers.Find (Name);
      if Util.Serialize.Mappers.Mapper_Map.Has_Element (Pos) then
         Map := Util.Serialize.Mappers.Mapper_Map.Element (Pos);
         return Map;
      end if;
      return null;
   end Find_Mapper;

   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   procedure Start_Object (Handler : in out Parser;
                           Name    : in String) is

      use type Util.Serialize.Mappers.Mapper_Access;

      Map : Util.Serialize.Mappers.Mapper_Access := Handler.Find_Mapper (Name);
   begin
      if Handler.Current /= null and then Handler.Current.Mapper /= null then
         Handler.Current.Mapper.Start_Object (Handler, Name);
      end if;
      Handler.Push (Map);
--        for I in Handler.Contexts loop
--           Map := Ctx.Find_Mapper (Name);
--           if Map /= null then
--              Ctx.Push_Mapper (Map);
--           end if;
--        end loop;

   end Start_Object;

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   procedure Finish_Object (Handler : in out Parser;
                            Name    : in String) is

      use type Util.Serialize.Mappers.Mapper_Access;
   begin
      Handler.Pop;
      if Handler.Current /= null and then Handler.Current.Mapper /= null then
         Handler.Current.Mapper.Finish_Object (Handler, Name);
      end if;
   end Finish_Object;

   procedure Start_Array (Handler : in out Parser;
                          Name    : in String) is
      Map : Util.Serialize.Mappers.Mapper_Access := Handler.Find_Mapper (Name);
   begin
      Handler.Push (Map);
   end Start_Array;

   procedure Finish_Array (Handler : in out Parser;
                           Name    : in String) is
   begin
      Handler.Pop;
   end Finish_Array;

   --  -----------------------
   --  Set the name/value pair on the current object.  For each active mapping,
   --  find whether a rule matches our name and execute it.
   --  -----------------------
   procedure Set_Member (Handler : in out Parser;
                         Name    : in String;
                         Value   : in Util.Beans.Objects.Object) is
      use type Util.Serialize.Mappers.Mapping_Access;
      use type Util.Serialize.Mappers.Mapper_Access;

      Map : Util.Serialize.Mappers.Mapping_Access;
   begin
      if Handler.Current /= null and then Handler.Current.Mapper /= null then
         Map := Handler.Current.Mapper.Find_Mapping (Name);
         if Map /= null then
            Map.Execute (Handler, Value);
         end if;
      end if;
   end Set_Member;

   --  Report an error while parsing the JSON stream.
  procedure Error (Handler : in out Parser;
                   Message : in String) is
   begin
      raise Parse_Error with Message;
   end Error;

   procedure Add_Mapping (Handler : in out Parser;
                          Path    : in String;
                          Mapper  : in Util.Serialize.Mappers.Mapper_Access) is
   begin
      Handler.Mappers.Insert (Path, Mapper);
   end Add_Mapping;

end Util.Serialize.IO;
