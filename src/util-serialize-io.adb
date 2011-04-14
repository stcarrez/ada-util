-----------------------------------------------------------------------
--  util-serialize-io -- IO Drivers for serialization
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
with Util.Streams.Files;
with Ada.Streams;
with Ada.Streams.Stream_IO;
package body Util.Serialize.IO is

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
      Context_Stack.Clear (Handler.Stack);
      Parser'Class (Handler).Parse (Buffer);
   end Parse;

   --  ------------------------------
   --  Parse the content string.
   --  ------------------------------
   procedure Parse_String (Handler : in out Parser;
                           Content : in String) is
      Stream : aliased Util.Streams.Buffered.Buffered_Stream;
   begin
      Stream.Initialize (Content  => Content);
      Context_Stack.Clear (Handler.Stack);
      Parser'Class (Handler).Parse (Stream);
   end Parse_String;

   --  ------------------------------
   --  Returns true if the <b>Parse</b> operation detected at least one error.
   --  ------------------------------
   function Has_Error (Handler : in Parser) return Boolean is
   begin
      return Handler.Error_Flag;
   end Has_Error;

   --  ------------------------------
   --  Push the current context when entering in an element.
   --  ------------------------------
   procedure Push (Handler : in out Parser) is
      use type Util.Serialize.Mappers.Mapper_Access;
   begin
      Context_Stack.Push (Handler.Stack);
   end Push;

   --  ------------------------------
   --  Pop the context and restore the previous context when leaving an element
   --  ------------------------------
   procedure Pop (Handler  : in out Parser) is
   begin
      Context_Stack.Pop (Handler.Stack);
   end Pop;

   function Find_Mapper (Handler : in Parser;
                         Name    : in String) return Util.Serialize.Mappers.Mapper_Access is
--        use type Util.Serialize.Mappers.Mapper_Access;
--
--        Pos     : Util.Serialize.Mappers.Mapper_Map.Cursor;
--        Map     : Util.Serialize.Mappers.Mapper_Access;
--        Current : constant Element_Context_Access := Context_Stack.Current (Handler.Stack);
   begin
--        if Current /= null and then Current.Mapper /= null then
--           Map := Current.Mapper.Find_Mapper (Name);
--           if Map /= null then
--              return Map;
--           end if;
--        end if;
--        Pos := Handler.Mappers.Find (Name);
--        if Util.Serialize.Mappers.Mapper_Map.Has_Element (Pos) then
--           Map := Util.Serialize.Mappers.Mapper_Map.Element (Pos);
--           return Map;
--        end if;
      return null;
   end Find_Mapper;


   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   procedure Start_Object (Handler : in out Parser;
                           Name    : in String) is

      use type Util.Serialize.Mappers.Mapper_Access;

      Current : constant Element_Context_Access := Context_Stack.Current (Handler.Stack);
      Next    : Element_Context_Access;
      Pos     : Positive;
   begin
      Context_Stack.Push (Handler.Stack);
      Next := Context_Stack.Current (Handler.Stack);
      if Current /= null then
         Pos := 1;

         --  Notify we are entering in the given node for each active mapping.
         for I in Current.Active_Nodes'Range loop
            declare
               Node  : constant Mappers.Mapper_Access := Current.Active_Nodes (I);
               Child : Mappers.Mapper_Access;
            begin
               exit when Node = null;
--                 Node.Start_Object (Handler, Name);
               Child := Node.Find_Mapper (Name => Name);
               if Child /= null then
                  Child.Start_Object (Handler, Name);
                  Next.Active_Nodes (Pos) := Child;
                  Pos := Pos + 1;
               end if;
            end;
         end loop;
         while Pos <= Next.Active_Nodes'Last loop
            Next.Active_Nodes (Pos) := null;
            Pos := Pos + 1;
         end loop;
      else
         Next.Active_Nodes (1) := Handler.Mapping_Tree.Find_Mapper (Name);
      end if;
   end Start_Object;

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   procedure Finish_Object (Handler : in out Parser;
                            Name    : in String) is

      use type Util.Serialize.Mappers.Mapper_Access;
   begin

      declare
         Current : constant Element_Context_Access := Context_Stack.Current (Handler.Stack);
      begin
         if Current /= null then
            --  Notify we are leaving the given node for each active mapping.
            for I in Current.Active_Nodes'Range loop
               declare
                  Node : constant Mappers.Mapper_Access := Current.Active_Nodes (I);
               begin
                  exit when Node = null;
                  Node.Finish_Object (Handler, Name);
               end;
            end loop;
         end if;
      end;
      Handler.Pop;
   end Finish_Object;

   --  Finish an object associated with the given name and set the value associated with
   --  that name.  The reader must be updated to be associated with the previous object.
   procedure Finish_Object (Handler : in out Parser;
                            Name    : in String;
                            Value   : in Util.Beans.Objects.Object) is
   begin
      null;
   end Finish_Object;

   procedure Start_Array (Handler : in out Parser;
                          Name    : in String) is
   begin
      Handler.Push;
   end Start_Array;

   procedure Finish_Array (Handler : in out Parser;
                           Name    : in String) is
      pragma Unreferenced (Name);
   begin
      Handler.Pop;
   end Finish_Array;

   --  -----------------------
   --  Set the name/value pair on the current object.  For each active mapping,
   --  find whether a rule matches our name and execute it.
   --  -----------------------
   procedure Set_Member (Handler   : in out Parser;
                         Name      : in String;
                         Value     : in Util.Beans.Objects.Object;
                         Attribute : in Boolean := False) is
      use Util.Serialize.Mappers;

      Current : constant Element_Context_Access := Context_Stack.Current (Handler.Stack);
   begin
      if Current /= null then

         --  Look each active mapping node.
         for I in Current.Active_Nodes'Range loop
            declare
               Node : constant Mapper_Access := Current.Active_Nodes (I);
            begin
               exit when Node = null;
               Node.Set_Member (Name      => Name,
                                Value     => Value,
                                Attribute => Attribute,
                                Context   => Handler);
            end;
         end loop;
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
      Handler.Mapping_Tree.Add_Mapping (Path, Mapper);
   end Add_Mapping;

end Util.Serialize.IO;
