-----------------------------------------------------------------------
--  util-serialize-mappers -- Serialize objects in various formats
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

with Util.Strings;
with Ada.Unchecked_Deallocation;
package body Util.Serialize.Mappers is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("Util.Serialize.Mappers",
                                                    Util.Log.WARN_LEVEL);

   --  -----------------------
   --  Execute the mapping operation on the object associated with the current context.
   --  The object is extracted from the context and the <b>Execute</b> operation is called.
   --  -----------------------
   procedure Execute (Handler : in Mapper;
                      Map     : in Mapping'Class;
                      Ctx     : in out Util.Serialize.Contexts.Context'Class;
                      Value   : in Util.Beans.Objects.Object) is
   begin
      if Handler.Mapper /= null then
         Handler.Mapper.all.Execute (Map, Ctx, Value);
      end if;
   end Execute;

   function Is_Proxy (Controller : in Mapper) return Boolean is
   begin
      return Controller.Is_Proxy_Mapper;
   end Is_Proxy;

   --  -----------------------
   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   --  -----------------------
   function Find_Mapper (Controller : in Mapper;
                         Name       : in String;
                         Attribute  : in Boolean := False) return Mapper_Access is
      use type Ada.Strings.Unbounded.Unbounded_String;
      Node : Mapper_Access := Controller.First_Child;
   begin
      if Node = null and Controller.Mapper /= null then
         return Controller.Mapper.Find_Mapper (Name, Attribute);
      end if;
      while Node /= null loop
         if Node.Name = Name then
            if (Attribute = False and Node.Mapping = null)
              or else not Node.Mapping.Is_Attribute then
               return Node;
            end if;
            if Attribute and Node.Mapping.Is_Attribute then
               return Node;
            end if;
         end if;
         Node := Node.Next_Mapping;
      end loop;
      return null;
   end Find_Mapper;

   --  -----------------------
   --  Find a path component representing a child mapper under <b>From</b> and
   --  identified by the given <b>Name</b>.  If the mapper is not found, a new
   --  Mapper_Node is created.
   --  -----------------------
   procedure Find_Path_Component (From   : in out Mapper'Class;
                                  Name   : in String;
                                  Result : out Mapper_Access) is
      use Ada.Strings.Unbounded;

      Node : Mapper_Access := From.First_Child;
   begin
      if Node = null then
         Result := new Mapper;
         Result.Name := To_Unbounded_String (Name);
         From.First_Child := Result;
         return;
      end if;
      loop
         if Node.Name = Name then
            Result := Node;
            return;
         end if;
         if Node.Next_Mapping = null then
            Result := new Mapper;
            Result.Name := To_Unbounded_String (Name);
            Node.Next_Mapping := Result;
            return;
         end if;
         Node := Node.Next_Mapping;
      end loop;
   end Find_Path_Component;

   --  -----------------------
   --  Build the mapping tree that corresponds to the given <b>Path</b>.
   --  Each path component is represented by a <b>Mapper_Node</b> element.
   --  The node is created if it does not exists.
   --  -----------------------
   procedure Build_Path (Into     : in out Mapper'Class;
                         Path     : in String;
                         Last_Pos : out Natural;
                         Node     : out Mapper_Access) is
      Pos      : Natural;
   begin
      Node     := Into'Unchecked_Access;
      Last_Pos := Path'First;
      loop
         Pos := Util.Strings.Index (Source => Path,
                                    Char   => '/',
                                    From   => Last_Pos);
         if Pos = 0 then
            Node.Find_Path_Component (Name   => Path (Last_Pos .. Path'Last),
                                      Result => Node);
            Last_Pos := Path'Last + 1;
         else
            Node.Find_Path_Component (Name   => Path (Last_Pos .. Pos - 1),
                                      Result => Node);
            Last_Pos := Pos + 1;
         end if;
         exit when Last_Pos > Path'Last;
      end loop;
   end Build_Path;

   --  -----------------------
   --  Add a mapping to associate the given <b>Path</b> to the mapper defined in <b>Map</b>.
   --  The <b>Path</b> string describes the matching node using a simplified XPath notation.
   --  Example:
   --     info/first_name    matches:  <info><first_name>...</first_name></info>
   --     info/a/b/name      matches:  <info><a><b><name>...</name></b></a></info>
   --  -----------------------
   procedure Add_Mapping (Into : in out Mapper;
                          Path : in String;
                          Map  : in Mapper_Access) is
      procedure Copy (To   : in Mapper_Access;
                      From : in Mapper_Access);

      Node     : Mapper_Access;
      Last_Pos : Natural;

      procedure Copy (To   : in Mapper_Access;
                      From : in Mapper_Access) is
         N   : Mapper_Access;
         Src : Mapper_Access := From;
      begin
         while Src /= null loop
            N := Src.Clone;
            N.Is_Clone := True;
            N.Next_Mapping := To.First_Child;
            To.First_Child := N;
            if Src.First_Child /= null then
               Copy (N, Src.First_Child);
            end if;
            Src := Src.Next_Mapping;
         end loop;
      end Copy;

   begin
      Log.Info ("Mapping '{0}' for mapper X", Path);

      --  Find or build the mapping tree.
      Into.Build_Path (Path, Last_Pos, Node);

      if Last_Pos < Path'Last then
         Log.Warn ("Ignoring the end of mapping path {0}", Path);
      end if;
      if Node.Mapper /= null then
         Log.Warn ("Overriding the mapping {0} for mapper X", Path);
      end if;
      if Map.First_Child /= null then
         Copy (Node, Map.First_Child);
      else
         Node.Mapper := Map;
      end if;
   end Add_Mapping;

   procedure Add_Mapping (Into : in out Mapper;
                          Path : in String;
                          Map  : in Mapping_Access) is
      use Ada.Strings.Unbounded;
      Node     : Mapper_Access;
      Last_Pos : Natural;
   begin
      Log.Info ("Mapping {0}", Path);

      --  Find or build the mapping tree.
      Into.Build_Path (Path, Last_Pos, Node);

      if Last_Pos < Path'Last then
         Log.Warn ("Ignoring the end of mapping path {0}", Path);
      end if;
      if Node.Mapping /= null then
         Log.Warn ("Overriding the mapping {0} for mapper X", Path);
      end if;
      if Length (Node.Name) = 0 then
         Log.Warn ("Mapped name is empty in mapping path {0}", Path);
      elsif Element (Node.Name, 1) = '@' then
         Delete (Node.Name, 1, 1);
         Map.Is_Attribute := True;
      else
         Map.Is_Attribute := False;
      end if;

      Node.Mapping := Map;
      Node.Mapper  := Into'Unchecked_Access;
   end Add_Mapping;

   --  -----------------------
   --  Clone the <b>Handler</b> instance and get a copy of that single object.
   --  -----------------------
   function Clone (Handler : in Mapper) return Mapper_Access is
      Result : constant Mapper_Access := new Mapper;
   begin
      Result.Name            := Handler.Name;
      Result.Mapper          := Handler.Mapper;
      Result.Mapping         := Handler.Mapping;
      Result.Is_Proxy_Mapper := Handler.Is_Proxy_Mapper;
      Result.Is_Clone        := True;
      return Result;
   end Clone;

   --  -----------------------
   --  Set the name/value pair on the current object.  For each active mapping,
   --  find whether a rule matches our name and execute it.
   --  -----------------------
   procedure Set_Member (Handler   : in Mapper;
                         Name      : in String;
                         Value     : in Util.Beans.Objects.Object;
                         Attribute : in Boolean := False;
                         Context   : in out Util.Serialize.Contexts.Context'Class) is
      Map : constant Mapper_Access := Mapper'Class (Handler).Find_Mapper (Name, Attribute);
   begin
      if Map /= null and then Map.Mapping /= null and then Map.Mapper /= null then
         Map.Mapper.all.Execute (Map.Mapping.all, Context, Value);
      end if;
   end Set_Member;

   procedure Start_Object (Handler : in Mapper;
                           Context : in out Util.Serialize.Contexts.Context'Class;
                           Name    : in String) is
   begin
      if Handler.Mapper /= null then
         Handler.Mapper.Start_Object (Context, Name);
      end if;
   end Start_Object;

   procedure Finish_Object (Handler : in Mapper;
                            Context : in out Util.Serialize.Contexts.Context'Class;
                            Name    : in String) is
   begin
      if Handler.Mapper /= null then
         Handler.Mapper.Finish_Object (Context, Name);
      end if;
   end Finish_Object;

   --  -----------------------
   --  Dump the mapping tree on the logger using the INFO log level.
   --  -----------------------
   procedure Dump (Handler : in Mapper'Class;
                   Log     : in Util.Log.Loggers.Logger'Class;
                   Prefix  : in String := "") is
      procedure Dump (Map : in Mapper'Class);

      --  -----------------------
      --  Dump the mapping description
      --  -----------------------
      procedure Dump (Map : in Mapper'Class) is
      begin
         if Map.Mapping /= null and then Map.Mapping.Is_Attribute then
            Log.Info (" {0}@{1}", Prefix,
                      Ada.Strings.Unbounded.To_String (Map.Mapping.Name));
         else
            Log.Info (" {0}/{1}", Prefix, Ada.Strings.Unbounded.To_String (Map.Name));
            Dump (Map, Log, Prefix & "/" & Ada.Strings.Unbounded.To_String (Map.Name));
         end if;
      end Dump;
   begin
      Iterate (Handler, Dump'Access);
   end Dump;

   procedure Iterate (Controller : in Mapper;
                      Process : not null access procedure (Map : in Mapper'Class)) is
      Node : Mapper_Access := Controller.First_Child;
   begin
      --  Pass 1: process the attributes first
      while Node /= null loop
         if Node.Mapping /= null and then Node.Mapping.Is_Attribute then
            Process.all (Node.all);
         end if;
         Node := Node.Next_Mapping;
      end loop;

      --  Pass 2: process the elements
      Node := Controller.First_Child;
      while Node /= null loop
         if Node.Mapping = null or else not Node.Mapping.Is_Attribute then
            Process.all (Node.all);
         end if;
         Node := Node.Next_Mapping;
      end loop;
   end Iterate;

   --  -----------------------
   --  Finalize the object and release any mapping.
   --  -----------------------
   overriding
   procedure Finalize (Controller : in out Mapper) is
      procedure Free is new Ada.Unchecked_Deallocation (Mapper'Class, Mapper_Access);
      procedure Free is new Ada.Unchecked_Deallocation (Mapping'Class, Mapping_Access);

      Node : Mapper_Access := Controller.First_Child;
      Next : Mapper_Access;
   begin
      Controller.First_Child := null;
      while Node /= null loop
         Next := Node.Next_Mapping;
         Free (Node);
         Node := Next;
      end loop;
      if not Controller.Is_Clone then
         Free (Controller.Mapping);
      else
         Controller.Mapping := null;
      end if;
   end Finalize;

end Util.Serialize.Mappers;
