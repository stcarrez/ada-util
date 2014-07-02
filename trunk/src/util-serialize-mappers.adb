-----------------------------------------------------------------------
--  util-serialize-mappers -- Serialize objects in various formats
--  Copyright (C) 2010, 2011, 2012, 2014 Stephane Carrez
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
with System.Address_Image;
with Util.Strings;

with Ada.Tags;
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
   --  Returns true if the mapper is a wildcard node (matches any element).
   --  -----------------------
   function Is_Wildcard (Controller : in Mapper) return Boolean is
   begin
      return Controller.Is_Wildcard;
   end Is_Wildcard;

   --  -----------------------
   --  Returns the mapping name.
   --  -----------------------
   function Get_Name (Controller : in Mapper) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Controller.Name);
   end Get_Name;

   --  -----------------------
   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   --  -----------------------
   function Find_Mapper (Controller : in Mapper;
                         Name       : in String;
                         Attribute  : in Boolean := False) return Mapper_Access is
      use type Ada.Strings.Unbounded.Unbounded_String;

      Node    : Mapper_Access := Controller.First_Child;
      Recurse : Boolean := True;
      Result  : Mapper_Access := null;
   begin
      if Node = null and Controller.Mapper /= null then
         return Controller.Mapper.Find_Mapper (Name, Attribute);
      end if;
      while Node /= null loop
         if not Attribute and Node.Is_Wildcard then
            Result := Node.Find_Mapper (Name, Attribute);
            if Result /= null then
               return Result;
            else
               return Node;
            end if;
         end if;
         if Node.Name = Name then
            if (Attribute = False and Node.Mapping = null)
              or else not Node.Mapping.Is_Attribute
            then
               return Node;
            end if;
            if Attribute and Node.Mapping.Is_Attribute then
               return Node;
            end if;
         end if;
         if Node.Is_Deep_Wildcard and not Attribute and Node.Mapper /= null and Recurse then
            Node := Node.Mapper.First_Child;
            Result := Node.Mapper;
            Recurse := False;
         else
            Node := Node.Next_Mapping;
         end if;
      end loop;
      return Result;
   end Find_Mapper;

   --  -----------------------
   --  Find a path component representing a child mapper under <b>From</b> and
   --  identified by the given <b>Name</b>.  If the mapper is not found, a new
   --  Mapper_Node is created.
   --  -----------------------
   procedure Find_Path_Component (From   : in out Mapper'Class;
                                  Name   : in String;
                                  Root   : in out Mapper_Access;
                                  Result : out Mapper_Access) is
      use Ada.Strings.Unbounded;

      Node          : Mapper_Access := From.First_Child;
      Previous      : Mapper_Access := null;
      Wildcard      : constant Boolean := Name = "*";
      Deep_Wildcard : constant Boolean := Name = "**";
   begin
      if Root = null and Deep_Wildcard then
         Root := Node;
      end if;
      if Node = null then
         Result := new Mapper;
         Result.Name             := To_Unbounded_String (Name);
         Result.Is_Wildcard      := Wildcard or Deep_Wildcard;
         Result.Is_Deep_Wildcard := Deep_Wildcard;
         From.First_Child := Result;
      else
         loop
            if Node.Name = Name then
               Result := Node;
               exit;
            end if;
            if Node.Next_Mapping = null then
               Result := new Mapper;
               Result.Name             := To_Unbounded_String (Name);
               Result.Is_Wildcard      := Wildcard or Deep_Wildcard;
               Result.Is_Deep_Wildcard := Deep_Wildcard;
               if not Wildcard and not Deep_Wildcard then
                  Result.Next_Mapping := Node;
                  if Previous = null then
                     From.First_Child := Result;
                  else
                     Previous.Next_Mapping := Result;
                  end if;
               else
                  Node.Next_Mapping := Result;
               end if;

               exit;
            end if;
            Previous := Node;
            Node := Node.Next_Mapping;
         end loop;
      end if;

      --  For deep wildcard mapping the mapping tree has to somehow redirect and use a
      --  root node (ie, the '**' node).  Create a proxy node to point to that wildcard root.
      --  The wildcard nodes must be checked last and therefore appear at end of the mapping list.
      if Root /= null then
         Previous := Result;
         while Previous.Next_Mapping /= null loop
            Previous := Previous.Next_Mapping;
         end loop;

         if not Previous.Is_Wildcard and not Previous.Is_Deep_Wildcard then
            Node := new Mapper;
            Node.Name             := To_Unbounded_String ("**");
            Node.Is_Deep_Wildcard := True;
            Node.Mapper           := Root;
            Previous.Next_Mapping := Node;
         end if;
      end if;
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
      Root     : Mapper_Access := null;
   begin
      Node     := Into'Unchecked_Access;
      Last_Pos := Path'First;
      loop
         Pos := Util.Strings.Index (Source => Path,
                                    Char   => '/',
                                    From   => Last_Pos);
         if Pos = 0 then
            Node.Find_Path_Component (Name   => Path (Last_Pos .. Path'Last),
                                      Root   => Root,
                                      Result => Node);
            Last_Pos := Path'Last + 1;
         else
            Node.Find_Path_Component (Name   => Path (Last_Pos .. Pos - 1),
                                      Root   => Root,
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
   --     */a/b/name         matches:  <i><i><j><a><b><name>...</name></b></a></j></i></i>
   --     **/name            matches:  <i><name>...</name></i>, <b><c><name>...</name></c></b>
   --  -----------------------
   procedure Add_Mapping (Into : in out Mapper;
                          Path : in String;
                          Map  : in Mapper_Access) is
      procedure Copy (To   : in Mapper_Access;
                      From : in Mapper_Access);
      procedure Add_Mapper (From, To : in Mapper_Access);
      function Find_Mapper (From : in Mapper_Access) return Mapper_Access;
      procedure Append (To   : in Mapper_Access;
                        Item : in Mapper_Access);

      --  For the support of deep wildcard mapping (**), we must map a proxy node mapper
      --  to the copy that was made.  We maintain a small list of mapper pairs.
      --  The implementation is intended to be simple for now...
      type Mapper_Pair is record
         First  : Mapper_Access;
         Second : Mapper_Access;
      end record;

      Node     : Mapper_Access;
      Last_Pos : Natural;
      Mappers  : array (1 .. 10) of Mapper_Pair;

      procedure Add_Mapper (From, To : in Mapper_Access) is
      begin
         for I in Mappers'Range loop
            if Mappers (I).First = null then
               Mappers (I).First := From;
               Mappers (I).Second := To;
               return;
            end if;
         end loop;
         Log.Error ("Too many wildcard mappers");
         raise Mapping_Error with "Too many wildcard mapping, mapping is too complex!";
      end Add_Mapper;

      function Find_Mapper (From : in Mapper_Access) return Mapper_Access is
      begin
         for I in Mappers'Range loop
            if Mappers (I).First = From then
               return Mappers (I).Second;
            end if;
         end loop;
         Log.Error ("Cannot find mapper {0}", System.Address_Image (From.all'Address));
         return null;
      end Find_Mapper;

      procedure Append (To   : in Mapper_Access;
                        Item : in Mapper_Access) is
         Node : Mapper_Access := To.First_Child;
      begin
         if Node = null then
            To.First_Child := Item;
         else
            while Node.Next_Mapping /= null loop
               Node := Node.Next_Mapping;
            end loop;
            Node.Next_Mapping := Item;
         end if;
      end Append;

      procedure Copy (To   : in Mapper_Access;
                      From : in Mapper_Access) is
         N   : Mapper_Access;
         Src : Mapper_Access := From;
      begin
         while Src /= null loop
            N := Src.Clone;
            N.Is_Clone := True;
            if N.Is_Deep_Wildcard then
               if N.Mapper /= null then
                  N.Mapper := Find_Mapper (N.Mapper);
               else
                  Add_Mapper (Src, N);
               end if;
            end if;
            Append (To, N);
            if Src.First_Child /= null then
               Copy (N, Src.First_Child);
            end if;
            Src := Src.Next_Mapping;
         end loop;
      end Copy;

      use type Util.Log.Level_Type;
   begin
      if Log.Get_Level >= Util.Log.INFO_LEVEL then
         Log.Info ("Mapping '{0}' for mapper {1}",
                   Path, Ada.Tags.Expanded_Name (Map'Tag));
      end if;

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
      if Log.Get_Level >= Util.Log.INFO_LEVEL then
         Log.Info ("Mapping '{0}' for mapper {1}",
                   Path, Ada.Tags.Expanded_Name (Map'Tag));
      end if;

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
      Result.Name             := Handler.Name;
      Result.Mapper           := Handler.Mapper;
      Result.Mapping          := Handler.Mapping;
      Result.Is_Proxy_Mapper  := Handler.Is_Proxy_Mapper;
      Result.Is_Clone         := True;
      Result.Is_Wildcard      := Handler.Is_Wildcard;
      Result.Is_Deep_Wildcard := Handler.Is_Deep_Wildcard;
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
         Name : constant String := Ada.Strings.Unbounded.To_String (Map.Name);
      begin
         if Map.Mapping /= null and then Map.Mapping.Is_Attribute then
            Log.Info (" {0}@{1}", Prefix,
                      Ada.Strings.Unbounded.To_String (Map.Mapping.Name));
         else
            Log.Info (" {0}/{1}", Prefix, Name);
            Dump (Map, Log, Prefix & "/" & Name);
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
