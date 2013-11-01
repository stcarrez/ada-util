-----------------------------------------------------------------------
--  util-properties-json -- read json files into properties
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
with Util.Serialize.IO.JSON;
with Util.Stacks;
with Util.Beans.Objects;
package body Util.Properties.JSON is

   type Natural_Access is access all Natural;

   package Length_Stack is new Util.Stacks (Element_Type        => Natural,
                                            Element_Type_Access => Natural_Access);

   type Parser is new Util.Serialize.IO.JSON.Parser with record
      Base_Name        : Ada.Strings.Unbounded.Unbounded_String;
      Lengths          : Length_Stack.Stack;
      Separator        : Ada.Strings.Unbounded.Unbounded_String;
      Separator_Length : Natural;
   end record;

   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   overriding
   procedure Start_Object (Handler : in out Parser;
                           Name    : in String);

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   overriding
   procedure Finish_Object (Handler : in out Parser;
                            Name    : in String);

   --  -----------------------
   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   --  -----------------------
   overriding
   procedure Start_Object (Handler : in out Parser;
                           Name    : in String) is
   begin
      if Name'Length > 0 then
         Ada.Strings.Unbounded.Append (Handler.Base_Name, Name);
         Ada.Strings.Unbounded.Append (Handler.Base_Name, Handler.Separator);
         Length_Stack.Push (Handler.Lengths);
         Length_Stack.Current (Handler.Lengths).all := Name'Length + Handler.Separator_Length;
      end if;
   end Start_Object;

   --  -----------------------
   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   --  -----------------------
   overriding
   procedure Finish_Object (Handler : in out Parser;
                            Name    : in String) is
      Len : constant Natural := Ada.Strings.Unbounded.Length (Handler.Base_Name);
   begin
      if Name'Length > 0 then
         Ada.Strings.Unbounded.Delete (Handler.Base_Name,
                                       Len - Name'Length - Handler.Separator_Length + 1, Len);
      end if;
   end Finish_Object;

   --  -----------------------
   --  Parse the JSON content and put the flattened content in the property manager.
   --  -----------------------
   procedure Parse_JSON (Manager : in out Util.Properties.Manager'Class;
                         Content : in String;
                         Flatten_Separator : in String := ".") is

      type Local_Parser is new Parser with record
         Manager : access Util.Properties.Manager'Class;
      end record;

      --  Set the name/value pair on the current object.  For each active mapping,
      --  find whether a rule matches our name and execute it.
      overriding
      procedure Set_Member (Handler   : in out Local_Parser;
                            Name      : in String;
                            Value     : in Util.Beans.Objects.Object;
                            Attribute : in Boolean := False);

      --  -----------------------
      --  Set the name/value pair on the current object.  For each active mapping,
      --  find whether a rule matches our name and execute it.
      --  -----------------------
      overriding
      procedure Set_Member (Handler   : in out Local_Parser;
                            Name      : in String;
                            Value     : in Util.Beans.Objects.Object;
                            Attribute : in Boolean := False) is
         pragma Unreferenced (Attribute);
      begin
         Handler.Manager.Set (Ada.Strings.Unbounded.To_String (Handler.Base_Name) & Name,
                              Util.Beans.Objects.To_String (Value));
      end Set_Member;

      P : Local_Parser;
   begin
      P.Separator        := Ada.Strings.Unbounded.To_Unbounded_String (Flatten_Separator);
      P.Separator_Length := Flatten_Separator'Length;
      P.Manager          := Manager'Access;
      P.Parse_String (Content);
      if P.Has_Error then
         raise Util.Serialize.IO.Parse_Error;
      end if;
   end Parse_JSON;

   --  -----------------------
   --  Read the JSON file into the property manager.
   --  The JSON content is flatten into Flatten the JSON content and add the properties.
   --  -----------------------
   procedure Read_JSON (Manager : in out Util.Properties.Manager'Class;
                        Path    : in String;
                        Flatten_Separator : in String := ".") is

      type Local_Parser is new Parser with record
         Manager : access Util.Properties.Manager'Class;
      end record;

      --  Set the name/value pair on the current object.  For each active mapping,
      --  find whether a rule matches our name and execute it.
      overriding
      procedure Set_Member (Handler   : in out Local_Parser;
                            Name      : in String;
                            Value     : in Util.Beans.Objects.Object;
                            Attribute : in Boolean := False);

      --  -----------------------
      --  Set the name/value pair on the current object.  For each active mapping,
      --  find whether a rule matches our name and execute it.
      --  -----------------------
      overriding
      procedure Set_Member (Handler   : in out Local_Parser;
                            Name      : in String;
                            Value     : in Util.Beans.Objects.Object;
                            Attribute : in Boolean := False) is
         pragma Unreferenced (Attribute);
      begin
         Handler.Manager.Set (Ada.Strings.Unbounded.To_String (Handler.Base_Name) & Name,
                              Util.Beans.Objects.To_String (Value));
      end Set_Member;

      P : Local_Parser;
   begin
      P.Manager   := Manager'Access;
      P.Separator := Ada.Strings.Unbounded.To_Unbounded_String (Flatten_Separator);
      P.Separator_Length := Flatten_Separator'Length;
      P.Parse (Path);
      if P.Has_Error then
         raise Util.Serialize.IO.Parse_Error;
      end if;
   end Read_JSON;

end Util.Properties.JSON;
