-----------------------------------------------------------------------
--  util-properties-form -- read json files into properties
--  Copyright (C) 2020 Stephane Carrez
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
with Util.Serialize.IO.Form;
with Util.Stacks;
with Util.Log;
with Util.Beans.Objects;
package body Util.Properties.Form is

   type Natural_Access is access all Natural;

   package Length_Stack is new Util.Stacks (Element_Type        => Natural,
                                            Element_Type_Access => Natural_Access);

   type Parser is abstract limited new Util.Serialize.IO.Reader with record
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
                           Name    : in String;
                           Logger  : in out Util.Log.Logging'Class);

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   overriding
   procedure Finish_Object (Handler : in out Parser;
                            Name    : in String;
                            Logger  : in out Util.Log.Logging'Class);

   overriding
   procedure Start_Array (Handler : in out Parser;
                          Name    : in String;
                          Logger  : in out Util.Log.Logging'Class);

   overriding
   procedure Finish_Array (Handler : in out Parser;
                           Name    : in String;
                           Count   : in Natural;
                           Logger  : in out Util.Log.Logging'Class);

   --  -----------------------
   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   --  -----------------------
   overriding
   procedure Start_Object (Handler : in out Parser;
                           Name    : in String;
                           Logger  : in out Util.Log.Logging'Class) is
      pragma Unreferenced (Logger);
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
                            Name    : in String;
                            Logger  : in out Util.Log.Logging'Class) is
      pragma Unreferenced (Logger);
      Len : constant Natural := Ada.Strings.Unbounded.Length (Handler.Base_Name);
   begin
      if Name'Length > 0 then
         Ada.Strings.Unbounded.Delete (Handler.Base_Name,
                                       Len - Name'Length - Handler.Separator_Length + 1, Len);
      end if;
   end Finish_Object;

   overriding
   procedure Start_Array (Handler : in out Parser;
                          Name    : in String;
                          Logger  : in out Util.Log.Logging'Class) is
   begin
      Handler.Start_Object (Name, Logger);
   end Start_Array;

   overriding
   procedure Finish_Array (Handler : in out Parser;
                           Name    : in String;
                           Count   : in Natural;
                           Logger  : in out Util.Log.Logging'Class) is
   begin
      Parser'Class (Handler).Set_Member ("length", Util.Beans.Objects.To_Object (Count), Logger);
      Handler.Finish_Object (Name, Logger);
   end Finish_Array;

   --  -----------------------
   --  Parse the application/form content and put the flattened content in the property manager.
   --  -----------------------
   procedure Parse_Form (Manager : in out Util.Properties.Manager'Class;
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
                            Logger    : in out Util.Log.Logging'Class;
                            Attribute : in Boolean := False);

      --  -----------------------
      --  Set the name/value pair on the current object.  For each active mapping,
      --  find whether a rule matches our name and execute it.
      --  -----------------------
      overriding
      procedure Set_Member (Handler   : in out Local_Parser;
                            Name      : in String;
                            Value     : in Util.Beans.Objects.Object;
                            Logger    : in out Util.Log.Logging'Class;
                            Attribute : in Boolean := False) is
         pragma Unreferenced (Logger, Attribute);
      begin
         Handler.Manager.Set (Ada.Strings.Unbounded.To_String (Handler.Base_Name) & Name,
                              Util.Beans.Objects.To_String (Value));
      end Set_Member;

      P : Local_Parser;
      R : Util.Serialize.IO.Form.Parser;
   begin
      P.Separator        := Ada.Strings.Unbounded.To_Unbounded_String (Flatten_Separator);
      P.Separator_Length := Flatten_Separator'Length;
      P.Manager          := Manager'Access;
      R.Parse_String (Content, P);
      if R.Has_Error then
         raise Util.Serialize.IO.Parse_Error;
      end if;
   end Parse_Form;

end Util.Properties.Form;
