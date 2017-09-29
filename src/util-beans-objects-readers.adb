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

   overriding
   procedure Start_Array (Handler : in out Parser;
                          Name    : in String) is
   begin
      Handler.Start_Object (Name);
--      Util.Serialize.IO.JSON.Parser (Handler).Start_Array (Name);
   end Start_Array;

   overriding
   procedure Finish_Array (Handler : in out Parser;
                           Name    : in String;
                           Count   : in Natural) is
   begin
      Parser'Class (Handler).Set_Member ("length", Util.Beans.Objects.To_Object (Count));
      Handler.Finish_Object (Name);
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
   begin
      Handler.Current.Set_Value (Name, Value);
   end Set_Member;

   --  Report an error while parsing the input stream.  The error message will be reported
   --  on the logger associated with the parser.  The parser will be set as in error so that
   --  the <b>Has_Error</b> function will return True after parsing the whole file.
   procedure Error (Handler : in out Parser;
                    Message : in String) is
   begin
      null;
   end Error;

end Util.Beans.Objects.Readers;
