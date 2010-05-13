-----------------------------------------------------------------------
--  properties -- Generic name/value property management
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Text_IO;
package Util.Properties is

   NO_PROPERTY : exception;

   use Ada.Strings.Unbounded;

   subtype Value is Ada.Strings.Unbounded.Unbounded_String;

   function "+" (S : String) return Value renames To_Unbounded_String;

   function "-" (S : Value) return String renames To_String;

   --  The manager holding the name/value pairs and providing the operations
   --  to get and set the properties.
   type Manager is new Ada.Finalization.Controlled with private;
   type Manager_Access is access Manager'Class;

   --  Returns TRUE if the property exists.
   function Exists (Self : in Manager'Class;
                    Name : in Value) return Boolean;

   --  Returns TRUE if the property exists.
   function Exists (Self : in Manager'Class;
                    Name : in String) return Boolean;

   --  Returns the property value.  Raises an exception if not found.
   function Get (Self : in Manager'Class;
                 Name : in String) return String;

   --  Returns the property value.  Raises an exception if not found.
   function Get (Self : in Manager'Class;
                 Name : in String) return Value;

   --  Returns the property value.  Raises an exception if not found.
   function Get (Self : in Manager'Class;
                 Name : in Value) return Value;

   --  Returns the property value.  Raises an exception if not found.
   function Get (Self : in Manager'Class;
                 Name : in Value) return String;

   --  Returns the property value or Default if it does not exist.
   function Get (Self : in Manager'Class;
                 Name : in String;
                 Default : in String) return String;

   --  Insert the specified property in the list.
   procedure Insert (Self : in out Manager'Class;
                     Name : in String;
                     Item : in String);

   --  Set the value of the property.  The property is created if it
   --  does not exists.
   procedure Set (Self : in out Manager'Class;
                  Name : in String;
                  Item : in String);

   --  Set the value of the property.  The property is created if it
   --  does not exists.
   procedure Set (Self : in out Manager'Class;
                  Name : in Unbounded_String;
                  Item : in Value);

   --  Remove the property given its name.  If the property does not
   --  exist, raises NO_PROPERTY exception.
   procedure Remove (Self : in out Manager'Class;
                     Name : in String);

   --  Remove the property given its name.  If the property does not
   --  exist, raises NO_PROPERTY exception.
   procedure Remove (Self : in out Manager'Class;
                     Name : in Value);

   type Name_Array is array (Natural range <>) of Value;

   --  Return the name of the properties defined in the manager.
   function Get_Names (Self : in Manager) return Name_Array;

   --  Load the properties from the file input stream.  The file must follow
   --  the definition of Java property files.
   procedure Load_Properties (Self : in out Manager'Class;
                              File : in Ada.Text_IO.File_Type);

   --  Load the properties from the file.  The file must follow the
   --  definition of Java property files.
   --  Raises NAME_ERROR if the file does not exist.
   procedure Load_Properties (Self : in out Manager'Class;
                              Path : in String);

   --  Copy the properties from FROM which start with a given prefix.
   --  If the prefix is empty, all properties are copied.
   procedure Copy (Self   : in out Manager'Class;
                   From   : in Manager'Class;
                   Prefix : in String := "");

private

   --  Abstract interface for the implementation of Properties
   --  (this allows to decouples the implementation from the API)
   package Interface_P is

      type Manager is abstract tagged limited record
         Count : Natural := 0;
      end record;
      type Manager_Access is access all Manager'Class;

      type Manager_Factory is access function return Manager_Access;

      --  Returns TRUE if the property exists.
      function Exists (Self : in Manager; Name : in Value)
                       return Boolean is abstract;

      --  Returns the property value.  Raises an exception if not found.
      function Get (Self : in Manager; Name : in Value)
                    return Value is abstract;

      procedure Insert (Self : in out Manager; Name : in Value;
                        Item : in Value) is abstract;

      --  Set the value of the property.  The property is created if it
      --  does not exists.
      procedure Set (Self : in out Manager; Name : in Value;
                     Item : in Value) is abstract;

      --  Remove the property given its name.
      procedure Remove (Self : in out Manager; Name : in Value) is abstract;

      --  Deep copy of properties stored in 'From' to 'To'.
      function Create_Copy (Self : in Manager)
                            return Manager_Access is abstract;

      procedure Delete (Self : in Manager; Obj : in out Manager_Access)
        is abstract;

      function Get_Names (Self : in Manager) return Name_Array is abstract;

   end Interface_P;

   procedure Set_Property_Implementation (Self : in out Manager;
                                          Impl : in Interface_P.Manager_Access);

   --  Create a property implementation if there is none yet.
   procedure Check_And_Create_Impl (Self : in out Manager);

   type Manager is new Ada.Finalization.Controlled with record
      Impl : Interface_P.Manager_Access := null;
   end record;

   procedure Adjust   (Object : in out Manager);
   procedure Finalize (Object : in out Manager);

end Util.Properties;
