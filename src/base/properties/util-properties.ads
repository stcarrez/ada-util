-----------------------------------------------------------------------
--  util-properties -- Generic name/value property management
--  Copyright (C) 2001 - 2020 Stephane Carrez
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
with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Strings.Vectors;
private with Util.Concurrent.Counters;

--  = Property Files =
--  The `Util.Properties` package and children implements support to read, write and use
--  property files either in the Java property file format or the Windows INI configuration file.
--  Each property is assigned a key and a value.  The list of properties are stored in the
--  `Util.Properties.Manager` tagged record and they are indexed by the key name.  A property
--  is therefore unique in the list.  Properties can be grouped together in sub-properties so
--  that a key can represent another list of properties.  To use the packages described here,
--  use the following GNAT project:
--
--    with "utilada_base";
--
--  == File formats ==
--  The property file consists of a simple name and value pair separated by the `=` sign.
--  Thanks to the Windows INI file format, list of properties can be grouped together
--  in sections by using the `[section-name]` notation.
--
--    test.count=20
--    test.repeat=5
--    [FileTest]
--    test.count=5
--    test.repeat=2
--
--  == Using property files ==
--  An instance of the `Util.Properties.Manager` tagged record must be declared and it provides
--  various operations that can be used.  When created, the property manager is empty.  One way
--  to fill it is by using the `Load_Properties` procedure to read the property file.  Another
--  way is by using the `Set` procedure to insert or change a property by giving its name
--  and its value.
--
--  In this example, the property file `test.properties` is loaded and assuming that it contains
--  the above configuration example, the `Get ("test.count")` will return the string `"20"`.
--  The property `test.repeat` is then modified to have the value `"23"` and the properties are
--  then saved in the file.
--
--    with Util.Properties;
--    ...
--       Props : Util.Properties.Manager;
--       ...
--          Props.Load_Properties (Path => "test.properties");
--          Ada.Text_IO.Put_Line ("Count: " & Props.Get ("test.count");
--          Props.Set ("test.repeat", "23");
--          Props.Save_Properties (Path => "test.properties");
--
--  To be able to access a section from the property manager, it is necessary to retrieve it
--  by using the `Get` function and giving the section name.  For example, to retrieve the
--  `test.count` property of the `FileTest` section, the following code is used:
--
--       FileTest : Util.Properties.Manager := Props.Get ("FileTest");
--       ...
--          Ada.Text_IO.Put_Line ("[FileTest] Count: "
--                                & FileTest.Get ("test.count");
--
--  When getting or removing a property, the `NO_PROPERTY` exception is raised if the property
--  name was not found in the map.  To avoid that exception, it is possible to check whether
--  the name is known by using the `Exists` function.
--
--       if Props.Exists ("test.old_count") then
--          ... --  Property exist
--       end if;
--
--  @include util-properties-json.ads
--  @include util-properties-bundles.ads
--
--  == Advance usage of properties ==
--  The property manager holds the name and value pairs by using an Ada Bean object.
--
--  It is possible to iterate over the properties by using the `Iterate` procedure that
--  accepts as parameter a `Process` procedure that gets the property name as well as the
--  property value.  The value itself is passed as an `Util.Beans.Objects.Object` type.
--
package Util.Properties is

   NO_PROPERTY : exception;

   use Ada.Strings.Unbounded;

   subtype Value is Util.Beans.Objects.Object;

   function "+" (S : String) return Unbounded_String renames To_Unbounded_String;

   function "-" (S : Unbounded_String) return String renames To_String;

   function To_String (V : in Value) return String
     renames Util.Beans.Objects.To_String;

   --  The manager holding the name/value pairs and providing the operations
   --  to get and set the properties.
   type Manager is new Ada.Finalization.Controlled and Util.Beans.Basic.Bean with private;
   type Manager_Access is access all Manager'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Manager;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   --  If the map contains the given name, the value changed.
   --  Otherwise name is added to the map and the value associated with it.
   overriding
   procedure Set_Value (From  : in out Manager;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Returns TRUE if the property manager is empty.
   function Is_Empty (Self : in Manager'Class) return Boolean;

   --  Returns TRUE if the property exists.
   function Exists (Self : in Manager'Class;
                    Name : in Unbounded_String) return Boolean;

   --  Returns TRUE if the property exists.
   function Exists (Self : in Manager'Class;
                    Name : in String) return Boolean;

   --  Returns the property value.  Raises an exception if not found.
   function Get (Self : in Manager'Class;
                 Name : in String) return String;

   --  Returns the property value.  Raises an exception if not found.
   function Get (Self : in Manager'Class;
                 Name : in String) return Unbounded_String;

   --  Returns the property value.  Raises an exception if not found.
   function Get (Self : in Manager'Class;
                 Name : in Unbounded_String) return Unbounded_String;

   --  Returns the property value.  Raises an exception if not found.
   function Get (Self : in Manager'Class;
                 Name : in Unbounded_String) return String;

   --  Returns the property value or Default if it does not exist.
   function Get (Self : in Manager'Class;
                 Name : in String;
                 Default : in String) return String;

   --  Returns a property manager that is associated with the given name.
   --  Raises NO_PROPERTY if there is no such property manager or if a property exists
   --  but is not a property manager.
   function Get (Self : in Manager'Class;
                 Name : in String) return Manager;

   --  Create a property manager and associated it with the given name.
   function Create (Self : in out Manager'Class;
                    Name : in String) return Manager;

   --  Set the value of the property.  The property is created if it
   --  does not exists.
   procedure Set (Self : in out Manager'Class;
                  Name : in String;
                  Item : in String);

   --  Set the value of the property.  The property is created if it
   --  does not exists.
   procedure Set (Self : in out Manager'Class;
                  Name : in String;
                  Item : in Unbounded_String);

   --  Set the value of the property.  The property is created if it
   --  does not exists.
   procedure Set (Self : in out Manager'Class;
                  Name : in Unbounded_String;
                  Item : in Unbounded_String);

   --  Remove the property given its name.  If the property does not
   --  exist, raises NO_PROPERTY exception.
   procedure Remove (Self : in out Manager'Class;
                     Name : in String);

   --  Remove the property given its name.  If the property does not
   --  exist, raises NO_PROPERTY exception.
   procedure Remove (Self : in out Manager'Class;
                     Name : in Unbounded_String);

   --  Iterate over the properties and execute the given procedure passing the
   --  property name and its value.
   procedure Iterate (Self    : in Manager'Class;
                      Process : access procedure (Name : in String;
                                                  Item : in Value));

   --  Collect the name of the properties defined in the manager.
   --  When a prefix is specified, only the properties starting with the prefix are
   --  returned.
   procedure Get_Names (Self   : in Manager;
                        Into   : in out Util.Strings.Vectors.Vector;
                        Prefix : in String := "");

   --  Load the properties from the file input stream.  The file must follow
   --  the definition of Java property files.  When a prefix is specified, keep
   --  only the properties that starts with the prefix.  When <b>Strip</b> is True,
   --  the prefix part is removed from the property name.
   procedure Load_Properties (Self   : in out Manager'Class;
                              File   : in Ada.Text_IO.File_Type;
                              Prefix : in String := "";
                              Strip  : in Boolean := False);

   --  Load the properties from the file.  The file must follow the
   --  definition of Java property files.  When a prefix is specified, keep
   --  only the properties that starts with the prefix.  When <b>Strip</b> is True,
   --  the prefix part is removed from the property name.
   --  Raises NAME_ERROR if the file does not exist.
   procedure Load_Properties (Self   : in out Manager'Class;
                              Path   : in String;
                              Prefix : in String := "";
                              Strip  : in Boolean := False);

   --  Save the properties in the given file path.
   procedure Save_Properties (Self   : in out Manager'Class;
                              Path   : in String;
                              Prefix : in String := "");

   --  Copy the properties from FROM which start with a given prefix.
   --  If the prefix is empty, all properties are copied.  When <b>Strip</b> is True,
   --  the prefix part is removed from the property name.
   procedure Copy (Self   : in out Manager'Class;
                   From   : in Manager'Class;
                   Prefix : in String := "";
                   Strip  : in Boolean := False);

   --  Get the property manager represented by the item value.
   --  Raise the Conversion_Error exception if the value is not a property manager.
   function To_Manager (Item : in Value) return Manager;

   --  Returns True if the item value represents a property manager.
   function Is_Manager (Item : in Value) return Boolean;

   --  Abstract interface for the implementation of Properties
   --  (this allows to decouples the implementation from the API)
   package Implementation is

      type Manager is limited interface and Util.Beans.Basic.Bean;
      type Manager_Access is access all Manager'Class;

      --  Returns TRUE if the property exists.
      function Exists (Self : in Manager;
                       Name : in String)
                       return Boolean is abstract;

      --  Remove the property given its name.
      procedure Remove (Self : in out Manager;
                        Name : in String) is abstract;

      --  Iterate over the properties and execute the given procedure passing the
      --  property name and its value.
      procedure Iterate
        (Self    : in Manager;
         Process : access procedure (Name : in String;
                                     Item : in Value)) is abstract;

      --  Deep copy of properties stored in 'From' to 'To'.
      function Create_Copy (Self : in Manager)
                            return Manager_Access is abstract;

      type Shared_Manager is limited interface and Manager;
      type Shared_Manager_Access is access all Shared_Manager'Class;

      function Is_Shared (Self : in Shared_Manager) return Boolean is abstract;

      procedure Set_Shared (Self   : in out Shared_Manager;
                            Shared : in Boolean) is abstract;

      procedure Adjust (Self : in out Shared_Manager) is abstract;

      procedure Finalize (Self    : in out Shared_Manager;
                          Release : out Boolean) is abstract;

      generic
         with function Allocator return Shared_Manager_Access;
      procedure Create (Self : in out Util.Properties.Manager'Class);

      generic
         with function Allocator return Shared_Manager_Access;
      procedure Initialize (Self : in out Util.Properties.Manager'Class);

      generic
         type Manager_Type is limited new Manager with private;
      package Shared_Implementation is

         type Manager is limited new Manager_Type and Shared_Manager with private;

         overriding
         function Is_Shared (Self : in Manager) return Boolean;

         overriding
         procedure Set_Shared (Self   : in out Manager;
                               Shared : in Boolean);

         overriding
         procedure Adjust (Self : in out Manager);

         overriding
         procedure Finalize (Self    : in out Manager;
                             Release : out Boolean);

      private

         type Manager is limited new Manager_Type and Shared_Manager with record
            Count  : Util.Concurrent.Counters.Counter := Util.Concurrent.Counters.ONE;
            Shared : Boolean := False;
         end record;

      end Shared_Implementation;

   end Implementation;

private

   type Manager is new Ada.Finalization.Controlled and Util.Beans.Basic.Bean with record
      Impl : Implementation.Shared_Manager_Access := null;
   end record;

   overriding
   procedure Adjust   (Object : in out Manager);

   overriding
   procedure Finalize (Object : in out Manager);

end Util.Properties;
