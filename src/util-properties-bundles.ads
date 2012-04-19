-----------------------------------------------------------------------
--  properties-bundles -- Generic name/value property management
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2011, 2012 Stephane Carrez
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
with Ada.Containers;
with Ada.Finalization;
with Ada.Containers.Hashed_Maps;
with Util.Strings;
with Util.Concurrent.Locks;
package Util.Properties.Bundles is

   NO_BUNDLE : exception;

   NOT_WRITEABLE : exception;

   type Manager is new Util.Properties.Manager with private;

   --  ------------------------------
   --  Bundle loader
   --  ------------------------------
   --  The <b>Loader</b> provides facilities for loading property bundles
   --  and maintains a cache of bundles.  The cache is thread-safe but the returned
   --  bundles are not thread-safe.
   type Loader is limited private;
   type Loader_Access is access all Loader;

   --  Initialize the bundle factory and specify where the property files are stored.
   procedure Initialize (Factory : in out Loader;
                         Path    : in String);

   --  Load the bundle with the given name and for the given locale name.
   procedure Load_Bundle (Factory : in out Loader;
                          Name    : in String;
                          Locale  : in String;
                          Bundle  : out Manager'Class);
private

   procedure Add_Bundle (Self : in out Manager; Props : in Manager_Access);
   --  Add a bundle

   type Bundle_Manager_Access is access all Manager'Class;

   type Manager is new Util.Properties.Manager with null record;

   overriding
   procedure Initialize (Object : in out Manager);

   overriding
   procedure Adjust (Object : in out Manager);

   package Bundle_Map is
     new Ada.Containers.Hashed_Maps
       (Element_Type    => Bundle_Manager_Access,
        Key_Type        => Util.Strings.Name_Access,
        Hash            => Util.Strings.Hash,
        Equivalent_Keys => Util.Strings.Equivalent_Keys);

   type Loader is new Ada.Finalization.Limited_Controlled with record
      Lock    : Util.Concurrent.Locks.RW_Lock;
      Bundles : Bundle_Map.Map;
      Path    : Unbounded_String;
   end record;

   --  Finalize the bundle loader and clear the cache
   overriding
   procedure Finalize (Factory : in out Loader);

   --  Clear the cache bundle
   procedure Clear_Cache (Factory : in out Loader);

   --  Find the bundle with the given name and for the given locale name.
   procedure Find_Bundle (Factory : in out Loader;
                          Name    : in String;
                          Locale  : in String;
                          Bundle  : out Manager'Class;
                          Found   : out Boolean);

   --  Load the bundle with the given name and for the given locale name.
   procedure Load_Bundle (Factory : in out Loader;
                          Name    : in String;
                          Found   : out Boolean);

end Util.Properties.Bundles;
