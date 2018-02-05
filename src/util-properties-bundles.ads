-----------------------------------------------------------------------
--  util-properties-bundles -- Generic name/value property management
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2011, 2012, 2018 Stephane Carrez
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

--  == Property bundles ==
--  Property bundles represent several property files that share some overriding rules and
--  capabilities.  Their introduction comes from Java resource bundles which allow to
--  localize easily some configuration files or some message.  When loading a property bundle
--  a locale is defined to specify the target language and locale.  If a specific property
--  file for that locale exists, it is used first.  Otherwise, the property bundle will use
--  the default property file.
--
--  A rule exists on the name of the specific property locale file: it must start with the
--  bundle name followed by `_` and the name of the locale.  The default property file must
--  be the bundle name.  For example, the bundle `dates` is associated with the following
--  property files:
--
--    dates.properties           Default values (English locale)
--    dates_fr.properties        French locale
--    dates_de.properties        German locale
--    dates_es.properties        Spain locale
--
--  Because a bundle can be associated with one or several property files, a specific loader is
--  used.  The loader instance must be declared and configured to indicate one or several search
--  directories that contain property files.
--
--    with Util.Properties.Bundles;
--    ...
--       Loader : Util.Properties.Bundles.Loader;
--       Bundle : Util.Properties.Bundles.Manager;
--       ...
--       Util.Properties.Bundles.Initialize (Loader,
--                                           "bundles;/usr/share/bundles");
--       Util.Properties.Bundles.Load_Bundle (Loader, "dates", "fr", Bundle);
--       Ada.Text_IO.Put_Line (Bundle.Get ("util.month1.long");
--
--  In this example, the `util.month1.long` key is first searched in the `dates_fr` French locale
--  and if it is not found it is searched in the default locale.
--
--  The restriction when using bundles is that they don't allow changing any value and the
--  `NOT_WRITEABLE` exception is raised when one of the `Set` operation is used.
--
--  When a bundle cannot be loaded, the `NO_BUNDLE` exception is raised by the `Load_Bundle`
--  operation.
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
