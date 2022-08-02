-----------------------------------------------------------------------
--  util-beans -- Interface Definition with Getter and Setters
--  Copyright (C) 2009, 2010, 2018, 2019, 2022 Stephane Carrez
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

--  = Ada Beans =
--  A [Java Bean](http://en.wikipedia.org/wiki/JavaBean) is an object that
--  allows to access its properties through getters and setters. Java Beans
--  rely on the use of Java introspection to discover the Java Bean object properties.
--
--  An Ada Bean has some similarities with the Java Bean as it tries to expose
--  an object through a set of common interfaces.  Since Ada does not have introspection,
--  some developer work is necessary. The Ada Bean framework consists of:
--
--    * An `Object` concrete type that allows to hold any data type such
--      as boolean, integer, floats, strings, dates and Ada bean objects.
--    * A `Bean` interface that exposes a `Get_Value` and `Set_Value`
--      operation through which the object properties can be obtained and modified.
--    * A `Method_Bean` interface that exposes a set of method bindings
--      that gives access to the methods provided by the Ada Bean object.
--
--  The benefit of Ada beans comes when you need to get a value or invoke
--  a method on an object but you don't know at compile time the object or method.
--  That step being done later through some external configuration or presentation file.
--
--  The Ada Bean framework is the basis for the implementation of
--  Ada Server Faces and Ada EL.  It allows the presentation layer to
--  access information provided by Ada beans.
--
--  To use the packages described here, use the following GNAT project:
--
--    with "utilada_base";
--
--  @include util-beans-objects.ads
--  @include util-beans-objects-maps.ads
--  @include util-beans-objects-vectors.ads
--  @include util-beans-objects-datasets.ads
--  @include util-beans-objects-iterators.ads
--  @include util-beans-basic.ads
package Util.Beans is

   pragma Preelaborate;

end Util.Beans;
