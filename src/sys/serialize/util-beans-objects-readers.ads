-----------------------------------------------------------------------
--  util-beans-objects-readers -- Datasets
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Beans.Objects.Maps;
with Util.Beans.Objects.Vectors;
with Util.Serialize.IO;
with Util.Stacks;
with Util.Log;
package Util.Beans.Objects.Readers is

   type Reader is limited new Util.Serialize.IO.Reader with private;

   --  Start a document.
   overriding
   procedure Start_Document (Handler : in out Reader);

   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   overriding
   procedure Start_Object (Handler : in out Reader;
                           Name    : in String;
                           Logger  : in out Util.Log.Logging'Class);

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   overriding
   procedure Finish_Object (Handler : in out Reader;
                            Name    : in String;
                            Logger  : in out Util.Log.Logging'Class);

   overriding
   procedure Start_Array (Handler : in out Reader;
                          Name    : in String;
                          Logger  : in out Util.Log.Logging'Class);

   overriding
   procedure Finish_Array (Handler : in out Reader;
                           Name    : in String;
                           Count   : in Natural;
                           Logger  : in out Util.Log.Logging'Class);

   --  Set the name/value pair on the current object.  For each active mapping,
   --  find whether a rule matches our name and execute it.
   overriding
   procedure Set_Member (Handler   : in out Reader;
                         Name      : in String;
                         Value     : in Util.Beans.Objects.Object;
                         Logger    : in out Util.Log.Logging'Class;
                         Attribute : in Boolean := False);

   --  Get the root object.
   function Get_Root (Handler : in Reader) return Object;

private

   type Object_Context is record
      Map  : Util.Beans.Objects.Maps.Map_Bean_Access;
      List : Util.Beans.Objects.Vectors.Vector_Bean_Access;
   end record;
   type Object_Context_Access is access all Object_Context;

   package Object_Stack is new Util.Stacks (Element_Type        => Object_Context,
                                            Element_Type_Access => Object_Context_Access);

   type Reader is limited new Util.Serialize.IO.Reader with record
      Context  : Object_Stack.Stack;
      Root     : Util.Beans.Objects.Object;
   end record;

end Util.Beans.Objects.Readers;
