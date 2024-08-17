-----------------------------------------------------------------------
--  properties -- Example of properties
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Exceptions;
with Util.Properties.Basic;

procedure Properties is

   use Util.Properties.Basic;

   Properties : Util.Properties.Manager;

   Count : Integer;
begin
   Properties.Load_Properties (Path => "samples/test.properties");

   Count := Integer_Property.Get (Properties, "test.count");
   Ada.Text_IO.Put_Line ("test.count = " & Integer'Image (Count));

   Count := Integer_Property.Get (Properties, "test.repeat");
   Ada.Text_IO.Put_Line ("test.repeat = " & Integer'Image (Count));

exception
   when E : Util.Properties.NO_PROPERTY =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
end Properties;
