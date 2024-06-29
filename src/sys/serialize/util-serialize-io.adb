-----------------------------------------------------------------------
--  util-serialize-io -- IO Drivers for serialization
--  Copyright (C) 2010, 2011, 2016, 2017, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Streams.Files;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Exceptions;
with Ada.IO_Exceptions;
package body Util.Serialize.IO is

   --  use Util.Log;
   use type Util.Log.Loggers.Logger_Access;

   --  The logger'
   Log : aliased constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Serialize.IO",
                                                                              Util.Log.WARN_LEVEL);
   procedure Write_Attribute (Stream : in out Output_Stream'Class;
                              Name   : in String;
                              Value  : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Stream.Write_Attribute (Name, Ada.Strings.Unbounded.To_String (Value));
   end Write_Attribute;

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Stream.Write_Entity (Name, Ada.Strings.Unbounded.To_String (Value));
   end Write_Entity;

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Util.Nullables.Nullable_String) is
   begin
      if Value.Is_Null then
         Stream.Write_Null_Entity (Name);
      else
         Stream.Write_Entity (Name, Value.Value);
      end if;
   end Write_Entity;

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Util.Nullables.Nullable_Time) is
   begin
      if Value.Is_Null then
         Stream.Write_Null_Entity (Name);
      else
         Stream.Write_Entity (Name, Value.Value);
      end if;
   end Write_Entity;

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Util.Nullables.Nullable_Boolean) is
   begin
      if Value.Is_Null then
         Stream.Write_Null_Entity (Name);
      else
         Stream.Write_Entity (Name, Value.Value);
      end if;
   end Write_Entity;

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Util.Nullables.Nullable_Integer) is
   begin
      if Value.Is_Null then
         Stream.Write_Null_Entity (Name);
      else
         Stream.Write_Entity (Name, Value.Value);
      end if;
   end Write_Entity;

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Util.Nullables.Nullable_Long) is
   begin
      if Value.Is_Null then
         Stream.Write_Null_Entity (Name);
      else
         Stream.Write_Entity (Name, Integer (Value.Value));
      end if;
   end Write_Entity;

   --  ------------------------------
   --  Set the filename to report by `Get_Location` when an error is found.
   --  ------------------------------
   procedure Set_Filename (Handler : in out Parser;
                           Name    : in String) is
   begin
      Handler.File := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Filename;

   --  ------------------------------
   --  Read the file and parse it using the JSON parser.
   --  ------------------------------
   procedure Parse (Handler : in out Parser;
                    File    : in String;
                    Sink    : in out Reader'Class) is
      Stream     : aliased Util.Streams.Files.File_Stream;
      Buffer     : Util.Streams.Buffered.Input_Buffer_Stream;
   begin
      if Handler.Error_Logger = null then
         Handler.Error_Logger := Log'Access;
      end if;
      Handler.Error_Logger.Info ("Reading file {0}", File);

      Handler.Set_Filename (File);
      Buffer.Initialize (Input  => Stream'Unchecked_Access,
                         Size   => 1024);
      Stream.Open (Mode => Ada.Streams.Stream_IO.In_File, Name => File);
      Sink.Start_Document;
      Parser'Class (Handler).Parse (Buffer, Sink);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Parser'Class (Handler).Error ("File '" & File & "' does not exist.");

      when E : others =>
         if not Handler.Error_Flag then
            Parser'Class (Handler).Error ("Exception " & Ada.Exceptions.Exception_Name (E));
         end if;
   end Parse;

   --  ------------------------------
   --  Parse the content string.
   --  ------------------------------
   procedure Parse_String (Handler : in out Parser;
                           Content : in String;
                           Sink    : in out Reader'Class) is
      Stream : aliased Util.Streams.Buffered.Input_Buffer_Stream;
   begin
      if Handler.Error_Logger = null then
         Handler.Error_Logger := Log'Access;
      end if;
      Handler.File := Ada.Strings.Unbounded.To_Unbounded_String ("<inline>");
      Stream.Initialize (Content  => Content);
      Sink.Start_Document;
      Parser'Class (Handler).Parse (Stream, Sink);

   exception
      when E : others =>
         if not Handler.Error_Flag then
            Parser'Class (Handler).Error ("Exception " & Ada.Exceptions.Exception_Name (E));
         end if;

   end Parse_String;

   --  ------------------------------
   --  Returns true if the <b>Parse</b> operation detected at least one error.
   --  ------------------------------
   function Has_Error (Handler : in Parser) return Boolean is
   begin
      return Handler.Error_Flag;
   end Has_Error;

   --  ------------------------------
   --  Set the error logger to report messages while parsing and reading the input file.
   --  ------------------------------
   procedure Set_Logger (Handler : in out Parser;
                         Logger  : in Util.Log.Loggers.Logger_Access) is
   begin
      Handler.Error_Logger := Logger;
   end Set_Logger;

   --  ------------------------------
   --  Get the current location (file and line) to report an error message.
   --  ------------------------------
   function Get_Location (Handler : in Parser) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Handler.File);
   end Get_Location;

   --  ------------------------------
   --  Report an error while parsing the input stream.  The error message will be reported
   --  on the logger associated with the parser.  The parser will be set as in error so that
   --  the <b>Has_Error</b> function will return True after parsing the whole file.
   --  ------------------------------
   overriding
   procedure Error (Handler : in out Parser;
                    Message : in String) is
   begin
      Handler.Error_Logger.Error ("{0}: {1}",
                                  Parser'Class (Handler).Get_Location,
                                  Message);
      Handler.Error_Flag := True;
   end Error;

end Util.Serialize.IO;
