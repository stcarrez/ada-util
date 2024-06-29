-----------------------------------------------------------------------
--  util-http -- HTTP Utility Library
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Strings;
package body Util.Http is

   --  Sets a header with the given name and date-value.
   --  The date is specified in terms of milliseconds since the epoch.
   --  If the header had already been set, the new value overwrites the previous one.
   --  The containsHeader method can be used to test for the presence of a header
   --  before setting its value.
   procedure Set_Date_Header (Request  : in out Abstract_Message'Class;
                              Name     : in String;
                              Date     : in Ada.Calendar.Time) is
   begin
      null;
   end Set_Date_Header;

   --  Adds a header with the given name and date-value. The date is specified
   --  in terms of milliseconds since the epoch. This method allows response headers
   --  to have multiple values.
   procedure Add_Date_Header (Request : in out Abstract_Message'Class;
                              Name    : in String;
                              Date    : in Ada.Calendar.Time) is
   begin
      null;
   end Add_Date_Header;

   --  ------------------------------
   --  Sets a header with the given name and integer value.
   --  If the header had already been set, the new value overwrites the previous one.
   --  The containsHeader  method can be used to test for the presence of a header
   --  before setting its value.
   --  ------------------------------
   procedure Set_Int_Header (Request  : in out Abstract_Message'Class;
                             Name     : in String;
                             Value    : in Integer) is
   begin
      Request.Set_Header (Name, Util.Strings.Image (Value));
   end Set_Int_Header;

   --  ------------------------------
   --  Adds a header with the given name and integer value. This method
   --  allows headers to have multiple values.
   --  ------------------------------
   procedure Add_Int_Header (Request  : in out Abstract_Message'Class;
                             Name     : in String;
                             Value    : in Integer) is
   begin
      Request.Add_Header (Name, Util.Strings.Image (Value));
   end Add_Int_Header;

end Util.Http;
