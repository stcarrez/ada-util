-----------------------------------------------------------------------
--  util-mail -- Mail Utility Library
--  Copyright (C) 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;

--  == Introduction ==
--  The <tt>Util.Mail</tt> package provides various operations related to sending email.

package Util.Mail is

   type Email_Address is record
      Name    : Ada.Strings.Unbounded.Unbounded_String;
      Address : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Parse the email address and separate the name from the address.
   function Parse_Address (E_Mail : in String) return Email_Address;

   --  Extract a first name from the email address.
   function Get_First_Name (From : in Email_Address) return String;

   --  Extract a last name from the email address.
   function Get_Last_Name (From : in Email_Address) return String;

end Util.Mail;
