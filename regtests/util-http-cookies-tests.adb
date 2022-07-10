-----------------------------------------------------------------------
--  util-http-cookies-tests - Unit tests for Cookies
--  Copyright (C) 2011, 2012, 2015, 2022 Stephane Carrez
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

with Util.Test_Caller;
with Util.Log.Loggers;
with Util.Measures;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
package body Util.Http.Cookies.Tests is

   use Ada.Strings.Fixed;
   use Util.Tests;
   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Cookies.Tests");

   procedure Free is new Ada.Unchecked_Deallocation (Cookie_Array, Cookie_Array_Access);

   package Caller is new Util.Test_Caller (Test, "Cookies");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ASF.Cookies.Create_Cookie",
                       Test_Create_Cookie'Access);
      Caller.Add_Test (Suite, "Test ASF.Cookies.To_Http_Header",
                       Test_To_Http_Header'Access);
      Caller.Add_Test (Suite, "Test ASF.Cookies.Get_Cookies",
                       Test_Parse_Http_Header'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of cookie
   --  ------------------------------
   procedure Test_Create_Cookie (T : in out Test) is
      C : constant Cookie := Create ("cookie", "value");
   begin
      T.Assert_Equals ("cookie", Get_Name (C), "Invalid name");
      T.Assert_Equals ("value", Get_Value (C), "Invalid value");
      T.Assert (not Is_Secure (C), "Invalid is_secure");
   end Test_Create_Cookie;

   --  ------------------------------
   --  Test conversion of cookie for HTTP header
   --  ------------------------------
   procedure Test_To_Http_Header (T : in out Test) is
      procedure Test_Cookie (C : in Cookie);
      procedure Test_Cookie (Name : in String; Value : in String);

      procedure Test_Cookie (C : in Cookie) is
         S : constant String := To_Http_Header (C);
      begin
         Log.Info ("Cookie {0}: {1}", Get_Name (C), S);
         T.Assert (S'Length > 0, "Invalid cookie length for: " & S);
         T.Assert (Index (S, "=") > 0, "Invalid cookie format; " & S);
      end Test_Cookie;

      procedure Test_Cookie (Name : in String; Value : in String) is
         C : Cookie := Create (Name, Value);
      begin
         Test_Cookie (C);
         for I in 1 .. 24 loop
            Set_Max_Age (C, I * 3600 + I);
            Test_Cookie (C);
         end loop;
         Set_Secure (C, True);
         Test_Cookie (C);
         Set_Http_Only (C, True);
         Test_Cookie (C);
         Set_Domain (C, "world.com");
         Test_Cookie (C);
         Set_Path (C, "/some-path");
         Test_Cookie (C);
      end Test_Cookie;

   begin
      Test_Cookie ("a", "b");
      Test_Cookie ("session_id", "79e2317bcabe731e2f681f5725bbc621-&v=1");
      Test_Cookie ("p_41_zy", "");
      Test_Cookie ("p_34", """quoted\t""");
      Test_Cookie ("d", "s");
      declare
         C : Cookie := Create ("cookie-name", "79e2317bcabe731e2f681f5725bbc621");
         Start : Util.Measures.Stamp;
      begin
         Set_Path (C, "/home");
         Set_Domain (C, ".mydomain.example.com");
         Set_Max_Age (C, 1000);
         Set_Http_Only (C, True);
         Set_Secure (C, True);
         Set_Comment (C, "some comment");
         for I in 1 .. 1000 loop
            declare
               S : constant String := To_Http_Header (C);
            begin
               if S'Length < 10 then
                  T.Assert (S'Length > 10, "Invalid cookie generation");
               end if;
            end;
         end loop;
         Util.Measures.Report (S     => Start,
                               Title => "Generation of 1000 cookies");
      end;
   end Test_To_Http_Header;

   --  ------------------------------
   --  Test parsing of client cookie to build a list of Cookie objects
   --  ------------------------------
   procedure Test_Parse_Http_Header (T : in out Test) is

      procedure Test_Parse (Header : in String;
                            Count  : in Natural;
                            Name   : in String;
                            Value  : in String);

      --  ------------------------------
      --  Parse the header and check for the expected cookie count and verify
      --  the value of a specific cookie
      --  ------------------------------
      procedure Test_Parse (Header : in String;
                            Count  : in Natural;
                            Name   : in String;
                            Value  : in String) is
         Start : Util.Measures.Stamp;
         C     : Cookie_Array_Access := Get_Cookies (Header);
         Found : Boolean := False;
      begin
         Util.Measures.Report (S     => Start,
                               Title => "Parsing of cookie " & Name);

         T.Assert (C /= null, "Null value returned by Get_Cookies");
         Assert_Equals (T, Count, C'Length, "Invalid count in result array");
         for I in C'Range loop
            Log.Debug ("Cookie: {0}={1}", Get_Name (C (I)), Get_Value (C (I)));
            if Name = Get_Name (C (I)) then
               Assert_Equals (T, Value, Get_Value (C (I)), "Invalid cookie: " & Name);
               Found := True;
            end if;
         end loop;
         T.Assert (Found, "Cookie " & Name & " not found");
         Free (C);
      end Test_Parse;

   begin
      Test_Parse ("A=B", 1, "A", "B");
      Test_Parse ("A=B=", 1, "A", "B=");

      Test_Parse ("CONTEXTE=mar=101&nab=84fbbe2a81887732fe9c635d9ac319b5"
                  & "&pfl=afide0&typsrv=we&sc=1&soumar=1011&srcurlconfigchapeau"
                  & "=sous_ouv_formulaire_chapeau_dei_config&nag=550&reg=14505; "
                  & "xtvrn=$354249$354336$; CEPORM=321169600.8782.0000; SES=auth=0&"
                  & "cartridge_url=&return_url=; CEPORC=344500416.8782.0000", 5,
                 "SES", "auth=0&cartridge_url=&return_url=");

      Test_Parse ("s_nr=1298652863627; s_sq=%5B%5BB%5D%5D; s_cc=true; oraclelicense="
                  & "accept-dbindex-cookie; gpv_p24=no%20value; gpw_e24=no%20value",
                  6, "gpw_e24", "no%20value");

      --  Parse a set of cookies from microsoft.com
      Test_Parse ("WT_FPC=id=00.06.036.020-2037773472.29989343:lv=1300442685103:ss=1300442685103; "
                  & "MC1=GUID=9d86c397c1a5ea4cab48d9fe19b76b4e&HASH=97c3&LV=20092&V=3; "
                  & "A=I&I=AxUFAAAAAABwBwAALSllub5HorZBtWWX8ZeXcQ!!&CS=114[3B002j2p[0302g3V309; "
                  & "WT_NVR_RU=0=technet|msdn:1=:2=; mcI=Fri, 31 Dec 2010 10:44:31 GMT; "
                  & "omniID=beb780dd_ab6d_4802_9f37_e33dde280adb; CodeSnippetContainerLang="
                  & "Visual Basic; MSID=Microsoft.CreationDate=11/09/2010 10:08:25&Microsoft."
                  & "LastVisitDate=03/01/2011 17:08:34&Microsoft.VisitStartDate=03/01/2011 "
                  & "17:08:34&Microsoft.CookieId=882efa27-ee6c-40c5-96ba-2297f985d9e3&Microsoft"
                  & ".TokenId=0c0a5e07-37a6-4ca3-afc0-0edf3de329f6&Microsoft.NumberOfVisits="
                  & "60&Microsoft.IdentityToken=RojEm8r/0KbCYeKasdfwekl3FtctotD5ocj7WVR72795"
                  & "dBj23rXVz5/xeldkkKHr/NtXFN3xAvczHlHQHKw14/9f/VAraQFIzEYpypGE24z1AuPCzVwU"
                  & "l4HZPnOFH+IA0u2oarcR1n3IMC4Gk8D1UOPBwGlYMB2Xl2W+Up8kJikc4qUxmFG+X5SRrZ3m7"
                  & "LntAv92B4v7c9FPewcQHDSAJAmTOuy7+sl6zEwW2fGWjqGe3G7bh+qqUWPs4LrvSyyi7T3UCW"
                  & "anSWn6jqJInP/MSeAxAvjbTrBwlJlE3AoUfXUJgL81boPYsIXZ30lPpqjMkyc0Jd70dffNNTo5"
                  & "qwkvfFhyOvnfYoQ7dZ0REw+TRA01xHyyUSPINOVgM5Vcu4SdvcUoIlMR3Y097nK+lvx8SqV4UU"
                  & "+QENW1wbBDa7/u6AQqUuk0616tWrBQMR9pYKwYsORUqLkQY5URzhVVA7Py28dLX002Nehqjbh68"
                  & "4xQv/gQYbPZMZUq/wgmPsqA5mJU/lki6A0S/H/ULGbrJE0PBQr8T+Hd+Op4HxEHvjvkTs=&Micr"
                  & "osoft.MicrosoftId=0004-1282-6244-7365; msresearch=%7B%22version%22%3A%224.6"
                  & "%22%2C%22state%22%3A%7B%22name%22%3A%22IDLE%22%2C%22url%22%3Aundefined%2C%2"
                  & "2timestamp%22%3A1296211850175%7D%2C%22lastinvited%22%3A1296211850175%2C%22u"
                  & "serid%22%3A%2212962118501758905232121057759%22%2C%22vendorid%22%3A1%2C%22su"
                  & "rveys%22%3A%5Bundefined%5D%7D; s_sq=%5B%5BB%5D%5D; s_cc=true; "
                  & "GsfxStatsLog=true; GsfxSessionCookie=231210164895294015; ADS=SN=175A21EF;"
                  & ".ASPXANONYMOUS=HJbsF8QczAEkAAAAMGU4YjY4YTctNDJhNy00NmQ3LWJmOWEtNjVjMzFmODY"
                  & "1ZTA5dhasChFIbRm1YpxPXPteSbwdTE01",
                  15, "s_sq", "%5B%5BB%5D%5D");

      Test_Parse ("A=B;", 1, "A", "B");
      Test_Parse ("A=B; ", 1, "A", "B");
      Test_Parse ("A=B; C&3", 1, "A", "B");
      Test_Parse ("A=C; C<3=4", 2, "A", "C");
   end Test_Parse_Http_Header;

end Util.Http.Cookies.Tests;
