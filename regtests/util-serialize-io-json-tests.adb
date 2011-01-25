-----------------------------------------------------------------------
--  serialize-io-json-tests -- Unit tests for JSON parser
--  Copyright (C) 2011 Stephane Carrez
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
with Util.Tests;
with Util.Log.Loggers;

package body Util.Serialize.IO.JSON.Tests is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Util.Serialize.IO.JSON");

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.JSON.Parse (parse errors)",
                       Test_Parse_Error'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.IO.JSON.Parse (parse Ok)",
                       Test_Parser'Access);
   end Add_Tests;

   --  ------------------------------
   --  Check various JSON parsing errors.
   --  ------------------------------
   procedure Test_Parse_Error (T : in out Test) is

      procedure Check_Parse_Error (Content : in String) is
         P : Parser;
      begin
         P.Parse_String (Content);
         Log.Error ("No exception raised for: " & Content);

      exception
         when Parse_Error =>
            null;
      end Check_Parse_Error;

   begin
      Check_Parse_Error ("{ ""person"":23");
      Check_Parse_Error ("{ person: 23]");
      Check_Parse_Error ("[ }");
      Check_Parse_Error ("{[]}");
      Check_Parse_Error ("{");
      Check_Parse_Error ("{[");
      Check_Parse_Error ("{ ""person");
      Check_Parse_Error ("{ ""person"":");
      Check_Parse_Error ("{ ""person"":""asf");
      Check_Parse_Error ("{ ""person"":""asf""");
      Check_Parse_Error ("{ ""person"":""asf"",");
   end Test_Parse_Error;

   --  ------------------------------
   --  Check various (basic) JSON valid strings (no mapper).
   --  ------------------------------
   procedure Test_Parser (T : in out Test) is

      procedure Check_Parse (Content : in String) is
         P : Parser;
      begin
         P.Parse_String (Content);

      exception
         when Parse_Error =>
            Log.Error ("Parse error for: " & Content);
            raise;
      end Check_Parse;

   begin
      Check_Parse ("{ ""person"":23}");
      Check_Parse ("{ }");
      Check_Parse ("{""person"":""asf""}");
   end Test_Parser;

end Util.Serialize.IO.JSON.Tests;
