-----------------------------------------------------------------------
--  serialize-tools-tests -- Unit tests for serialization tools
--  Copyright (C) 2012 Stephane Carrez
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
with Util.Beans.Objects;

package body Util.Serialize.Tools.Tests is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("Util.Processes.Tests");

   package Caller is new Util.Test_Caller (Test, "Serialize.Tools");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Serialize.Tools.To_JSON",
                       Test_To_JSON'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.Tools.From_JSON",
                       Test_From_JSON'Access);
      Caller.Add_Test (Suite, "Test Util.Serialize.Tools.To_JSON/From_JSON",
                       Test_To_From_JSON'Access);
   end Add_Tests;

   --  -----------------------
   --  Test the To_JSON operation.
   --  -----------------------
   procedure Test_To_JSON (T : in out Test) is
      Map : Util.Beans.Objects.Maps.Map;
   begin
      Util.Tests.Assert_Equals (T, "", To_JSON (Map),
                                "Invalid empty map serialization");
      Map.Include ("testing", Util.Beans.Objects.To_Object (Integer (23)));
      Util.Tests.Assert_Equals (T, "{""params"":[{""name"":""testing"",""value"":23}]}",
                                To_JSON (Map),
                                "Invalid empty map serialization");
      Map.Include ("string", Util.Beans.Objects.To_Object (ASCII.LF & ASCII.CR & " special'"""));
      Log.Info ("JSON: {0}", To_JSON (Map));
      Util.Tests.Assert_Matches (T, ".*testing.*",
                                To_JSON (Map),
                                "Missing testing");
      Util.Tests.Assert_Matches (T, ".*\\n\\r special'.*",
                                 To_JSON (Map),
                                 "Missing special value");
   end Test_To_JSON;

   --  -----------------------
   --  Test the From_JSON operation.
   --  -----------------------
   procedure Test_From_JSON (T : in out Test) is
   begin
      declare
         Map : constant Util.Beans.Objects.Maps.Map := From_JSON ("");
      begin
         Util.Tests.Assert_Equals (T, 0, Integer (Map.Length), "Invalid map");
      end;
      declare
         Map : constant Util.Beans.Objects.Maps.Map
           := From_JSON ("{""params"":[{""name"":""testing"",""value"":23}]}");
      begin
         Util.Tests.Assert_Equals (T, 1, Integer (Map.Length), "Invalid map");
         T.Assert (Map.Contains ("testing"), "The 'name' object is not present");
         Util.Tests.Assert_Equals (T, "23",
                                   Util.Beans.Objects.To_String (Map.Element ("testing")),
                                   "The 'name' object is invalid");
      end;
   end Test_From_JSON;

   --  -----------------------
   --  Test the To_JSON and From_JSON
   --  -----------------------
   procedure Test_To_From_JSON (T : in out Test) is
   begin
      for I in 1 .. 20 loop
         declare
            Map  : Util.Beans.Objects.Maps.Map;
            Name : String (1 .. I) := (others => ' ');
         begin
            for J in 1 .. I loop
               for K in 1 .. I loop
                  Name (K) := Character'Val (J mod 255);
               end loop;
               Map.Include (Name, Util.Beans.Objects.To_Object (Name));
            end loop;
            Util.Tests.Assert_Equals (T, I, Integer (Map.Length), "Invalid map length");
            declare
               JSON : constant String := To_JSON (Map);
            begin
               T.Assert (JSON'Length > 0, "JSON is too small");
               declare
                  Result : constant Util.Beans.Objects.Maps.Map := From_JSON (JSON);
               begin
                  Util.Tests.Assert_Equals (T, I, Integer (Result.Length),
                                            "Invalid result length");
                  for J in 1 .. I loop
                     for K in 1 .. I loop
                        Name (K) := Character'Val (J mod 255);
                     end loop;
                     T.Assert (Result.Contains (Name), "The value '" & Name & "' not found");
                     Util.Tests.Assert_Equals (T, Name,
                                               Beans.Objects.To_String (Result.Element (Name)),
                                               "Invalid value");
                  end loop;
               end;
            end;
         end;
      end loop;
   end Test_To_From_JSON;

end Util.Serialize.Tools.Tests;
