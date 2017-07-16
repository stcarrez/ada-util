-----------------------------------------------------------------------
--  serialize -- JSON serialization
--  Copyright (C) 2017 Stephane Carrez
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
with Ada.Text_IO;
with Util.Serialize.IO.JSON;
with Util.Streams.Texts;
with Util.Streams.Buffered;
procedure Serialize is
   Output : aliased Util.Streams.Texts.Print_Stream;
   Stream : Util.Serialize.IO.JSON.Output_Stream;
begin
   Output.Initialize (Size => 10000);
   Stream.Initialize (Output => Output'Unchecked_Access);
   Stream.Start_Document;
   Stream.Start_Entity ("person");
   Stream.Write_Entity ("name", "Harry Potter");
   Stream.Write_Entity ("gender", "male");
   Stream.Write_Entity ("age", 17);
   Stream.End_Entity ("person");
   Stream.End_Document;
   Ada.Text_IO.Put_Line (Util.Streams.Texts.To_String (Output));
end Serialize;
