with Ada.Text_IO;
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Interfaces;
with Util.Strings;
with Util.Encoders;
with Util.Streams.Files;
with Util.Streams.Base64;
with Util.Streams.Buffered.Parts;
procedure Dumpcert is

   use Ada.Streams;
   use Interfaces;
   use Ada.Text_IO;

   procedure Dump_Cert (Source  : in String);
   procedure Dump_ASN1 (Level   : in Ada.Text_IO.Positive_Count;
                        Content : in Stream_Element_Array);

   Hex  : constant Util.Encoders.Encoder := Util.Encoders.Create (Util.Encoders.BASE_16);

   procedure Dump_ASN1 (Level   : in Ada.Text_IO.Positive_Count;
                        Content : in Stream_Element_Array) is

      function Get_Value (Count : Natural) return Unsigned_64;
      function Get_B128 return Unsigned_64;
      function Get_Length return Unsigned_64;
      function Get_Tag_Name (Tag : Unsigned_8) return String;

      Pos  : Stream_Element_Offset := Content'First;

      function Get_Value (Count : Natural) return Unsigned_64 is
         L : Unsigned_64 := 0;
      begin
         L := 0;
         for I in 1 .. Count loop
            L := Shift_Left (L, 8) or Unsigned_64 (Content (Pos));
            Pos := Pos + 1;
         end loop;
         return L;
      end Get_Value;

      function Get_B128 return Unsigned_64 is
         L : Unsigned_64 := 0;
         C : Stream_Element;
      begin
         while Pos <= Content'Last loop
            L := L * 128;
            C := Content (Pos);
            Pos := Pos + 1;
            L := L + (Unsigned_64 (C) and 16#7F#);
            exit when (C and 16#80#) = 0;
         end loop;
         return L;
      end Get_B128;

      function Get_Length return Unsigned_64 is
         C : Stream_Element := Content (Pos);
      begin
         if (C and 16#80#) = 0 then
            Pos := Pos + 1;
            return Unsigned_64 (C);
         end if;
         C := C and 16#7F#;
         Pos := Pos + 1;
         return Get_Value (Natural (C));
      end Get_Length;

      function Get_Tag_Name (Tag : Unsigned_8) return String is
      begin
         case Tag and 16#3F# is
            when 2 =>
               return "INTEGER";

            when 3 =>
               return "BITSTRING";

            when 4 =>
               return "OCTET STRING";

            when 5 =>
               return "NULL";

            when 6 =>
               return "OID";

            when 12 =>
               return "UTF8STRING";

            when 16 | 48 =>
               return "SEQUENCE";

            when 17 | 49 =>
               return "SET";

            when 19 =>
               return "PRINTABLESTRING";

            when others =>
               return "Tag " & Util.Encoders.Encode_Unsigned_16 (Hex, Unsigned_16 (Tag));

         end case;
      end Get_Tag_Name;

      Tag : Stream_Element := Content (Pos);
      Len : Unsigned_64;
      Val : Unsigned_64;
   begin
      while Pos < Content'Last loop
         Tag := Content (Pos);
         Pos := Pos + 1;
         if Tag = 16#a0# then
            Len := 1;
         else
            Len := Get_Length;
         end if;
         Set_Col (Level);
         Tag := Tag and 16#3F#;
         if Tag in 16 | 48 | 17 | 49 then
            Put_Line ("Tag: " & Get_Tag_Name (Unsigned_8 (Tag)) & " Len " & Len'Image);
            Dump_ASN1 (Level + 2, Content (Pos .. Pos + Stream_Element_Offset (Len - 1)));
            Pos := Pos + Stream_Element_Offset (Len);
         elsif Tag = 2 then
            Val := Get_Value (Natural (Len));
            Put_Line ("Tag: INTEGER len " & Len'Image & " value: " & Val'Image);
         elsif Tag = 12 then
            Put ("Tag: UTF8STRING len " & Len'Image & " value: ");
            for I in 1 .. Len loop
               Put (Character'Val (Content (Pos)));
               Pos := Pos + 1;
            end loop;
            New_Line;
         elsif Tag = 19 then
            Put ("Tag: PRINTABLESTRING len " & Len'Image & " value: ");
            for I in 1 .. Len loop
               Put (Character'Val (Content (Pos)));
               Pos := Pos + 1;
            end loop;
            New_Line;
         elsif Tag = 6 then
            Put ("Tag: OID len " & Len'Image & " value: ");
            declare
               C  : constant Stream_Element := Content (Pos);
               V1 : constant Stream_Element := C / 40;
               V2 : constant Stream_Element := C mod 40;
               End_Pos : constant Stream_Element_Offset := Pos + Stream_Element_Offset (Len);
            begin
               Put (Util.Strings.Image (Natural (V1)));
               Put (".");
               Put (Util.Strings.Image (Natural (V2)));
               Pos := Pos + 1;
               while Pos < End_Pos loop
                  Put (".");
                  Put (Util.Strings.Image (Long_Long_Integer (Get_B128)));
               end loop;
               New_Line;
            end;
         else
            Put_Line ("Tag: " & Get_Tag_Name (Unsigned_8 (Tag)) & " Len " & Len'Image);
            Pos := Pos + Stream_Element_Offset (Len);
         end if;
      end loop;
   end Dump_ASN1;

   procedure Dump_Cert (Source  : in String) is
      In_Stream   : aliased Util.Streams.Files.File_Stream;
      Part_Stream : aliased Util.Streams.Buffered.Parts.Input_Part_Stream;
      Base64      : aliased Util.Streams.Base64.Decoding_Stream;
      Drop        : Ada.Streams.Stream_Element;
      Content     : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last        : Ada.Streams.Stream_Element_Offset;
   begin
      In_Stream.Open (Mode => Ada.Streams.Stream_IO.In_File, Name => Source);
      Part_Stream.Initialize (Input => In_Stream'Unchecked_Access, Size => 4096);
      Part_Stream.Set_Boundary ("-----BEGIN CERTIFICATE-----" & ASCII.LF);
      while not Part_Stream.Is_Eob loop
         Part_Stream.Read (Drop);
      end loop;
      Part_Stream.Set_Boundary ("-----END CERTIFICATE-----");
      Base64.Transform.Set_Ignore_Line_Break (True);
      Base64.Consumes (Input => Part_Stream'Unchecked_Access, Size => 32768);
      Base64.Read (Content, Last);
      Dump_ASN1 (1, Content (Content'First .. Last));

   exception
      when Ada.IO_Exceptions.Data_Error =>
         Ada.Text_IO.Put_Line ("It seems this is not a PEM certificate file");
   end Dump_Cert;

begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: dumpcert certificate.pem");
      return;
   end if;

   Dump_Cert (Source => Ada.Command_Line.Argument (1));
end Dumpcert;
