with Ada.Strings.Unbounded;
   --  ------------------------------
   --  Format the date passed in `Date` with a simple format pattern.
   --  The pattern is composed of minimalist sequences that are replaced by
   --  values, unrecognized characters are passed as is:
   --  YYYY : year     MM : month     dd: day    HH: hour   mm: minute   ss: second
   --  ------------------------------
function Util.Dates.Simple_Format (Pattern : in String;
                                   Date    : in Ada.Calendar.Time) return String is
   use Ada.Strings.Unbounded;

   procedure Append_Number (Into    : in out Unbounded_String;
                            Value   : in Natural;
                            Padding : in Character;
                            Length  : in Natural := 2);

   Pos    : Natural := Pattern'First;
   TM     : Date_Record;
   Result : Unbounded_String;

   --  ------------------------------
   --  Append a number with padding if necessary
   --  ------------------------------
   procedure Append_Number (Into    : in out Unbounded_String;
                            Value   : in Natural;
                            Padding : in Character;
                            Length  : in Natural := 2) is
      N : constant String := Natural'Image (Value);
   begin
      if Length = 0 or else (Padding /= ' ' and then Padding /= '0') then
         Append (Into, N (N'First + 1 .. N'Last));
      elsif N'Length <= Length then
         Append (Into, Padding);
         Append (Into, N (N'First + 1 .. N'Last));
      else
         Append (Into, N (N'Last - Length + 1 .. N'Last));
      end if;
   end Append_Number;

begin
   Split (TM, Date);
   while Pos <= Pattern'Last loop
      if Pos + 3 <= Pattern'Last and then Pattern (Pos .. Pos + 3) = "YYYY" then
         Pos := Pos + 4;
         Append_Number (Result, Natural (TM.Year), Padding => '0', Length => 4);
      elsif Pos + 1 <= Pattern'Last and then Pattern (Pos .. Pos + 1) = "MM" then
         Pos := Pos + 2;
         Append_Number (Result, Natural (TM.Month), Padding => '0', Length => 2);
      elsif Pos + 1 <= Pattern'Last and then Pattern (Pos .. Pos + 1) = "dd" then
         Pos := Pos + 2;
         Append_Number (Result, Natural (TM.Month_Day), Padding => '0', Length => 2);
      elsif Pos + 1 <= Pattern'Last and then Pattern (Pos .. Pos + 1) = "HH" then
         Pos := Pos + 2;
         Append_Number (Result, Natural (TM.Hour), Padding => '0', Length => 2);
      elsif Pos + 1 <= Pattern'Last and then Pattern (Pos .. Pos + 1) = "mm" then
         Pos := Pos + 2;
         Append_Number (Result, Natural (TM.Minute), Padding => '0', Length => 2);
      elsif Pos + 1 <= Pattern'Last and then Pattern (Pos .. Pos + 1) = "ss" then
         Pos := Pos + 2;
         Append_Number (Result, Natural (TM.Second), Padding => '0', Length => 2);
      else
         Append (Result, Pattern (Pos));
         Pos := Pos + 1;
      end if;
   end loop;
   return To_String (Result);
end Util.Dates.Simple_Format;
