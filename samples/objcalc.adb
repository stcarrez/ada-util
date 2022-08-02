with Ada.Text_IO;
with Util.Beans.Objects;
procedure ObjCalc is

   package UBO renames Util.Beans.Objects;

   use type UBO.Object;

   Value : UBO.Object := UBO.To_Object (Integer (0));
begin
   Value := Value + UBO.To_Object (Integer (123));
   Value := Value - UBO.To_Object (String '("12"));

   --  Should print '111'
   Ada.Text_IO.Put_Line (UBO.To_String (Value));
end ObjCalc;
