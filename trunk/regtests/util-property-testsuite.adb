with AUnit.Test_Suites; use AUnit.Test_Suites;
with Util.Property.Property_Test;
with Util.Log.Log_Test;
with Util.Measures.Test;
function Util.Property.Testsuite return Access_Test_Suite is
   Result : Access_Test_Suite := new Test_Suite;
begin
   Add_Test (Result, new Util.Property.Property_Test.test_Case);
   Add_Test (Result, new Util.Log.Log_Test.Test_Case);
   Add_Test (Result, new Util.Measures.Test.Test_Case);
   return Result;
end Util.Property.Testsuite;
