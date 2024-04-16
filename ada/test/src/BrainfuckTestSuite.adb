with AUnit.Test_Caller;
with BrainfuckTest;

package body BrainfuckTestSuite is

   package Caller is new AUnit.Test_Caller (BrainfuckTest.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
          (Caller.Create ("Test Hello World", BrainfuckTest.Test_HelloWorld'Access));
      return Ret;
   end Suite;

end BrainfuckTestSuite;