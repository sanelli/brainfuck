with AUnit.Test_Caller;
with BrainfuckTest;

package body BrainfuckTestSuite is

   package Caller is new AUnit.Test_Caller (BrainfuckTest.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
          (Caller.Create ("Hello World", BrainfuckTest.Test_HelloWorld'Access));
      Ret.Add_Test
          (Caller.Create ("Echo", BrainfuckTest.Test_Echo'Access));
      Ret.Add_Test
          (Caller.Create ("ROT13", BrainfuckTest.Test_ROT13'Access));
      Ret.Add_Test
          (Caller.Create ("wc", BrainfuckTest.Test_Wc'Access));
      return Ret;
   end Suite;

end BrainfuckTestSuite;