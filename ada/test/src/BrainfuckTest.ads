with AUnit.Test_Fixtures;

package BrainfuckTest is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   overriding procedure Set_Up (T : in out Test);
   procedure Test_HelloWorld (T : in out Test);

end BrainfuckTest;