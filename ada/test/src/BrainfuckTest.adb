with AUnit.Assertions; use AUnit.Assertions;
with BrainfuckInterpreter; use BrainfuckInterpreter;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body BrainfuckTest is
   ActualTestOutput : Unbounded_String;
   ActualTestInput : Unbounded_String;
   ActualTestInputPtr : Natural;

   function TestDefaultCin return Byte is
   begin
      ActualTestInputPtr := ActualTestInputPtr + 1;
      return Character'Pos (Element (ActualTestInput, ActualTestInputPtr));
   end TestDefaultCin;

   procedure TestDefaultCout (c : Byte) is
   begin
      Append (ActualTestOutput, Character'Val (c));
   end TestDefaultCout;

   overriding procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);
   begin
      ActualTestOutput := Ada.Strings.Unbounded.To_Unbounded_String ("");
      ActualTestInput := Ada.Strings.Unbounded.To_Unbounded_String ("");
      ActualTestInputPtr := 0;
   end Set_Up;

   procedure Test_HelloWorld (T : in out Test) is
      pragma Unreferenced (T);
      Interpreter : TBrainfuckInterpreter;
      Program : Unbounded_String;
   begin
      -- Sample from https://github.com/brain-lang/brainfuck/blob/master/brainfuck.md#hello-world-example
      Program := Ada.Strings.Unbounded.To_Unbounded_String ("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.");
      Interpreter.Initialize (Program, TestDefaultCin'Access, TestDefaultCout'Access);
      Interpreter.Run;
      Assert (ActualTestOutput = Ada.Strings.Unbounded.To_Unbounded_String ("Hello World!" & Ada.Characters.Latin_1.LF), "Incorrect value generated");
   end Test_HelloWorld;

end BrainfuckTest;