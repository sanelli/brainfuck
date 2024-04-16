with AUnit.Assertions; use AUnit.Assertions;
with BrainfuckInterpreter; use BrainfuckInterpreter;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

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

   procedure Test_Echo (T : in out Test)  is
      pragma Unreferenced (T);
      Interpreter : TBrainfuckInterpreter;
      Program : Unbounded_String;
   begin
      ActualTestInput := Ada.Strings.Unbounded.To_Unbounded_String ("x");
      Program := Ada.Strings.Unbounded.To_Unbounded_String (",.");
      Interpreter.Initialize (Program, TestDefaultCin'Access, TestDefaultCout'Access);
      Interpreter.Run;
      Assert (ActualTestOutput = Ada.Strings.Unbounded.To_Unbounded_String ("x"), "Incorrect value generated");
      Assert (ActualTestInputPtr = 1, "Nothing read from the input");
   end Test_Echo;

   procedure Test_ROT13 (T : in out Test) is
      pragma Unreferenced (T);
      Interpreter : TBrainfuckInterpreter;
      Program : Unbounded_String;
   begin
      ActualTestInput := Ada.Strings.Unbounded.To_Unbounded_String ("stefano" & Ada.Characters.Latin_1.NUL);
      -- Source https://brainfuck.org/rot13.b
      Program := Ada.Strings.Unbounded.To_Unbounded_String ("," &
            "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" &
            "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" &
            "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" &
            "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" &
            "[>++++++++++++++<-" &
            "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" &
            "[>>+++++[<----->-]<<-" &
            "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" &
            "[>++++++++++++++<-" &
            "[>+<-[>+<-[>+<-[>+<-[>+<-" &
            "[>++++++++++++++<-" &
            "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" &
            "[>>+++++[<----->-]<<-" &
            "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" &
            "[>++++++++++++++<-" &
            "[>+<-]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]" &
            "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]>.[-]<,]");
      Interpreter.Initialize (Program, TestDefaultCin'Access, TestDefaultCout'Access);
      Interpreter.Run;
      Assert (ActualTestOutput = Ada.Strings.Unbounded.To_Unbounded_String ("fgrsnab"), "Incorrect value generated");
      Assert (ActualTestInputPtr = 8, "Incorrect number of characters read from input");
   end Test_ROT13;

   procedure Test_Wc (T : in out Test) is
      pragma Unreferenced (T);
      Interpreter : TBrainfuckInterpreter;
      Program : Unbounded_String;
   begin
      ActualTestInput := Ada.Strings.Unbounded.To_Unbounded_String ("Hello world" & Ada.Characters.Latin_1.LF & "this is me" & Ada.Characters.Latin_1.NUL);
      -- Source https://brainfuck.org/wc.b
      Program := Ada.Strings.Unbounded.To_Unbounded_String (" >>>+>>>>>+>>+>>+[<<],[" &
            "     -[-[-[-[-[-[-[-[<+>-[>+<-[>-<-[-[-[<++[<++++++>-]<" &
            "        [>>[-<]<[>]<-]>>[<+>-[<->[-]]]]]]]]]]]]]]]]" &
            "    <[-<<[-]+>]<<[>>>>>>+<<<<<<-]>[>]>>>>>>>+>[" &
            "        <+[" &
            "            >+++++++++<-[>-<-]++>[<+++++++>-[<->-]+[+>>>>>>]]" &
            "            <[>+<-]>[>>>>>++>[-]]+<" &
            "        ]>[-<<<<<<]>>>>" &
            "    ]," &
            "]+<++>>>[[+++++>>>>>>]<+>+[[<++++++++>-]<.<<<<<]>>>>>>>>]");
      Interpreter.Initialize (Program, TestDefaultCin'Access, TestDefaultCout'Access);
      Interpreter.Run;
      Assert (ActualTestOutput = Ada.Strings.Unbounded.To_Unbounded_String ("" & Ada.Characters.Latin_1.HT & "1" & Ada.Characters.Latin_1.HT & "5" & Ada.Characters.Latin_1.HT & "22" & Ada.Characters.Latin_1.LF), "Incorrect value generated");
      Assert (ActualTestInputPtr = 23, "Incorrect number of characters read from input");
   end Test_Wc;
end BrainfuckTest;