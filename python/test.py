import unittest

from BrainfuckInterpreter import BrainfuckInterpreter, DefaultTapeSize

class BrainfuckInterpreterTests(unittest.TestCase):
    output : str = ""
    input : str = ""
    inputPtr : int = 0

    def setUp(self) -> None:
        super().setUp()
        self.output = ""
        self.input = ""
        self.inputPtr = 0

    def cin(self) -> int:
        value : int = self.input[self.inputPtr]
        self.inputPtr += 1
        return ord(value)
    
    def cout(self, b : int) -> None:
        self.output += chr(b)

    def test_HelloWorld(self) -> None:
        # Sample from https://github.com/brain-lang/brainfuck/blob/master/brainfuck.md#hello-world-example
        program = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
        interpreter = BrainfuckInterpreter(program, DefaultTapeSize, self.cin, self.cout)
        interpreter.run()
        self.assertEqual("Hello World!\n", self.output)

    def test_Echo(self) -> None:
        program = ",."
        self.input = "x"
        interpreter = BrainfuckInterpreter(program, DefaultTapeSize, self.cin, self.cout)
        interpreter.run()
        self.assertEqual("x", self.output)

    def test_ROT13(self) -> None:
        # Sample from https://brainfuck.org/rot13.b
        program =  """
            ,
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>++++++++++++++<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>>+++++[<----->-]<<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>++++++++++++++<-
            [>+<-[>+<-[>+<-[>+<-[>+<-
            [>++++++++++++++<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>>+++++[<----->-]<<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>++++++++++++++<-
            [>+<-]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
            ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]>.[-]<,]
            """
        self.input = "stefano\0"
        interpreter = BrainfuckInterpreter(program, DefaultTapeSize, self.cin, self.cout)
        interpreter.run()
        self.assertEqual("fgrsnab", self.output)

    def test_wc(self) -> None:
        # Sample from https://brainfuck.org/wc.b
        program =  """
            >>>+>>>>>+>>+>>+[<<],[
                -[-[-[-[-[-[-[-[<+>-[>+<-[>-<-[-[-[<++[<++++++>-]<
                    [>>[-<]<[>]<-]>>[<+>-[<->[-]]]]]]]]]]]]]]]]
                <[-<<[-]+>]<<[>>>>>>+<<<<<<-]>[>]>>>>>>>+>[
                    <+[
                        >+++++++++<-[>-<-]++>[<+++++++>-[<->-]+[+>>>>>>]]
                        <[>+<-]>[>>>>>++>[-]]+<
                    ]>[-<<<<<<]>>>>
                ],
            ]+<++>>>[[+++++>>>>>>]<+>+[[<++++++++>-]<.<<<<<]>>>>>>>>]
            """
        self.input = "Hello world\nthis is me\0"
        interpreter = BrainfuckInterpreter(program, DefaultTapeSize, self.cin, self.cout)
        interpreter.run()
        self.assertEqual("\t1\t5\t22\n", self.output)

if __name__ == '__main__':
    unittest.main()