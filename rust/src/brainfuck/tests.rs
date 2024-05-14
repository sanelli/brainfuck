#[allow(unused_imports)]
use crate::brainfuck::interpreter::BrainfuckInterpreter;
use crate::brainfuck::interpreter::BrainfuckInterpreterIO;

#[allow(dead_code)]
struct TestIO {
    output: String,
    input: String,
    input_ptr: usize,
}

impl BrainfuckInterpreterIO for TestIO {
    fn cin(&mut self) -> i8 {
        let result = self
            .input
            .chars()
            .nth(self.input_ptr)
            .expect("Reached the end of the string");
        self.input_ptr = self.input_ptr + 1;
        return result as u8 as i8;
    }
    fn cout(&mut self, value: i8) -> () {
        self.output.push(value as u8 as char)
    }
}

impl TestIO {
    #[allow(dead_code)]
    fn new(input: String) -> TestIO {
        return TestIO {
            output: String::from(""),
            input: input,
            input_ptr: 0,
        };
    }
}

#[test]
fn test_hello_world() {
    // Sample from https://github.com/brain-lang/brainfuck/blob/master/brainfuck.md#hello-world-example
    let mut test_io = TestIO::new(String::from(""));
    let program = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    let interpreter = BrainfuckInterpreter::new(String::from(program));
    interpreter.run_with_io(Box::new(&mut test_io));
    assert_eq!(test_io.output, String::from("Hello World!\n"));
}

#[test]
fn test_echo() {
    let mut test_io = TestIO::new(String::from("x"));
    let program = ",.";
    let interpreter = BrainfuckInterpreter::new(String::from(program));
    interpreter.run_with_io(Box::new(&mut test_io));
    assert_eq!(test_io.output, String::from("x"));
    assert_eq!(test_io.input_ptr, 1);
}

#[test]
fn test_rot13() {
    // https://brainfuck.org/rot13.b
    let mut test_io = TestIO::new(String::from("stefano\0"));
    let program = r#"
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
    ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]>.[-]<,];
    "#;
    let interpreter = BrainfuckInterpreter::new(String::from(program));
    interpreter.run_with_io(Box::new(&mut test_io));
    assert_eq!(test_io.output, String::from("fgrsnab"));
    assert_eq!(test_io.input_ptr, 8);
}

#[test]
fn test_wc() {
    // https://brainfuck.org/wc.b
    let mut test_io = TestIO::new(String::from("Hello world\nthis is me\0"));
    let program = r#"
    >>>+>>>>>+>>+>>+[<<],[
        -[-[-[-[-[-[-[-[<+>-[>+<-[>-<-[-[-[<++[<++++++>-]<
            [>>[-<]<[>]<-]>>[<+>-[<->[-]]]]]]]]]]]]]]]]
        <[-<<[-]+>]<<[>>>>>>+<<<<<<-]>[>]>>>>>>>+>[
            <+[
                >+++++++++<-[>-<-]++>[<+++++++>-[<->-]+[+>>>>>>]]
                <[>+<-]>[>>>>>++>[-]]+<
            ]>[-<<<<<<]>>>>
        ],
    ]+<++>>>[[+++++>>>>>>]<+>+[[<++++++++>-]<.<<<<<]>>>>>>>>];
    "#;
    let interpreter = BrainfuckInterpreter::new(String::from(program));
    interpreter.run_with_io(Box::new(&mut test_io));
    assert_eq!(test_io.output, String::from("\t1\t5\t22\n"));
    assert_eq!(test_io.input_ptr, 23);
}
