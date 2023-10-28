package interpreter

import (
	"testing"
)

func TestEcho(test *testing.T) {
	output := ""
	interpreter := BrainfuckInterpreter{
		Program:   ",.",
		JumpTable: nil,
		TapeSize:  30000,
		Cin:       func() byte { return byte('x') },
		Cout:      func(b byte) { output = output + string(b) }}
	interpreter.BuildJumpTable()
	interpreter.Run()
	if output != "x" {
		test.Fail()
	}
}

func TestHelloWorld(test *testing.T) {
	output := ""
	interpreter := BrainfuckInterpreter{
		// Sample from https://github.com/brain-lang/brainfuck/blob/master/brainfuck.md#hello-world-example
		Program:   "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.",
		JumpTable: nil,
		TapeSize:  30000,
		Cin:       func() byte { return 0 },
		Cout:      func(b byte) { output = output + string(b) }}
	interpreter.BuildJumpTable()
	interpreter.Run()
	if output != "Hello World!\n" {
		test.Fail()
	}
}

func TestRot13(test *testing.T) {
	output := ""
	inputPtr := 0
	interpreter := BrainfuckInterpreter{
		// Sample from https://brainfuck.org/rot13.b
		Program: `
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
		]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]>.[-]<,]`,
		JumpTable: nil,
		TapeSize:  30000,
		Cin: func() byte {
			value := "stefano\x00"[inputPtr]
			inputPtr++
			return value
		},
		Cout: func(b byte) { output = output + string(b) }}
	interpreter.BuildJumpTable()
	interpreter.Run()
	if output != "fgrsnab" {
		test.Fail()
	}
}

func TestWc(test *testing.T) {
	output := ""
	inputPtr := 0
	interpreter := BrainfuckInterpreter{
		// Sample from https://brainfuck.org/wc.b
		Program: `
		>>>+>>>>>+>>+>>+[<<],[
			-[-[-[-[-[-[-[-[<+>-[>+<-[>-<-[-[-[<++[<++++++>-]<
				[>>[-<]<[>]<-]>>[<+>-[<->[-]]]]]]]]]]]]]]]]
			<[-<<[-]+>]<<[>>>>>>+<<<<<<-]>[>]>>>>>>>+>[
				<+[
					>+++++++++<-[>-<-]++>[<+++++++>-[<->-]+[+>>>>>>]]
					<[>+<-]>[>>>>>++>[-]]+<
				]>[-<<<<<<]>>>>
			],
		]+<++>>>[[+++++>>>>>>]<+>+[[<++++++++>-]<.<<<<<]>>>>>>>>]`,
		JumpTable: nil,
		TapeSize:  30000,
		Cin: func() byte {
			value := "Hello world\nthis is me\x00"[inputPtr]
			inputPtr++
			return value
		},
		Cout: func(b byte) { output = output + string(b) }}
	interpreter.BuildJumpTable()
	interpreter.Run()
	if output != "\t1\t5\t22\n" {
		test.Fail()
	}
}
