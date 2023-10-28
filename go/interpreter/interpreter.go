package interpreter

import (
	"errors"
)

type BrainfuckInterpreter struct {
	Program   string
	JumpTable map[int]int
	TapeSize  int
	Cin       func() byte
	Cout      func(byte)
}

func (interpreter *BrainfuckInterpreter) BuildJumpTable() error {
	if interpreter.JumpTable == nil {
		interpreter.JumpTable = make(map[int]int)
	} else {
		for k := range interpreter.JumpTable {
			delete(interpreter.JumpTable, k)
		}
	}

	var stack []int
	for program_counter := 0; program_counter < len(interpreter.Program); program_counter++ {
		if interpreter.Program[program_counter] == '[' {
			stack = append(stack, program_counter)
		} else if interpreter.Program[program_counter] == ']' {
			if len(stack) == 0 {
				return errors.New("Missing matching '['")
			}

			n := len(stack) - 1
			open_jump := stack[n]
			stack = stack[:n]
			interpreter.JumpTable[open_jump] = program_counter
			interpreter.JumpTable[program_counter] = open_jump
		}
	}

	if len(stack) > 0 {
		return errors.New("Missing matching ']'")
	}

	return nil
}

func (interpreter *BrainfuckInterpreter) Run() {
	pointer := interpreter.TapeSize
	tape := make([]byte, 2*interpreter.TapeSize)

	for program_counter := 0; program_counter < len(interpreter.Program); program_counter++ {
		switch interpreter.Program[program_counter] {
		case '>':
			pointer++
		case '<':
			pointer--
		case '+':
			tape[pointer]++
		case '-':
			tape[pointer]--
		case '.':
			interpreter.Cout(tape[pointer])
		case ',':
			tape[pointer] = interpreter.Cin()
		case '[':
			if tape[pointer] == 0 {
				program_counter = interpreter.JumpTable[program_counter] - 1
			}
		case ']':
			if tape[pointer] != 0 {
				program_counter = interpreter.JumpTable[program_counter] - 1
			}
		}
	}
}
