package main

import (
	"brainfuck/interpreter"
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	if len(os.Args) < 2 {
		log.Fatal(fmt.Sprintf("Usage: %s <FILENAME>", os.Args[0]))
		os.Exit(1)
	}

	fileBytes, e := os.ReadFile(os.Args[1])
	if e != nil {
		log.Fatal(fmt.Sprintf("Cannot open file %s: %s", os.Args[0], e))
		os.Exit(1)
	}

	program := string(fileBytes)

	interpreter := interpreter.BrainfuckInterpreter{
		Program:   program,
		JumpTable: nil,
		TapeSize:  30000,
		Cin:       default_cin,
		Cout:      default_cout}
	e = interpreter.BuildJumpTable()
	if e != nil {
		log.Fatal(e)
		os.Exit(1)
	}

	interpreter.Run()

	os.Exit(0)
}

func default_cin() byte {
	reader := bufio.NewReader(os.Stdin)
	input, _ := reader.ReadString('\n')
	return input[0]
}

func default_cout(b byte) {
	fmt.Printf("%s", string(b))
}
