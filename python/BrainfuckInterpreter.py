from collections.abc import Callable

DefaultTapeSize : int = 30000

class BrainfuckInterpreter():
    program : str
    cin : Callable[[], int]
    cout : Callable[[int], None]
    jumptable: dict[int, int]
    tapeSize : int

    def __init__(self, program: str, tapeSize: int, cin : Callable[[], int], cout : Callable[[int], None]) -> None:
        self.program = program
        self.cin = cin
        self.cout = cout
        self.tapeSize = tapeSize
        self.jumptable = self._buildJumpTable()

    def _buildJumpTable(self) -> dict[int, int]:
        table : dict[int, int] = {}
        stack : list = []
        for programCounter in range(len(self.program)):
            if self.program[programCounter] == '[':
                stack.append(programCounter)
            elif self.program[programCounter] == ']':
                if len(stack) == 0:
                    raise Exception("Missing matching '['.")
                
                openParPosition = stack.pop()
                table[openParPosition] = programCounter
                table[programCounter] = openParPosition

        if len(stack) > 0:
            raise Exception("Missing matching ']'.")
        
        return table

    def run(self) -> None:
        pointer = self.tapeSize
        tape = [ 0 for x in range(2 * self.tapeSize) ]
        programCounter = 0
        while programCounter < len(self.program):
            match self.program[programCounter]:
                case '>':
                    pointer += 1
                case '<':
                    pointer -= 1
                case '+':
                    tape[pointer] += 1
                    tape[pointer] %= 256
                case '-':
                    tape[pointer] -= 1
                    tape[pointer] %= 256
                case '.':
                    self.cout(tape[pointer])
                case ',':
                    tape[pointer] = self.cin() % 256
                case '[':
                    if tape[pointer] == 0:
                        programCounter = self.jumptable[programCounter] - 1
                case ']':
                    if tape[pointer] != 0:
                        programCounter = self.jumptable[programCounter] - 1

            programCounter += 1
