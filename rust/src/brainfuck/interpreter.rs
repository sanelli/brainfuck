struct BrainfuckJumpTableEntry {
    program_counter: u32,
    other_program_counter: u32,
}

pub trait BrainfuckInterpreterIO {
    fn cin(&mut self) -> i8;
    fn cout(&mut self, value: i8);
}

struct DefaultBrainfuckInterpreterIO
{
}

impl BrainfuckInterpreterIO for DefaultBrainfuckInterpreterIO
{
    fn cin(&mut self) -> i8 {
        return 0;
    }
    fn cout(&mut self, value: i8) -> () {
        print!("{}", value as u8 as char)
    }
}

pub struct BrainfuckInterpreter {
    program: String,
    jump_table: Vec<BrainfuckJumpTableEntry>
}

impl BrainfuckInterpreter {
    fn build_jump_table(program: &String) -> Vec<BrainfuckJumpTableEntry> {
        let mut jump_table: Vec<BrainfuckJumpTableEntry> = Vec::new();
        let mut stack: Vec<u32> = Vec::new();
        for (program_counter, character) in program.char_indices() {
            match character {
                '[' => {
                    stack.push(program_counter as u32);
                }
                ']' => {
                    let other_program_counter = stack.pop().expect("Missing matching [ parentesis");
                    jump_table.push(BrainfuckJumpTableEntry {
                        program_counter: program_counter as u32,
                        other_program_counter: other_program_counter,
                    });
                }
                _ => {}
            }
        }

        if !stack.is_empty() {
            panic!("Missing matching ] parenthesis");
        }

        return jump_table;
    }

    fn get_pair_jump(&self, program_counter: u32) -> u32 {
        for entry in self.jump_table.iter() {
            if entry.program_counter == program_counter {
                return entry.other_program_counter;
            }
            if entry.other_program_counter == program_counter {
                return entry.program_counter;
            }
        }

        panic!("Cannot find matching pairs of []");
    }

    pub fn new(program: String) -> BrainfuckInterpreter {
        let jump_table = BrainfuckInterpreter::build_jump_table(&program);
        let interpreter = BrainfuckInterpreter {
            program,
            jump_table};

        return interpreter;
    }

    pub fn run_with_io(&self, io: Box<&mut dyn BrainfuckInterpreterIO>) {
        let mut pointer = 30000;
        #[allow(unused_mut)]
        let mut tape: [i8; 60000] = [0; 60000];
        let mut program_counter: u32 = 0;
        let number_of_characters = self.program.chars().count() as u32;

        while program_counter < number_of_characters {
            let character = self
                .program
                .chars()
                .nth(program_counter as usize)
                .expect("Indexed invalid part of the program");

            match character {
                '>' => {
                    pointer += 1;
                }
                '<' => {
                    pointer -= 1;
                }
                '+' => {
                    tape[pointer] += 1;
                }
                '-' => {
                    tape[pointer] -= 1;
                }
                ',' => {
                    tape[pointer] = io.cin();
                }
                '.' => {
                    io.cout(tape[pointer]);
                }
                '[' => {
                    if tape[pointer] == 0 {
                        let target = self.get_pair_jump(program_counter);
                        program_counter = target - 1;
                    }
                }
                ']' => {
                    if tape[pointer] != 0 {
                        let target = self.get_pair_jump(program_counter);
                        program_counter = target - 1;
                    }
                }
                _ => {}
            }

            program_counter += 1;
        }
    }

    pub fn run(&self) {
        let mut io = DefaultBrainfuckInterpreterIO { };
        self.run_with_io(Box::new(&mut io))
    }
}