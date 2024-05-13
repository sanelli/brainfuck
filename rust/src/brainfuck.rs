pub mod interpreter {

    struct BrainfuckJumpTableEntry {
        program_counter: u32,
        other_program_counter: u32,
    }

    pub struct BrainfuckInterpreter {
        program: String,
        jump_table: Vec<BrainfuckJumpTableEntry>,
        cout: Box<dyn Fn(i8) -> ()>,
        cin: Box<dyn Fn() -> i8>,
    }

    impl BrainfuckInterpreter {
        fn default_cin() -> i8 {
            return 0;
        }
        fn default_cout(value: i8) -> () {
            println!("{}", value as u8 as char)
        }

        fn build_jump_table(program: &String) -> Vec<BrainfuckJumpTableEntry> {
            let mut jump_table: Vec<BrainfuckJumpTableEntry> = Vec::new();
            let mut stack: Vec<u32> = Vec::new();
            for (program_counter, character) in program.char_indices() {
                match character {
                    '[' => {
                        stack.push(program_counter as u32);
                    }
                    ']' => {
                        let other_program_counter =
                            stack.pop().expect("Missing matching [ parentesis");
                        jump_table.push(BrainfuckJumpTableEntry {
                            program_counter: program_counter as u32,
                            other_program_counter: other_program_counter,
                        });

                        println!("[dbg] ADDED {} <-> {}", program_counter, other_program_counter);
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

            println!("[dbg] Cannot find {}", program_counter);
            panic!("Cannot find matching pairs of []");
        }

        pub fn new(program: String) -> BrainfuckInterpreter {
            let jump_table = BrainfuckInterpreter::build_jump_table(&program);
            let interpreter = BrainfuckInterpreter {
                program: program,
                jump_table: jump_table,
                cin: Box::new(&BrainfuckInterpreter::default_cin),
                cout: Box::new(&BrainfuckInterpreter::default_cout)
            };

            return interpreter;
        }

        pub fn run(&self) {
            let mut pointer = 30000;
            #[allow(unused_mut)]
            let mut tape: [i8; 60000] = [0; 60000];
            let mut program_counter: u32 = 0;
            let mut characters = self.program.chars();
            let number_of_characters = self.program.chars().count() as u32;

            while program_counter < number_of_characters {
                let character = characters
                    .nth(program_counter as usize)
                    .expect("Indexed invalid part of the program");

                println!("[dbg] Character is {} @ {}", character, program_counter);

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
                        tape[pointer] = (*self.cin)();
                    }
                    '.' => {
                        (*self.cout)(tape[pointer]);
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
    }
}
