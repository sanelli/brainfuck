module interpreter;

import std.stdio;
import std.file;
import std.exception;
import core.stdc.stdio;
import std.string;
import std.container.array;
import std.range.primitives;
import std.functional;

byte bf_defualt_cin()
{
    return cast(byte) getchar();
}

void bf_defualt_cout(byte c)
{
    write("%c".format(cast(char) c));
}

class Interpreter
{
    private class JumpTableEntry
    {
        private int pc, other_pc;
        this(int pc, int other_pc)
        {
            this.pc = pc;
            this.other_pc = other_pc;
        }

        public int get_match(int pc)
        {
            if (this.pc == pc)
            {
                return this.other_pc;
            }

            if (this.other_pc == pc)
            {
                return this.pc;
            }

            return -1;
        }
    }

    private string program;
    private JumpTableEntry[] jump_table;
    private byte delegate() cin;
    private void delegate(byte) cout;

    private this(string program)
    {
        this.program = program;
        this.set_jump_table();
        this.cin = std.functional.toDelegate(&bf_defualt_cin);
        this.cout = std.functional.toDelegate(&bf_defualt_cout);
    }

    static interpreter.Interpreter from_file(string filename)
    {
        auto prg = readText(filename);
        return from_string(prg);
    }

    static interpreter.Interpreter from_string(string program)
    {
        return new Interpreter(program);
    }

    void run()
    {
        const TAPE_SIZE = 30_000;
        byte[2 * TAPE_SIZE] tape;
        for (int index = 0; index < tape.length; ++index)
        {
            tape[index] = 0;
        }

        int pointer = TAPE_SIZE;
        for (auto program_counter = 0; program_counter < this.program.length; ++program_counter)
        {
            switch (this.program[program_counter])
            {
            case '>':
                ++pointer;
                break;
            case '<':
                --pointer;
                break;
            case '+':
                ++tape[pointer];
                break;
            case '-':
                --tape[pointer];
                break;
            case '.':
                this.cout(tape[pointer]);
                break;
            case ',':
                tape[pointer] = this.cin();
                break;
            case '[':
                if (tape[pointer] == 0)
                {
                    int target = this.get_jump(program_counter);
                    enforce(target != -1, "Invalid jump from '['");
                    program_counter = target - 1;
                }

                break;
            case ']':
                if (tape[pointer] != 0)
                {
                    int target = this.get_jump(program_counter);
                    enforce(target != -1, "Invalid jump from ']'");
                    program_counter = target - 1;
                }
                break;

            default:
                // Ignore any other character.
                break;
            }
        }

    }

    private void set_jump_table()
    {
        int[] stack;
        for (int program_counter = 0; program_counter < this.program.length; ++program_counter)
        {
            switch (this.program[program_counter])
            {
            case '[':
                stack ~= program_counter;
                break;
            case ']':
                enforce(stack.length != 0, "Missing matching [ at %d".format(program_counter));
                auto other_pc = stack.back;
                stack.popBack();
                this.jump_table ~= new JumpTableEntry(program_counter, other_pc);
                break;
            default:
                // Ignore any other character
                break;
            }
        }

        enforce(stack.length == 0, "Missing matching ]");
    }

    private int get_jump(int pc)
    {
        foreach (entry; this.jump_table)
        {
            auto match = entry.get_match(pc);
            if (match != -1)
            {
                return match;
            }
        }

        return -1;
    }

    unittest  /* hello world */
    {
        string outstr = "";
        void test_cout(byte b)
        {
            outstr ~= "%c".format(cast(char) b);
        }

        byte test_cin()
        {
            assert(false, "CIN should not be invoked");
        }

        // Sample from https://github.com/brain-lang/brainfuck/blob/master/brainfuck.md#hello-world-example
        Interpreter intrp = Interpreter.from_string(
            "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>." ~
                ">---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.");
        intrp.cout = &test_cout;
        intrp.cin = &test_cin;
        intrp.run();

        assert(outstr == "Hello World!\n", "Hello world failed");
    }

    unittest  /* echo */
    {
        string outstr = "";
        void test_cout(byte b)
        {
            outstr ~= "%c".format(cast(char) b);
        }

        string intstr = "x";
        int instr_ptr = 0;
        byte test_cin()
        {
            return intstr[instr_ptr++];
        }

        Interpreter intrp = Interpreter.from_string(",.");
        intrp.cout = &test_cout;
        intrp.cin = &test_cin;
        intrp.run();

        assert(outstr == "x", "Echo failed");
        assert(instr_ptr == 1, "Echo failed");
    }

    unittest  /* rot13 */
    {
        string outstr = "";
        void test_cout(byte b)
        {
            outstr ~= "%c".format(cast(char) b);
        }

        string intstr = "stefano\0";
        int instr_ptr = 0;
        byte test_cin()
        {
            return intstr[instr_ptr++];
        }

        // https://brainfuck.org/rot13.b
        Interpreter intrp = Interpreter.from_string(
            "," ~
                "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" ~
                "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" ~
                "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" ~
                "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" ~
                "[>++++++++++++++<-" ~
                "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" ~
                "[>>+++++[<----->-]<<-" ~
                "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" ~
                "[>++++++++++++++<-" ~
                "[>+<-[>+<-[>+<-[>+<-[>+<-" ~
                "[>++++++++++++++<-" ~
                "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" ~
                "[>>+++++[<----->-]<<-" ~
                "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-" ~
                "[>++++++++++++++<-" ~
                "[>+<-]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]" ~
                "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]>.[-]<,]");
        intrp.cout = &test_cout;
        intrp.cin = &test_cin;
        intrp.run();

        assert(outstr == "fgrsnab", "ROT13 failed");
        assert(instr_ptr == 8, "ROT13 failed");
    }

    unittest  /* wc */
    {
        string outstr = "";
        void test_cout(byte b)
        {
            outstr ~= "%c".format(cast(char) b);
        }

        string intstr = "Hello world\nthis is me\0";
        int instr_ptr = 0;
        byte test_cin()
        {
            return intstr[instr_ptr++];
        }

        // https://brainfuck.org/wc.b
        Interpreter intrp = Interpreter.from_string(
            ">>>+>>>>>+>>+>>+[<<],[" ~
                "    -[-[-[-[-[-[-[-[<+>-[>+<-[>-<-[-[-[<++[<++++++>-]<" ~
                "        [>>[-<]<[>]<-]>>[<+>-[<->[-]]]]]]]]]]]]]]]]" ~
                "    <[-<<[-]+>]<<[>>>>>>+<<<<<<-]>[>]>>>>>>>+>[" ~
                "        <+[" ~
                "            >+++++++++<-[>-<-]++>[<+++++++>-[<->-]+[+>>>>>>]]" ~
                "            <[>+<-]>[>>>>>++>[-]]+<" ~
                "        ]>[-<<<<<<]>>>>" ~
                "    ]," ~
                "]+<++>>>[[+++++>>>>>>]<+>+[[<++++++++>-]<.<<<<<]>>>>>>>>]");
        intrp.cout = &test_cout;
        intrp.cin = &test_cin;
        intrp.run();

        assert(outstr == "\t1\t5\t22\n", "wc failed");
        assert(instr_ptr == 23, "wc failed");
    }
}
