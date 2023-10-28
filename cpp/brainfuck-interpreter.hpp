#pragma once

#include <type_traits>
#include <string>
#include <unordered_map>
#include <stack>
#include <stdexcept>
#include <array>

using byte_t = unsigned char;
using program_counter_t = std::string::size_type;

template<typename TCin, typename TCout, int TAPE_SIZE = 30000>
class brainfuck_interpreter 
{
    std::string program;
    TCin cin;
    TCout cout;
    std::unordered_map<program_counter_t, program_counter_t> jump_table;

    void fill_jump_table()
    {
        jump_table.clear();
        std::stack<program_counter_t> stack;
        for(program_counter_t program_counter = 0; program_counter < program.length(); ++program_counter)
        {
             if (program[program_counter] == '[')
             {
                stack.push(program_counter);
             }
             else if(program[program_counter] == ']')
             {
                if(stack.empty())
                {
                    throw std::logic_error("Missing matching [");
                }

                auto open = stack.top();
                stack.pop();
                jump_table.insert_or_assign(open, program_counter);
                jump_table.insert_or_assign(program_counter, open);
             }
        }

        if(!stack.empty())
        {
            throw std::logic_error("Missing matching ]");
        }
    }

    public:
    template<typename TString, typename = typename std::enable_if_t<std::is_same_v<std::string, std::decay_t<TString>>>>
    brainfuck_interpreter(TString&& source, TCin input, TCout output)
        : program(std::move(source)), cin(input), cout(output)
    {
        fill_jump_table();
    }

    void run()
    {
        std::array<byte_t, 2 * TAPE_SIZE> tape;
        int pointer = TAPE_SIZE;

        tape.fill(static_cast<byte_t>(0));
        for (program_counter_t program_counter = 0; program_counter < program.length(); ++program_counter)
        {
            switch (program[program_counter])
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
                    cout(tape[pointer]);
                    break;
                case ',':
                    tape[pointer] = cin();
                    break;
                case '[':
                    if (tape[pointer] == 0)
                    {
                        program_counter = jump_table[program_counter] - 1;
                    }

                    break;
                case ']':
                    if (tape[pointer] != 0)
                    {
                        program_counter = jump_table[program_counter] - 1;
                    }

                    break;
            }
        }
    }
};