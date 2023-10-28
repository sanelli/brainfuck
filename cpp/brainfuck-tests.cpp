#include <catch2/catch_test_macros.hpp>
#include <string>

#include "brainfuck-interpreter.hpp"

TEST_CASE( "Echo", "[brainfuck][echo]" )
{
    std::string program(",.");
    std::string input("x");
    std::string output;
    int inputPtr = 0;
    brainfuck_interpreter interpreter(
        program,
        [&input, &inputPtr]() { return static_cast<byte_t>(input[inputPtr++]); },
        [&output](byte_t b){ output += static_cast<char>(b); });
    interpreter.run();
    REQUIRE( output == input );
}

TEST_CASE( "HelloWorld", "[brainfuck][hello-world]" )
{
    // Sample from https://github.com/brain-lang/brainfuck/blob/master/brainfuck.md#hello-world-example
    std::string program("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.");
    std::string input;
    std::string output;
    brainfuck_interpreter interpreter(
        program,
        []() { return 0; },
        [&output](byte_t b){ output += static_cast<char>(b); });
    interpreter.run();
    REQUIRE( output == "Hello World!\n" );
}

TEST_CASE( "ROT13", "[brainfuck][ROT13]" )
{
    // https://brainfuck.org/rot13.b
    std::string program(R"bf(
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
            )bf");
    std::string input("stefano\0");
    std::string output;
    typename std::string::size_type inputPtr = 0;
    brainfuck_interpreter interpreter(
        program,
        [&input, &inputPtr]() { return inputPtr >= input.length() ? 0 : static_cast<byte_t>(input[inputPtr++]); },
        [&output](byte_t b){ output += static_cast<char>(b); });
    interpreter.run();
    REQUIRE( output == "fgrsnab" );
}

TEST_CASE( "wc", "[brainfuck][wc]" )
{
    // https://brainfuck.org/wc.b
    std::string program(R"bf(
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
            )bf");
    std::string input("Hello world\nthis is me\0");
    std::string output;
    typename std::string::size_type inputPtr = 0;
    brainfuck_interpreter interpreter(
        program,
        [&input, &inputPtr]() { return inputPtr >= input.length() ? 0 : static_cast<byte_t>(input[inputPtr++]); },
        [&output](byte_t b){ output += static_cast<char>(b); });
    interpreter.run();
    REQUIRE( output == "\t1\t5\t22\n" );
}