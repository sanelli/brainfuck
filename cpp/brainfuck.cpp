#include <cstdlib>
#include <string>
#include <cstdio>
#include <iostream>
#include <stdexcept>
#include <fstream>

#include "brainfuck-interpreter.hpp"

byte_t default_cin()
{
    return static_cast<byte_t>(getchar() % 256);
}

void default_cout(byte_t b)
{
    std::cout << static_cast<char>(b);
}

int main(int argc, char *argv[])
{
    if(argc < 2)
    {
         std::cerr << "Usage: brainfuck <INPUT_FILE>" << std::endl;
        return EXIT_FAILURE;
    }

    std::ifstream stream(argv[1]);
    std::string program((std::istreambuf_iterator<char>(stream)), std::istreambuf_iterator<char>());

    try
    {
        brainfuck_interpreter interpreter(program, default_cin, default_cout);
        interpreter.run();
    }
    catch (std::logic_error &error)
    {
        std::cerr << error.what() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}