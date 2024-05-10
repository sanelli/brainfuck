cmake_minimum_required(VERSION 3.27)
project(brainfuck C)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_COMPILE_WARNING_AS_ERROR ON)

if (MSVC)
    # warning level 4
    add_compile_options(/W4)
else()
    # additional warnings
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

add_executable(brainfuck brainfuck.c brainfuck-interpreter.c stack.c)
include_directories(brainfuck .)

add_executable(brainfuck-tests brainfuck-tests.c brainfuck-interpreter.c stack.c)
include_directories(brainfuck-tests .)

add_test(NAME brainfuck-tests COMMAND brainfuck-tests)