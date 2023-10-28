cmake_minimum_required(VERSION 3.27)
project(brainfuck)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_COMPILE_WARNING_AS_ERROR ON)

if (MSVC)
    # warning level 4
    add_compile_options(/W4)
else()
    # additional warnings
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

add_subdirectory(libs)
# find_package(Catch2 3 REQUIRED)

add_executable(brainfuck brainfuck.cpp)
add_executable(brainfuck-tests brainfuck-tests.cpp)
target_link_libraries(brainfuck-tests PRIVATE Catch2::Catch2WithMain)

add_test(NAME brainfuck-tests COMMAND brainfuck-tests)