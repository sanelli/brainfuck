#include <stdio.h>
#include <stdlib.h>

#include "brainfuck-interpreter.h"
#include "common.h"

int main(int argc, char *argv[])
{
    struct brainfuck_interpreter *interpreter = NULL;

    if (argc != 2)
    {
        printf("Usage: %s <filename>\n", argv[0]);
        return EXIT_FAILURE;
    }

    interpreter = bf_alloc_from_file(argv[1]);
    if (interpreter == NULL)
    {
        printf("An error occurred initializing the interpreter: %s", bf_error());
        bf_free(interpreter);
        return EXIT_FAILURE;
    }

    if (!bf_run(interpreter))
    {
        printf("An error occurred while executing the program: %s", bf_error());
        bf_free(interpreter);
        return EXIT_FAILURE;
    }

    bf_free(interpreter);
    return EXIT_SUCCESS;
}
