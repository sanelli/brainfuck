#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "brainfuck-interpreter.h"
#include "common.h"

typedef BOOL (*test_t)(void);

BOOL run_test(const char *name, test_t);
BOOL test_hello_world(void);
BOOL test_echo(void);
BOOL test_rot13(void);
BOOL test_wc(void);

int main(void)
{
    BOOL success = TRUE;
    test_t tests[] = {test_hello_world, test_echo, test_rot13, test_wc};
    const char *names[] = {"Hello world", "Echo", "ROT 13", "wc"};

    for (size_t index = 0; index < sizeof(tests) / sizeof(test_t); ++index)
    {
        BOOL test_success = run_test(names[index], tests[index]);
        success = success && test_success;
    }

    printf("\nTests %s!\n", success ? "run successfully" : "failed");

    return success ? EXIT_SUCCESS : EXIT_FAILURE;
}

BOOL run_test(const char *name, test_t test)
{
    printf("- %s: ", name);
    BOOL test_success = test();
    printf("%s", test_success ? "OK\n" : "KO\n");
    return test_success;
}

char *input = NULL;
int input_ptr = 0;
char output[1024];
int output_ptr = 0;

void reset_io(char *expected_input)
{
    input = expected_input;
    input_ptr = 0;
    memset(output, 0, 1024);
    output_ptr = 0;
}

char test_cin(void)
{
    return input[input_ptr++];
}

void test_cout(char c)
{
    output[output_ptr++] = c;
}

BOOL test_hello_world(void)
{
    // Sample from https://github.com/brain-lang/brainfuck/blob/master/brainfuck.md#hello-world-example
    const char *program = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    reset_io("");
    struct brainfuck_interpreter *interpreter = bf_alloc_from_string(program);
    bf_set_cin(interpreter, test_cin);
    bf_set_cout(interpreter, test_cout);
    if (interpreter == NULL)
    {
        bf_free(interpreter);
        return FALSE;
    }

    BOOL success = bf_run(interpreter);
    if (success)
    {
        success = strcmp("Hello World!\n", output) == 0;
        success = success && input_ptr == 0;
    }

    bf_free(interpreter);
    return success;
}

BOOL test_echo(void)
{
    const char *program = ",.";
    reset_io("x");
    struct brainfuck_interpreter *interpreter = bf_alloc_from_string(program);
    bf_set_cin(interpreter, test_cin);
    bf_set_cout(interpreter, test_cout);
    if (interpreter == NULL)
    {
        bf_free(interpreter);
        return FALSE;
    }

    BOOL success = bf_run(interpreter);
    if (success)
    {
        success = strcmp("x", output) == 0;
        success = success && input_ptr == 1;
    }

    bf_free(interpreter);
    return success;
}

BOOL test_rot13(void)
{
    // https://brainfuck.org/rot13.b
    const char *program =
        ","
        "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
        "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
        "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
        "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
        "[>++++++++++++++<-"
        "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
        "[>>+++++[<----->-]<<-"
        "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
        "[>++++++++++++++<-"
        "[>+<-[>+<-[>+<-[>+<-[>+<-"
        "[>++++++++++++++<-"
        "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
        "[>>+++++[<----->-]<<-"
        "[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
        "[>++++++++++++++<-"
        "[>+<-]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
        "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]>.[-]<,]";
    reset_io("stefano");
    struct brainfuck_interpreter *interpreter = bf_alloc_from_string(program);
    bf_set_cin(interpreter, test_cin);
    bf_set_cout(interpreter, test_cout);
    if (interpreter == NULL)
    {
        bf_free(interpreter);
        return FALSE;
    }

    BOOL success = bf_run(interpreter);
    if (success)
    {
        success = strcmp("fgrsnab", output) == 0;
        success = success && input_ptr == 8;
    }

    bf_free(interpreter);
    return success;
}

BOOL test_wc(void)
{
    const char *program =
        ">>>+>>>>>+>>+>>+[<<],["
        "    -[-[-[-[-[-[-[-[<+>-[>+<-[>-<-[-[-[<++[<++++++>-]<"
        "        [>>[-<]<[>]<-]>>[<+>-[<->[-]]]]]]]]]]]]]]]]"
        "    <[-<<[-]+>]<<[>>>>>>+<<<<<<-]>[>]>>>>>>>+>["
        "        <+["
        "            >+++++++++<-[>-<-]++>[<+++++++>-[<->-]+[+>>>>>>]]"
        "            <[>+<-]>[>>>>>++>[-]]+<"
        "        ]>[-<<<<<<]>>>>"
        "    ],"
        "]+<++>>>[[+++++>>>>>>]<+>+[[<++++++++>-]<.<<<<<]>>>>>>>>]";
    reset_io("Hello world\nthis is me");
    struct brainfuck_interpreter *interpreter = bf_alloc_from_string(program);
    bf_set_cin(interpreter, test_cin);
    bf_set_cout(interpreter, test_cout);
    if (interpreter == NULL)
    {
        bf_free(interpreter);
        return FALSE;
    }

    BOOL success = bf_run(interpreter);
    if (success)
    {
        success = strcmp("\t1\t5\t22\n", output) == 0;
        success = success && input_ptr == 23;
    }

    bf_free(interpreter);
    return success;
}
