#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "brainfuck-interpreter.h"
#include "common.h"
#include "stack.h"

#define TAPE_SIZE 30000
#define BUFFER_SIZE 1024

char *brainfuck_error = NULL;

struct brainfuck_interpreter_jump_table_entry
{
    int pc;
    int other_pc;
    struct brainfuck_interpreter_jump_table_entry *next;
};

typedef struct brainfuck_interpreter_jump_table_entry brainfuck_interpreter_jump_table_entry;

void bf_reset_error(void)
{
    brainfuck_error = NULL;
}

void bf_set_error(const char *error)
{
    brainfuck_error = (char *)error;
}

const char *bf_error(void)
{
    return brainfuck_error;
}

BOOL bf_compute_jump_table(struct brainfuck_interpreter *interpreter)
{
    stack_entry *stack = NULL;
    stack = alloc_stack();

    bf_reset_error();

    for (int pc = 0; pc < interpreter->program_size; ++pc)
    {
        switch (interpreter->program[pc])
        {
        case '[':
            stack = push_stack(stack, pc);
            break;

        case ']':
            if (is_stack_empty(stack))
            {
                bf_set_error("Missing matching [");
                return FALSE;
            }

            int other_pc = peek_stack(stack);
            stack = pop_stack(stack);

            struct brainfuck_interpreter_jump_table_entry *entry = (struct brainfuck_interpreter_jump_table_entry *)malloc(sizeof(brainfuck_interpreter_jump_table_entry));
            entry->pc = pc;
            entry->other_pc = other_pc;
            entry->next = interpreter->jump_table;

            interpreter->jump_table = entry;
            break;
        }
    }

    if (!is_stack_empty(stack))
    {
        bf_set_error("Missing matching ]");
        free_stack(stack);
        stack = NULL;
        return FALSE;
    }

    free_stack(stack);
    return TRUE;
}

int bf_get_pc_pair(const struct brainfuck_interpreter *interpreter, int pc)
{
    struct brainfuck_interpreter_jump_table_entry *cursor = interpreter->jump_table;
    while (cursor != NULL)
    {
        if (cursor->pc == pc)
            return cursor->other_pc;
        if (cursor->other_pc == pc)
            return cursor->pc;

        cursor = cursor->next;
    }

    return -1;
}

char bf_default_cin(void)
{
    return (char)getchar();
}

void bf_default_cout(char c)
{
    putchar(c);
}

struct brainfuck_interpreter *bf_alloc_from_string(const char *program)
{
    bf_reset_error();
    struct brainfuck_interpreter *interpreter = (struct brainfuck_interpreter *)malloc(sizeof(brainfuck_interpreter));
    interpreter->cin = bf_default_cin;
    interpreter->cout = bf_default_cout;

    interpreter->program_size = strlen(program);
    interpreter->program = (char *)malloc(interpreter->program_size + 1);
    strcpy(interpreter->program, program);

    interpreter->jump_table = NULL;

    if (!bf_compute_jump_table(interpreter))
    {
        const char *current_error = bf_error();
        bf_free(interpreter);
        bf_set_error(current_error);
        return NULL;
    }

    return interpreter;
}

struct brainfuck_interpreter *bf_alloc_from_file(const char *filename)
{
    long file_size = 0L;
    char buffer[BUFFER_SIZE];
    FILE *file = NULL;
    char *program = NULL;
    struct brainfuck_interpreter *interpreter = NULL;

    bf_reset_error();

    memset(buffer, 0, BUFFER_SIZE);

    file = fopen(filename, "r");
    if (file == NULL)
    {
        bf_set_error("Cannot open file.");
        return NULL;
    }

    if (fseek(file, 0, SEEK_END) == -1)
    {
        fclose(file);
        bf_set_error("Cannot seek the end of the file.");
        return NULL;
    }

    file_size = ftell(file);
    if (file_size == -1)
    {
        fclose(file);
        bf_set_error("Cannot obtain the size of the file.");
        return NULL;
    }

    if (fseek(file, 0, SEEK_SET) == -1)
    {
        fclose(file);
        bf_set_error("Cannot seek the end of the file.");
        return NULL;
    }

    program = (char *)malloc(file_size + 1);
    memset(program, 0, file_size + 1);

    while (fgets(buffer, BUFFER_SIZE - 1, file) != NULL)
    {
        program = strcat(program, buffer);
    }

    if (fclose(file) != 0)
    {
        free(program);
        bf_set_error("Cannot close the file.");
        return NULL;
    }

    interpreter = bf_alloc_from_string(program);
    free(program);

    return interpreter;
}

BOOL bf_run(const struct brainfuck_interpreter *interpreter)
{
    if (interpreter == NULL)
    {
        bf_set_error("Cannot run before allocating the interpreter");
        return FALSE;
    }

    char tape[2 * TAPE_SIZE];
    memset(tape, 0, 2 * TAPE_SIZE);
    int pointer = TAPE_SIZE;
    for (int program_counter = 0; program_counter < interpreter->program_size; ++program_counter)
    {
        switch (interpreter->program[program_counter])
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
            interpreter->cout(tape[pointer]);
            break;
        case ',':
            tape[pointer] = interpreter->cin();
            break;
        case '[':
            if (tape[pointer] == 0)
            {
                int target = bf_get_pc_pair(interpreter, program_counter);
                if (target == -1)
                {
                    bf_set_error("Invalid jump from '['");
                    return FALSE;
                }

                program_counter = target - 1;
            }

            break;
        case ']':
            if (tape[pointer] != 0)
            {
                int target = bf_get_pc_pair(interpreter, program_counter);
                if (target == -1)
                {
                    bf_set_error("Invalid jump from ']'");
                    return FALSE;
                }

                program_counter = target - 1;
            }

            break;
        }
    }

    return TRUE;
}

void bf_free(struct brainfuck_interpreter *interpreter)
{
    bf_reset_error();
    if (interpreter != NULL)
    {
        if (interpreter->program != NULL)
        {
            free(interpreter->program);
        }

        while (interpreter->jump_table != NULL)
        {
            struct brainfuck_interpreter_jump_table_entry *next = interpreter->jump_table->next;
            free(interpreter->jump_table);
            interpreter->jump_table = next;
        }

        free(interpreter);
    }
}

void bf_set_cin(struct brainfuck_interpreter *interpreter, cin_t cin)
{
    interpreter->cin = cin;
}

void bf_set_cout(struct brainfuck_interpreter *interpreter, cout_t cout)
{
    interpreter->cout = cout;
}
