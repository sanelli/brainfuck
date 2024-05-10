#ifndef __BRAINFUCK_INTERPRETER__
#define __BRAINFUCK_INTERPRETER__

#include "common.h"

typedef char (*cin_t)(void);
typedef void (*cout_t)(char);

struct brainfuck_interpreter_jump_table_entry;

struct brainfuck_interpreter
{
    char *program;
    int program_size;
    struct brainfuck_interpreter_jump_table_entry *jump_table;
    cin_t cin;
    cout_t cout;
};

typedef struct brainfuck_interpreter brainfuck_interpreter;

struct brainfuck_interpreter *bf_alloc_from_string(const char *program);
struct brainfuck_interpreter *bf_alloc_from_file(const char *filename);
BOOL bf_run(const struct brainfuck_interpreter *interpreter);
void bf_set_cin(struct brainfuck_interpreter *interpreter, cin_t cin);
void bf_set_cout(struct brainfuck_interpreter *interpreter, cout_t cout);

void bf_free(struct brainfuck_interpreter *interpreter);

const char *bf_error(void);

#endif // __BRAINFUCK_INTERPRETER__
