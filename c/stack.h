#ifndef __STACK__
#define __STACK__

#include "common.h"

struct stack_entry
{
    int value;
    struct stack_entry *next;
};

typedef struct stack_entry stack_entry;

struct stack_entry *alloc_stack(void);
void free_stack(struct stack_entry *stack);
struct stack_entry *push_stack(struct stack_entry *stack, int value);
int peek_stack(struct stack_entry *stack);
struct stack_entry *pop_stack(struct stack_entry *stack);
BOOL is_stack_empty(struct stack_entry *stack);

#endif // __STACK__
