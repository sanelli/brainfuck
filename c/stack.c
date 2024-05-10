#include "stack.h"
#include "common.h"

#include <stdlib.h>

struct stack_entry *alloc_stack(void)
{
    return NULL;
}

void free_stack(struct stack_entry *stack)
{
    while (!is_stack_empty(stack))
    {
        pop_stack(stack);
    }
}

struct stack_entry *push_stack(struct stack_entry *stack, int value)
{
    struct stack_entry *head = malloc(sizeof(stack_entry));
    head->next = stack;
    head->value = value;
    return head;
}

int peek_stack(struct stack_entry *stack)
{
    if (stack == NULL)
    {
        return -1;
    }

    return stack->value;
}

struct stack_entry *pop_stack(struct stack_entry *stack)
{
    if (stack == NULL)
    {
        return NULL;
    }

    struct stack_entry *head = stack->next;
    free(stack);

    return head;
}

BOOL is_stack_empty(struct stack_entry *stack)
{
    return stack == NULL;
}
