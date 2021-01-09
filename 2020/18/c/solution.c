#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

typedef enum op op_t;
enum op
{
  ADD,
  MUL,
};

typedef struct expr expr_t;
struct expr
{
  long n;
  op_t op;
};

const expr_t zero = { 0, ADD };

long
eval (expr_t expr, long n)
{
  switch (expr.op)
    {
      case ADD:
        return expr.n + n;

      case MUL:
        return expr.n * n;
    }
}

typedef struct stack stack_t;
struct stack
{
  int i;
  expr_t exprs[100];
};

void
push (stack_t *stack, expr_t expr)
{
  assert (stack->i < 99);
  stack->exprs[ stack->i++ ] = expr;
}

expr_t
pop (stack_t *stack)
{
  assert (stack->i > 0);
  return stack->exprs[ --stack->i ];
}

bool
is_empty (stack_t *stack)
{
  return stack->i == 0;
}

expr_t
apply (stack_t *stack, expr_t curr)
{
  expr_t prev = pop (stack);
  prev.n = eval (prev, curr.n);
  return prev;
}

int
main (int argc, char **argv)
{
  stack_t stack = { 0 };
  expr_t expr = zero;
  long sum = 0;
  char c;
  int line_num = 1;
  while ((c = getchar ()) != EOF)
    {
      if (c == '\n')
        {
          while (!is_empty (&stack))
            expr = apply (&stack, expr);

          printf ("Line %d = %ld\n", line_num++, expr.n);
          sum += expr.n;
          expr = zero;
          continue;
        }

      if (c == '(')
        {
          push (&stack, expr);
          expr = zero;
          continue;
        }

      if (c == ')')
        {
          expr = apply (&stack, expr);
          continue;
        }

      if (c >= '0' && c <= '9')
        {
          expr.n = eval (expr, c - '0');
          continue;
        }

      if (c == '+')
        {
          expr.op = ADD;
          continue;
        }

      if (c == '*')
        {
          expr.op = MUL;
          continue;
        }
    }

  printf ("Part 1: %ld\n", sum);

  return 0;
}
