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

int
depth (stack_t *stack)
{
  return stack->i;
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
  stack_t p1_stack = { 0 };
  stack_t p2_stack = { 0 };

  int depths[10];
  int depth_i = 0;

  long p1_sum = 0;
  long p2_sum = 0;

  expr_t p1_expr = zero;
  expr_t p2_expr = zero;

  char c;
  while ((c = getchar ()) != EOF)
    {
      if (c == '\n')
        {
          while (!is_empty (&p1_stack))
            p1_expr = apply (&p1_stack, p1_expr);
          p1_sum += p1_expr.n;
          p1_expr = zero;

          while (!is_empty (&p2_stack))
            p2_expr = apply (&p2_stack, p2_expr);
          p2_sum += p2_expr.n;
          p2_expr = zero;

          continue;
        }

      if (c == '(')
        {
          push (&p1_stack, p1_expr);
          p1_expr = zero;

          depths[ depth_i++ ] = depth (&p2_stack);
          push (&p2_stack, p2_expr);
          p2_expr = zero;

          continue;
        }

      if (c == ')')
        {
          p1_expr = apply (&p1_stack, p1_expr);

          depth_i -= 1;
          while (depth (&p2_stack) > depths[depth_i])
            p2_expr = apply (&p2_stack, p2_expr);

          continue;
        }

      if (c >= '0' && c <= '9')
        {
          p1_expr.n = eval (p1_expr, c - '0');
          p2_expr.n = eval (p2_expr, c - '0');
          continue;
        }

      if (c == '+')
        {
          p1_expr.op = ADD;
          p2_expr.op = ADD;
          continue;
        }

      if (c == '*')
        {
          p1_expr.op = MUL;

          p2_expr.op = MUL;
          push (&p2_stack, p2_expr);
          p2_expr = zero;

          continue;
        }
    }

  printf ("Part 1: %ld\n", p1_sum);
  printf ("Part 2: %ld\n", p2_sum);

  return 0;
}
