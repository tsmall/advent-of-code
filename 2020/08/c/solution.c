#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define MEMORY_SIZE 641


typedef enum operation_t operation_t;
enum operation_t
{
  NOP,
  ACC,
  JMP,
};


typedef struct instruction_t instruction_t;
struct instruction_t
{
  operation_t operation;
  int argument;
};


int accumulator = 0;
int program_counter = 0;
instruction_t memory[MEMORY_SIZE];
int execution_count[MEMORY_SIZE];


void
initialize_cpu ()
{
  for (int i = 0; i < MEMORY_SIZE; i++)
    execution_count[i] = 0;
}


operation_t
parse_operation (char op_string[4])
{
  if (strncmp (op_string, "nop", 3) == 0)
    return NOP;

  if (strncmp (op_string, "acc", 3) == 0)
    return ACC;

  if (strncmp (op_string, "jmp", 3) == 0)
    return JMP;

  fprintf (stderr, "ERROR Unrecognized operation: %s\n", op_string);
  exit (1);
}


void
load_memory ()
{
  int counter = 0;

  char op_string[4];
  int argument;
  while ((scanf ("%s %d", op_string, &argument)) == 2)
    {
      instruction_t *instruction = &memory[counter++];
      instruction->operation = parse_operation (op_string);
      instruction->argument = argument;
    }
}


void
run_instruction ()
{
  instruction_t *instruction = &memory[program_counter];

  switch (instruction->operation)
    {
      case NOP:
        program_counter++;
        break;

      case ACC:
        accumulator += instruction->argument;
        program_counter++;
        break;

      case JMP:
        program_counter += instruction->argument;
        break;
    }
}


int
main (int argc, char **argv)
{
  initialize_cpu ();
  load_memory ();

  while ((++execution_count[program_counter]) < 2)
    run_instruction ();

  printf ("Part 1: %d\n", accumulator);

  return 0;
}
