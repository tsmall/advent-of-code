#include <stdbool.h>
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


int accumulator;
int program_counter;
instruction_t memory[MEMORY_SIZE];
int execution_count[MEMORY_SIZE];


void
reset_cpu ()
{
  accumulator = 0;
  program_counter = 0;

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


typedef enum termination_condition_t termination_condition_t;
enum termination_condition_t
{
  INFINITE_LOOP,
  NORMAL,
};


termination_condition_t
run_to_end_or_loop ()
{
  while (1)
    {
      if ((++execution_count[program_counter]) == 2)
        return INFINITE_LOOP;

      if (program_counter >= MEMORY_SIZE)
        return NORMAL;

      run_instruction ();
    }
}


bool
is_swappable_instruction (int index)
{
  instruction_t *instruction = &memory[index];
  return (instruction->operation == NOP
          || instruction->operation == JMP);
}


void
swap_instruction (int index)
{
  instruction_t *instruction = &memory[index];
  switch (instruction->operation)
    {
      case NOP:
        instruction->operation = JMP;
        break;

      case JMP:
        instruction->operation = NOP;
        break;

      case ACC:
        // Shouldn't be changed
        break;
    }
}


int
change_next_instruction (int start_index)
{
  if (start_index >= 0)
    swap_instruction (start_index);

  for (int i = start_index + 1; i < MEMORY_SIZE; i++)
    {
      if (is_swappable_instruction (i))
        {
          swap_instruction (i);
          return i;
        }
    }

  fprintf (stderr, "ERROR No instructions to change\n");
  exit (1);
}


int
main (int argc, char **argv)
{
  reset_cpu ();
  load_memory ();

  run_to_end_or_loop ();
  printf ("Part 1: %d\n", accumulator);

  int last_instruction_changed = -1;
  termination_condition_t result = INFINITE_LOOP;
  while (result != NORMAL)
    {
      reset_cpu ();
      last_instruction_changed = change_next_instruction (last_instruction_changed);
      result = run_to_end_or_loop ();
    }

  printf ("Part 2: %d\n", accumulator);

  return 0;
}
