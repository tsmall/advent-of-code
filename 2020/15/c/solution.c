#include <assert.h>
#include <stdio.h>


#define NEVER -1
#define MEMORY_CAPACITY 2000


int turn = 1;
int last_number = NEVER;
int memory[MEMORY_CAPACITY];


void
wipe_memory ()
{
  for (int i = 0; i < MEMORY_CAPACITY; i++)
    memory[i] = NEVER;
}


void
speak (int number)
{
  last_number = number;
}


int
recall_and_remember ()
{
  assert (last_number < MEMORY_CAPACITY);

  int previous_turn = memory[last_number];
  memory[last_number] = turn - 1;
  return previous_turn;
}


void
read_starting_numbers ()
{
  int number;
  while (scanf ("%d", &number) == 1)
    {
      if (last_number != NEVER)
        recall_and_remember ();

      speak (number);
      turn += 1;

      if (getchar () == EOF)
        break;
    }
}


void
play_a_turn ()
{
  int previous_turn = recall_and_remember ();
  if (previous_turn == NEVER)
    speak (0);
  else
    speak (turn - 1 - previous_turn);

  turn += 1;
}


void
play_to_turn (int limit)
{
  while (turn <= limit)
    play_a_turn ();
}


int
main (int argc, char **argv)
{
  wipe_memory ();
  read_starting_numbers ();
  play_to_turn (2020);

  printf ("Part 1: %d\n", last_number);

  return 0;
}
