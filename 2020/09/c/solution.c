#include <stdbool.h>
#include <stdio.h>


#define PREAMBLE_LENGTH 25


void
load_preamble (int buffer[PREAMBLE_LENGTH])
{
  for (int i = 0; i < PREAMBLE_LENGTH; i++)
    scanf ("%d", &buffer[i]);
}


bool
is_valid (int number, int buffer[PREAMBLE_LENGTH])
{
  for (int i = 0; i < PREAMBLE_LENGTH; i++)
    for (int j = i + 1; j < PREAMBLE_LENGTH; j++)
      if (buffer[i] + buffer[j] == number)
        return true;

  return false;
}


int
main (int argc, char **argv)
{
  int buffer[PREAMBLE_LENGTH];
  load_preamble (buffer);

  int oldest_index = 0;
  int next_number;
  while ((scanf ("%d", &next_number)) == 1)
    {
      if (!is_valid (next_number, buffer))
        break;

      buffer[oldest_index] = next_number;
      oldest_index = (oldest_index + 1) % PREAMBLE_LENGTH;
    }

  printf ("Part 1: %d\n", next_number);

  return 0;
}
