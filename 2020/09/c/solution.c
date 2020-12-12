#include <assert.h>
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

  int all_numbers[1000];
  int all_numbers_index = 0;
  for (int i = 0; i < PREAMBLE_LENGTH; i++)
    all_numbers[all_numbers_index++] = buffer[i];

  int oldest_index = 0;
  int next_number;
  while ((scanf ("%d", &next_number)) == 1)
    {
      if (!is_valid (next_number, buffer))
        break;

      buffer[oldest_index] = next_number;
      oldest_index = (oldest_index + 1) % PREAMBLE_LENGTH;

      all_numbers[all_numbers_index++] = next_number;
      assert (all_numbers_index < 1000);
    }

  printf ("Part 1: %d\n", next_number);

  int sum;
  int smallest;
  int largest;
  for (int i = 0; i < all_numbers_index; i++)
    {
      sum = 0;
      smallest = __INT_MAX__;
      largest = 0;

      int n;
      for (int j = i; j < all_numbers_index; j++)
        {
          n = all_numbers[j];
          sum += n;
          if (n < smallest)
            smallest = n;
          if (n > largest)
            largest = n;

          if (sum == next_number)
            goto DONE;
        }
    }

  DONE:
  printf ("Part 2: %d\n", smallest + largest);

  return 0;
}
