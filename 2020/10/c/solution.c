#include <assert.h>
#include <stdio.h>
#include <stdlib.h>


#define MAX_POSSIBLE_ADAPTERS 100


int
compare_adapters (const void *a1, const void *a2)
{
  int *adapter1 = (int *)a1;
  int *adapter2 = (int *)a2;

  if (*adapter1 < *adapter2)
    return -1;
  else if (*adapter1 > *adapter2)
    return 1;
  else
    return 0;
}


int
load_adapters (int *adapters)
{
  // The outlet.
  adapters[0] = 0;

  int count = 1;
  while ((scanf ("%d", &adapters[count])) == 1)
    {
      count++;
      assert (count < MAX_POSSIBLE_ADAPTERS);
    }

  qsort (adapters, count, sizeof (int), &compare_adapters);

  // Your device's adapter.
  adapters[count] = adapters[count - 1] + 3;
  count++;

  return count;
}


void
count_diffs (int diff_count[4], int *adapters, int adapter_count)
{
  int previous_adapter = adapters[0];
  for (int i = 1; i < adapter_count; i++)
    {
      int current_adapter = adapters[i];
      int diff = current_adapter - previous_adapter;
      assert (diff == 1 || diff == 3);

      diff_count[diff]++;

      previous_adapter = current_adapter;
    }
}


/*
  Many thanks to Nunki3 on the Advent of Code Reddit
  for helping me figure out an algorithm
  that solves this problem.
  https://www.reddit.com/r/adventofcode/comments/kacdbl/2020_day_10c_part_2_no_clue_how_to_begin/gf9lzhd/
*/
long int
count_arrangements (int *adapters, int adapter_count)
{
  long int *paths = calloc (adapter_count, sizeof (long int));
  paths[0] = 1;

  for (int i = 0; i < adapter_count; i++)
    for (int j = 1; (i+j < adapter_count) && (adapters[i+j] - adapters[i] <= 3); j++)
      paths[i+j] += paths[i];

  free (paths);
  return paths[adapter_count - 1];
}


int
main (int argc, char **argv)
{
  int adapters[MAX_POSSIBLE_ADAPTERS];
  int adapter_count = load_adapters (adapters);

  int diff_count[4] = {0, 0, 0, 0};
  count_diffs (diff_count, adapters, adapter_count);

  long int arrangement_count = count_arrangements (adapters, adapter_count);

  printf ("Part 1: %d\n", diff_count[1] * diff_count[3]);
  printf ("  %d with a difference of 1\n", diff_count[1]);
  printf ("  %d with a difference of 3\n", diff_count[3]);
  printf ("Part 2: %ld\n", arrangement_count);

  return 0;
}
