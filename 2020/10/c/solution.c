#include <assert.h>
#include <stdio.h>
#include <stdlib.h>


#define MAX_POSSIBLE_ADAPTERS 100


int
load_input (int adapters[11])
{
  int count = 0;
  while ((scanf ("%d", &adapters[count])) == 1)
    {
      count++;
      assert (count < MAX_POSSIBLE_ADAPTERS);
    }

  return count;
}


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
main (int argc, char **argv)
{
  int adapters[MAX_POSSIBLE_ADAPTERS];
  int adapter_count = load_input (adapters);
  qsort (adapters, adapter_count, sizeof (int), &compare_adapters);

  int diff_count[4] = {0, 0, 0, 1};

  int previous_adapter = 0;
  for (int i = 0; i < adapter_count; i++)
    {
      int current_adapter = adapters[i];
      int diff = current_adapter - previous_adapter;
      assert (diff == 1 || diff == 3);

      diff_count[diff]++;

      previous_adapter = current_adapter;
    }

  printf ("Part 1: %d\n", diff_count[1] * diff_count[3]);
  printf ("  %d with a difference of 1\n", diff_count[1]);
  printf ("  %d with a difference of 3\n", diff_count[3]);

  return 0;
}
