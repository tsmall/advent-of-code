#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char **argv)
{
  int expenses[200];
  int last_index = 0;

  int current;
  while ((scanf("%d", &current)) != EOF)
    {
      for (int i = last_index - 1; i >= 0; i--)
        {
          if (current + expenses[i] == 2020)
            {
              printf("Part 1: %d\n", current * expenses[i]);
              exit(0);
            }
        }

      expenses[last_index++] = current;
    }

  printf("Part 1: No answer found\n");
  return 1;
}
