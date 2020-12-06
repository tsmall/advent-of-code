#include <stdio.h>
#include <stdlib.h>

int
seat (const char *ticket)
{
  int max = 7;
  int min = 0;
  int mid;
  for (int i = 0; i < 3; i++)
    {
      mid = (min + max) / 2;
      switch (ticket[7 + i])
        {
          case 'L':
            max = mid;
            break;
          case 'R':
            min = mid;
            break;
          default:
            printf ("ERROR: Unrecognized seat code '%c'\n", ticket[7 + i]);
            exit (1);
        }
    }

  return max;
}

int
row (const char *ticket)
{
  int max = 127;
  int min = 0;
  int mid;
  for (int i = 0; i < 7; i++)
    {
      mid = (min + max) / 2;
      switch (ticket[i])
        {
          case 'F':
            max = mid;
            break;
          case 'B':
            min = mid;
            break;
          default:
            printf ("ERROR: Unrecognized row code '%c'\n", ticket[i]);
            exit (1);
        }
    }

  return max;
}

int
id (const char *ticket)
{
  return (row (ticket) * 8) + seat (ticket);
}

int
main (int argc, char **argv)
{
  int highest_id = 0;

  size_t length = 11;
  char *ticket = malloc (length * sizeof (char));
  while (getline (&ticket, &length, stdin) > 0)
    {
      int n = id (ticket);
      if (n > highest_id)
        highest_id = n;
    }

  printf ("Part 1: %d\n", highest_id);

  free (ticket);
  return 0;
}
