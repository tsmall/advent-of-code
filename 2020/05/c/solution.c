#include <stdio.h>
#include <stdlib.h>

#define TICKET_COUNT 843

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

void
read_ids (int *all_ids)
{
  int index = 0;
  size_t length = 11;
  char *ticket = malloc (length * sizeof (char));
  while (getline (&ticket, &length, stdin) > 0)
    {
      all_ids[index] = id (ticket);
      index++;
    }

  free (ticket);
}

int
compare_ids (const void *id1, const void *id2)
{
  int *one = (int *)id1;
  int *two = (int *)id2;

  if (*one > *two)
    return 1;
  else if (*one < *two)
    return -1;
  else
    return 0;
}

int
missing_id (int all_ids[TICKET_COUNT])
{
  int prev = all_ids[0];
  for (int i = 1; i < TICKET_COUNT; i++)
    {
      if ((all_ids[i] - 1) != prev)
        return all_ids[i] - 1;

      prev = all_ids[i];
    }

  return -1;
}

int
main (int argc, char **argv)
{
  int all_ids[TICKET_COUNT];

  read_ids (all_ids);
  qsort (all_ids, TICKET_COUNT, sizeof (int), &compare_ids);

  printf ("Part 1: %d\n", all_ids[TICKET_COUNT - 1]);
  printf ("Part 2: %d\n", missing_id (all_ids));

  return 0;
}
