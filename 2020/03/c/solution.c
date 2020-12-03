#include <stdio.h>
#include <stdlib.h>

int
main (int argc, char **argv)
{
  int x = 0;
  int trees_hit = 0;

  size_t line_width = 35;
  char *line = malloc (sizeof (char) * line_width);
  int chars_read;
  while ((chars_read = getline (&line, &line_width, stdin)) > 0)
    {
      if (line[x] == '#')
        trees_hit++;

      // chars_read includes the '\n' character;
      // subtract one to ignore that
      x = (x + 3) % (chars_read - 1);
    }

  printf ("Part 1: %d\n", trees_hit);

  free (line);
  return 0;
}
