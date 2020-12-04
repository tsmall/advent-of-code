#include <stdio.h>
#include <stdlib.h>

long long int
multiply (const int *numbers, int length)
{
  long long int answer = numbers[0];
  for (int i = 1; i < length; i++)
    answer *= numbers[i];
  return answer;
}

int
main (int argc, char **argv)
{
  const int slope_xs[5] = {1, 3, 5, 7, 1};
  const int slope_ys[5] = {1, 1, 1, 1, 2};

  int xs[5]        = {0, 0, 0, 0, 0};
  int ys[5]        = {0, 0, 0, 0, 0};
  int trees_hit[5] = {0, 0, 0, 0, 0};

  size_t line_width = 35;
  char *line = malloc (sizeof (char) * line_width);
  int chars_read;
  while ((chars_read = getline (&line, &line_width, stdin)) > 0)
    {
      for (int i = 0; i < 5; i++)
        {
          if (ys[i] == slope_ys[i])
            {
              // chars_read includes the '\n' character;
              // subtract one to ignore that
              xs[i] = (xs[i] + slope_xs[i]) % (chars_read - 1);
              ys[i] = 0;

              if (line[xs[i]] == '#')
                trees_hit[i]++;
            }

          ys[i] += 1;
        }
    }

  printf ("Part 1: %d\n", trees_hit[1]);
  printf ("Part 2: %lld\n", multiply(trees_hit, 5));

  free (line);
  return 0;
}
