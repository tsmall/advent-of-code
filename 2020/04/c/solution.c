#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BYR 0b00000001
#define IYR 0b00000010
#define EYR 0b00000100
#define HGT 0b00001000
#define HCL 0b00010000
#define ECL 0b00100000
#define PID 0b01000000
#define CID 0b10000000

void
update (int *passport, char *field)
{
  if (strncmp (field, "byr", 3) == 0)
    {
      *passport |= BYR;
      return;
    }
  else if (strncmp (field, "iyr", 3) == 0)
    {
      *passport |= IYR;
      return;
    }
  else if (strncmp (field, "eyr", 3) == 0)
    {
      *passport |= EYR;
      return;
    }
  else if (strncmp (field, "hgt", 3) == 0)
    {
      *passport |= HGT;
      return;
    }
  else if (strncmp (field, "hcl", 3) == 0)
    {
      *passport |= HCL;
      return;
    }
  else if (strncmp (field, "ecl", 3) == 0)
    {
      *passport |= ECL;
      return;
    }
  else if (strncmp (field, "pid", 3) == 0)
    {
      *passport |= PID;
      return;
    }
  else if (strncmp (field, "cid", 3) == 0)
    {
      *passport |= CID;
      return;
    }
}

void
parse_line (const char *line, int *passport)
{
  char field[3 + 1];
  char value[100];
  int chars_read;
  const char *remaining = line;
  while ((sscanf (remaining, "%3c:%s%n", field, value, &chars_read)) == 2)
    {
      update (passport, field);

      remaining += chars_read;
      while (*remaining == ' ')
        remaining++;
    }
}

bool
is_valid (int passport)
{
  return (passport & 0b1111111) == 0b1111111;
}

int
main (int argc, char **argv)
{
  int passport = 0;
  int valid_passports = 0;

  size_t line_length = 200;
  char *line = malloc (sizeof (char) * line_length);
  int chars_read;
  while ((chars_read = getline (&line, &line_length, stdin)) > 0)
    {
      parse_line (line, &passport);

      if (line[0] == '\n')
        {
          if (is_valid (passport))
            valid_passports++;

          passport = 0;
        }
    }

  if (is_valid (passport))
    valid_passports++;

  printf ("Part 1: %d\n", valid_passports);

  free (line);
  return 0;
}
