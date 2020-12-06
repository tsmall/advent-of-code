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

bool
update (int *passport, char *field, char *value)
{
  if (strncmp (field, "byr", 3) == 0)
    {
      *passport |= BYR;

      int year;
      return (sscanf (value, "%d", &year) == 1
              && year >= 1920
              && year <= 2002);
    }
  else if (strncmp (field, "iyr", 3) == 0)
    {
      *passport |= IYR;
      
      int year;
      return (sscanf (value, "%d", &year) == 1
              && year >= 2010
              && year <= 2020);
    }
  else if (strncmp (field, "eyr", 3) == 0)
    {
      *passport |= EYR;
      
      int year;
      return (sscanf (value, "%d", &year) == 1
              && year >= 2020
              && year <= 2030);
    }
  else if (strncmp (field, "hgt", 3) == 0)
    {
      *passport |= HGT;

      int height;
      char unit[3];
      if (sscanf (value, "%d%3c", &height, unit) == 2)
        {
          if (strncmp (unit, "cm", 2) == 0)
            return height >= 150 && height <= 193;
          if (strncmp (unit, "in", 2) == 0)
            return height >= 59 && height <= 76;
        }

      return false;
    }
  else if (strncmp (field, "hcl", 3) == 0)
    {
      *passport |= HCL;
      
      char color[7];
      return (sscanf (value, "#%6[0-9a-f]", color) == 1);
    }
  else if (strncmp (field, "ecl", 3) == 0)
    {
      *passport |= ECL;

      char color[4];
      return (sscanf (value, "%3c", color) == 1
              && (strncmp (color, "amb", 3) == 0
                  || strncmp (color, "blu", 3) == 0
                  || strncmp (color, "brn", 3) == 0
                  || strncmp (color, "gry", 3) == 0
                  || strncmp (color, "grn", 3) == 0
                  || strncmp (color, "hzl", 3) == 0
                  || strncmp (color, "oth", 3) == 0));
    }
  else if (strncmp (field, "pid", 3) == 0)
    {
      *passport |= PID;
      
      char number[100];
      return (sscanf (value, "%[0-9]", number) == 1
              && strlen (number) == 9);
    }
  else if (strncmp (field, "cid", 3) == 0)
    {
      *passport |= CID;

      return true;
    }

  fprintf (stderr, "ERROR: Unrecognized field '%s'\n", field);
  exit (1);
}

bool
parse_line (const char *line, int *passport)
{
  bool all_values_are_valid = true;

  char field[3 + 1];
  char value[100];
  int chars_read;
  const char *remaining = line;
  while ((sscanf (remaining, "%3c:%s%n", field, value, &chars_read)) == 2)
    {
      bool value_is_valid = update (passport, field, value);
      all_values_are_valid = all_values_are_valid && value_is_valid;

      remaining += chars_read;
      while (*remaining == ' ')
        remaining++;
    }

  return all_values_are_valid;
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
  int passports_with_required_fields = 0;
  int passports_with_valid_data = 0;

  size_t line_length = 200;
  char *line = malloc (sizeof (char) * line_length);
  int chars_read;
  bool passport_data_valid = true;
  while ((chars_read = getline (&line, &line_length, stdin)) > 0)
    {
      bool line_valid = parse_line (line, &passport);
      passport_data_valid = passport_data_valid && line_valid;

      if (line[0] == '\n')
        {
          if (is_valid (passport))
            {
              passports_with_required_fields++;
              
              if (passport_data_valid)
                passports_with_valid_data++;
            }

          passport = 0;
          passport_data_valid = true;
        }
    }

  if (is_valid (passport))
    {
      passports_with_required_fields++;

      if (passport_data_valid)
        passports_with_valid_data++;
    }

  printf ("Part 1: %d\n", passports_with_required_fields);
  printf ("Part 2: %d\n", passports_with_valid_data);

  free (line);
  return 0;
}
