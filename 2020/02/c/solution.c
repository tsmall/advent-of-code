#include <stdbool.h>
#include <stdio.h>

#define MAX_PASSWORD_LENGTH 50

typedef struct policy policy;
struct policy
{
  char letter;
  unsigned int min_allowed;
  unsigned int max_allowed;
};

int
nextline (policy *policy, char *password)
{
  return scanf ("%u-%u %c: %s",
                &policy->min_allowed,
                &policy->max_allowed,
                &policy->letter,
                password);
}

bool
valid (policy policy, char *password)
{
  unsigned int letter_count = 0;

  int i = 0;
  char c;
  while ((c = password[i++]) != '\0')
    if (c == policy.letter)
      letter_count++;

  return (letter_count >= policy.min_allowed)
    && (letter_count <= policy.max_allowed);
}

int
main (int argc, char **argv)
{
  unsigned int valid_count = 0;

  policy policy;
  char password[MAX_PASSWORD_LENGTH];
  while ((nextline (&policy, password)) != EOF)
    if (valid (policy, password))
      valid_count++;

  printf ("Part 1: %d\n", valid_count);
  return 0;
}
