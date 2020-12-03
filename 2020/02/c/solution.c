#include <stdbool.h>
#include <stdio.h>

#define MAX_PASSWORD_LENGTH 50

typedef struct policy policy;
struct policy
{
  char letter;
  unsigned int num_one;
  unsigned int num_two;
};

int
nextline (policy *policy, char *password)
{
  return scanf ("%u-%u %c: %s",
                &policy->num_one,
                &policy->num_two,
                &policy->letter,
                password);
}

bool
valid_sled (policy policy, char *password)
{
  unsigned int letter_count = 0;

  int i = 0;
  char c;
  while ((c = password[i++]) != '\0')
    if (c == policy.letter)
      letter_count++;

  return ((letter_count >= policy.num_one)
          && (letter_count <= policy.num_two));
}

bool
valid_toboggan (policy policy, char *password)
{
  unsigned int matches = 0;

  if (password[policy.num_one - 1] == policy.letter)
    matches++;

  if (password[policy.num_two - 1] == policy.letter)
    matches++;

  return matches == 1;
}

int
main (int argc, char **argv)
{
  unsigned int valid_sled_count = 0;
  unsigned int valid_toboggan_count = 0;

  policy policy;
  char password[MAX_PASSWORD_LENGTH];
  while ((nextline (&policy, password)) != EOF)
    {
      if (valid_sled (policy, password))
        valid_sled_count++;

      if (valid_toboggan (policy, password))
        valid_toboggan_count++;
    }

  printf ("Part 1: %d\n", valid_sled_count);
  printf ("Part 2: %d\n", valid_toboggan_count);
  return 0;
}
