#include <stdio.h>
#include <stdlib.h>

#define NO_ANSWER -1

int
check_for_part_one_answer(int *expenses, int last_index)
{
  int current = expenses[last_index];
  for (int i = last_index - 1; i >= 0; i--)
    if (current + expenses[i] == 2020)
      return current * expenses[i];

  return NO_ANSWER;
}

int
check_for_part_two_answer(int *expenses, int last_index)
{
  int current = expenses[last_index];
  for (int i = last_index - 1; i >= 0; i--)
    for (int j = i - 1; j >= 0; j--)
      if (current + expenses[i] + expenses[j] == 2020)
        return current * expenses[i] * expenses[j];

  return NO_ANSWER;
}

int
main(int argc, char **argv)
{
  int expenses[200];
  int next_index = 0;

  int part_one_answer = NO_ANSWER;
  int part_two_answer = NO_ANSWER;

  int current;
  while ((scanf("%d", &current)) != EOF)
    {
      expenses[next_index] = current;

      if (part_one_answer == NO_ANSWER)
        part_one_answer = check_for_part_one_answer(expenses, next_index);

      if (part_two_answer == NO_ANSWER)
        part_two_answer = check_for_part_two_answer(expenses, next_index);

      if (part_one_answer != NO_ANSWER && part_two_answer != NO_ANSWER)
        break;

      next_index++;
    }

  printf("Part 1: %d\n", part_one_answer);
  printf("Part 2: %d\n", part_two_answer);
  return 1;
}
