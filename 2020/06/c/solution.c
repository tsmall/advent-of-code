#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

inline static
int
question_index (char letter)
{
  return letter - 'a';
}

void
reset (bool questions[26])
{
  for (int i = 0; i < 26; i++)
    questions[i] = false;
}

void
record_answers (bool questions[26], char *answers, int answer_length)
{
  for (int i = 0; i < answer_length; i++)
    {
      int index = question_index (answers[i]);
      questions[index] = true;
    }
}

int
yes_count (bool questions[26])
{
  int count = 0;
  for (int i = 0; i < 26; i++)
    if (questions[i])
      count++;

  return count;
}

int
main (int argc, char **argv)
{
  int total_yes_answers = 0;

  bool questions[26];
  reset (questions);

  size_t line_length = 50;
  char *line = malloc (line_length * sizeof (char));
  int chars_read;
  while ((chars_read = getline (&line, &line_length, stdin)) > 0)
    {
      record_answers (questions, line, line_length);
      if (line[0] == '\n')
        {
          total_yes_answers += yes_count (questions);
          reset (questions);
        }
    }
  total_yes_answers += yes_count (questions);

  printf ("Part 1: %d\n", total_yes_answers);

  free (line);
  return 0;
}
