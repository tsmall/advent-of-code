#include <stdio.h>
#include <stdlib.h>

inline static
int
question_index (char letter)
{
  return letter - 'a';
}

void
reset (int questions[26])
{
  for (int i = 0; i < 26; i++)
    questions[i] = 0;
}

void
record_answers (int questions[26], char *answers, int answer_length)
{
  for (int i = 0; i < answer_length; i++)
    {
      int index = question_index (answers[i]);
      questions[index]++;
    }
}

int
yes_count (const int questions[26])
{
  int count = 0;
  for (int i = 0; i < 26; i++)
    {
      if (questions[i] > 0)
        count++;
    }

  return count;
}

int
agreed_count (const int questions[26], int party_size)
{
  int count = 0;
  for (int i = 0; i < 26; i++)
    if (questions[i] == party_size)
      count++;

  return count;
}

int
main (int argc, char **argv)
{
  int total_yes_answers = 0;
  int total_agreed_answers = 0;

  int party_size = 0;
  int questions[26];
  reset (questions);

  size_t line_length = 50;
  char *line = malloc (line_length * sizeof (char));
  int chars_read;
  while ((chars_read = getline (&line, &line_length, stdin)) > 0)
    {
      if (line[0] == '\n')
        {

          total_yes_answers += yes_count (questions);
          total_agreed_answers += agreed_count (questions, party_size);

          reset (questions);
          party_size = 0;

          continue;
        }

      party_size++;
      record_answers (questions, line, chars_read - 1);
    }

  total_yes_answers += yes_count (questions);
  total_agreed_answers += agreed_count (questions, party_size);

  printf ("Part 1: %d\n", total_yes_answers);
  printf ("Part 2: %d\n", total_agreed_answers);

  free (line);
  return 0;
}
