#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ALLOWED_VISITED_HOUSES 5000

typedef struct house house;
struct house
  {
    int x;
    int y;
  };

void
move (house *current, char direction)
{
  switch (direction)
    {
      case '>':
        current->x++;
        break;
      case '<':
        current->x--;
        break;
      case '^':
        current->y++;
        break;
      case 'v':
        current->y--;
        break;
    }
}

typedef struct visit_log visit_log;
struct visit_log
  {
    house log[MAX_ALLOWED_VISITED_HOUSES];
    int count;
  };

void
record_visit (visit_log *log, house current)
{
  for (int i = 0; i < log->count; i++)
    {
      house past = log->log[i];
      if (past.x == current.x && past.y == current.y)
        return;
    }

  if ((log->count + 1) == MAX_ALLOWED_VISITED_HOUSES)
    {
      fprintf(stderr, "ERROR: Visited too many houses\n");
      exit(1);
    }
  
  log->log[log->count] = current;
  log->count += 1;
}

void
swap (house **current, house *santa, house *robot)
{
  if (*current == santa)
    *current = robot;
  else
    *current = santa;
}

int
main (int argc, char **argv)
{
  visit_log lone_log;
  lone_log.count = 0;

  house lone_santa = { .x = 0, .y = 0 };
  record_visit(&lone_log, lone_santa);

  visit_log pair_log;
  pair_log.count = 0;

  house pair_santa = { .x = 0, .y = 0 };
  house robo_santa = { .x = 0, .y = 0 };
  house *current_santa = &pair_santa;
  record_visit(&pair_log, *current_santa);

  char command;
  while ((command = fgetc(stdin)) != EOF)
    {
      // Part 1
      move(&lone_santa, command);
      record_visit(&lone_log, lone_santa);

      // Part 2
      move(current_santa, command);
      record_visit(&pair_log, *current_santa);
      swap(&current_santa, &pair_santa, &robo_santa);
    }

  printf("Part 1: %d\n", lone_log.count);
  printf("Part 2: %d\n", pair_log.count);
}
