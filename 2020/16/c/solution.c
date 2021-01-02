#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>


#define MAX_ALLOWED_RULES 20
#define MAX_ALLOWED_FIELDS 30


/* Tickets --------------------------------------------------------- */


typedef struct ticket ticket_t;
struct ticket
{
  int count;
  int *numbers;
};


void
free_ticket (ticket_t *ticket)
{
  free (ticket->numbers);
  free (ticket);
}


void
print_ticket (ticket_t *ticket)
{
  printf ("Ticket:");
  for (int i = 0; i < ticket->count; i++)
    printf (" %d", ticket->numbers[i]);
  printf ("\n");
}


/* Rules ----------------------------------------------------------- */


typedef struct rule rule_t;
struct rule
{
  int min_1;
  int max_1;
  int min_2;
  int max_2;
};


void
free_rules (rule_t *rules)
{
  free (rules);
}


void
print_rules (rule_t *rules, int rule_count)
{
  printf ("Rules (%d):\n", rule_count);
  for (int i = 0; i < rule_count; i++)
    printf ("  #%d: %d-%d or %d-%d\n",
            i + 1,
            rules[i].min_1, rules[i].max_1,
            rules[i].min_2, rules[i].max_2);
}


bool
passes_rule (rule_t rule, int number)
{
  bool too_small = (number < rule.min_1);
  bool in_between = (number > rule.max_1 && number < rule.min_2);
  bool too_large = (number > rule.max_2);

  return !(too_small || in_between || too_large);
}


int
error_rate (rule_t *rules, int rule_count, ticket_t ticket)
{
  int error_rate = 0;

  int number;
  for (int i = 0; i < ticket.count; i++)
    {
      number = ticket.numbers[i];
      for (int j = 0; j < rule_count; j++)
        if (passes_rule (rules[j], number))
          goto passed_a_rule;

      // If we get here, the number doesn't pass any of the rules,
      // so it counts towards our error rate.
      error_rate += number;
      continue;

      passed_a_rule:
      continue;
    }

  return error_rate;
}


/* Input Parsing --------------------------------------------------- */


int
parse_rules (rule_t **rules)
{
  int count = 0;
  *rules = malloc (MAX_ALLOWED_RULES * sizeof (rule_t));
  assert (*rules != NULL);

  char field_name[200];
  rule_t *rule = *rules;
  char *line;
  size_t line_length = 0;
  int matches;
  while (getline (&line, &line_length, stdin) > 0)
    {
      if (line[0] == '\n')
        break;

      matches = sscanf (line, "%[^:]: %d-%d or %d-%d",
                        field_name,
                        &rule->min_1, &rule->max_1,
                        &rule->min_2, &rule->max_2);

      if (matches == 5)
        {
          count += 1;
          rule += 1;
        }
    }

  free (line);
  return count;
}


bool
parse_ticket (ticket_t **ticket)
{
  ticket_t *t = *ticket;
  if (t == NULL)
    {
      t = malloc (sizeof (ticket_t));
      assert (t != NULL);

      t->numbers = malloc (MAX_ALLOWED_FIELDS * sizeof (int));
      assert (t->numbers != NULL);

      *ticket = t;
    }

  t->count = 0;
  while (scanf ("%d", t->numbers + t->count) == 1)
    {
      t->count += 1;
      assert (t->count < MAX_ALLOWED_FIELDS);

      if (getchar () == '\n')
        break;
    }

  return t->count > 0;
}


void
skip_line ()
{
  while (getchar () != '\n')
    ;
}


/* Main ------------------------------------------------------------ */


int
main (int argc, char **argv)
{
  rule_t *rules;
  int rule_count = parse_rules (&rules);
  print_rules (rules, rule_count);

  skip_line ();

  ticket_t *ticket = NULL;
  parse_ticket (&ticket);
  print_ticket (ticket);

  skip_line ();
  skip_line ();

  int rate = 0;
  while (parse_ticket (&ticket))
    {
      print_ticket (ticket);
      rate += error_rate (rules, rule_count, *ticket);
    }

  printf ("Part 1: %d\n", rate);

  free_rules (rules);
  free_ticket (ticket);

  return 0;
}
