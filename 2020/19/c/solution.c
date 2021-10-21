#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* -------------------------------------------------------------------------- */
/* Constants                                                                  */
/* -------------------------------------------------------------------------- */

#define ARRAY_CAPACITY 5
#define RULE_ARRAY_CAPACITY 150

/* -------------------------------------------------------------------------- */
/* Array                                                                      */
/* -------------------------------------------------------------------------- */

struct array
{
  int length;
  int capacity;
  int *data;
};
typedef struct array array_t;

array_t *
array_new ()
{
  array_t *array = malloc (sizeof (array_t));
  array->length = 0;
  array->capacity = ARRAY_CAPACITY;
  array->data = malloc ((sizeof (int)) * ARRAY_CAPACITY);
  return array;
}

void
array_free (array_t *array)
{
  free (array->data);
  free (array);
}

void
array_append (array_t *array, int n)
{
  assert (array->length < ARRAY_CAPACITY);
  array->data[ array->length++ ] = n;
}

void
array_print (array_t *array)
{
  printf("array_t { ");
  for (int i = 0; i < array->length; i++)
    printf("%d ", array->data[i]);
  printf("}");
}

/* -------------------------------------------------------------------------- */
/* Rule                                                                       */
/* -------------------------------------------------------------------------- */

typedef enum { Value, Subrules } rule_type;

struct rule
{
  int id;
  rule_type type;
  union
  {
    char value;
    array_t *arrays[2];
  };
};
typedef struct rule rule_t;

rule_t *
rule_new_value (int id, char value)
{
  rule_t *rule = malloc (sizeof (rule_t));
  rule->type = Value;
  rule->id = id;
  rule->value = value;
  return rule;
}

rule_t *
rule_new_subrules (int id)
{
  rule_t *rule = malloc (sizeof (rule_t));
  rule->type = Subrules;
  rule->id = id;

  for (int i = 0; i < 2; i++)
    rule->arrays[i] = NULL;

  return rule;
}

void
rule_free (rule_t *rule)
{
  if (rule->type == Subrules)
    for (int i = 0; i < 2; i++)
      if (rule->arrays[i] != NULL)
        free (rule->arrays[i]);

  free (rule);
}

void
rule_append_array (rule_t *rule, array_t *array)
{
  assert (rule->type == Subrules);

  if (rule->arrays[0] == NULL)
    {
      rule->arrays[0] = array;
      return;
    }

  if (rule->arrays[1] == NULL)
    {
      rule->arrays[1] = array;
      return;
    }

  assert (false);
}

void
rule_print (rule_t *rule)
{
  printf ("rule_t { id=%d, ", rule->id);
  switch (rule->type)
    {
    case Value:
      printf ("value=%c", rule->value);
      break;
    case Subrules:
      printf ("arrays=[ ");
      if (rule->arrays[0] != NULL) array_print(rule->arrays[0]);
      if (rule->arrays[1] != NULL)
        {
          printf (", ");
          array_print(rule->arrays[1]);
        }
      printf (" ]");
      break;
    }
  printf (" }\n");
}

/* -------------------------------------------------------------------------- */
/* Rule Array                                                                 */
/* -------------------------------------------------------------------------- */

struct rule_array
{
  int length;
  int capacity;
  rule_t **rules;
};
typedef struct rule_array rule_array_t;

rule_array_t *
rule_array_new ()
{
  rule_array_t *rules = malloc (sizeof (rule_array_t));

  rules->length = 0;
  rules->capacity = RULE_ARRAY_CAPACITY;
  rules->rules = malloc (sizeof (rule_t*) * RULE_ARRAY_CAPACITY);

  return rules;
}

void
rule_array_free (rule_array_t *rules)
{
  for (int i = 0; i < rules->length; i++)
    rule_free (rules->rules[i]);

  free (rules->rules);
  free (rules);
}

void
rule_array_append (rule_array_t *rules, rule_t *rule)
{
  assert (rules->length < RULE_ARRAY_CAPACITY);
  rules->rules[ rules->length++ ] = rule;
}

/* -------------------------------------------------------------------------- */
/* Parsing                                                                    */
/* -------------------------------------------------------------------------- */

int
parse_number (char **string)
{
  int number = 0;
  while (**string >= '0' && **string <= '9')
    {
      number *= 10;
      number += **string - '0';
      *string += 1;
    }
  return number;
}

array_t *
parse_array (char **line)
{
  array_t *array = array_new ();

  int id;
  while (**line != '\n' && **line != '\0' && **line != '|')
    {
      id = parse_number (line);
      array_append (array, id);

      if (**line == ' ')
        *line += 1;
    }

  return array;
}

rule_t *
parse_rule (char *line)
{
  int id = parse_number (&line);

  /* Skip colon and space after ID */
  line += 2;

  rule_t *rule = NULL;
  array_t *array = NULL;
  char value;
  switch (*line)
    {
    case '"':
      line += 1;
      value = *line;
      rule = rule_new_value (id, value);
      break;

    default:
      rule = rule_new_subrules (id);
      array = parse_array (&line);
      rule_append_array (rule, array);

      if (*line == '|')
        {
          /* Skip the bar and space */
          line += 2;

          array = parse_array (&line);
          rule_append_array (rule, array);
        }
      break;
    }

  return rule;
}

rule_array_t *
parse_rules ()
{
  rule_array_t *rules = rule_array_new ();

  rule_t *rule;
  char *line = NULL;
  size_t line_length;
  while (getline (&line, &line_length, stdin) > 0 && *line != '\n')
    {
      rule = parse_rule (line);
      rule_array_append (rules, rule);
    }
  free (line);

  return rules;
}

/* -------------------------------------------------------------------------- */
/* Puzzle Logic                                                               */
/* -------------------------------------------------------------------------- */

rule_t *
find_rule (int id, rule_array_t *rules)
{
  for (int i = 0; i < rules->length; i++)
    if (rules->rules[i]->id == id)
      return rules->rules[i];

  assert (0);
}

char *
rule_match (int, rule_array_t*, char*);

char *
rule_array_match (array_t *array, rule_array_t *rules, char *message)
{
  char *remainder = message;
  char *result;
  for (int i = 0; i < array->length; i++)
    {
      result = rule_match (array->data[i], rules, remainder);
      if (result == remainder)
        return message;
      remainder = result;
    }

  return remainder;
}

char *
rule_match (int rule_id, rule_array_t *rules, char *message)
{
  rule_t *rule = find_rule (rule_id, rules);
  char *remainder = message;
  switch (rule->type)
    {
    case Value:
      if (rule->value == message[0])
        remainder++;
      break;

    case Subrules:
      remainder = rule_array_match (rule->arrays[0], rules, message);
      if (remainder == message && rule->arrays[1] != NULL)
        remainder = rule_array_match (rule->arrays[1], rules, message);
      break;
    }

  return remainder;
}

/* -------------------------------------------------------------------------- */
/* Main                                                                       */
/* -------------------------------------------------------------------------- */

int
main (int argc, char **argv)
{
  rule_array_t *rules = parse_rules ();

  int matches = 0;

  char message[250];
  char *remainder;
  while (scanf ("%250s", message) == 1)
    {
      remainder = rule_match (0, rules, message);
      if (*remainder == '\0')
        matches += 1;
    }

  printf ("Part 1: %d\n", matches);
  printf ("Part 2: TODO\n");

  rule_array_free (rules);

  return 0;
}
