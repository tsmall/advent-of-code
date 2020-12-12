#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct bag_contents_t bag_contents_t;
typedef struct bag_t bag_t;

struct bag_t
{
  char adjective[25];
  char color[25];
  bag_contents_t *contents[4];
};

struct bag_contents_t
{
  int count;
  bag_t *bag;
};

void
bag_init (bag_t *bag)
{
  bag->adjective[0] = '\0';
  bag->color[0] = '\0';
  for (int i = 0; i < 4; i++)
    bag->contents[i] = NULL;
}

void
bag_free (bag_t *bag)
{
  for (int i = 0; i < 4; i++)
    if (bag->contents[i] != NULL)
      free (bag->contents[i]);
}

bool
bag_is_mine (bag_t *bag)
{
  return (strcmp (bag->adjective, "shiny") == 0
          && strcmp (bag->color, "gold") == 0);
}

bool
bag_can_contain_mine (bag_t bag)
{
  bag_contents_t *contained;
  for (int i = 0; i < 4; i++)
    {
      contained = bag.contents[i];
      if (contained == NULL)
        return false;

      if (bag_is_mine (contained->bag))
        return true;
    }
  
  return false;
}

void
bag_add_contained (bag_t *parent, bag_t *child, int count)
{
  for (int i = 0; i < 4; i++)
    if (parent->contents[i] == NULL)
      {
        bag_contents_t *contents = malloc (sizeof (bag_contents_t));
        assert (contents != NULL);

        contents->count = count;
        contents->bag = child;
        parent->contents[i] = contents;
        return;
      }

  // If we got here, we ran out of room, which shouldn't happen.
  assert (false);
}

int
bag_contents_count (bag_t *bag)
{
  int count = 0;

  for (int i = 0; i < 4; i++)
    {
      bag_contents_t *contents = bag->contents[i];
      if (contents == NULL)
        break;

      // Count the contained bags themselves ...
      count += contents->count;

      // ... and all the bags they contain.
      count += contents->count * bag_contents_count (contents->bag);
    }

  return count;
}

bool
bags_equal (bag_t b1, bag_t b2)
{
  return (strcmp (b1.adjective, b2.adjective) == 0
          && strcmp (b1.color, b2.color) == 0);
}

typedef struct set_t set_t;
struct set_t
{
  int size;
  int last_index;
  bag_t *contents;
};

void
set_alloc (set_t *set, int size)
{
  void *memory = malloc (size * sizeof (bag_t));
  assert (memory != NULL);

  set->size = size;
  set->last_index = 0;
  set->contents = memory;
}

void
set_free (set_t *set)
{
  for (int i = 0; i < set->last_index; i++)
    {
      bag_t *bag = &set->contents[i];
      for (int j = 0; j < 4; j++)
        if (bag->contents[j] != NULL)
          free (bag->contents[j]);
    }

  free (set->contents);
  set->size = 0;
  set->last_index = 0;
}

bag_t *
set_contains (set_t *set, bag_t bag)
{
  for (int i = 0; i < set->last_index; i++)
    if (bags_equal (set->contents[i], bag))
      return &set->contents[i];

  return NULL;
}

bag_t *
set_add (set_t *set, bag_t bag)
{
  assert (set->last_index < set->size);

  bag_t *ref;
  if ((ref = set_contains (set, bag)))
    return ref;

  set->contents[set->last_index] = bag;
  ref = &set->contents[set->last_index];
  set->last_index++;

  return ref;
}

typedef struct set_ref_t set_ref_t;
struct set_ref_t
{
  int last_index;
  bag_t *contents[600];
};

void
set_ref_clear (set_ref_t *set)
{
  set->last_index = 0;
}

bool
set_ref_is_empty (set_ref_t *set)
{
  return set->last_index == 0;
}

bool
set_ref_contains (set_ref_t *set, bag_t *bag)
{
  for (int i = 0; i < set->last_index; i++)
    if (set->contents[i] == bag)
      return true;

  return false;
}

void
set_ref_add (set_ref_t *set, bag_t *bag)
{
  assert (set->last_index < 600);

  if (set_ref_contains (set, bag))
    return;

  set->contents[set->last_index++] = bag;
}

void
set_ref_add_all (set_ref_t *dest, set_ref_t *src)
{
  for (int i = 0; i < src->last_index; i++)
    set_ref_add (dest, src->contents[i]);
}

int
set_ref_size (set_ref_t *set)
{
  return set->last_index;
}

bool
bag_contains_any (bag_t *bag, set_ref_t *set)
{
  for (int i = 0; i < 4; i++)
    if (bag->contents[i] != NULL)
      if (set_ref_contains (set, bag->contents[i]->bag))
        return true;

  return false;
}

typedef struct set_iterator_t set_iterator_t;
struct set_iterator_t
{
  set_t *set;
  int index;
};

set_iterator_t
set_iterator (set_t *set)
{
  set_iterator_t iter = {set, 0};
  return iter;
}

bag_t *
set_next (set_iterator_t *iter)
{
  if (iter->index == iter->set->last_index)
    return NULL;

  return &iter->set->contents[ iter->index++ ];
}

void
parse_input (set_t *all_bags, set_ref_t *starter_set, bag_t **my_bag)
{
  bag_t bag;
  bag_init (&bag);
  while (scanf ("%s %s bags contain", bag.adjective, bag.color) != EOF)
    {
      bag_t *parent = set_add (all_bags, bag);

      // Prepare bag for next loop
      bag_init (&bag);

      bag_t contained;
      bag_init (&contained);
      int count;
      while (scanf ("%d %s %s %*s", &count, contained.adjective, contained.color) == 3)
        {
          bag_t *child = set_add (all_bags, contained);
          bag_add_contained (parent, child, count);
        }
      scanf ("no other bags.");

      if (bag_is_mine (parent))
        *my_bag = parent;

      if (bag_can_contain_mine (*parent))
        set_ref_add (starter_set, parent);
    }
}

int
main (int argc, char **argv)
{
  set_t all_bags;
  set_alloc (&all_bags, 600);

  set_ref_t bags_able_to_hold_mine = {0};

  bag_t *my_bag;

  parse_input (&all_bags, &bags_able_to_hold_mine, &my_bag);

  set_ref_t current;
  set_ref_t next = {0};
  set_ref_add_all (&next, &bags_able_to_hold_mine);

  while (!set_ref_is_empty (&next))
    {
      current = next;
      set_ref_clear (&next);

      set_iterator_t iter = set_iterator (&all_bags);
      bag_t *bag;
      while ((bag = set_next (&iter)) != NULL)
        if (bag_contains_any (bag, &current))
          {
            set_ref_add (&next, bag);
            set_ref_add (&bags_able_to_hold_mine, bag);
          }
    }

  printf ("Part 1: %d\n", set_ref_size (&bags_able_to_hold_mine));
  printf ("Part 2: %d\n", bag_contents_count (my_bag));

  set_free (&all_bags);
  return 0;
}
