#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* Memory ---------------------------------------------------------- */


typedef struct address address_t;
struct address
{
  long id;
  long value;
};


typedef struct memory memory_t;
struct memory
{
  long capacity;
  long index;
  address_t *addresses;
};


memory_t*
init_memory ()
{
  memory_t *memory = malloc (sizeof (memory_t));

  memory->capacity = 1 << 21;
  memory->index = 0;
  memory->addresses = malloc (memory->capacity * sizeof (address_t));

  return memory;
}


void
free_memory (memory_t *memory)
{
  free (memory->addresses);
  free (memory);
}


address_t*
find_address (memory_t *memory, long int id)
{
  for (int i = 0; i < memory->index; i++)
    if (memory->addresses[i].id == id)
      return &memory->addresses[i];

  assert (memory->index < memory->capacity);
  address_t *address = &memory->addresses[memory->index];
  address->id = id;

  memory->index += 1;

  return address;
}


void
set_memory (memory_t *memory, long int id, long int value)
{
  address_t *address = find_address (memory, id);
  address->value = value;
}


long int
sum_memory (memory_t *memory)
{
  long sum = 0;
  for (int i = 0; i < memory->index; i++)
    sum += memory->addresses[i].value;
  return sum;
}


/* Mask ------------------------------------------------------------ */


typedef struct mask mask_t;
struct mask
{
  long and_mask;
  long or_mask;
};


void
update_mask (mask_t *mask, char *format)
{
  mask->and_mask = 0xFFFFFFFF;
  mask->or_mask  = 0x00000000;

  int bit;
  for (int i = 35; i >= 0; i--)
    {
      bit = 35 - i;

      if (format[i] == '0')
        mask->and_mask &= ~(1L << bit);

      if (format[i] == '1')
        mask->or_mask |= (1L << bit);

      if (format[i] == 'X')
        {
          mask->and_mask |= (1L << bit);
          mask->or_mask &= ~(1L << bit);
        }
    }
}


/* Input ----------------------------------------------------------- */


typedef enum line_type line_type;
enum line_type
{
  MASK,
  MEMSET,
};


typedef struct line line_t;
struct line
{
  line_type type;
  union
  {
    struct
    {
      char value[37];
    } mask;
    struct
    {
      long int addr;
      long int value;
    } memset;
  } value;
};


bool
read_line (line_t *line)
{
  char *buffer = NULL;
  size_t length = 0;
  if (getline (&buffer, &length, stdin) == -1)
    return false;

  bool result = false;

  char mask[37];
  if (sscanf (buffer, "mask = %s", mask) == 1)
    {
      line->type = MASK;
      strncpy (line->value.mask.value, mask, 37);
      result = true;
      goto done;
    }

  long int addr;
  long int value;
  if (sscanf (buffer, "mem[%ld] = %ld", &addr, &value) == 2)
    {
      line->type = MEMSET;
      line->value.memset.addr = addr;
      line->value.memset.value = value;
      result = true;
      goto done;
    }

  done:
  free (buffer);
  return result;
}


/* Solution -------------------------------------------------------- */


void
execute (memory_t *memory, mask_t *mask, line_t line)
{
  switch (line.type)
    {
      case MASK:
        update_mask (mask, line.value.mask.value);
        return;

      case MEMSET:
        long int value = line.value.memset.value;
        value &= mask->and_mask;
        value |= mask->or_mask;
        set_memory (memory, line.value.memset.addr, value);
        return;
    }
}


int
main (int argc, char **argv)
{
  memory_t *memory = init_memory ();
  mask_t mask;

  line_t line;
  while (read_line (&line))
    execute (memory, &mask, line);

  printf ("Part 1: %ld\n", sum_memory (memory));

  free_memory (memory);
  return 0;
}
