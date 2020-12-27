#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>


/* Instruction ----------------------------------------------------- */


typedef struct instruction instruction_t;
struct instruction
{
  char action;
  int value;
};


bool
read_instruction (instruction_t *instruction)
{
  char action;
  int value;
  if (scanf ("%c", &action) != 1)
    return false;
  if (scanf ("%d\n", &value) != 1)
    return false;

  instruction->action = action;
  instruction->value = value;
  return true;
}


void
print_instruction (instruction_t instruction)
{
  printf ("instruction_t { '%c', %d }\n",
          instruction.action,
          instruction.value);
}


/* Boat ------------------------------------------------------------ */


typedef enum direction direction_t;
enum direction
{
  NORTH,
  EAST,
  SOUTH,
  WEST,
};


typedef struct boat boat_t;
struct boat
{
  direction_t dir;
  int x;
  int y;
};


const char*
direction_description (direction_t dir)
{
  switch (dir)
    {
      case NORTH:
        return "NORTH";
      case SOUTH:
        return "SOUTH";
      case EAST:
        return "EAST";
      case WEST:
        return "WEST";
    }

  assert (false);
}


void
print_boat (boat_t boat)
{
  printf ("boat_t { %s, %d, %d }\n",
          direction_description (boat.dir),
          boat.x, boat.y);
}


/* Solution -------------------------------------------------------- */


void
move_forward (boat_t *boat, int amount)
{
  switch (boat->dir)
    {
      case NORTH:
        boat->y += amount;
        return;

      case SOUTH:
        boat->y -= amount;
        return;

      case EAST:
        boat->x += amount;
        return;

      case WEST:
        boat->x -= amount;
        return;
    }
}


void
change_direction (boat_t *boat, int degrees)
{
  int steps = degrees / 90;
  boat->dir = (boat->dir + steps) % 4;
}


void
follow (instruction_t instruction, boat_t *boat)
{
  switch (instruction.action)
    {
      case 'E':
        boat->x += instruction.value;
        return;

      case 'F':
        move_forward (boat, instruction.value);
        return;

      case 'L':
        change_direction (boat, instruction.value * -1);
        return;

      case 'N':
        boat->y += instruction.value;
        return;

      case 'R':
        change_direction (boat, instruction.value);
        return;

      case 'S':
        boat->y -= instruction.value;
        return;

      case 'W':
        boat->x -= instruction.value;
        return;
    }
}


int
main (int argc, char **argv)
{
  boat_t boat = { EAST, 0, 0 };

  instruction_t instruction;
  while (read_instruction (&instruction))
    follow (instruction, &boat);

  printf ("Part 1: %d\n", abs(boat.x) + abs(boat.y));

  return 0;
}
