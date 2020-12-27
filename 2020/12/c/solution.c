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
print_boat (boat_t boat)
{
  printf ("boat_t { %s, %d, %d }\n",
          direction_description (boat.dir),
          boat.x, boat.y);
}


/* Waypoint -------------------------------------------------------- */


typedef struct waypoint waypoint_t;
struct waypoint
{
  int x;
  int y;
};


void
rotate_waypoint (waypoint_t *waypoint, int degrees)
{
  if (degrees < 0)
    degrees = 360 + degrees;

  int x = waypoint->x;
  int y = waypoint->y;
  switch (degrees)
    {
      case 90:
        waypoint->x = y;
        waypoint->y = x * -1;
        return;

      case 180:
        waypoint->x = x * -1;
        waypoint->y = y * -1;
        return;

      case 270:
        waypoint->x = y * -1;
        waypoint->y = x;
        return;
    }
}


void
print_waypoint (waypoint_t waypoint)
{
  printf ("waypoint_t { %d, %d }\n", waypoint.x, waypoint.y);
}


/* Solution -------------------------------------------------------- */


void
follow_guessed_rules (instruction_t instruction, boat_t *boat)
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


void
follow_actual_rules (instruction_t instruction, boat_t *boat, waypoint_t *waypoint)
{
  switch (instruction.action)
    {
      case 'E':
        waypoint->x += instruction.value;
        return;

      case 'F':
        boat->x += waypoint->x * instruction.value;
        boat->y += waypoint->y * instruction.value;
        return;

      case 'L':
        rotate_waypoint (waypoint, instruction.value * -1);
        return;

      case 'N':
        waypoint->y += instruction.value;
        return;

      case 'R':
        rotate_waypoint (waypoint, instruction.value);
        return;

      case 'S':
        waypoint->y -= instruction.value;
        return;

      case 'W':
        waypoint->x -= instruction.value;
        return;
    }
}


int
manhattan_distance (boat_t boat)
{
  return abs(boat.x) + abs(boat.y);
}


int
main (int argc, char **argv)
{
  boat_t guessed_boat = { EAST, 0, 0 };

  boat_t actual_boat = { EAST, 0, 0 };
  waypoint_t waypoint = { 10, 1 };

  instruction_t instruction;
  while (read_instruction (&instruction))
    {
      follow_guessed_rules (instruction, &guessed_boat);
      follow_actual_rules (instruction, &actual_boat, &waypoint);
    }

  printf ("Part 1: %d\n", manhattan_distance (guessed_boat));
  printf ("Part 2: %d\n", manhattan_distance (actual_boat));

  return 0;
}
