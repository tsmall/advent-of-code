#include <assert.h>
#include <stdbool.h>
#include <stdio.h>


#define MAX_BUS_COUNT 50


typedef struct bus_list bus_list_t;
struct bus_list
{
  int ids[MAX_BUS_COUNT];
  int count;
};


void
read_input (int *time, bus_list_t *buses)
{
  scanf ("%d", time);

  buses->count = 0;
  int id, n;
  char buffer[100];
  while (true)
    {
      assert (buses->count < MAX_BUS_COUNT);

      if (scanf ("%d", &id) == 1)
        buses->ids[buses->count++] = id;

      // Skip forward to the next number.
      while (true)
        {
          n = scanf ("%[^0-9]", buffer);
          if (n == EOF)
            return;
          if (n != 1)
            break;
        }
    }
}


int
find_departing_bus (int timestamp, bus_list_t buses)
{
  for (int i = 0; i < buses.count; i++)
    if (timestamp % buses.ids[i] == 0)
      return buses.ids[i];
  return 0;
}


int
main (int argc, char **argv)
{
  int earliest_depart_time;
  bus_list_t buses;
  read_input (&earliest_depart_time, &buses);

  int timestamp = earliest_depart_time;
  int my_bus_id = 0;
  while (!my_bus_id)
    {
      timestamp += 1;
      my_bus_id = find_departing_bus (timestamp, buses);
    }

  printf ("Part 1: %d\n", (timestamp - earliest_depart_time) * my_bus_id);

  return 0;
}
