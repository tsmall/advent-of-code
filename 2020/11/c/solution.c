#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define MAX_ROW_COUNT 100
#define MAX_COL_COUNT 100


/* Point ----------------------------------------------------------- */


typedef struct point point_t;
struct point
{
  int x;
  int y;
};


point_t
dxdy (point_t p, int dx, int dy)
{
  point_t moved = { p.x + dx, p.y + dy };
  return moved;
}


/* Matrix ---------------------------------------------------------- */


typedef struct matrix matrix_t;
struct matrix
{
  int row_count;
  int col_count;

  /* seats[row][col] */
  char **seats;
};


void
free_matrix (matrix_t *m)
{
  for (int row = 0; row < m->row_count; row++)
    free (m->seats[row]);

  free (m->seats);
}


void
copy_matrix (matrix_t *dest, matrix_t src)
{
  // They must both be the same size already
  assert (src.col_count == dest->col_count);
  assert (src.row_count == dest->row_count);

  for (int row = 0; row < src.row_count; row++)
    for (int col = 0; col < src.col_count; col++)
      dest->seats[row][col] = src.seats[row][col];
}


void
clone_matrix (matrix_t *dest, matrix_t src)
{
  dest->row_count = src.row_count;
  dest->col_count = src.col_count;

  dest->seats = malloc (src.row_count * sizeof (char *));
  assert (dest->seats != NULL);

  for (int row = 0; row < dest->row_count; row++)
    {
      dest->seats[row] = malloc (src.col_count * sizeof (char));
      assert (dest->seats[row] != NULL);
    }

  copy_matrix (dest, src);
}


inline static
bool
is_in_matrix (matrix_t m, point_t p)
{
  return p.y >= 0 && p.y < m.row_count
      && p.x >= 0 && p.x < m.col_count;
}


bool
matrix_are_equal (matrix_t m1, matrix_t m2)
{
  assert (m1.row_count == m2.row_count);
  assert (m1.col_count == m2.col_count);

  for (int row = 0; row < m1.row_count; row++)
    if (memcmp (m1.seats[row], m2.seats[row], m1.col_count) != 0)
      return false;

  return true;
}


void
print_matrix (const char *label, matrix_t *m)
{
  printf ("%s:\n", label);
  printf ("  Matrix rows=%d cols=%d\n", m->row_count, m->col_count);
  for (int row = 0; row < m->row_count; row++)
    {
      printf ("    ");
      for (int col = 0; col < m->col_count; col++)
        printf ("%c", m->seats[row][col]);
      printf ("\n");
    }
}


/* Input Parsing --------------------------------------------------- */


void
load_input (matrix_t *m)
{
  m->row_count = 0;
  m->seats = malloc (MAX_ROW_COUNT * sizeof (char *));
  assert (m->seats != NULL);

  char *row;
  size_t line_length = MAX_COL_COUNT;
  int chars_read;
  do
    {
      row = malloc (line_length * sizeof (char));
      assert (row != NULL);

      chars_read = getline (&row, &line_length, stdin);
      if (chars_read > 0)
        {
          m->seats[m->row_count++] = row;
          m->col_count = chars_read - 1;
        }
      else
        free (row);
    }
  while (chars_read > 0);
}


/* Solution -------------------------------------------------------- */


inline static
bool
is_occupied (point_t p, matrix_t m)
{
  char seat = m.seats[p.y][p.x];
  return seat == '#';
}


void
check_occupancy (point_t p, matrix_t m, int *count)
{
  if (!is_in_matrix (m, p))
    return;

  if (is_occupied (p, m))
    *count += 1;
}


int
occupied_seats_near (point_t p, matrix_t m)
{
  int count = 0;

  check_occupancy (dxdy (p, -1, -1), m, &count);
  check_occupancy (dxdy (p,  0, -1), m, &count);
  check_occupancy (dxdy (p,  1, -1), m, &count);
  check_occupancy (dxdy (p, -1,  0), m, &count);
  check_occupancy (dxdy (p,  1,  0), m, &count);
  check_occupancy (dxdy (p, -1,  1), m, &count);
  check_occupancy (dxdy (p,  0,  1), m, &count);
  check_occupancy (dxdy (p,  1,  1), m, &count);

  return count;
}


void
check_line (point_t p, int dx, int dy, matrix_t m, int *count)
{
  char seat;
  while (true)
    {
      p = dxdy (p, dx, dy);
      if (!is_in_matrix (m, p))
        return;

      seat = m.seats[p.y][p.x];
      if (seat == '#')
        *count += 1;

      if (seat != '.')
        return;
    }
}


int
occupied_seats_visible (point_t p, matrix_t m)
{
  int count = 0;

  check_line (p, -1, -1, m, &count);
  check_line (p,  0, -1, m, &count);
  check_line (p,  1, -1, m, &count);
  check_line (p, -1,  0, m, &count);
  check_line (p,  1,  0, m, &count);
  check_line (p, -1,  1, m, &count);
  check_line (p,  0,  1, m, &count);
  check_line (p,  1,  1, m, &count);

  return count;
}


int
occupied_seat_count (matrix_t m)
{
  int count = 0;
  for (int row = 0; row < m.row_count; row++)
    for (int col = 0; col < m.col_count; col++)
      if (m.seats[row][col] == '#')
        count++;
  return count;
}


typedef int (*seat_fn)(point_t, matrix_t);

void
apply_rules (matrix_t *curr_state, matrix_t *next_state, seat_fn fn, int max_occupied)
{
  char seat;
  int nearby_occupant_count;
  for (int row = 0; row < curr_state->row_count; row++)
    for (int col = 0; col < curr_state->col_count; col++)
      {
        seat = curr_state->seats[row][col];
        nearby_occupant_count = fn ((point_t){col, row}, *curr_state);

        if (seat == 'L' && nearby_occupant_count == 0)
          next_state->seats[row][col] = '#';

        if (seat == '#' && nearby_occupant_count >= max_occupied)
          next_state->seats[row][col] = 'L';
      }
}


static inline
void
swap_state (matrix_t **curr_state, matrix_t **next_state)
{
  matrix_t *tmp = *curr_state;
  *curr_state = *next_state;
  *next_state = tmp;
}


int
analyze (matrix_t original, seat_fn fn, int max_occupied)
{
  matrix_t m1;
  clone_matrix (&m1, original);

  matrix_t m2;
  clone_matrix (&m2, m1);

  matrix_t *curr_state = &m1;
  matrix_t *next_state = &m2;

  while (true)
    {
      apply_rules (curr_state, next_state, fn, max_occupied);

      if (matrix_are_equal (*curr_state, *next_state))
        break;

      swap_state (&curr_state, &next_state);
      copy_matrix (next_state, *curr_state);
    }

  int answer = occupied_seat_count (*next_state);

  free_matrix (&m1);
  free_matrix (&m2);

  return answer;
}


int
part_one (matrix_t original)
{
  return analyze (original, &occupied_seats_near, 4);
}


int
part_two (matrix_t original)
{
  return analyze (original, &occupied_seats_visible, 5);
}


int
main (int argc, char **argv)
{
  matrix_t original;
  load_input (&original);

  printf ("Part 1: %d\n", part_one (original));
  printf ("Part 2: %d\n", part_two (original));

  free_matrix (&original);
  return 0;
}
