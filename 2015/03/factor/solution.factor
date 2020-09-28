! Advent of Code 2015 Day 3
! https://adventofcode.com/2015/day/3

! To load:
! "/Users/tom/Projects/advent-of-code/2015/03/factor/solution.factor" run-file
! USE: aoc.2015.03

USING:
  kernel combinators
  io.files io.encodings.utf8
  sequences
  sets
  vectors math.vectors
;
IN: aoc.2015.03

: input ( -- lines )
  "/Users/tom/Projects/advent-of-code/2015/03/input.txt"
  utf8 file-lines
;

: move ( point char -- point' )
  {
    { CHAR: ^ [ { 0 1 } v+ ] }
    { CHAR: > [ { 1 0 } v+ ] }
    { CHAR: v [ { 0 1 } v- ] }
    { CHAR: < [ { 1 0 } v- ] }
  }
  case
;

: record ( visited point -- visited' point )
  [ swap adjoin ] 2keep
;

: follow-alone ( visited location instructions -- visited' location' )
  [ move record ] each
;

: follow-paired ( visited loc1 loc2 instructions -- visited' loc1' loc2' )
  [ move swap [ record ] dip ] each
;

: setup ( -- visited location )
  V{ } clone
  { 0 0 }
  record
;

: part1 ( -- n )
  setup
  input first follow-alone
  drop cardinality
;

: part2 ( -- n )
  setup { 0 0 }
  input first follow-paired
  2drop cardinality
;
