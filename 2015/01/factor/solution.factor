! Advent of Code 2015 Day 1
! https://adventofcode.com/2015/day/1

! To load:
! "/Users/tom/Projects/advent-of-code/2015/01/factor/solution.factor" run-file
! USE: aoc.2015.01

USING:
  kernel combinators sequences math
  io.files io.encodings.utf8
;
IN: aoc.2015.01

: move ( floor char -- floor )
  {
    { CHAR: ( [ 1 + ] }
    { CHAR: ) [ 1 - ] }
    [ drop ]
  } case
;

: inc-index ( index basement floor -- index' basement floor )
  [ 1 + ] 2dip
;

: first-basement? ( index basement floor -- index basement ? )
  0 <
  [ dup >boolean not ] dip
  and
;

: monitor-basement ( index basement floor -- index' basement' floor' )
  [
    first-basement?
    [ drop dup ] [ ] if
  ] keep
  inc-index
;

: follow ( index basement floor str -- index' basement' floor' )
  [ move monitor-basement ] each
;

: input ( -- lines )
  "/Users/tom/Projects/advent-of-code/2015/01/input.txt"
  utf8 file-lines
;

: answer ( -- floor basement )
  1 f 0
  input first
  follow
  -rot nip
;
