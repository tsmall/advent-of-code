! Advent of Code 2015 Day 2
! https://adventofcode.com/2015/day/2

! To load:
! "/Users/tom/Projects/advent-of-code/2015/02/factor/solution.factor" run-file
! USE: aoc.2015.02

USING:
  kernel
  accessors classes.tuple
  arrays sequences sorting
  math math.order math.parser
  splitting
  io.files io.encodings.utf8
;
IN: aoc.2015.02

TUPLE: box length width height ;

: input ( -- lines )
  "/Users/tom/Projects/advent-of-code/2015/02/input.txt"
  utf8 file-lines
;

: line>box ( line -- box )
  "x" split
  [ string>number ] map
  first3 box boa
;

: areas ( box -- areas )
  [ [ length>> ] [ width>> ] bi * ]
  [ [ width>> ] [ height>> ] bi * ]
  [ [ height>> ] [ length>> ] bi * ]
  tri
  3array
;

: surface-area ( box -- n )
  areas [ 2 * ] map sum
;

: smallest-area ( box -- n )
  areas infimum
;

: paper-needed ( box -- n )
  [ surface-area ] [ smallest-area ] bi +
;

: smallest-perimeter ( box -- n )
  tuple-slots natural-sort first2
  + 2 *
;

: cubic-volume ( box -- n )
  [ length>> ] [ width>> ] [ height>> ] tri * *
;

: ribbon-needed ( box -- n )
  [ smallest-perimeter ] [ cubic-volume ] bi +
;

: part1 ( -- n )
  input
  [ line>box paper-needed ] map
  sum
;

: part2 ( -- n )
  input
  [ line>box ribbon-needed ] map
  sum
;
