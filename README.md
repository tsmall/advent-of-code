# Advent of Code Solutions


This project contains my solutions to the [Advent of Code][aoc] exercises.


**Contents**

- [Project Organization](#project-organization)
- [Program Design](#program-design)





<a id="project-organization"></a>
## Project Organization


The project is organized like this:

    <root>
      |
      + -- <year>
      |      |
      |      +-- <day>
      |            |
      |            +-- problem.txt
      |            |
      |            +-- input.txt
      |            |
      |            +-- <language>
      |
      +-- lib
           |
           +-- <language>





<a id="program-design"></a>
## Program Design


Each program is designed to follow the [Unix Philosophy][unix].
They are standalone, command-line programs
that follow these rules:

- Read input from `stdin`
- Print output to `stdout`
- Print error information to `stderr`
- Print debugging info to `stderr` if `-v` or `--verbose` flag is present

Since the programs take input from `stdin`,
you can use your shell's input redirection
or a program like `cat`
to feed them input:

    $ cat input.txt | <program>





<!-- References -->

[aoc]: https://adventofcode.com
[unix]: https://en.wikipedia.org/wiki/Unix_philosophy
