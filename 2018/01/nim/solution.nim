import std/intsets
import std/math
import std/strformat
import std/strutils


proc loadInput(): seq[int] =
  result = newSeq[int]()
  while not endOfFile(stdin):
    let line = readLine(stdin)
    let change = parseInt(line)
    result.add(change)


iterator repeatForever[A](xs: openarray[A]): A =
  while true:
    for x in xs:
      yield x


iterator allFrequencies(changes: openarray[int]): int =
  var frequency = 0
  for change in repeatForever(changes):
    frequency += change
    yield frequency


proc findFirstRepeat(changes: openarray[int]): int =
  var frequenciesSeen = initIntSet()
  for frequency in allFrequencies(changes):
    if frequency in frequenciesSeen:
      return frequency
    else:
      frequenciesSeen.incl(frequency)


proc main() =
  let
    changes = loadInput()
    finalFrequency = changes.sum()
    firstRepeatFrequency = findFirstRepeat(changes)

  echo fmt"Part 1: {finalFrequency}"
  echo fmt"Part 2: {firstRepeatFrequency}"


main()
