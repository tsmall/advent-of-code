import std/options
import std/strformat
import std/strutils
import std/tables


proc containsValue[A](t: CountTable[A], value: int): bool =
  result = false
  for v in t.values():
    if v == value:
      return true


proc partOne(ids: openArray[string]): int =
  var
    twoCount = 0
    threeCount = 0

  for id in ids:
    let counts = id.toCountTable
    if counts.containsValue(2): inc twoCount
    if counts.containsValue(3): inc threeCount

  return twoCount * threeCount


proc findOnlyDifferingChar(a, b: string): Option[char] =
  var
    diffCount = 0
    diffChar: char

  for i, c in a:
    if b[i] != c:
      diffChar = c
      inc diffCount
      if diffCount > 1:
        break

  if diffCount == 1:
    return some(diffChar)


proc removeFirst(s: string, c: char): string =
  let index = s.find(c)
  if index == -1:
    return s

  result = s
  result.delete(index..index)


iterator combinations[A](xs: openArray[A]): (A, A) =
  for i in 0 ..< xs.len:
    for j in (i + 1) ..< xs.len:
      yield (xs[i], xs[j])


proc partTwo(ids: openArray[string]): string =
  for a, b in combinations(ids):
    let result = findOnlyDifferingChar(a, b)
    if result.isSome:
      return a.removeFirst(result.get)


proc input(): seq[string] =
  while not endOfFile(stdin):
    let id = readLine(stdin)
    result.add(id)


proc main() =
  let ids = input()
  echo fmt"Part 1: {partOne(ids)}"
  echo fmt"Part 2: {partTwo(ids)}"


main()
