import streams
import tables


# ------------------------------------------------------------------------------
# Part 1

proc disallowed(prev, curr: char): bool =
  (prev == 'a' and curr == 'b') or
  (prev == 'c' and curr == 'd') or
  (prev == 'p' and curr == 'q') or
  (prev == 'x' and curr == 'y')

const vowels = {'a', 'e', 'i', 'o', 'u'}

proc isVowelAscii(c: char): bool =
  c in vowels

proc countPart1(count: var int, str: string) =
  var
    prev:     char = '\0'
    nvowels:  int8 = 0
    ndoubles: int8 = 0
  for curr in str:
    if disallowed(prev, curr):
      return
    if curr.isVowelAscii:
      nvowels.inc()
    if prev == curr:
      ndoubles.inc()
    prev = curr
  if nvowels >= 3 and ndoubles >= 1:
    count.inc()


# ------------------------------------------------------------------------------
# Part 2

type
  Pair = tuple[prev, curr: char]

proc countPart2(count: var int, str: string) =
  var
    prev2:    char = '\0'
    prev1:    char = '\0'
    ntriples: int8 = 0
    npairs:   int8 = 0
    pairSeen       = initTable[Pair, int]()
  for i, curr in str:
    if prev2 == curr:
      ntriples.inc()
    let pair = (prev1, curr)
    if pair in pairSeen:
      if i - pairSeen[pair] > 1:
        npairs.inc()
        pairSeen[pair] = i
    else:
      pairSeen[pair] = i
    prev2 = prev1
    prev1 = curr
  if ntriples >= 1 and npairs >= 1:
    count.inc()


# ------------------------------------------------------------------------------
# Main

proc main() =
  var
    input  = newFileStream(stdin)
    count1 = 0
    count2 = 0
  for str in input.lines:
    countPart1(count1, str)
    countPart2(count2, str)
  echo "Part 1: ", count1
  echo "Part 2: ", count2

main()
