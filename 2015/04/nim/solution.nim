import md5
import strutils

const
  prefix_1 = "00000"
  prefix_2 = "000000"

type
  Flag {.size: sizeof(int8).} = enum
    Part1
    Part2
  Flags = set[Flag]

proc main() =
  let secretKey = stdin.readLine()
  var
    answer: uint32 = 1
    found:  Flags
  while card(found) < 2:
    let
      str  = secretKey & $answer
      hash = getMD5(str)
    if Part1 notin found and hash.startsWith(prefix_1):
      echo "Part 1: ", answer
      found.incl(Part1)
    if Part2 notin found and hash.startsWith(prefix_2):
      echo "Part 2: ", answer
      found.incl(Part2)
    answer.inc()

main()
