import math
import streams
import strscans

type
  SqFt = int
  Box = tuple[length, width, height: int]
  ParseError = object of Exception

proc parse(box: string): Box =
  var l, w, h: int
  if scanf(box, "$ix$ix$i", l, w, h):
    result = (l, w, h)
  else:
    raise newException(ParseError, "Cannot parse: " & box)

proc paperNeeded(box: Box): SqFt =
  let areas = [
    box.length * box.width,
    box.width * box.height,
    box.height * box.length
  ]
  result = (2 * areas.sum) + areas.min

proc ribbonNeeded(box: Box): int =
  let perimeters = [
    2 * (box.length + box.width),
    2 * (box.width + box.height),
    2 * (box.height + box.length)
  ]
  let volume = box.length * box.width * box.height
  result = perimeters.min + volume

proc main =
  var
    input = newFileStream(stdin)
    totalPaper: int
    totalRibbon: int
    box: Box
  for line in input.lines:
    box = parse(line)
    totalPaper += paperNeeded(box)
    totalRibbon += ribbonNeeded(box)
  echo "Part 1: ", totalPaper
  echo "Part 2: ", totalRibbon

main()
