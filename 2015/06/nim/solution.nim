import options
import parseutils
import sets
import streams
import strformat
import strscans
import tables except rightSize

type
  Action = enum
    TurnOn
    TurnOff
    Toggle
  Coordinate = tuple
    x, y: int
  Instruction = object
    action: Action
    first, last: Coordinate

# ------------------------------------------------------------------------------
# Parsing

proc action(input: string, act: var Action; start: int): int =
  var i, j: int
  i = skip(input, "toggle", start = start)
  if i != 0:
    act = Toggle
    return i
  i = skip(input, "turn ", start = start)
  j = skip(input, "on", start = i)
  if j != 0:
    act = TurnOn
    return i + j
  j = skip(input, "off", start = i)
  if j != 0:
    act = TurnOff
    return i + j
  return 0

proc parse(line: string): Option[Instruction] =
  var parsed: Instruction
  if scanf(line, "${action} $i,$i through $i,$i",
           parsed.action,
           parsed.first.x, parsed.first.y,
           parsed.last.x, parsed.last.y):
    result = some(parsed)


# ------------------------------------------------------------------------------
# Part 1

proc coordSet(first, last: Coordinate): HashSet[Coordinate] =
  for y in first.y .. last.y:
    for x in first.x .. last.x:
      let coord = (x, y)
      result.incl(coord)

proc part1(instruction: Instruction, onLights: var HashSet[Coordinate]) =
  let coords = coordSet(instruction.first, instruction.last)
  case instruction.action
  of Toggle:
    let
      nowOffLights = intersection(coords, onLights)
      nowOnLights = difference(coords, nowOffLights)
    onLights.excl(nowOffLights)
    onLights.incl(nowOnLights)
  of TurnOn:
    onLights.incl(coords)
  of TurnOff:
    onLights.excl(coords)
    

# ------------------------------------------------------------------------------
# Part 2

type
  Brightnesses = Table[Coordinate, int]

proc inc(table: var Brightnesses; key: Coordinate; val: Positive = 1) =
  table[key] = table.getOrDefault(key, 0) + val

proc dec(table: var Brightnesses; key: Coordinate; val: Positive = 1) =
  let newVal = table.getOrDefault(key, 0) - val
  if newVal > 0:
    table[key] = newVal
  else:
    table.del(key)

proc sum(table: Brightnesses): int =
  result = 0
  for v in table.values():
    result += v

proc part2(instruction: Instruction, brightnesses: var Brightnesses) =
  let coords = coordSet(instruction.first, instruction.last)
  case instruction.action
  of Toggle:
    for c in coords:
      brightnesses.inc(c, 2)
  of TurnOn:
    for c in coords:
      brightnesses.inc(c)
  of TurnOff:
    for c in coords:
      brightnesses.dec(c)


# ------------------------------------------------------------------------------
# Main

const
  lightCount = 1_000_000

proc main() =
  var
    onLights = initHashSet[Coordinate](rightSize(lightCount))
    brightnesses = initTable[Coordinate, int](rightSize(lightCount))
  let input = newFileStream(stdin)
  for line in input.lines:
    let instruction = parse(line)
    echo "Instruction: ", instruction
    if instruction.isSome:
      instruction.get.part1(onLights)
      instruction.get.part2(brightnesses)
  echo fmt"Part 1: {onLights.len} lights on"
  echo fmt"Part 2: {brightnesses.sum()} total brightness"

main()
