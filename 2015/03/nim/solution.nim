import options
import sets
import streams

# Direction --------------------------------------------------------------------

type Direction = enum
  north, south, east, west

proc parse(direction: char): Option[Direction] =
  case direction
  of '^':
    some(north)
  of 'v':
    some(south)
  of '>':
    some(east)
  of '<':
    some(west)
  else:
    none(Direction)


# Point ------------------------------------------------------------------------

type Point = tuple[x, y: int]

let originPoint: Point =
  (0, 0)

proc move(point: var Point, direction: Direction) =
  case direction
  of north:
    point.y += 1
  of south:
    point.y -= 1
  of east:
    point.x += 1
  of west:
    point.x -= 1


# Tracker ----------------------------------------------------------------------

type Tracker = ref object of RootObj
  houses: ref HashSet[Point]

proc newTracker: Tracker =
  let houses = new HashSet[Point]
  houses[].init()
  Tracker(houses: houses)

proc recordVisit(t: Tracker, p: Point) =
  t.houses[].incl(p)

proc housesVisited(t: Tracker): int =
  t.houses[].len


# Santa ------------------------------------------------------------------------

type Santa = ref object of RootObj
  position: Point
  tracker: Tracker

proc move(s: var Santa, dir: Direction) =
  s.position.move(dir)
  s.tracker.recordVisit(s.position)

proc newSanta(t: Tracker): Santa =
  result = Santa(position: originPoint, tracker: t)
  t.recordVisit(result.position)


# Main -------------------------------------------------------------------------

iterator directions: Direction {.inline.} =
  var
    direction: Option[Direction]
    input = newFileStream(stdin)
  while not input.atEnd:
    direction = input.readChar().parse()
    if direction.isSome:
      yield direction.get

proc main =
  var
    loneTracker = newTracker()
    loneSanta = newSanta(loneTracker)
    pairTracker = newTracker()
    pairSanta = newSanta(pairTracker)
    roboSanta = newSanta(pairTracker)
    currentSanta = pairSanta

  proc swap(s: var Santa) =
    if s == pairSanta:
      s = roboSanta
    else:
      s = pairSanta

  for direction in directions():
    loneSanta.move(direction)
    currentSanta.move(direction)
    swap(currentSanta)

  echo "Houses Visited (Part One): " & $loneTracker.housesVisited
  echo "Houses Visited (Part Two): " & $pairTracker.housesVisited

main()
