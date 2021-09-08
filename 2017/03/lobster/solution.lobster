import dictionary
import std
import stdtype

let input = 289326

def for_all_squares(f):
    var curr = xy { 0, 0 }
    var step = xy { 1, 0 }
    var dest = xy { 1, 0 }
    var location = 1
    while true:
        f(curr, location)
        curr += step
        location += 1
        if curr == dest:
            step, dest = next_step_and_dest(step, dest)

def find_coord_for_location(target):
    for_all_squares() coord, location:
        if location == target:
            return coord
    fatal_exit("Never found location: " + target)

def next_step_and_dest(step, dest):
    if   step == xy {  1,  0 }: return xy {  0,  1 }, xy { dest.x,          dest.x      }
    elif step == xy {  0,  1 }: return xy { -1,  0 }, xy { dest.x * -1,     dest.y      }
    elif step == xy { -1,  0 }: return xy {  0, -1 }, xy { dest.x,          dest.y * -1 }
    elif step == xy {  0, -1 }: return xy {  1,  0 }, xy { dest.x * -1 + 1, dest.y      }
    else: fatal_exit("Unexpected step: " + step)

def manhattan_distance(xy):
    return abs(xy.x) + abs(xy.y)

def neighbors(xy):
    return [
      xy { xy.x - 1, xy.y - 1 },  xy { xy.x, xy.y - 1 },  xy { xy.x + 1, xy.y - 1 },
      xy { xy.x - 1, xy.y     },                          xy { xy.x + 1, xy.y     },
      xy { xy.x - 1, xy.y + 1 },  xy { xy.x, xy.y + 1 },  xy { xy.x + 1, xy.y + 1 }
  ]

def find_first_value_larger_than(num):
    let values = dictionary<xy_i, int> { 1009 }
    values.set(xy_i { 0, 0 }, 1)
    for_all_squares() coord, location:
        if coord != xy_i { 0, 0 }:
            var sum = 0
            for(neighbors(coord)) neighbor:
                sum += values.get(neighbor, 0)
            values.set(coord, sum)
            if sum > num:
                return sum

print "Part 1: " + manhattan_distance(find_coord_for_location(input))
print "Part 2: " + find_first_value_larger_than(input)
