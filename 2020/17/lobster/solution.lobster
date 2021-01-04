import set
import std


struct cube : xyz_i

    def neighbors():
        let r = []
        for(neighbor_diffs()) diff:
            r.push(this + diff)
        return r

    private def neighbor_diffs():
        let range = [-1, 0, 1]
        let r = []
        for(range) dx:
            for(range) dy:
                for(range) dz:
                    if(dx != 0 or dy != 0 or dz != 0):
                        r.push(cube { dx, dy, dz })
        return r


def neighbors_on(c, on_cubes):
    var count = 0
    for(c.neighbors) n:
        if(on_cubes.contains(n)):
            count++
    return count


enum state:
    on
    off


def next_cycle(current, next):
    current.each() c:
        for(c.neighbors) n:
            if(next_state(n, current) == on):
                next.add(n)
        if(next_state(c, current) == on):
            next.add(c)


def next_state(c, on_cubes):
    let n = neighbors_on(c, on_cubes)
    if(on_cubes.contains(c)):
        if(n == 2 or n == 3): return on else: return off
    else:
        if(n == 3): return on else: return off


def each_line(f):
    var line = ""
    while (line = get_line("")) and line.length > 0:
        f(line)


def parse_input():
    let r = set<cube> { 1009 }
    let z = 0
    var y = 0
    each_line() line:
        for(line) c, x:
            if(c == '#'):
                r.add(cube { x, y, z })
        y++
    return r


var current = set<cube> { 1009 }
var next = parse_input()

for(7) cycle:
    current = next
    next = set<cube> { 1009 }
    next_cycle(current, next)

print "Part 1: " + current.cardinality
