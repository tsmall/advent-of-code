import std

def each_line(f):
    var line = ""
    while (line = get_line("")) and line.length > 0:
        f(line)

class slope:
    dx:int
    dy:int
    x:int = 0
    y:int = 0
    hits:int = 0

let slopes = [
    slope { dx: 1, dy: 1 },
    slope { dx: 3, dy: 1 },
    slope { dx: 5, dy: 1 },
    slope { dx: 7, dy: 1 },
    slope { dx: 1, dy: 2 },
]

each_line() line:
    for(slopes) slope:
        let index = slope.x % line.length
        let symbol = line[index]
        if (slope.y % slope.dy == 0) and symbol == '#':
            slope.hits++
        slope.x += slope.dx
        slope.y += 1

print "Part 1: " + slopes[1].hits
print "Part 2: " + product(map(slopes): _.hits)
