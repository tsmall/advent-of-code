var floor = 0
var basement = -1
var index = 1

def loop():
    let input = get_line("")
    for (input) c:
        floor += switch c:
            case '(': 1
            case ')': -1
            default: 0
        if (basement == -1) and (floor < 0):
            basement = index
        else:
            index = index + 1
    return length(input)

var continue = 1
while continue:
    continue = loop()

print "Part 1: " + floor
print "Part 2: " + basement
